/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <string.h>
#include <stdint.h>
#include <sys/types.h>
#ifdef __x86_64__
#include <emmintrin.h>
#include <xmmintrin.h>
#include <immintrin.h>
#include <cpuid.h>
#endif
#include <stdbool.h>

#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>
#endif

bool has_avx512_vl_bw() {
#if defined(__x86_64__) && (__GNUC__ >= 6 || defined(__clang_major__))
  uint32_t eax = 0, ebx = 0, ecx = 0, edx = 0;
  __get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx);
  // https://en.wikipedia.org/wiki/CPUID#EAX=7,_ECX=0:_Extended_Features
  const bool has_avx512_bw = ebx & (1 << 30);
  const bool has_avx512_vl = ebx & (1 << 31);
  // printf("cpuid=%d=cpuid\n", has_avx512_bw && has_avx512_vl);
  return has_avx512_bw && has_avx512_vl;
#else
  return false;
#endif
}

/*
  measure_off_naive / measure_off_avx / measure_off_sse
  take a UTF-8 sequence between src and srcend, and a number of characters cnt.
  If the sequence is long enough to contain cnt characters, then return how many bytes
  remained unconsumed. Otherwise, if the sequence is shorter, return
  negated count of lacking characters. Cf. _hs_text_measure_off below.
*/

static inline const ssize_t measure_off_naive(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
  // Count leading bytes in 8 byte sequence
  while (src < srcend - 7){
    uint64_t w64;
    memcpy(&w64, src, sizeof(uint64_t));
    size_t leads = __builtin_popcountll(((w64 << 1) | ~w64) & 0x8080808080808080ULL);
    if (cnt < leads) break;
    cnt-= leads;
    src+= 8;
  }

  // Skip until next leading byte
  while (src < srcend){
    uint8_t w8 = *src;
    if ((int8_t)w8 >= -0x40) break;
    src++;
  }

  // Finish up with tail
  while (src < srcend && cnt > 0){
    uint8_t leadByte = *src++;
    cnt--;
    src+= (leadByte >= 0xc0) + (leadByte >= 0xe0) + (leadByte >= 0xf0);
  }

  return cnt == 0 ? (ssize_t)(srcend - src) : (ssize_t)(- cnt);
}

#ifdef __x86_64__
__attribute__((target("avx512vl,avx512bw")))
static const ssize_t measure_off_avx(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
  while (src < srcend - 63){
    __m512i w512 = _mm512_loadu_si512((__m512i *)src);
    // Which bytes are either < 128 or >= 192?
    uint64_t mask = _mm512_cmpgt_epi8_mask(w512, _mm512_set1_epi8(0xBF));
    size_t leads = __builtin_popcountll(mask);
    if (cnt < leads) break;
    cnt-= leads;
    src+= 64;
  }

  // Cannot proceed to measure_off_sse, because of AVX-SSE transition penalties
  // https://software.intel.com/content/www/us/en/develop/articles/avoiding-avx-sse-transition-penalties.html

  if (src < srcend - 31){
    __m256i w256 = _mm256_loadu_si256((__m256i *)src);
    uint32_t mask = _mm256_cmpgt_epi8_mask(w256, _mm256_set1_epi8(0xBF));
    size_t leads = __builtin_popcountl(mask);
    if (cnt >= leads){
      cnt-= leads;
      src+= 32;
    }
  }

  if (src < srcend - 15){
    __m128i w128 = _mm_maskz_loadu_epi16(0xFF, (__m128i *)src); // not _mm_loadu_si128; and GCC does not have _mm_loadu_epi16
    uint16_t mask = _mm_cmpgt_epi8_mask(w128, _mm_set1_epi8(0xBF)); // not _mm_movemask_epi8
    size_t leads = __builtin_popcountl(mask);
    if (cnt >= leads){
      cnt-= leads;
      src+= 16;
    }
  }

  return measure_off_naive(src, srcend, cnt);
}
#endif

static const ssize_t measure_off_sse(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
#ifdef __x86_64__
  while (src < srcend - 15){
    __m128i w128 = _mm_loadu_si128((__m128i *)src);
    // Which bytes are either < 128 or >= 192?
    uint16_t mask = _mm_movemask_epi8(_mm_cmpgt_epi8(w128, _mm_set1_epi8(0xBF)));
    size_t leads = __builtin_popcount(mask);
    if (cnt < leads) break;
    cnt-= leads;
    src+= 16;
  }
#endif

  return measure_off_naive(src, srcend, cnt);
}

typedef const ssize_t (*measure_off_t) (const uint8_t*, const uint8_t*, size_t);

/*
  _hs_text_measure_off takes a UTF-8 encoded buffer, specified by (src, off, len),
  and a number of code points (aka characters) cnt. If the buffer is long enough
  to contain cnt characters, then _hs_text_measure_off returns a non-negative number,
  measuring their size in code units (aka bytes). If the buffer is shorter,
  _hs_text_measure_off returns a non-positive number, which is a negated total count
  of characters available in the buffer. If len = 0 or cnt = 0, this function returns 0
  as well.

  This scheme allows us to implement both take/drop and length with the same C function.

  The input buffer (src, off, len) must be a valid UTF-8 sequence,
  this condition is not checked.
*/
ssize_t _hs_text_measure_off(const uint8_t *src, size_t off, size_t len, size_t cnt) {
  static _Atomic measure_off_t s_impl = (measure_off_t)NULL;
  measure_off_t impl = atomic_load_explicit(&s_impl, memory_order_relaxed);
  if (!impl) {
#ifdef __x86_64__
    impl = has_avx512_vl_bw() ? measure_off_avx : measure_off_sse;
#else
    impl = measure_off_sse;
#endif
    atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
  }
  ssize_t ret = (*impl)(src + off, src + off + len, cnt);
  return ret >= 0 ? ((ssize_t)len - ret) : (- (cnt + ret));
}

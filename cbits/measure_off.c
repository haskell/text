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

// stdatomic.h has been introduces in gcc 4.9
#if !(__GNUC__ >= 5 || __GNUC__ == 4 && __GNUC_MINOR__ >= 9 || defined(__clang_major__))
#define __STDC_NO_ATOMICS__
#endif

#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>
#endif

/*
  Clang-6 does not enable proper -march flags for assembly modules
  which leads to "error: instruction requires: AVX-512 ISA"
  at the assembler phase.

  Apple LLVM version 10.0.0 (clang-1000.11.45.5) is based on clang-6
  https://en.wikipedia.org/wiki/Xcode#Toolchain_versions
  and it's the latest available version on macOS 10.13.

  Disable AVX-512 instructions as they are most likely not supported
  on the hardware running clang-6.
*/
#if !((defined(__apple_build_version__) && __apple_build_version__ <= 10001145) \
      || (defined(__clang_major__) && __clang_major__ <= 6)) && !defined(__STDC_NO_ATOMICS__)
#define COMPILER_SUPPORTS_AVX512
#endif


#if defined(__x86_64__) && defined(COMPILER_SUPPORTS_AVX512)
bool has_avx512_vl_bw() {
#if (__GNUC__ >= 7 || __GNUC__ == 6 && __GNUC_MINOR__ >= 3) || defined(__clang_major__)
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
#endif

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
    // find leading bytes by finding every byte that is not a continuation
    // byte. The bit twiddle only results in a 0 if the original byte starts
    // with 0b11...
    w64 =  ((w64 << 1) | ~w64) & 0x8080808080808080ULL;
    // compute the popcount of w64 with two bit shifts and a multiplication
    size_t leads = (  (w64 >> 7)              // w64 >> 7           = Sum{0<= i <= 7} x_i * 256^i    (x_i \in {0,1})
                    * (0x0101010101010101ULL) // 0x0101010101010101 = Sum{0<= i <= 7} 256^i
                                              //              (Sum{0<= i <= 7} x_i * 256^i) * (Sum{0<= j <= 7} 256^j) 
                                              // =(mod 256^8) (Sum{0<= k <= 7} (256^k) * (Sum {0 <= l < 7} x_l) 
                                              // as the coefficients of 256^k in the result are the x_i such that i+j =(mod 8) k
                                              // and each i satisfies this equation for exactly one such j
                                              // So each byte of the result contains the sum we want.
                   ) >> 56; // bit shift to get a single byte which contains Sum {0 <= j < 7} x_j
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

#if defined(__x86_64__) && defined(COMPILER_SUPPORTS_AVX512)
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

/* Count the number of bits set in the argument 
 * 
   This is a temporary workaround for the fact that
   the GHC RTS linker does not recognize the `__builtin_popcountll`
   symbol.
 
   See https://gitlab.haskell.org/ghc/ghc/-/issues/21787
       https://gitlab.haskell.org/ghc/ghc/-/issues/19900
       https://github.com/haskell/text/issues/450
 
   It can be removed and the usages of popcount64 replaced with
   `__builtin_popcountll` once affected versions of the compiler
   are no longer in widespread use.
 
   Once GHC learns to recognize the symbol, this can be gated
   by CPP to call __builtin_popcountll when using the appropriate
   version of GHC.
*/
static inline const size_t popcount16(uint16_t x) {

  // Taken from https://en.wikipedia.org/wiki/Hamming_weight
  const uint16_t m1  = 0x5555; //binary: 0101...
  const uint16_t m2  = 0x3333; //binary: 00110011..
  const uint16_t m4  = 0x0f0f; //binary:  4 zeros,  4 ones ...
  x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
  x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits 
  x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits 
  return (x >> 8) + (x & 0x00FF);
}

static const ssize_t measure_off_sse(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
#ifdef __x86_64__
  while (src < srcend - 15){
    __m128i w128 = _mm_loadu_si128((__m128i *)src);
    // Which bytes are either < 128 or >= 192?
    uint16_t mask = _mm_movemask_epi8(_mm_cmpgt_epi8(w128, _mm_set1_epi8(0xBF)));
    size_t leads = popcount16(mask);
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
#ifndef __STDC_NO_ATOMICS__
  static _Atomic measure_off_t s_impl = (measure_off_t)NULL;
  measure_off_t impl = atomic_load_explicit(&s_impl, memory_order_relaxed);
  if (!impl) {
#if defined(__x86_64__) && defined(COMPILER_SUPPORTS_AVX512)
    impl = has_avx512_vl_bw() ? measure_off_avx : measure_off_sse;
#else
    impl = measure_off_sse;
#endif
    atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
  }
#else
  measure_off_t impl = measure_off_sse;
#endif
  ssize_t ret = (*impl)(src + off, src + off + len, cnt);
  return ret >= 0 ? ((ssize_t)len - ret) : (- (cnt + ret));
}

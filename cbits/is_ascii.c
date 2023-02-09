/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <string.h>
#include <stdint.h>
#include <sys/types.h>
#ifdef __x86_64__
#include <emmintrin.h>
#include <xmmintrin.h>
#endif
#include <stdbool.h>

/*
  _hs_text_is_ascii takes a UTF-8 encoded buffer,
  and returns the length of the ASCII-compatible prefix.
*/
const size_t _hs_text_is_ascii(const uint8_t *src0, const uint8_t *srcend){
  const uint8_t *src = src0;

#ifdef __x86_64__
  // I experimented with larger vector registers,
  // but did not notice any measurable speed up, so let's keep it simple.
  while (src < srcend - 15){
    __m128i w128 = _mm_loadu_si128((__m128i *)src);
    // Which bytes are < 128?
    uint16_t mask = _mm_movemask_epi8(w128);
    if (mask) break;
    src+= 16;
  }
#endif

  while (src < srcend - 7){
    uint64_t w64;
    memcpy(&w64, src, sizeof(uint64_t));
    if (w64 & 0x8080808080808080ULL) break;
    src+= 8;
  }

  while (src < srcend){
    uint8_t leadByte = *src;
    if(leadByte >= 0x80) break;
    src++;
  }

  return src - src0;
}

/*
  _hs_text_is_ascii_offset is a helper for calling _hs_text_is_ascii on Texts.
*/
const size_t _hs_text_is_ascii_offset(const uint8_t *arr, size_t off, size_t len){
    return _hs_text_is_ascii(arr + off, arr + off + len);
}

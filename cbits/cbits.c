/*
 * Copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 *
 * Portions copyright (c) 2008-2010 Björn Höhrmann <bjoern@hoehrmann.de>.
 *
 * See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
 */

#include <string.h>
#include <stdint.h>
#include <stdio.h>
#if defined(__x86_64__)
#include <emmintrin.h>
#include <xmmintrin.h>
#endif

#include "text_cbits.h"

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  /*
   * The first part of the table maps bytes to character classes that
   * to reduce the size of the transition table and create bitmasks.
   */
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  /*
   * The second part is a transition table that maps a combination of
   * a state of the automaton and a character class to a state.
   */
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
};

static inline uint32_t
decode(uint32_t *state, uint32_t* codep, uint32_t byte) {
  uint32_t type = utf8d[byte];

  *codep = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte);

  return *state = utf8d[256 + *state + type];
}

size_t
_hs_text_decode_latin1(uint8_t *dest, const uint8_t *src,
                       const uint8_t *srcend)
{
  const uint8_t *dest0 = dest;
  const uint8_t *p = src;

  while (p != srcend){
    uint8_t codepoint = *p++;
    if(codepoint < 0x80){
      *dest++ = (uint8_t)codepoint;
    } else {
      *dest++ = (uint8_t) (0xC0 + (codepoint >> 6));
      *dest++ = (uint8_t) (0x80 + (codepoint & 0x3F));
    }
  }

  return (dest - dest0);
}

/*
 * A best-effort decoder. Runs until it hits either end of input or
 * the start of an invalid byte sequence.
 *
 * At exit, we update *destoff with the next offset to write to, *src
 * with the next source location past the last one successfully
 * decoded, and return the next source location to read from.
 *
 * Moreover, we expose the internal decoder state (state0 and
 * codepoint0), allowing one to restart the decoder after it
 * terminates (say, due to a partial codepoint).
 *
 * In particular, there are a few possible outcomes,
 *
 *   1) We decoded the buffer entirely:
 *      In this case we return srcend
 *      state0 == UTF8_ACCEPT
 *
 *   2) We met an invalid encoding
 *      In this case we return the address of the first invalid byte
 *      state0 == UTF8_REJECT
 *
 *   3) We reached the end of the buffer while decoding a codepoint
 *      In this case we return a pointer to the first byte of the partial codepoint
 *      state0 != UTF8_ACCEPT, UTF8_REJECT
 *
 */
#if defined(__GNUC__) || defined(__clang__)
static inline uint8_t const *
_hs_text_decode_utf8_int(uint8_t *const dest, size_t *destoff,
			 const uint8_t **src, const uint8_t *srcend,
			 uint32_t *codepoint0, uint32_t *state0)
  __attribute((always_inline));
#endif

static inline uint8_t const *
_hs_text_decode_utf8_int(uint8_t *const dest, size_t *destoff,
			 const uint8_t **src, const uint8_t *srcend,
			 uint32_t *codepoint0, uint32_t *state0)
{
  uint8_t *d = dest + *destoff;
  const uint8_t *s = *src, *last = *src;
  uint32_t state = *state0;
  uint32_t codepoint = *codepoint0;

  while (s < srcend) {
    if (decode(&state, &codepoint, *s++) != UTF8_ACCEPT) {
      if (state != UTF8_REJECT)
	      continue;
      break;
    }

    if(codepoint < 0x80){
      *d++ = (uint8_t) codepoint;
    } else if(codepoint < 0x800){
      *d++ = (uint8_t) (0xC0 + (codepoint >> 6));
      *d++ = (uint8_t) (0x80 + (codepoint & 0x3F));
    } else if(codepoint < 0x10000){
      *d++ = (uint8_t) (0xE0 + (codepoint >> 12));
      *d++ = (uint8_t) (0x80 + ((codepoint >> 6) & 0x3F));
      *d++ = (uint8_t) (0x80 + (codepoint & 0x3F));
    } else {
      *d++ = (uint8_t) (0xF0 + (codepoint >> 18));
      *d++ = (uint8_t) (0x80 + ((codepoint >> 12) & 0x3F));
      *d++ = (uint8_t) (0x80 + ((codepoint >> 6) & 0x3F));
      *d++ = (uint8_t) (0x80 + (codepoint & 0x3F));
    }

    last = s;
  }

  *destoff = d - dest;
  *codepoint0 = codepoint;
  *state0 = state;
  *src = last;

  return s;
}

uint8_t const *
_hs_text_decode_utf8_state(uint8_t *const dest, size_t *destoff,
                           const uint8_t **src,
                           const uint8_t *srcend,
                           uint32_t *codepoint0, uint32_t *state0)
{
  _hs_text_decode_utf8_int(dest, destoff, src, srcend, codepoint0, state0);

  return *src;
}

/*
 * Helper to decode buffer and discard final decoder state
 */
const uint8_t *
_hs_text_decode_utf8(uint8_t *const dest, size_t *destoff,
                     const uint8_t *src, const uint8_t *const srcend)
{
  uint32_t codepoint;
  uint32_t state = UTF8_ACCEPT;
  _hs_text_decode_utf8_int(dest, destoff, &src, srcend,
                          &codepoint, &state);
  return src;
}

/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <string.h>
#include <sys/types.h>
#include <arm_neon.h>

/*
  measure_off_naive / measure_off_neon
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

static inline const ssize_t measure_off_neon(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
  while (src < srcend - 63){
    uint8x16_t w128[4] = {vld1q_u8(src), vld1q_u8(src + 16), vld1q_u8(src + 32), vld1q_u8(src + 48)};
    // Which bytes are either < 128 or >= 192?
    uint8x16_t mask0 = vcgtq_s8((int8x16_t)w128[0], vdupq_n_s8(0xBF));
    uint8x16_t mask1 = vcgtq_s8((int8x16_t)w128[1], vdupq_n_s8(0xBF));
    uint8x16_t mask2 = vcgtq_s8((int8x16_t)w128[2], vdupq_n_s8(0xBF));
    uint8x16_t mask3 = vcgtq_s8((int8x16_t)w128[3], vdupq_n_s8(0xBF));

    uint8x16_t mask01 = vaddq_u8(mask0, mask1);
    uint8x16_t mask23 = vaddq_u8(mask2, mask3);
    uint8x16_t mask = vaddq_u8(mask01, mask23);

    size_t leads = (size_t)(-vaddvq_s8((int8x16_t)mask));

    if (cnt < leads) break;
    cnt-= leads;
    src+= 64;
  }

  while (src < srcend - 15){
    uint8x16_t w128 = vld1q_u8(src);
    // Which bytes are either < 128 or >= 192?
    uint8x16_t mask = vcgtq_s8((int8x16_t)w128, vdupq_n_s8(0xBF));
    size_t leads = (size_t)(-vaddvq_s8((int8x16_t)mask));
    if (cnt < leads) break;
    cnt-= leads;
    src+= 16;
  }

  return measure_off_naive(src, srcend, cnt);
}

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
  ssize_t ret = measure_off_neon(src + off, src + off + len, cnt);
  return ret >= 0 ? ((ssize_t)len - ret) : (- (cnt + ret));
}

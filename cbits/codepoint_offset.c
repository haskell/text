
#include <string.h>
#include <stdint.h>
#include <sys/types.h>
#ifdef __x86_64__
#include <emmintrin.h>
#include <xmmintrin.h>
#endif
#include <stdbool.h>
#include <sys/cdefs.h>

// The following is from FreeBSD's memmem.c
// https://github.com/freebsd/freebsd-src/blob/9921563f43a924d21c7bf43db4a34e724577db95/lib/libc/string/memmem.c

/*-
 * SPDX-License-Identifier: MIT
 *
 * Copyright (c) 2005-2014 Rich Felker, et al, 2022 Alex Mason.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

static uint8_t *
twobyte_memmem(const uint8_t *h, size_t hlen, const uint8_t *n)
{
	uint16_t nw = n[0] << 8 | n[1], hw = h[0] << 8 | h[1];
	for (h += 2, hlen -= 2; hlen; hlen--, hw = hw << 8 | *h++)
		if (hw == nw)
			return (uint8_t *)h - 2;
	return hw == nw ? (uint8_t *)h - 2 : 0;
}

static uint8_t *
threebyte_memmem(const uint8_t *h, size_t hlen, const uint8_t *n)
{
	uint32_t nw = (uint32_t)n[0] << 24 | n[1] << 16 | n[2] << 8;
	uint32_t hw = (uint32_t)h[0] << 24 | h[1] << 16 | h[2] << 8;
	for (h += 3, hlen -= 3; hlen; hlen--, hw = (hw | *h++) << 8)
		if (hw == nw)
			return (uint8_t *)h - 3;
	return hw == nw ? (uint8_t *)h - 3 : 0;
}

static uint8_t *
fourbyte_memmem(const uint8_t *h, size_t hlen, const uint8_t *n)
{
	uint32_t nw = (uint32_t)n[0] << 24 | n[1] << 16 | n[2] << 8 | n[3];
	uint32_t hw = (uint32_t)h[0] << 24 | h[1] << 16 | h[2] << 8 | h[3];
	for (h += 4, hlen -= 4; hlen; hlen--, hw = hw << 8 | *h++)
		if (hw == nw)
			return (uint8_t *)h - 4;
	return hw == nw ? (uint8_t *)h - 4 : 0;
}

static int _hs_codepoint_to_utf8(uint8_t asUtf8[4], uint32_t codepoint)
{

	if (codepoint < 0x80)
	{
		asUtf8[0] = codepoint;
		return 1;
	}
	else if (codepoint < 0x0800)
	{
		asUtf8[0] = (uint8_t)(((codepoint >> 6) & 0x1F) | 0xC0);
		asUtf8[1] = (uint8_t)(((codepoint >> 0) & 0x3F) | 0x80);
		return 2;
	}
	else if (codepoint < 0x10000)
	{
		asUtf8[0] = (uint8_t)(((codepoint >> 12) & 0x0F) | 0xE0);
		asUtf8[1] = (uint8_t)(((codepoint >> 6) & 0x3F) | 0x80);
		asUtf8[2] = (uint8_t)(((codepoint >> 0) & 0x3F) | 0x80);
		return 3;
	}
	else
	{
		asUtf8[0] = (uint8_t)(((codepoint >> 18) & 0x07) | 0xF0);
		asUtf8[1] = (uint8_t)(((codepoint >> 12) & 0x3F) | 0x80);
		asUtf8[2] = (uint8_t)(((codepoint >> 6) & 0x3F) | 0x80);
		asUtf8[3] = (uint8_t)(((codepoint >> 0) & 0x3F) | 0x80);
		return 4;
	}
}

size_t _hs_offset_of_codepoint(const uint8_t *haystack0, const size_t hoffset, const size_t hlen0, const size_t needle)
{
	const uint8_t *haystack = haystack0 + hoffset;
	uint8_t *res = NULL;
	uint8_t asUtf8[4] = {0};
	const int codepointLen = _hs_codepoint_to_utf8(asUtf8, needle);

	// Skip to first location that could contain the character.
	uint8_t *haystackFirst = (uint8_t *)memchr(haystack, asUtf8[0], hlen0);

	if (haystackFirst)
	{
		const size_t hlen = hlen0 - (haystackFirst - haystack);

		switch (codepointLen)
		{
		case 1:
			res = haystackFirst;
			break;
		case 2:
			res = twobyte_memmem(haystackFirst, hlen, asUtf8);
			break;
		case 3:
			res = threebyte_memmem(haystackFirst, hlen, asUtf8);
			break;
		case 4:
			res = fourbyte_memmem(haystackFirst, hlen, asUtf8);
			break;
		default:
			res = NULL;
			break;
		}
	}

	return res == NULL ? -1 : (size_t)((uint8_t *)res - haystack);
}

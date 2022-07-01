
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
 * Copyright (c) 2005-2014 Rich Felker, et al.
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

static char *
twobyte_memmem(const unsigned char *h, size_t k, const unsigned char *n)
{
	uint16_t nw = n[0] << 8 | n[1], hw = h[0] << 8 | h[1];
	for (h += 2, k -= 2; k; k--, hw = hw << 8 | *h++)
		if (hw == nw)
			return (char *)h - 2;
	return hw == nw ? (char *)h - 2 : 0;
}

static char *
threebyte_memmem(const unsigned char *h, size_t k, const unsigned char *n)
{
	uint32_t nw = (uint32_t)n[0] << 24 | n[1] << 16 | n[2] << 8;
	uint32_t hw = (uint32_t)h[0] << 24 | h[1] << 16 | h[2] << 8;
	for (h += 3, k -= 3; k; k--, hw = (hw | *h++) << 8)
		if (hw == nw)
			return (char *)h - 3;
	return hw == nw ? (char *)h - 3 : 0;
}

static char *
fourbyte_memmem(const unsigned char *h, size_t k, const unsigned char *n)
{
	uint32_t nw = (uint32_t)n[0] << 24 | n[1] << 16 | n[2] << 8 | n[3];
	uint32_t hw = (uint32_t)h[0] << 24 | h[1] << 16 | h[2] << 8 | h[3];
	for (h += 4, k -= 4; k; k--, hw = hw << 8 | *h++)
		if (hw == nw)
			return (char *)h - 4;
	return hw == nw ? (char *)h - 4 : 0;
}

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

#define BITOP(a, b, op) \
	((a)[(size_t)(b) / (8 * sizeof *(a))] op(size_t) 1 << ((size_t)(b) % (8 * sizeof *(a))))

/*
 * Two Way string search algorithm, with a bad shift table applied to the last
 * byte of the window. A bit array marks which entries in the shift table are
 * initialized to avoid fully initializing a 1kb/2kb table.
 *
 * Reference: CROCHEMORE M., PERRIN D., 1991, Two-way string-matching,
 * Journal of the ACM 38(3):651-675
 */
static char *
twoway_memmem(const unsigned char *h, const unsigned char *z,
							const unsigned char *n, size_t l)
{
	size_t i, ip, jp, k, p, ms, p0, mem, mem0;
	size_t byteset[32 / sizeof(size_t)] = {0};
	size_t shift[256];

	/* Computing length of needle and fill shift table */
	for (i = 0; i < l; i++)
		BITOP(byteset, n[i], |=), shift[n[i]] = i + 1;

	/* Compute maximal suffix */
	ip = -1;
	jp = 0;
	k = p = 1;
	while (jp + k < l)
	{
		if (n[ip + k] == n[jp + k])
		{
			if (k == p)
			{
				jp += p;
				k = 1;
			}
			else
				k++;
		}
		else if (n[ip + k] > n[jp + k])
		{
			jp += k;
			k = 1;
			p = jp - ip;
		}
		else
		{
			ip = jp++;
			k = p = 1;
		}
	}
	ms = ip;
	p0 = p;

	/* And with the opposite comparison */
	ip = -1;
	jp = 0;
	k = p = 1;
	while (jp + k < l)
	{
		if (n[ip + k] == n[jp + k])
		{
			if (k == p)
			{
				jp += p;
				k = 1;
			}
			else
				k++;
		}
		else if (n[ip + k] < n[jp + k])
		{
			jp += k;
			k = 1;
			p = jp - ip;
		}
		else
		{
			ip = jp++;
			k = p = 1;
		}
	}
	if (ip + 1 > ms + 1)
		ms = ip;
	else
		p = p0;

	/* Periodic needle? */
	if (memcmp(n, n + p, ms + 1))
	{
		mem0 = 0;
		p = MAX(ms, l - ms - 1) + 1;
	}
	else
		mem0 = l - p;
	mem = 0;

	/* Search loop */
	for (;;)
	{
		/* If remainder of haystack is shorter than needle, done */
		if (z - h < l)
			return 0;

		/* Check last byte first; advance by shift on mismatch */
		if (BITOP(byteset, h[l - 1], &))
		{
			k = l - shift[h[l - 1]];
			if (k)
			{
				if (k < mem)
					k = mem;
				h += k;
				mem = 0;
				continue;
			}
		}
		else
		{
			h += l;
			mem = 0;
			continue;
		}

		/* Compare right half */
		for (k = MAX(ms + 1, mem); k < l && n[k] == h[k]; k++)
			;
		if (k < l)
		{
			h += k - ms;
			mem = 0;
			continue;
		}
		/* Compare left half */
		for (k = ms + 1; k > mem && n[k - 1] == h[k - 1]; k--)
			;
		if (k <= mem)
			return (char *)h;
		h += p;
		mem = mem0;
	}
}

void *
_hs_memmem_standard(const void *h0, size_t k, const void *n0, size_t l)
{
	const unsigned char *h = h0, *n = n0;

	/* Return immediately on empty needle */
	if (!l)
		return (void *)h;

	/* Return immediately when needle is longer than haystack */
	if (k < l)
		return 0;

	/* Use faster algorithms for short needles */
	h = memchr(h0, *n, k);
	if (!h || l == 1)
		return (void *)h;
	k -= h - (const unsigned char *)h0;
	if (k < l)
		return 0;
	if (l == 2)
		return twobyte_memmem(h, k, n);
	if (l == 3)
		return threebyte_memmem(h, k, n);
	if (l == 4)
		return fourbyte_memmem(h, k, n);

	return twoway_memmem(h, h + k, n, l);
}

size_t
_hs_text_memmem(const void *h0, size_t hoff, size_t hlen, const void *n0, size_t noff, size_t nlen)
{
	void *res = _hs_memmem_standard(h0 + hoff, hlen, n0 + noff, nlen);
	return res == NULL ? -1 : (size_t)(res - h0);
}

const size_t _hs_offset_of_codepoint(const uint8_t *haystack0, const size_t hoffset, const size_t hlen, size_t needle)
{
	const uint8_t *haystack = haystack0 + hoffset;
	uint8_t asUtf8[4];
	size_t codepointLen;
	if (needle < 0x80)
	{
		codepointLen = 1;
		asUtf8[0] = needle;
	}
	else if (needle < 0x0800)
	{
		codepointLen = 2;
		asUtf8[0] = (uint8_t)(((needle >> 6) & 0x1F) | 0xC0);
		asUtf8[1] = (uint8_t)(((needle >> 0) & 0x3F) | 0x80);
	}
	else if (needle < 0x10000)
	{
		codepointLen = 3;
		asUtf8[0] = (uint8_t)(((needle >> 12) & 0x0F) | 0xE0);
		asUtf8[1] = (uint8_t)(((needle >> 6) & 0x3F) | 0x80);
		asUtf8[2] = (uint8_t)(((needle >> 0) & 0x3F) | 0x80);
	}
	else
	{
		codepointLen = 4;
		asUtf8[0] = (uint8_t)(((needle >> 18) & 0x07) | 0xF0);
		asUtf8[1] = (uint8_t)(((needle >> 12) & 0x3F) | 0x80);
		asUtf8[2] = (uint8_t)(((needle >> 6) & 0x3F) | 0x80);
		asUtf8[3] = (uint8_t)(((needle >> 0) & 0x3F) | 0x80);
	}
	const void *res = _hs_memmem_standard(haystack, hlen, asUtf8, codepointLen);
	return res == NULL ? -1 : (size_t)((uint8_t *)res - haystack);
}
/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

/* Changed name to disambiguate from _hs_text_memcmp,
   which could be present in system-wide headers from installed ghc package */
int _hs_text_memcmp2(const void *arr1, size_t off1, const void *arr2, size_t off2, size_t len)
{
  return memcmp(arr1 + off1, arr2 + off2, len);
}

ssize_t _hs_text_memchr(const void *arr, size_t off, size_t len, uint8_t byte)
{
  const void *ptr = memchr(arr + off, byte, len);
  return ptr == NULL ? -1 : ptr - (arr + off);
}

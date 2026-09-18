/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

ssize_t _hs_text_memchr(const void *arr, size_t off, size_t len, uint8_t byte)
{
  const void *ptr = memchr(arr + off, byte, len);
  return ptr == NULL ? -1 : ptr - (arr + off);
}

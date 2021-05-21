/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <stdio.h>
#include <string.h>

int _hs_text_memcmp(const void *arr1, size_t off1, const void *arr2, size_t off2, size_t len)
{
  return memcmp(arr1 + off1, arr2 + off2, len);
}

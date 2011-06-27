#include <string.h>

void _hs_text_memcpy(void *dest, size_t doff, const void *src, size_t soff,
		     size_t n)
{
  memcpy(dest + (doff<<1), src + (soff<<1), n<<1);
}

int _hs_text_memcmp(const void *a, size_t aoff, const void *b, size_t boff,
		    size_t n)
{
  return memcmp(a + (aoff<<1), b + (boff<<1), n<<1);
}

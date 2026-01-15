#include "simdutf_c.h"

int _hs_text_is_valid_utf8(const char *buf, size_t len) {
  return simdutf_validate_utf8(buf, len);
}

int _hs_text_is_valid_utf8_offset(const char *buf, size_t off, size_t len) {
  return simdutf_validate_utf8(buf + off, len);
}

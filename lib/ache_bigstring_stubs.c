#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

#define BS(v) ((unsigned char *)Caml_ba_data_val(v))

value ache_bigstring_blit(value src, intnat src_off, value dst, intnat dst_off,
                          intnat len)
{
  memmove(BS(dst) + dst_off, BS(src) + src_off, len);
  return Val_unit;
}

value ache_bigstring_blit_byte(value src, value src_off, value dst,
                               value dst_off, value len)
{
  return ache_bigstring_blit(src, Long_val(src_off), dst, Long_val(dst_off),
                             Long_val(len));
}

value ache_bigstring_blit_from_string(value src, intnat src_off, value dst,
                                      intnat dst_off, intnat len)
{
  memcpy(BS(dst) + dst_off, String_val(src) + src_off, len);
  return Val_unit;
}

value ache_bigstring_blit_from_string_byte(value src, value src_off, value dst,
                                           value dst_off, value len)
{
  return ache_bigstring_blit_from_string(src, Long_val(src_off), dst,
                                         Long_val(dst_off), Long_val(len));
}

value ache_bigstring_blit_to_bytes(value src, intnat src_off, value dst,
                                   intnat dst_off, intnat len)
{
  memcpy(Bytes_val(dst) + dst_off, BS(src) + src_off, len);
  return Val_unit;
}

value ache_bigstring_blit_to_bytes_byte(value src, value src_off, value dst,
                                        value dst_off, value len)
{
  return ache_bigstring_blit_to_bytes(src, Long_val(src_off), dst,
                                      Long_val(dst_off), Long_val(len));
}

intnat ache_bigstring_memchr(value bs, intnat off, intnat chr, intnat len)
{
  unsigned char *base = BS(bs);
  unsigned char *found = memchr(base + off, (int)chr, len);
  return found == NULL ? -1 : (intnat)(found - base);
}

value ache_bigstring_memchr_byte(value bs, value off, value chr, value len)
{
  return Val_long(ache_bigstring_memchr(bs, Long_val(off), Long_val(chr),
                                        Long_val(len)));
}

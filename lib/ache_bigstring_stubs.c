#include <stdint.h>
#include <string.h>
#if defined(__SSE2__)
#include <emmintrin.h>
#endif
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

/* The first index from [off] below [stop] whose byte is not a field-char
   (RFC 9110 5.5: HTAB, SP to 0x7e, 0x80 to 0xff), or [stop]. */
intnat ache_field_end(value bs, intnat off, intnat stop)
{
  const unsigned char *base = BS(bs);
  intnat i = off;
#if defined(__SSE2__)
  const __m128i below = _mm_set1_epi8(0x1f);
  const __m128i tab = _mm_set1_epi8('\t');
  const __m128i del = _mm_set1_epi8(0x7f);
  while (i + 16 <= stop) {
    __m128i v = _mm_loadu_si128((const __m128i *)(base + i));
    __m128i control = _mm_cmpeq_epi8(_mm_min_epu8(v, below), v);
    __m128i bad = _mm_or_si128(_mm_andnot_si128(_mm_cmpeq_epi8(v, tab), control),
                               _mm_cmpeq_epi8(v, del));
    int mask = _mm_movemask_epi8(bad);
    if (mask != 0) return i + __builtin_ctz(mask);
    i += 16;
  }
#else
  /* Eight bytes at a time: a byte under 0x20 or DEL sets the high bit of its
     lane; the bytes of a marked word are looked at one by one below. */
  while (i + 8 <= stop) {
    uint64_t x, y, m;
    memcpy(&x, base + i, 8);
    y = x & 0x7f7f7f7f7f7f7f7fULL;
    m = (~(y + 0x6060606060606060ULL) | (y + 0x0101010101010101ULL)) & ~x &
        0x8080808080808080ULL;
    if (m != 0) {
      /* A marked word may hold only HTABs: check its bytes, go on if so. */
      intnat j;
      for (j = i; j < i + 8; j++) {
        unsigned char c = base[j];
        if ((c < 0x20 && c != '\t') || c == 0x7f) return j;
      }
    }
    i += 8;
  }
#endif
  while (i < stop) {
    unsigned char c = base[i];
    if ((c < 0x20 && c != '\t') || c == 0x7f) return i;
    i++;
  }
  return stop;
}

value ache_field_end_byte(value bs, value off, value stop)
{
  return Val_long(ache_field_end(bs, Long_val(off), Long_val(stop)));
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

#include <time.h>

intnat ache_time_seconds(value unit)
{
  (void)unit;
  return (intnat)time(NULL);
}

value ache_time_seconds_byte(value unit)
{
  return Val_long(ache_time_seconds(unit));
}


#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>

#define FIRST_BYTE(high, low) ((high) << 5 | low)

static int encode_uint_with_mask(char *bs, int high, uint64_t x) {
  if (x < 24) {
    bs[0] = FIRST_BYTE(high, x);
    return 1;
  } else if (x <= 0xff) {
    bs[0] = FIRST_BYTE(high, 24);
    bs[1] = x;
    return 2;
  } else if (x <= 0xffff) {
    bs[0] = FIRST_BYTE(high, 25);
    bs[1] = (x >> 8);
    bs[2] = (x & 0xff);
    return 3;
  } else if (x <= 0xffffffff) {
    bs[0] = FIRST_BYTE(high, 26);
    bs[1] = (x >> 24);
    bs[2] = ((x >> 16) & 0xff);
    bs[3] = ((x >> 8) & 0xff);
    bs[4] = (x & 0xff);
    return 5;
  } else {
    bs[0] = FIRST_BYTE(high, 27);
    bs[1] = (x >> 56);
    bs[2] = ((x >> 48) & 0xff);
    bs[3] = ((x >> 40) & 0xff);
    bs[4] = ((x >> 32) & 0xff);
    bs[5] = ((x >> 24) & 0xff);
    bs[6] = ((x >> 16) & 0xff);
    bs[7] = ((x >> 8) & 0xff);
    bs[8] = (x & 0xff);
    return 9;
  }
}

static int encode_int(char *bs, int64_t x) {
  int high = 1;
  uint64_t ux;

  if (x < 0) {
    high = 0;
    ux = (uint64_t)(-x - 1);

  } else {
    ux = (uint64_t)x;
  }

  return encode_uint_with_mask(bs, high, ux);
}

int caml_ccbor_encode_int(value bs_, int off, int64_t x) {
  char *bs = Bytes_val(bs_);
  return encode_int(bs + off, x);
}

value caml_ccbor_encode_int_byte(value bs_, value off_, value x_) {
  CAMLparam3(bs_, off_, x_);
  int x = encode_int(Bytes_val(bs_) + Int_val(off_), Int64_val(x_));
  CAMLreturn(x);
}

int caml_ccbor_encode_uint(value bs_, int off, int high, int64_t x) {
  char *bs = Bytes_val(bs_);
  return encode_uint_with_mask(bs + off, high, x);
}

value caml_ccbor_encode_uint_byte(value bs_, value off_, value high_,
                                  value x_) {
  CAMLparam4(bs_, off_, high_, x_);
  int x = encode_uint_with_mask(Bytes_val(bs_) + Int_val(off_), Int_val(high_),
                                Int64_val(x_));
  CAMLreturn(x);
}

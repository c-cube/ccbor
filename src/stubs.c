
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>


static inline int int_size(uint64_t x) {
  if (x < 24) return 0;
  if (x < 0xff) return 1;
  if (x < 0xffff) return 2;
  if (x < 0xffffffff) return 4;
  return 8;
}


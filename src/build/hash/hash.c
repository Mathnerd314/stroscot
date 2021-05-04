#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#define ghc_cc __attribute__((preserve_all))

typedef void ghc_cc (*HsCallInt)(int64_t*, int64_t*, int64_t*, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t,
                          int64_t);

extern ghc_cc  __attribute__((always_inline)) void test2(
    int64_t* restrict baseReg,
    int64_t* restrict sp,
    int64_t* restrict hp,
    int64_t r1,
    int64_t r2,
    int64_t r3,
    int64_t r4,
    int64_t r5,
    int64_t r6,
    int64_t spLim)
{
  const int64_t iUndef;

  uint64_t u = (uint64_t) r1;
  uint64_t a = u + u;
  uint64_t b = u * u;
  const HsCallInt fun = (HsCallInt) sp[0];
  return fun(baseReg, sp, hp, a, b, iUndef, iUndef, iUndef, iUndef, spLim);
}

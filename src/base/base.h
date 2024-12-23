#ifndef BASE_H
#define BASE_H

#include <assert.h>

#include "types.h" // size_t

#define ASSERT_NOT_REACHED assert(0 && "panic: unreachable code reached")

#define VA_NUMBER_OF_ARGS(...) (sizeof((int[]){ __VA_ARGS__ }) / sizeof(int))

#define ARRAY_LENGTH(arr) (size_t)(sizeof(arr) / sizeof((arr)[0]))

#define IS_BETWEEN(x, lower, upper) (((lower) <= (x)) && ((x) <= (upper)))

// TODO: we assume NULL is 0x0 in many places. This holds true for every compiler I've ever seen,
//       but its not guaranteed. So we should give an error if we detect it is in fact not 0.
//       Same is true for page size. sac_single assumes a page size of 4096. Usually true, not
//       guaranteed.

#endif /* BASE_H */

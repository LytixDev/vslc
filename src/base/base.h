#ifndef BASE_H
#define BASE_H

#include <assert.h>

#include "types.h"

#define ASSERT_NOT_REACHED assert(0 && "panic: unreachable code reached")

#define ARRAY_LENGTH(arr) (size_t)(sizeof(arr) / sizeof((arr)[0]))

#endif /* BASE_H */

/*
 *  Copyright (C) 2024 Nicolai Brand (lytix.dev)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef STR_H
#define STR_H

#include "types.h"
#include "sac_single.h"

/* 
 * Should always be null terminated.
 * Size does not include null terminator.
 */
typedef struct {
    size_t size;
    u8 *str;
} Str8;

/*
 * A view into ...
 */
typedef Str8 StrView8;


#endif /* STR_H */

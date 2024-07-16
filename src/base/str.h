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

#include "sac_single.h"
#include "types.h"

/*
 * Should always be null terminated.
 * Size does not include null terminator.
 */
typedef struct {
    u8 *str;
    size_t len;
} Str8;

typedef struct {
    Arena *arena;
    Str8 str;
    u32 cap;
} StrBuilder;

/*
 * A view into memory.
 * Not guaranteed to be null terminated.
 */
typedef Str8 StrView8;


StrBuilder make_str_builder(Arena *arena);
// void str_builder_append_str8(StrBuilder *sb, Str8 str);
void str_builder_append_u8(StrBuilder *sb, u8 c);
void str_builder_append_cstr(StrBuilder *sb, char *cstr, u32 len);

#endif /* STR_H */

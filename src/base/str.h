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

#include "base.h"
#include "sac_single.h"
#include "types.h"

/*
 * Should always be null terminated.
 * Size does not include null terminator.
 */
typedef struct {
    size_t len;
    u8 *str;
} Str8;

// TODO: VLA considered harmful, but maybe this is more ergonomic?
// typedef struct {
//     size_t len;
//     u8 str[]; // length + 1
// } Str8;

typedef struct {
    Arena *arena;
    Str8 str;
    u32 cap;
} Str8Builder;

typedef struct {
    Str8 *strs; // On the heap.
    u32 len;
    u32 cap;
} Str8List;

/*
 * A view into memory.
 * Not guaranteed to be null terminated.
 */
typedef Str8 Str8View;

/* Used with %.*s */
#define STR8VIEW_PRINT(view) (int)(view).len, (const char *)(view).str
/* Str8View from a literal */
#define STR8VIEW_LIT(literal)                \
    (Str8View)                               \
    {                                        \
        sizeof(literal) - 1, (u8 *)(literal) \
    }
#define STR8VIEW_EQUAL(a, b) \
    ((a).len == (b).len && ((a).len == 0 || memcmp((a).str, (b).str, (a).len) == 0))

#define STR8_LIT(literal)                    \
    (Str8)                                   \
    {                                        \
        sizeof(literal) - 1, (u8 *)(literal) \
    }


u32 str_view_to_u32(Str8View view, bool *success);

Str8Builder make_str_builder(Arena *arena);
void str_builder_append_u8(Str8Builder *sb, u8 c);
void str_builder_append_cstr(Str8Builder *sb, char *cstr, u32 len);
void str_builder_append_str8(Str8Builder *sb, Str8 str);
void str_builder_sprintf(Str8Builder *sb, char *fmt, int count, ...);
Str8 str_builder_end(Str8Builder *sb, bool add_null_terminator);

void str_list_init(Str8List *list);
void str_list_free(Str8List *list);
u32 str_list_push(Str8List *list, Str8 str);
u32 str_list_push_cstr(Arena *arena, Str8List *list, char *cstr);
void str_list_print(Str8List *list);

#endif /* STR_H */

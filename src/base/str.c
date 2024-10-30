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
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "sac_single.h"
#include "str.h"
#include "types.h"


Str8Builder make_str_builder(Arena *arena)
{
    Str8Builder sb = {
        .arena = arena,
        .str = (Str8){ .len = 0 },
        .cap = 16,
    };

    sb.str.str = m_arena_alloc(arena, sb.cap);
    return sb;
}

void str_builder_append_u8(Str8Builder *sb, u8 c)
{
    if (sb->str.len == sb->cap) {
        /* Double the allocation */
        (void)m_arena_alloc(sb->arena, sb->cap);
        sb->cap *= 2;
    }

    sb->str.str[sb->str.len++] = c;
}

void str_builder_append_cstr(Str8Builder *sb, char *cstr, u32 len)
{
    while (sb->str.len + len > sb->cap) {
        /* Double the allocation */
        (void)m_arena_alloc(sb->arena, sb->cap);
        sb->cap *= 2;
    }

    memcpy((char *)sb->str.str, cstr, len);
    sb->str.len += len;
}

Str8 str_builder_end(Str8Builder *sb)
{
    // TODO: Remove this quirk, or make it more apparent:
    // Return sb->str, but len does not include null terminator
    Str8 final = sb->str;
    assert(final.str[final.len] == 0);
    assert(final.len != 0);
    final.len -= 1;
    return final;
}

void str_list_init(Str8List *list)
{
    list->len = 0;
    list->cap = 16;
    list->strs = malloc(sizeof(Str8) * list->cap);
}

void str_list_free(Str8List *list)
{
    free(list->strs);
}

u32 str_list_push(Str8List *list, Str8 str)
{
    if (list->len >= list->cap) {
        list->cap *= 2;
        list->strs = realloc(list->strs, sizeof(Str8) * list->cap);
    }
    list->strs[list->len] = str;
    list->len++;
    return list->len - 1;
}

u32 str_list_push_cstr(Arena *arena, Str8List *list, char *cstr)
{
    size_t len = strlen(cstr);
    u8 *str = m_arena_alloc(arena, len + 1);
    memcpy(str, cstr, len);
    str[len] = 0;
    return str_list_push(list, (Str8){ .len = len, .str = str });
}

void str_list_print(Str8List *list)
{
    for (u32 i = 0; i < list->len; i++) {
        printf("[%d] %s\n", i, list->strs[i].str);
    }
}

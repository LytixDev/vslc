/* Single header library for sac: simple arena allocator */

/*
 *  Copyright (C) 2022-2024 Nicolai Brand (lytix.dev)
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
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>. */
#ifndef SAC_H
#define SAC_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>

#ifndef __unix__
#  define SAC_BAD_AARCH
#endif

/* Try to define SAC_MAP_ANON */
#ifdef MAP_ANON
#  define SAC_MAP_ANON MAP_ANON
#endif /* MAP_ANON */

#ifndef SAC_MAP_ANON
#  ifdef MAP_ANONYMOUS
#    define SAC_MAP_ANON MAP_ANONYMOUS
#  endif /* MAP_ANONYMOUS */
#endif /* SAC_MAP_ANON */

#ifndef SAC_MAP_ANON
#  define SAC_BAD_AARCH
#endif /* SAC_MAP_ANON */

#ifndef SAC_DEFAULT_ALIGNMENT
#  define SAC_DEFAULT_ALIGNMENT (sizeof(void *))
#endif

typedef struct m_arena Arena;
typedef struct m_arena_tmp ArenaTmp;

/* types */
typedef struct m_arena Arena;
typedef struct m_arena_tmp ArenaTmp;

/*
 * generic memory arena that dynamically grows its committed size.
 * more complex memory arenas can be built using this as a base.
 */
struct m_arena {
    uint8_t *memory;    // the backing memory
    size_t offset;      // first unused position in the backing memory
    bool is_dynamic;
    union {
        size_t max_pages;
        size_t backing_length;
    };
    size_t page_size;
    size_t pages_commited;   // how much of the backing memory is acutally "backing"
};

struct m_arena_tmp {
    struct m_arena *arena;
    size_t offset;
};


/* functions */
void m_arena_init(struct m_arena *arena, void *backing_memory, size_t backing_length);
void m_arena_init_dynamic(struct m_arena *arena, size_t starting_pages, size_t max_pages);
void m_arena_release(struct m_arena *arena);

void *m_arena_alloc_internal(struct m_arena *arena, size_t size, size_t align, bool zero);
#define m_arena_alloc(arena, size) m_arena_alloc_internal(arena, size, SAC_DEFAULT_ALIGNMENT, false)
#define m_arena_alloc_zero(arena, size) m_arena_alloc_internal(arena, size, SAC_DEFAULT_ALIGNMENT, true)

void m_arena_clear(struct m_arena *arena);
void *m_arena_get(struct m_arena *arena, size_t byte_idx);

#define m_arena_alloc_array(arena, type, count) (type *)m_arena_alloc((arena), sizeof(type) * (count))
#define m_arena_alloc_array_zero(arena, type, count) (type *)m_arena_alloc_zero((arena), sizeof(type) * (count))
#define m_arena_alloc_struct(arena, type) (type *)m_arena_alloc((arena), sizeof(type))
#define m_arena_alloc_struct_zero(arena, type) (type *)m_arena_alloc_zero((arena), sizeof(type))
#define m_arena_gett(arena, idx, type) (type *)m_arena_get((arena), sizeof(type) * (idx))


struct m_arena_tmp m_arena_tmp_init(struct m_arena *arena);
void m_arena_tmp_release(struct m_arena_tmp tmp);
/* cursed */
#define ARENA_TMP(___arena) for (size_t ___i = 0, ___offset = (___arena)->offset; ___i == 0; ___i += 1, (___arena)->offset = ___offset)

#endif /* !SAC_H */
#ifdef SAC_IMPLEMENTATION
/* IMPORT START */
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
/* IMPORT END */
/* IMPL START */
/* internal functions */
static bool m_arena_commit(struct m_arena *arena, size_t pages_to_commit)
{
    assert(arena->is_dynamic);

    /* no more space to commit */
    if (arena->pages_commited + pages_to_commit > arena->max_pages)
        return false;

    int rc = mprotect(arena->memory + (arena->pages_commited * arena->page_size), pages_to_commit * arena->page_size, PROT_READ | PROT_WRITE);
    if (rc == -1)
        return false;

    arena->pages_commited += pages_to_commit;
    return true;
}

static bool m_arena_ensure_commited(struct m_arena *arena)
{
    if (!arena->is_dynamic)
        return arena->offset <= arena->backing_length;

    /* know arena is dynamic */
    size_t memory_committed = arena->pages_commited * arena->page_size;
    if (arena->offset <= memory_committed)
        return true;

    //TODO: this math can probably be optimized
    size_t delta = arena->offset - memory_committed;
    size_t pages_to_commit = (delta / arena->page_size) + 1;
    if (arena->pages_commited + pages_to_commit > arena->max_pages)
        return false;

    m_arena_commit(arena, pages_to_commit);
    return true;
}

/*
 * stolen from:
 * https://www.gingerbill.org/article/2019/02/08/memory-allocation-strategies-002/
 */
static bool is_power_of_two(uintptr_t x)
{
    return (x & (x - 1)) == 0;
}

/*
 * slightly modified, but mostly stolen from:
 * https://www.gingerbill.org/article/2019/02/08/memory-allocation-strategies-002/
 */
static uintptr_t align_forward(uintptr_t ptr, size_t align)
{
    assert(is_power_of_two(align));

    uintptr_t p = ptr;
    uintptr_t a = (uintptr_t)align;
    uintptr_t modulo = p & (a - 1);

    if (modulo != 0)
        p += a - modulo;

    return p;
}


/* SAC functions */
void m_arena_init(struct m_arena *arena, void *backing_memory, size_t backing_length)
{
    assert(backing_memory != NULL && backing_length > 0);

    arena->is_dynamic = false;
    arena->memory = backing_memory;
    arena->backing_length = backing_length;
    arena->offset = 0;
}

void m_arena_init_dynamic(struct m_arena *arena, size_t starting_pages, size_t max_pages)
{
    assert(starting_pages <= max_pages);

    arena->is_dynamic = true;
    arena->max_pages = max_pages;
    arena->page_size = sysconf(_SC_PAGE_SIZE);
    arena->offset = 0;
    arena->pages_commited = 0;

    arena->memory = mmap(NULL, arena->max_pages * arena->page_size, PROT_NONE, MAP_PRIVATE | SAC_MAP_ANON, -1, 0); 
    if (arena->memory == MAP_FAILED) {
        fprintf(stderr, "sac: map failed in file %s on line %d\n", __FILE__, __LINE__);
        exit(1);
    }

    if (starting_pages != 0)
        m_arena_commit(arena, starting_pages);
}

void m_arena_release(struct m_arena *arena)
{
    assert(arena != NULL);

    /* the implementation does not manage the backing memory */
    if (!arena->is_dynamic)

    munmap(arena->memory, arena->max_pages);
}

/*
 * heavily modified, but inspired by:
 * https://www.gingerbill.org/article/2019/02/08/memory-allocation-strategies-002/
 */
void *m_arena_alloc_internal(struct m_arena *arena, size_t size, size_t alignment, bool zero)
{
    /* curr_ptr will be the first non-used memory address */
    uintptr_t curr_ptr = (uintptr_t)arena->memory + (uintptr_t)arena->offset;
    /* offset wil be the first aligned to one word non-used memory adress */
    uintptr_t offset = align_forward(curr_ptr, alignment);
    /* change to relative offset from the first memory adress */
    offset -= (uintptr_t)arena->memory;
    arena->offset = offset + size;

    bool success = m_arena_ensure_commited(arena);
    if (!success)
        return NULL;

    void *ptr = arena->memory + offset;
    if (zero)
        memset(ptr, 0, size);

    return ptr;
}

void m_arena_clear(struct m_arena *arena)
{
    arena->offset = 0;
}

void *m_arena_get(struct m_arena *arena, size_t byte_idx)
{
    if (byte_idx > arena->offset)
        return NULL;

    return arena->memory + byte_idx;
}

struct m_arena_tmp m_arena_tmp_init(struct m_arena *arena)
{
    return (struct m_arena_tmp){ .arena = arena, .offset = arena->offset };
}

void m_arena_tmp_release(struct m_arena_tmp tmp)
{
    tmp.arena->offset = tmp.offset;
}
#endif /* SAC_IMPLEMENTATION */

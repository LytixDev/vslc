/*
 *  Copyright (C) 2024 Nicolai Brand (https://lytix.dev)
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
#ifndef COMPILER_H
#define COMPILER_H

#include "base/sac_single.h"
#include "base/types.h"
#include "base/str.h"

typedef struct {
    bool had_error;
    // For temporary data that ONLY needs to persist for the duration of a single compiler pass
    // Cleared before each pass.
    Arena *persist_arena;
    Arena *pass_arena;

    Str8List str_list;
} Compiler;

#endif /* COMPILER_H */

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

#include "base/nicc.h"
#include "base/sac_single.h"
#include "type.h"

typedef struct error_handler_t ErrorHandler; // forward decl from error.h


typedef struct compiler_t {
    Arena *pass_arena; // Temporary data which only persist for the duration of a single "pass".
    Arena *persist_arena;
    ErrorHandler *e;

    SymbolTable symt_root;

    ArrayList all_types; // Holds TypeInfo **. Every base type lives here.
    ArrayList struct_types; // Holds TypeInfoStruct **
} Compiler;

#endif /* COMPILER_H */

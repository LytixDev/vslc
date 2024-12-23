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
#ifndef INTERPRET_H
#define INTERPRET_H


#include "base/types.h"
#include "compiler/ast.h"

typedef s64 BytecodeWord;


typedef enum {
    OP_CONSTANT,

    /* bin ops */
    OP_PLUS,
    OP_MINUS,
    OP_STAR,
    OP_SLASH,
    OP_LSHIFT,
    OP_RSHIFT,

    OP_PRINT,

    OP_RETURN,

    OP_TYPE_LEN,
} OpCode;


extern char *op_code_str_map[OP_TYPE_LEN];

#define STACK_MAX 512


typedef struct {
    // TODO: Pool allocated or something
    u8 code[2048];
    u32 code_offset;
} Bytecode;

typedef struct {
    Bytecode b;
    u8 *ip;

    BytecodeWord stack[STACK_MAX];
    u8 *sp;
} MetagenVM;


Bytecode ast_to_bytecode(AstRoot *root);
u32 run_bytecode(Bytecode b);

#endif /* INTERPRET_H */

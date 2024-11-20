/*
 *  Copyright (C) 2023-2024 Nicolai Brand (https://lytix.dev)
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
#include <stdio.h>

#include "compiler/ast.h"
#include "compiler/compiler.h"
#include "compiler/error.h"
#include "compiler/gen.h"
#include "compiler/parser.h"
#include "compiler/type.h"
#include "compiler/interpret.h"

#include "base/str.h"
#define NICC_IMPLEMENTATION
#include "base/nicc.h"
#define SAC_IMPLEMENTATION
#include "base/sac_single.h"


typedef void (*CompilerPass)(Compiler *c, AstRoot *root);

bool run_compiler_pass(Compiler *c, AstRoot *root, CompilerPass pass)
{
    m_arena_clear(c->pass_arena);
    pass(c, root);
    for (CompilerError *err = c->e->head; err != NULL; err = err->next) {
        printf("%s\n", err->msg.str);
    }
    return c->e->n_errors > 0;
}

u32 compile(char *input)
{
    Arena arena;
    Arena lex_arena;
    Arena pass_arena;
    m_arena_init_dynamic(&arena, 2, 512);
    m_arena_init_dynamic(&lex_arena, 1, 512);
    m_arena_init_dynamic(&pass_arena, 1, 512);

    ErrorHandler e;
    error_handler_init(&e, input, "test.meta");

    Compiler compiler = { .persist_arena = &arena, .pass_arena = &pass_arena, .e = &e };
    arraylist_init(&compiler.struct_types, sizeof(TypeInfoStruct *));
    arraylist_init(&compiler.all_types, sizeof(TypeInfo *));

    AstRoot *ast_root = parse(&arena, &lex_arena, &e, input);
    for (CompilerError *err = e.head; err != NULL; err = err->next) {
        printf("%s\n", err->msg.str);
    }
    if (e.n_errors != 0) {
        goto done;
    }

    ast_print((AstNode *)ast_root, 0);
    putchar('\n');


    if (run_compiler_pass(&compiler, ast_root, typegen)) {
        goto done;
    }
    if (run_compiler_pass(&compiler, ast_root, infer)) {
        goto done;
    }
    if (run_compiler_pass(&compiler, ast_root, typecheck)) {
        goto done;
    }

    Bytecode b = ast_to_bytecode(ast_root);
    run_bytecode(b);

    transpile_to_c(&compiler);

done:
    for (CompilerError *err = e.head; err != NULL; err = err->next) {
        printf("%s\n", err->msg.str);
    }
    // We could be "good citizens" and release the memory here, but the OS is going to do it
    // anyways on the process terminating, so it doesn't really make a difference.

    // arraylist_free ...
    // error_handler_release(&e);
    // m_arena_release(&arena);
    // m_arena_release(&lex_arena);
    return e.n_errors;
}


int main(void)
{
    Arena input_arena;
    m_arena_init_dynamic(&input_arena, 1, 512);
    char *input = m_arena_alloc_zero(&input_arena, 4096);
    char c;
    u32 i = 0;
    while ((c = getchar()) != EOF) {
        if (i >= input_arena.pages_commited * 4096) {
            m_arena_alloc_zero(&input_arena, 4096);
        }
        input[i] = c;
        i++;
    }

    u32 n_errors = compile(input);
    if (n_errors == 0)
        return 0;
    return 1;
}

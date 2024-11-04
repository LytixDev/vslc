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

#include "base/str.h"
#define NICC_IMPLEMENTATION
#include "base/nicc.h"
#define SAC_IMPLEMENTATION
#include "base/sac_single.h"

u32 run(char *input)
{
    Arena arena;
    Arena lex_arena;
    m_arena_init_dynamic(&arena, 2, 512);
    m_arena_init_dynamic(&lex_arena, 1, 512);

    ErrorHandler e;
    error_handler_init(&e, input, "test.meta");

    AstRoot *ast_root = parse(&arena, &lex_arena, &e, input);
    for (CompilerError *err = e.head; err != NULL; err = err->next) {
        printf("%s\n", err->msg.str);
    }
    if (e.n_errors != 0) {
        goto done;
    }

    error_handler_reset(&e);
    Compiler compiler = { .persist_arena = &arena, .e = &e };
    symbol_generate(&compiler, ast_root);
    for (CompilerError *err = e.head; err != NULL; err = err->next) {
        printf("%s\n", err->msg.str);
    }

    // ast_print((AstNode *)ast_root, 0);
    // putchar('\n');

    transpile_to_c(&compiler);

done:
    error_handler_release(&e);
    m_arena_release(&arena);
    m_arena_release(&lex_arena);
    return e.n_errors;
}


int main(void)
{
    Arena input_arena;
    m_arena_init_dynamic(&input_arena, 1, 32);
    char *input = m_arena_alloc_zero(&input_arena, 4096);
    char c;
    u32 i = 0;
    while ((c = getchar()) != EOF) {
        if (input_arena.offset >= input_arena.pages_commited * 4096) {
            m_arena_alloc_zero(&input_arena, 4096);
        }
        input[i] = c;
        i++;
    }

    u32 n_errors = run(input);
    if (n_errors == 0)
        return 0;
    return 1;
}

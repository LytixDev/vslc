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

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include "base/sac_single.h"
#include "compiler/lex.h"
#include "tests.h"


void test_lexer(void)
{
    char *input = "var a = \"some_str\"; // ignored\nvar b = \"not_ignored\"";
    Lexer lexer;
    Arena arena;
    lex_init(&lexer, input);
    m_arena_init_dynamic(&arena, 2, 512);

    printf("input: '%s'\n", input);
    while (1) {
        Token next = lex_next(&arena, &lexer);
        token_print(next);
        if (next.type == TOKEN_EOF)
            break;
    }
}

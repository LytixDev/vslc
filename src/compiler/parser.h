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
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "base/sac_single.h"
#include "error.h"
#include "lex.h"

typedef struct {
    Arena *arena; // Allocator for all dynamic allocations performed by the parser
    Arena *lex_arena;
    Lexer lexer;
    /*
     * When unlex is true we do not invoke the lexer in lex_next() and instead return
     * the previously lexed token
     */
    bool unlex;
    Token previous;
} Parser;

AstRoot *parse(Arena *arena, Arena *lex_arena, ErrorHandler *e, char *input);

#endif /* PARSER_H */

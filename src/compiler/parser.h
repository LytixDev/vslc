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
#include "lex.h"

typedef enum {
    PET_EXPECTED_RPAREN,
    PET_EXPECTED_DO,
    PET_EXPECTED_THEN,
    PET_CUSTOME,

    PET_LEN,
} ParseErrorType;


extern char *PARSE_ERROR_MSGS[PET_LEN];

typedef struct parse_error_t ParseError;
struct parse_error_t {
    ParseErrorType type;
    ParseError *next;
    char *msg; // @NULLABLE. If NULL then use the error message based on the type.
    Token *failed; // The token that caused the error
};

typedef struct {
    u32 n_errors;
    ParseError *err_head;
    AstStmt *head;
    Str8 *str_list; // Heap-alloced
    u32 str_list_len;
} ParseResult;

typedef struct {
    Arena arena; // Allocator for all dynamic allocations performed by the parser
    Lexer lexer;
    u32 n_errors;
    ParseError *err_head;
    ParseError *err_tail;
} Parser;

ParseResult parse(char *input);

#endif /* PARSER_H */

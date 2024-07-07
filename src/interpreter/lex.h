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
#ifndef LEX_H
#define LEX_H

#include "base/sac_single.h"
#include "base/types.h"
#include "base/str.h"

typedef struct {
    u32 l; // Line
    u32 c; // Column
} Point;

typedef enum {
    TOKEN_NUM = 0,
    TOKEN_PLUS,
    TOKEN_STAR,
    TOKEN_SEMICOLON,
    TOKEN_EOF,
    TOKEN_TYPE_LEN,
} TokenType;

typedef struct {
    TokenType type;
    Point start;
    Point end;
    StrView8 lexeme;

    union {
        Str8 str_value;
        f64 num_value;
    };
} Token;

typedef struct lexer_t Lexer;

//typedef struct {
//} StateFn;

typedef Token (*StateFn)(Lexer *);

// TODO: should also keep track of the position of each newline
struct lexer_t {
    bool had_error;
	char *input; // The input string being scanned.
    u32 pos_start;
    u32 pos_current;
    Point start; // Start point of the current token being processed
    Point current; // Current point in the input

    StateFn state;
    Arena arena;
};



Token lex_any(Lexer *lexer);

Token get_next_token(Lexer *lexer);
Token peek_token(Lexer *lexer, u32 lookahead);



#endif /* LEX_H */

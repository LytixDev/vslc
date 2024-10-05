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

#include "base/str.h"
#include "base/types.h"

typedef struct {
    u32 l; // Line
    u32 c; // Column
} Point;

typedef enum {
    TOKEN_ERR = 0,

    // Types
    TOKEN_NUM,
    TOKEN_STR,

    // Assignment
    TOKEN_ASSIGNMENT,

    // Basic arithmetic
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,

    // Bitwise shifting
    TOKEN_LSHIFT,
    TOKEN_RSHIFT,

    // Relational
    TOKEN_EQ,
    TOKEN_NEQ,
    TOKEN_LESS,
    TOKEN_GREATER,

    // Misc..
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_COMMA,
    TOKEN_EOF,

    // Identifier and reserved words
    TOKEN_IDENTIFIER,
    TOKEN_FUNC,
    TOKEN_BEGIN,
    TOKEN_END,
    TOKEN_RETURN,
    TOKEN_PRINT,
    TOKEN_BREAK,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_ELSE,
    TOKEN_WHILE,
    TOKEN_DO,
    TOKEN_VAR,

    TOKEN_TYPE_ENUM_COUNT,
} TokenType;

typedef struct {
    TokenType type;
    Point start;
    Point end;
    StrView8 lexeme;

    union {
        u32 str_list_idx;
        s32 num_value;
    };
} Token;


typedef struct lex_error_t LexError;
struct lex_error_t {
    LexError *next;
    char *msg;
    Point start;
    Point point_of_failure;
};


// TODO: should also keep track of the position of each newline ?
typedef struct lexer_t {
    char *input; // The input string being scanned.
    u32 pos_start;
    u32 pos_current;
    Point start; // Start point of the current token being processed
    Point current; // Current point in the input
    bool has_next;
    Token next;

    // TODO: maybe not the best datastructure
    Str8 *str_list;
    u32 str_list_len;
    u32 str_list_cap;

    u32 n_errors;
    LexError *err_head;
    LexError *err_tail;
} Lexer;


void lex_init(Lexer *lexer, char *input);
Token lex_next(Arena *arena, Lexer *lexer);
Token lex_peek(Arena *arena, Lexer *lexer);


/* Debug stuff */
void token_print(Token token);
extern char *token_type_str_map[TOKEN_TYPE_ENUM_COUNT];


#endif /* LEX_H */

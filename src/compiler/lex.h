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

typedef struct error_handler_t ErrorHandler; // forward decl from error.h

typedef struct {
    u32 l; // Line
    u32 c; // Column
} Point;

typedef enum {
    TOKEN_ERR = 0,

    // Literal Types
    TOKEN_NUM,
    TOKEN_STR,

    TOKEN_COLON,
    TOKEN_ASSIGNMENT, // :=

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
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_DOT,
    TOKEN_COMMA,
    TOKEN_AMPERSAND, // &
    TOKEN_CARET, // ^
    TOKEN_AT, // @
    TOKEN_EOF,

    // Identifier and reserved words
    TOKEN_IDENTIFIER,
    TOKEN_FUNC,
    TOKEN_STRUCT,
    TOKEN_ENUM,
    TOKEN_BEGIN,
    TOKEN_END,
    TOKEN_RETURN,
    TOKEN_PRINT,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_IF,
    TOKEN_THEN,
    TOKEN_ELSE,
    TOKEN_WHILE,
    TOKEN_DO,
    TOKEN_VAR,
    TOKEN_NULL,
    TOKEN_COMPILER,

    TOKEN_TYPE_ENUM_COUNT,
} TokenKind;

typedef struct {
    TokenKind kind;
    Point start;
    Point end;
    Str8View lexeme; // For identifiers and strings, these are actually arena allocated Str8's
} Token;

typedef struct lexer_t {
    ErrorHandler *e;
    char *input; // The input string being scanned.
    u32 pos_start;
    u32 pos_current;
    Point start; // Start point of the current token being processed
    Point current; // Current point in the input
    // NOTE: If we want to store more than one next token we could use a ring buffer
    bool has_next;
    Token next;
} Lexer;


void lex_init(Lexer *lexer, ErrorHandler *e, char *input);
Token lex_next(Arena *arena, Lexer *lexer);
Token lex_peek(Arena *arena, Lexer *lexer);


/* Debug stuff */
void token_print(Token token);
extern char *token_type_str_map[TOKEN_TYPE_ENUM_COUNT];


#endif /* LEX_H */

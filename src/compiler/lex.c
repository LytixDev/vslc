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
#include "lex.h"
#include "base/base.h"
#include <stdio.h>
#include <string.h>

#ifndef EOF
#define EOF (-1)
#endif /* EOF */


char *reserved_words[] = {
    "func", "begin", "end", "return", "print", "break", "if", "then", "else", "while", "do", "var",
};


static Token lex_ident(Arena *arena, Lexer *lexer);
static Token lex_num(Lexer *lexer);
static Token lex_str(Arena *arena, Lexer *lexer);
static Token lex_comment(Arena *arena, Lexer *lexer);

static void reset_token_ctx(Lexer *lexer)
{
    lexer->pos_start = lexer->pos_current;
    lexer->start = lexer->current;
}

static LexError *make_lex_error(Arena *arena, Lexer *lexer, char *msg)
{
    LexError *lex_error = m_arena_alloc(arena, sizeof(LexError));
    size_t msg_len = strlen(msg) + 1; // TODO: avoid strlen?
    lex_error->msg = m_arena_alloc(arena, msg_len);
    memcpy(lex_error->msg, msg, msg_len);
    lex_error->start = lexer->start;
    lex_error->point_of_failure = lexer->current;
    lex_error->next = NULL;
    return lex_error;
}

static void lex_error_append(Arena *arena, Lexer *lexer, char *msg)
{
    LexError *lex_error = make_lex_error(arena, lexer, msg);
    if (lexer->err_head == NULL) {
        lexer->err_head = lex_error;
    } else {
        lexer->err_tail->next = lex_error;
    }
    lexer->err_tail = lex_error;

    lexer->n_errors++;

    reset_token_ctx(lexer);
}


static u32 str_list_push(Lexer *lexer, Str8 str)
{
    if (lexer->str_list_len == lexer->str_list_cap) {
        lexer->str_list_cap *= 2;
        lexer->str_list = realloc(lexer->str_list, lexer->str_list_cap);
    }
    lexer->str_list[lexer->str_list_len] = str;
    lexer->str_list_len++;
    return lexer->str_list_len - 1;
}

static char next(Lexer *lexer)
{
    char c = lexer->input[lexer->pos_current];
    if (c == 0)
        return EOF;
    if (c == '\n')
        lexer->current.l++;

    lexer->pos_current++;
    lexer->current.c++;
    return c;
}

static char peek(Lexer *lexer)
{
    return lexer->input[lexer->pos_current];
}

static bool match(Lexer *lexer, char expected)
{
    if (peek(lexer) == expected) {
        next(lexer);
        return true;
    }
    return false;
}

static void backup(Lexer *lexer)
{
    if (lexer->pos_current == 0)
        ASSERT_NOT_REACHED;

    lexer->current.c--;
    lexer->pos_current--;
    if (peek(lexer) == '\n')
        lexer->current.l--;
}

static bool accept(Lexer *lexer, char *accept_list)
{
    char c = next(lexer);
    for (char *accept = accept_list; *accept != 0; accept++) {
        if (*accept == c)
            return true;
    }
    backup(lexer);
    return false;
}

static void accept_run(Lexer *lexer, char *accept_list)
{
    while (accept(lexer, accept_list))
        ;
}

static Token emit(Lexer *lexer, TokenType type)
{
    Token token = {
        .type = type,
        .start = lexer->start,
        .end = lexer->current,
        .lexeme = (StrView8){ .str = (u8 *)(lexer->input) + lexer->pos_start,
                              .len = lexer->pos_current - lexer->pos_start + 1 },
    };

    /* Set value based on type */
    if (type == TOKEN_NUM) {
        // TODO: convert to number

        char *lexeme[token.lexeme.len + 1];
        memcpy(lexeme, token.lexeme.str, token.lexeme.len);
        lexeme[token.lexeme.len] = 0;
        token.num_value = atof((const char *)lexeme);
    }

    reset_token_ctx(lexer);
    return token;
}

static Token emit_str(Lexer *lexer, StrBuilder *sb, TokenType type)
{
    Token token = emit(lexer, type);
    token.str_list_idx = str_list_push(lexer, sb->str);
    return token;
}

static bool is_numeric(char c)
{
    return c >= '0' && c <= '9';
}

static bool is_alpha(char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_valid_identifier(char c)
{
    return is_numeric(c) || is_alpha(c) || c == '_';
}

void lex_init(Lexer *lexer, char *input)
{
    *lexer = (Lexer){
        .input = input,
        .pos_start = 0,
        .pos_current = 0,
        .start = (Point){ 0 },
        .current = (Point){ 0 },
        .str_list_len = 0,
        .str_list_cap = 16,
        .n_errors = 0,
        .err_head = NULL,
        .err_tail = NULL,
    };

    lexer->str_list = malloc(sizeof(Str8) * lexer->str_list_cap);
}

Token lex_peek(Arena *arena, Lexer *lexer)
{
    Token next = lex_next(arena, lexer);
    lexer->has_next = true;
    lexer->next = next;
    return next;
}

Token lex_next(Arena *arena, Lexer *lexer)
{
    if (lexer->has_next) {
        lexer->has_next = false;
        return lexer->next;
    }
    while (1) {
        char c = next(lexer);
        switch (c) {
        case EOF:
            return (Token){ .type = TOKEN_EOF };

        /* Whitespace */
        case ' ':
        case '\n':
        case '\t':
        case '\r':
        case '\v':
            reset_token_ctx(lexer);
            break;

        /* Comments or slash (divide) */
        case '/': {
            if (match(lexer, '/')) {
                return lex_comment(arena, lexer);
            }
            return emit(lexer, TOKEN_SLASH);
            break;
        };

        /* Strings */
        case '"':
            return lex_str(arena, lexer);

        /* Single-character tokens */
        case '+':
            return emit(lexer, TOKEN_PLUS);
        case '-':
            return emit(lexer, TOKEN_MINUS);
        case '*':
            return emit(lexer, TOKEN_STAR);
        case '(':
            return emit(lexer, TOKEN_LPAREN);
        case ')':
            return emit(lexer, TOKEN_RPAREN);
        case '=':
            return emit(lexer, TOKEN_EQ);
        case ',':
            return emit(lexer, TOKEN_COMMA);

        /* Single- or two-character tokens */
        case '<':
            return match(lexer, '<') ? emit(lexer, TOKEN_LSHIFT) : emit(lexer, TOKEN_LESS);
        case '>':
            return match(lexer, '>') ? emit(lexer, TOKEN_RSHIFT) : emit(lexer, TOKEN_GREATER);
        case ':': {
            if (match(lexer, '=')) {
                return emit(lexer, TOKEN_ASSIGNMENT);
            } else {
                lex_error_append(arena, lexer, "Expected '=' after ':'");
                return (Token){ .type = TOKEN_ERR };
            }
        };
        case '!': {
            if (match(lexer, '=')) {
                return emit(lexer, TOKEN_NEQ);
            } else {
                lex_error_append(arena, lexer, "Expected '=' after '!'");
                return (Token){ .type = TOKEN_ERR };
            }
        };

        default: {
            if (is_numeric(c)) {
                return lex_num(lexer);
            }
            if (!(is_alpha(c))) {
                lex_error_append(arena, lexer, "Unrecognized character");
                return (Token){ .type = TOKEN_ERR };
            }
            /* Reserved words and identifiers */
            return lex_ident(arena, lexer);
        }
        }
    }
}

static Token lex_ident(Arena *arena, Lexer *lexer)
{
    char c;
    do {
        c = next(lexer);
    } while (is_valid_identifier(c));

    backup(lexer);
    char *ident = lexer->input + lexer->pos_start;
    u32 ident_len = lexer->pos_current - lexer->pos_start;

    /* Check if identifier is a keyword */
    size_t reserved_words_len = ARRAY_LENGTH(reserved_words);
    // TODO: Linear search could be optimized into a search based on a hash map.
    for (size_t i = 0; i < reserved_words_len; i++) {
        char *reserved = reserved_words[i];
        size_t this_len = (u32)strlen(reserved);
        if (ident_len != this_len) {
            continue;
        }

        bool match = true;
        for (size_t j = 0; j < ident_len; j++) {
            if (ident[j] != reserved[j]) {
                match = false;
                break;
            }
        }
        if (match) {
            return emit(lexer, TOKEN_FUNC + i);
        }
    }

    StrBuilder sb = make_str_builder(arena);
    str_builder_append_cstr(&sb, ident, ident_len);
    str_builder_append_u8(&sb, 0);
    return emit_str(lexer, &sb, TOKEN_IDENTIFIER);
}

static Token lex_num(Lexer *lexer)
{
    char *digits = "0123456789";
    accept_run(lexer, digits);
    return emit(lexer, TOKEN_NUM);
}

static Token lex_str(Arena *arena, Lexer *lexer)
{
    /* Came from '"' */
    bool had_error = false;
    StrBuilder sb = make_str_builder(arena);
    char c;
    while ((c = next(lexer)) != '"') {
        if (c == EOF || c == '\n') {
            lex_error_append(arena, lexer, "Recieved newline or EOF inside string literal");
            return (Token){ .type = TOKEN_ERR };
        }
        if (c == '\\') {
            c = next(lexer);
            if (c != '"') {
                lex_error_append(arena, lexer, "Unknown escape character inside string literal");
                had_error = true;
                continue; // To gracefully recover
            }
        }
        str_builder_append_u8(&sb, (u8)c);
    }

    if (had_error) {
        reset_token_ctx(lexer);
        return (Token){ .type = TOKEN_ERR };
    }
    str_builder_append_u8(&sb, 0);
    return emit_str(lexer, &sb, TOKEN_STR);
}

static Token lex_comment(Arena *arena, Lexer *lexer)
{
    /* Came from '//' */
    char c;
    while ((c = next(lexer)) != '\n') {
        if (c == EOF) {
            return emit(lexer, TOKEN_EOF);
        }
    }

    /* Comments are ignored, so we need to lex the next non-ignored token */
    reset_token_ctx(lexer);
    return lex_next(arena, lexer);
}

/* Debug stuff */
char *token_type_str_map[TOKEN_TYPE_ENUM_COUNT] = {
    "ERR",    "NUM",    "STR",        "ASSIGNMENT", "PLUS",  "MINUS",   "STAR",   "SLASH",
    "LSHIFT", "RSHIFT", "EQ",         "NEQ",        "LESS",  "GREATER", "LPAREN", "RPAREN",
    "COMMA",  "EOF",    "IDENTIFIER", "FUNC",       "BEGIN", "END",     "RETURN", "PRINT",
    "BREAK",  "IF",     "THEN",       "ELSE",       "WHILE", "DO",      "VAR",
};


void token_print(Token token)
{
    printf("%s\n", token_type_str_map[token.type]);
}

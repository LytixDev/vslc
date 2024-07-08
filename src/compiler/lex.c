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
#include <string.h>

#ifndef EOF
#define EOF -1
#endif /* EOF */


static Token lex_num(Lexer *lexer);
static Token lex_str(Arena *arena, Lexer *lexer);
static Token lex_comment(Lexer *lexer);


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

static void reset_token_ctx(Lexer *lexer)
{
	lexer->pos_start = lexer->pos_current;
	lexer->start = lexer->current;
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

static Token emit_str(Lexer *lexer, StrBuilder *sb)
{
    Token token = emit(lexer, TOKEN_STR);
    token.str_list_idx = str_list_push(lexer, sb->str);
    return token;
}

static bool is_numeric(char c)
{
	return c >= '0' && c <= '9';
}

void lex_init(Lexer *lexer, char *input)
{
	*lexer = (Lexer){
		.had_error = false,
		.input = input,
		.pos_start = 0,
		.pos_current = 0,
		.start = (Point){ 0 },
		.current = (Point){ 0 },
        .str_list_len = 0,
        .str_list_cap = 16,
	};

    lexer->str_list = malloc(sizeof(Str8) * lexer->str_list_cap);
}

Token lex_peek(Arena *arena, Lexer *lexer, u32 lookahead)
{
	// TODO: this is stupid because we do work which we later discard that work
	//       instead we should not reset the state of the lexer, but instead
	//       store the result of the previous work we did
	Lexer lexer_save = *lexer;
	for (u32 i = 0; i < lookahead - 1; i++) {
        lex_next(arena, lexer);
	}
	Token final = lex_next(arena, lexer);
	*lexer = lexer_save;
	return final;
}

Token lex_next(Arena *arena, Lexer *lexer)
{
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
                return lex_comment(lexer);
            }
            return emit(lexer, TOKEN_SLASH);
            break;
        };

        /* Strings */
        case '"':
            return lex_str(arena, lexer);

		/* Single character tokens */
		case '+':
			return emit(lexer, TOKEN_PLUS);
		case '*':
			return emit(lexer, TOKEN_STAR);
		case ';':
			return emit(lexer, TOKEN_SEMICOLON);
		case '(':
			return emit(lexer, TOKEN_LPAREN);
		case ')':
			return emit(lexer, TOKEN_RPAREN);

		default: {
			if (is_numeric(c)) {
				return lex_num(lexer);
			}
		}
	    }
	}
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
    StrBuilder sb = make_str_builder(arena);
    char c;
	while ((c = next(lexer)) != '"') {
		if (c == EOF || c == '\n') {
            // Error
        }
        if (c == '\\') {
            c = next(lexer);
            if (c != '"') {
                // Unknown escape sequence
            }
        }
        str_builder_append_u8(&sb, (u8)c);
	}
    str_builder_append_u8(&sb, 0);
    return emit_str(lexer, &sb);
}

static Token lex_comment(Lexer *lexer)
{
    /* Came from '//' */
    char c;
	while ((c = next(lexer)) != '\n') {
		if (c == EOF) {
            // Error
        }
	}
    reset_token_ctx(lexer);
    return (Token){ .type = TOKEN_ERR }; // Ignored
}

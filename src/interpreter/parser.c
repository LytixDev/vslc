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
#include <string.h>

#include "ast.h"
#include "base/base.h"
#include "base/sac_single.h"
#include "lex.h"
#include "parser.h"

TokenType token_precedences[TOKEN_TYPE_LEN] = {
	0, /* TOKEN_ERR */
	1, /* TOKEN_NUM */
	10, /* TOKEN_PLUS */
	20, /* TOKEN_STAR */
	0, /* TOKEN_LPAREN */
	0, /* TOKEN_RPAREN */
	1, /* TOKEN_SEMICOLON */
	1, /* TOKEN_EOF */
};

static AstExpr *parse_expr(Parser *parser, u32 precedence);

static ParseError *make_parse_error(Arena *arena, Token *failed, ParseErrorType pet, char *msg)
{
	ParseError *parse_error = m_arena_alloc(arena, sizeof(ParseError));
	if (msg != NULL) {
		size_t msg_len = strlen(msg) + 1; // TODO: avoid strlen?
		parse_error->msg = m_arena_alloc(arena, msg_len);
		memcpy(parse_error->msg, msg, msg_len);
	} else {
		parse_error->msg = NULL;
	}

	parse_error->type = pet;
	parse_error->failed = failed;
	parse_error->next = NULL;
	return parse_error;
}

static void parse_error_append(Parser *parser, Token *failed, ParseErrorType pet, char *msg)
{
	ParseError *parse_error = make_parse_error(&parser->arena, failed, pet, msg);
	if (parser->err_head == NULL) {
		parser->err_head = parse_error;
	} else {
		parser->err_tail->next = parse_error;
	}
	parser->err_tail = parse_error;

	parser->n_errors++;
}

static AstExprBinary *make_binary(Arena *arena, AstExpr *left, TokenType op, AstExpr *right)
{
	AstExprBinary *binary = m_arena_alloc(arena, sizeof(AstExprBinary));
	binary->type = EXPR_BINARY;
	binary->op = op;
	binary->left = left;
	binary->right = right;
	return binary;
}

static AstExprLiteral *make_literal(Arena *arena, Token token)
{
	AstExprLiteral *literal = m_arena_alloc(arena, sizeof(AstExprLiteral));
	literal->type = EXPR_LITERAL;
	literal->lit_type = LIT_NUM;
	literal->num_value = token.num_value;
	return literal;
}


static bool is_bin_op(Token token)
{
	switch (token.type) {
	case TOKEN_PLUS:
	case TOKEN_STAR:
		return true;
	default:
		return false;
	}
}

static Token consume_or_err(Parser *parser, TokenType expected, ParseErrorType pet)
{
	Token token = lex_advance(&parser->lexer);
	if (token.type != expected) {
		parse_error_append(parser, &token, pet, NULL);
		return (Token){ .type = TOKEN_ERR };
	}
	return token;
}

static AstExpr *parse_primary(Parser *parser)
{
	Token token = lex_advance(&parser->lexer);
	switch (token.type) {
	case TOKEN_LPAREN: {
		AstExpr *expr = parse_expr(parser, 0);
		Token err = consume_or_err(parser, TOKEN_RPAREN, PET_EXPECTED_RPAREN);
		if (err.type == TOKEN_ERR) {
			// TODO: gracefully continue
		}
		return expr;
	}
	case TOKEN_NUM:
		return (AstExpr *)make_literal(&parser->arena, token);
	default:
		ASSERT_NOT_REACHED;
	}
}

static AstExpr *parse_increasing_precedence(Parser *parser, AstExpr *left, u32 precedence)
{
	Token next = lex_peek(&parser->lexer, 1);
	if (!is_bin_op(next))
		return left;

	u32 next_precedence = token_precedences[next.type];
	if (precedence >= next_precedence)
		return left;

	lex_advance(&parser->lexer);
	AstExpr *right = parse_expr(parser, next_precedence);
	return (AstExpr *)make_binary(&parser->arena, left, next.type, right);
}

static AstExpr *parse_expr(Parser *parser, u32 precedence)
{
	AstExpr *left = parse_primary(parser);
	AstExpr *expr;
	while (1) {
		expr = parse_increasing_precedence(parser, left, precedence);
		if (expr == left) // pointer comparison
			break;

		left = expr;
	}

	return left;
}

ParseResult parse(char *input)
{
	Lexer lexer = {
		.had_error = false,
		.input = input,
		.pos_start = 0,
		.pos_current = 0,
		.start = (Point){ 0 },
		.current = (Point){ 0 },
		.state = lex_any,
	};
	Parser parser = {
		.lexer = lexer,
		.n_errors = 0,
		.err_head = NULL,
		.err_tail = NULL,
	};
	m_arena_init_dynamic(&parser.arena, 2, 512);

	AstExpr *head = parse_expr(&parser, 0);

	return (ParseResult){
		.n_errors = parser.n_errors,
		.err_head = parser.err_head,
		.head = head,
	};
}

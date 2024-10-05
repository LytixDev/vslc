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
#include <string.h>

#include "ast.h"
#include "base/base.h"
#include "base/sac_single.h"
#include "lex.h"
#include "parser.h"

TokenType token_precedences[TOKEN_TYPE_ENUM_COUNT] = {
    0, // TOKEN_ERR,
    0, // TOKEN_NUM,
    0, // TOKEN_STR,
    1, // TOKEN_ASSIGNMENT,
    5, // TOKEN_PLUS,
    5, // TOKEN_MINUS,
    10, // TOKEN_STAR,
    10, // TOKEN_SLASH,
    4, // TOKEN_LSHIFT,
    4, // TOKEN_RSHIFT,
    3, // TOKEN_EQ,
    3, // TOKEN_NEQ,
    3, // TOKEN_LESS,
    3, // TOKEN_GREATER,
    0, // TOKEN_LPAREN,
    0, // TOKEN_RPAREN,
    0, // TOKEN_EOF,
    0, // TOKEN_COMMA,
    0, // TOKEN_IDENTIFIER,
    0, // TOKEN_FUNC,
    0, // TOKEN_BEGIN,
    0, // TOKEN_END,
    0, // TOKEN_RETURN,
    0, // TOKEN_PRINT,
    0, // TOKEN_BREAK,
    0, // TOKEN_IF,
    0, // TOKEN_THEN,
    0, // TOKEN_ELSE,
    0, // TOKEN_WHILE,
    0, // TOKEN_DO,
    0, // TOKEN_VAR,
};

char *PARSE_ERROR_MSGS[PET_LEN] = {
    "Expected ')' to terminate the group expression", /* PET_EXPECTED_RPAREN */
    "Expected 'do' keyword to start the while-loop", /* PET_EXPECTED_DO */
    "Expected 'then' keyword after if-statement condition", /* PET_EXPECTED_THEN */
    "", /* PET_CUSTOME */
};

static AstExpr *parse_expr(Parser *parser, u32 precedence);
static AstStmt *parse_stmt(Parser *parser);

/* Wrapper so we can print the token in debug mode */
static inline Token next_token(Parser *parser)
{
    Token token = lex_next(&parser->arena, &parser->lexer);
#ifdef DEBUG
    printf("Consumed: %s\n", token_type_str_map[token.type]);
#endif
    return token;
}

static inline Token peek_token(Parser *parser)
{
    Token token = lex_peek(&parser->arena, &parser->lexer);
#ifdef DEBUG
    printf("Peek: %s\n", token_type_str_map[token.type]);
#endif
    return token;
}


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

#ifdef DEBUG
    fprintf(stderr, "%s\n", msg);
#endif
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

static bool is_bin_op(Token token)
{
    switch (token.type) {
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_STAR:
    case TOKEN_SLASH:
    case TOKEN_LSHIFT:
    case TOKEN_RSHIFT:
        return true;
    default:
        return false;
    }
}

static bool is_relation_op(Token token)
{
    switch (token.type) {
    case TOKEN_EQ:
    case TOKEN_NEQ:
    case TOKEN_LESS:
    case TOKEN_GREATER:
        return true;
    default:
        return false;
    }
}

static Token consume_or_err(Parser *parser, TokenType expected, ParseErrorType pet)
{
    Token token = peek_token(parser);
    if (token.type != expected) {
        next_token(parser);
        parse_error_append(parser, &token, pet, NULL);
        return (Token){ .type = TOKEN_ERR };
    }
    next_token(parser);
    return token;
}

static AstExpr *parse_primary(Parser *parser)
{
    Token token = next_token(parser);
    switch (token.type) {
    case TOKEN_LPAREN: {
        AstExpr *expr = parse_expr(parser, 0);
        Token err = consume_or_err(parser, TOKEN_RPAREN, PET_EXPECTED_RPAREN);
        if (err.type == TOKEN_ERR) {
            // TODO: gracefully continue
            fprintf(stderr, "err");
        }
        return expr;
    }
    case TOKEN_MINUS: {
        /* Unary minus */
        AstExpr *expr = parse_expr(parser, 0);
        return (AstExpr *)make_unary(&parser->arena, expr, TOKEN_MINUS);
    }
    case TOKEN_NUM:
    case TOKEN_IDENTIFIER:
        return (AstExpr *)make_literal(&parser->arena, token);
    default:
        ASSERT_NOT_REACHED;
    }
}

static AstExpr *parse_increasing_precedence(Parser *parser, AstExpr *left, u32 precedence)
{
    Token next = peek_token(parser);
    if (!is_bin_op(next))
        return left;

    u32 next_precedence = token_precedences[next.type];
    if (precedence >= next_precedence)
        return left;

    next_token(parser);
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

static AstExprBinary *parse_relation(Parser *parser)
{
    AstExpr *left = parse_expr(parser, 0);
    Token op = peek_token(parser);
    if (!is_relation_op(op)) {
        make_parse_error(&parser->arena, &op, PET_CUSTOME, "Expected a relation operator.");
    } else {
        next_token(parser);
    }
    AstExpr *right = parse_expr(parser, 0);
    return make_binary(&parser->arena, left, op.type, right);
}

static AstExpr *parse_expr_list(Parser *parser, bool allow_str)
{
    /* allow_str is true means the list parsers strings and/or exprs */
    AstExpr *expr;
    if (allow_str && peek_token(parser).type == TOKEN_STR) {
        expr = (AstExpr *)make_literal(&parser->arena, next_token(parser));
    } else {
        expr = parse_expr(parser, 0);
    }
    if (!(peek_token(parser).type == TOKEN_COMMA)) {
        return expr;
    }

    AstExprList *list = make_list(&parser->arena, expr);
    AstExprListNode *list_node;
    do {
        next_token(parser);
        if (allow_str && peek_token(parser).type == TOKEN_STR) {
            expr = (AstExpr *)make_literal(&parser->arena, next_token(parser));
        } else {
            expr = parse_expr(parser, 0);
        }
        list_node = make_list_node(&parser->arena, expr);
        list->tail->next = list_node;
        list->tail = list_node;
    } while (peek_token(parser).type == TOKEN_COMMA);

    return (AstExpr *)list;
}


/* Parsing of statements */
static AstStmtPrint *parse_print(Parser *parser)
{
    /* Came from TOKEN_PRINT */
    AstExpr *print_list = parse_expr_list(parser, true);
    return make_print(&parser->arena, print_list);
}

static AstStmtWhile *parse_while(Parser *parser)
{
    /* Came from TOKEN_WHILE */
    AstExpr *condition = (AstExpr *)parse_relation(parser);
    consume_or_err(parser, TOKEN_DO, PET_EXPECTED_DO);
    AstStmt *body = parse_stmt(parser);
    return make_while(&parser->arena, condition, body);
}

static AstStmtIf *parse_if(Parser *parser)
{
    /* Came from TOKEN_IF */
    AstExpr *condition = (AstExpr *)parse_relation(parser);
    consume_or_err(parser, TOKEN_THEN, PET_EXPECTED_THEN);
    AstStmt *then = parse_stmt(parser);
    AstStmt *else_ = NULL;
    if (peek_token(parser).type == TOKEN_ELSE) {
        next_token(parser);
        else_ = parse_stmt(parser);
    }

    return make_if(&parser->arena, condition, then, else_);
}

static AstStmtBlock *parse_block(Parser *parser)
{
    /* Came from TOKEN_BLOCK */
    AstStmt *first = parse_stmt(parser);
    AstStmtList *stmts = make_stmt_list(&parser->arena, first);

    Token next;
    while ((next = peek_token(parser)).type != TOKEN_END) {
        if (next.type == TOKEN_EOF) {
            parse_error_append(parser, &next, PET_CUSTOME,
                               "Unexpected EOF inside a block. Expected END");
            break;
        }
        AstStmt *stmt = parse_stmt(parser);
        AstStmtListNode *list_node = make_stmt_list_node(&parser->arena, stmt);
        stmts->tail->next = list_node;
        stmts->tail = list_node;
    }

    /* Consume the END token */
    if (next.type != TOKEN_EOF) {
        next_token(parser);
    }
    return make_block(&parser->arena, NULL, stmts);
}

static AstStmt *parse_stmt(Parser *parser)
{
    Token token = next_token(parser);
    switch (token.type) {
    case TOKEN_WHILE:
        return (AstStmt *)parse_while(parser);
    case TOKEN_IF:
        return (AstStmt *)parse_if(parser);
    case TOKEN_PRINT:
        return (AstStmt *)parse_print(parser);
    case TOKEN_BEGIN:
        return (AstStmt *)parse_block(parser);
    default:
        parse_error_append(parser, &token, PET_CUSTOME, "Unrecognized token ...");
        return NULL;
    }
}

ParseResult parse(char *input)
{
    Parser parser = {
        .n_errors = 0,
        .err_head = NULL,
        .err_tail = NULL,
    };
    lex_init(&parser.lexer, input);
    m_arena_init_dynamic(&parser.arena, 2, 512);

    AstStmt *head = parse_stmt(&parser);
    return (ParseResult){
        .n_errors = parser.n_errors,
        .err_head = parser.err_head,
        .head = head,
        .str_list = parser.lexer.str_list,
        .str_list_len = parser.lexer.str_list_len,
    };
}

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
    0, // TOKEN_COLON,
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
    "Expected ']' to terminate the array indexing", /* PET_EXPECTED_RBRACKET */
    "Expected 'do' keyword to start the while-loop", /* PET_EXPECTED_DO */
    "Expected 'then' keyword after if-statement condition", /* PET_EXPECTED_THEN */
    "Expected ')' to end function call ", /* PET_EXPCETED_CALL_END */
    "", /* PET_CUSTOME */
};

/* Forwards */
static AstExpr *parse_expr(Parser *parser, u32 precedence);
static AstNode *parse_expr_list(Parser *parser);
static AstStmt *parse_stmt(Parser *parser);
static TypedVarList parse_local_decl_list(Parser *parser);

/* Wrapper so we can print the token in debug mode */
static Token next_token(Parser *parser)
{
    Token token = lex_next(parser->lex_arena, &parser->lexer);
#ifdef DEBUG
    printf("Consumed: %s\n", token_type_str_map[token.type]);
#endif
    return token;
}

static Token peek_token(Parser *parser)
{
    // TODO: if we need more lookahead, this must change
    // assert(!parser->lexer.has_next);
    Token token = lex_peek(parser->lex_arena, &parser->lexer);
    // #ifdef DEBUG
    //     printf("Peek: %s\n", token_type_str_map[token.type]);
    // #endif
    return token;
}

// TODO: use this in more places
static bool match_token(Parser *parser, TokenType type)
{
    if (peek_token(parser).type == type) {
        next_token(parser);
        return true;
    }
    return false;
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
    ParseError *parse_error = make_parse_error(parser->arena, failed, pet, msg);
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

static AstTypeInfo parse_type(Parser *parser, bool allow_array_types)
{
    consume_or_err(parser, TOKEN_COLON, PET_CUSTOM);
    Token name = consume_or_err(parser, TOKEN_IDENTIFIER, PET_CUSTOM);
    if (!match_token(parser, TOKEN_LBRACKET)) {
        return (AstTypeInfo){ .name = name.str_list_idx, .is_array = false };
    }
    if (!allow_array_types) {
        consume_or_err(parser, TOKEN_RBRACKET, PET_CUSTOM);
        return (AstTypeInfo){ .name = name.str_list_idx, .is_array = false };
    }

    s32 elements = -1;
    Token peek = peek_token(parser);
    if (peek.type == TOKEN_NUM) {
        next_token(parser);
        elements = peek.num_value;
    }
    consume_or_err(parser, TOKEN_RBRACKET, PET_CUSTOM);
    return (AstTypeInfo){ .name = name.str_list_idx, .is_array = true, .elements = elements };
}

static AstExprCall *parse_call(Parser *parser, Token identifier)
{
    /* Came from TOKEN_IDENTIFIER and then peeked TOKEN_LPAREN */
    next_token(parser);
    AstNode *expr_list = NULL;
    if (!match_token(parser, TOKEN_RPAREN)) {
        expr_list = parse_expr_list(parser);
        consume_or_err(parser, TOKEN_RPAREN, PET_EXPCETED_CALL_END);
    }
    return make_call(parser->arena, identifier.str_list_idx, expr_list);
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
        return (AstExpr *)make_unary(parser->arena, expr, TOKEN_MINUS);
    }
    case TOKEN_NUM:
    case TOKEN_IDENTIFIER: {
        Token next = peek_token(parser);
        if (next.type == TOKEN_LPAREN) {
            return (AstExpr *)parse_call(parser, token);
        } else if (next.type == TOKEN_LBRACKET) {
            /* Parse array indexing as a binary op */
            next_token(parser);
            AstExpr *left = (AstExpr *)make_literal(parser->arena, token);
            AstExpr *right = parse_expr(parser, 0);
            consume_or_err(parser, TOKEN_RBRACKET, PET_EXPECTED_RBRACKET);
            return (AstExpr *)make_binary(parser->arena, left, next.type, right);
        } else {
            /* Parse single identifier */
            return (AstExpr *)make_literal(parser->arena, token);
        }
    }
    default:
        // TODO: gracefully continue
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
    return (AstExpr *)make_binary(parser->arena, left, next.type, right);
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
        make_parse_error(parser->arena, &op, PET_CUSTOM, "Expected a relation operator.");
    } else {
        next_token(parser);
    }
    AstExpr *right = parse_expr(parser, 0);
    return make_binary(parser->arena, left, op.type, right);
}

static AstNode *parse_expr_list(Parser *parser)
{
    AstExpr *expr;
    if (peek_token(parser).type == TOKEN_STR) {
        expr = (AstExpr *)make_literal(parser->arena, next_token(parser));
    } else {
        expr = parse_expr(parser, 0);
    }
    if (!(peek_token(parser).type == TOKEN_COMMA)) {
        // TODO: We could simplify certain parts of the compiler of the expr list always
        //       wrapped a single literal inside a list.
        return (AstNode *)expr;
    }

    AstList *list = make_list(parser->arena, (AstNode *)expr);
    AstListNode *list_node;
    do {
        next_token(parser);
        if (peek_token(parser).type == TOKEN_STR) {
            expr = (AstExpr *)make_literal(parser->arena, next_token(parser));
        } else {
            expr = parse_expr(parser, 0);
        }
        list_node = make_list_node(parser->arena, (AstNode *)expr);
        ast_list_push_back(list, list_node);
    } while (peek_token(parser).type == TOKEN_COMMA);

    return (AstNode *)list;
}


/* Parsing of statements */
static AstStmtWhile *parse_while(Parser *parser)
{
    /* Came from TOKEN_WHILE */
    AstExpr *condition = (AstExpr *)parse_relation(parser);
    consume_or_err(parser, TOKEN_DO, PET_EXPECTED_DO);
    AstStmt *body = parse_stmt(parser);
    return make_while(parser->arena, condition, body);
}

static AstStmtIf *parse_if(Parser *parser)
{
    /* Came from TOKEN_IF */
    AstExpr *condition = (AstExpr *)parse_relation(parser);
    consume_or_err(parser, TOKEN_THEN, PET_EXPECTED_THEN);
    AstStmt *then = parse_stmt(parser);
    AstStmt *else_ = NULL;
    if (match_token(parser, TOKEN_ELSE)) {
        else_ = parse_stmt(parser);
    }
    return make_if(parser->arena, condition, then, else_);
}

static AstStmtBlock *parse_block(Parser *parser)
{
    /* Came from TOKEN_BLOCK */
    TypedVarList declarations = { 0 };
    if (match_token(parser, TOKEN_VAR)) {
        declarations = parse_local_decl_list(parser);
    }

    AstStmt *first = parse_stmt(parser);
    AstList *stmts = make_list(parser->arena, (AstNode *)first);

    Token next;
    while ((next = peek_token(parser)).type != TOKEN_END) {
        if (next.type == TOKEN_EOF) {
            parse_error_append(parser, &next, PET_CUSTOM,
                               "Unexpected EOF inside a block. Expected END");
            break;
        }
        AstStmt *stmt = parse_stmt(parser);
        AstListNode *list_node = make_list_node(parser->arena, (AstNode *)stmt);
        ast_list_push_back(stmts, list_node);
    }

    /* Consume the END token */
    if (next.type != TOKEN_EOF) {
        next_token(parser);
    }
    return make_block(parser->arena, declarations, stmts);
}

static AstStmt *parse_stmt(Parser *parser)
{
    Token token = next_token(parser);
    switch (token.type) {
    case TOKEN_WHILE:
        return (AstStmt *)parse_while(parser);
    case TOKEN_IF:
        return (AstStmt *)parse_if(parser);
    case TOKEN_PRINT: {
        AstNode *print_list = parse_expr_list(parser);
        return (AstStmt *)make_single(parser->arena, STMT_PRINT, print_list);
    }
    case TOKEN_RETURN: {
        AstExpr *expr = parse_expr(parser, 0);
        return (AstStmt *)make_single(parser->arena, STMT_ABRUPT_RETURN, (AstNode *)expr);
    }
    case TOKEN_IDENTIFIER: {
        Token next = peek_token(parser);
        if (next.type == TOKEN_LPAREN) {
            /* Function call, promoted to a statement */
            AstNode *call = (AstNode *)parse_call(parser, token);
            return (AstStmt *)make_single(parser->arena, STMT_EXPR, call);
        }

        next_token(parser);
        AstExpr *left = (AstExpr *)make_literal(parser->arena, token);

        if (next.type == TOKEN_LBRACKET) {
            /* Assignment, left-hand side is an array indexing */
            AstExpr *right = parse_expr(parser, 0);
            consume_or_err(parser, TOKEN_RBRACKET, PET_EXPECTED_RBRACKET);
            left = (AstExpr *)make_binary(parser->arena, left, next.type, right);
            next = next_token(parser);
        }

        if (next.type != TOKEN_ASSIGNMENT) {
            parse_error_append(parser, &next, PET_CUSTOM, "Expected assignment");
            return NULL;
        }

        AstExpr *right = parse_expr(parser, 0);
        return (AstStmt *)make_assignment(parser->arena, left, right);
    }
    case TOKEN_BREAK:
        return (AstStmt *)make_single(parser->arena, STMT_ABRUPT_BREAK, NULL);
    case TOKEN_CONTINUE:
        return (AstStmt *)make_single(parser->arena, STMT_ABRUPT_CONTINUE, NULL);
    case TOKEN_BEGIN:
        return (AstStmt *)parse_block(parser);
    default:
        parse_error_append(parser, &token, PET_CUSTOM, "Unrecognized first token in parse_stmt");
        return NULL;
    }
}

static TypedVarList parse_variable_list(Parser *parser, bool allow_array_types)
{
    TypedVarList typed_vars = { .vars = m_arena_alloc_struct(parser->arena, TypedVar), .len = 0 };

    TypedVar *indices_head = typed_vars.vars;
    do {
        /*
         * If not first iteration of loop then we need to consume the comma we already peeked and
         * allocate space for the next identifier.
         */
        if (typed_vars.len != 0) {
            next_token(parser);
            indices_head = m_arena_alloc_struct(parser->arena, TypedVar);
        }
        Token identifier = consume_or_err(parser, TOKEN_IDENTIFIER, PET_CUSTOM);
        AstTypeInfo type_info = parse_type(parser, allow_array_types);
        TypedVar new = { .name = identifier.str_list_idx, .ast_type_info = type_info };
        /* Alloc space for next TypedVar, store current, update len and head */
        *indices_head = new;
        typed_vars.len++;
    } while (peek_token(parser).type == TOKEN_COMMA);

    return typed_vars;
}

static TypedVarList parse_local_decl_list(Parser *parser)
{
    /* Came from TOKEN_VAR */
    TypedVarList identifiers = parse_variable_list(parser, false);
    while (match_token(parser, TOKEN_VAR)) {
        TypedVarList next_identifiers = parse_variable_list(parser, false);
        /*
         * Since parse_variable_list ensures the identifiers are stored contigiously, and we do no
         * other allocations on the parser arena, consequetive retain this contigious property.
         */
        identifiers.len += next_identifiers.len;
    }
    return identifiers;
}

static AstFunc *parse_func(Parser *parser)
{
    /* Came from TOKEN_FUNC */
    // consume_or_err(parser, TOKEN_FUNC, PET_CUSTOM);
    Token identifier = consume_or_err(parser, TOKEN_IDENTIFIER, PET_CUSTOM);

    consume_or_err(parser, TOKEN_LPAREN, PET_CUSTOM);
    TypedVarList vars = { 0 };
    if (peek_token(parser).type != TOKEN_RPAREN) {
        vars = parse_variable_list(parser, true);
    }
    consume_or_err(parser, TOKEN_RPAREN, PET_CUSTOM);

    AstTypeInfo return_type = parse_type(parser, true);
    AstStmt *body = parse_stmt(parser);
    AstFunc *func = make_function(parser->arena, identifier.str_list_idx, vars, body, return_type);
    return func;
}

static AstRoot *parse_root(Parser *parser)
{
    AstList declarations = { .type = AST_LIST, .head = NULL, .tail = NULL };
    AstList functions = { .type = AST_LIST, .head = NULL, .tail = NULL };
    AstList structs = { .type = AST_LIST, .head = NULL, .tail = NULL };

    Token next;
    while ((next = next_token(parser)).type != TOKEN_EOF) {
        switch (next.type) {
        case TOKEN_FUNC: {
            AstFunc *func = parse_func(parser);
            AstListNode *func_node = make_list_node(parser->arena, (AstNode *)func);
            if (functions.head == NULL) {
                functions.head = func_node;
                functions.tail = functions.head;
            } else {
                ast_list_push_back(&functions, func_node);
            }
        }; break;
        case TOKEN_VAR: {
            /* Parse global declarations list */
            TypedVarList vars = parse_variable_list(parser, true);
            AstNodeVarList *node_vars = make_node_var_list(parser->arena, vars);
            AstListNode *node_node = make_list_node(parser->arena, (AstNode *)node_vars);
            if (declarations.head == NULL) {
                declarations.head = node_node;
                declarations.tail = declarations.head;
            } else {
                ast_list_push_back(&declarations, node_node);
            }
        }; break;
        case TOKEN_STRUCT: {
            Token name = consume_or_err(parser, TOKEN_IDENTIFIER, PET_CUSTOM);
            consume_or_err(parser, TOKEN_ASSIGNMENT, PET_CUSTOM);
            TypedVarList members = parse_variable_list(parser, true);
            AstStruct *struct_decl = make_struct(parser->arena, name.str_list_idx, members);
            AstListNode *node_node = make_list_node(parser->arena, (AstNode *)struct_decl);
            if (structs.head == NULL) {
                structs.head = node_node;
                structs.tail = structs.head;
            } else {
                ast_list_push_back(&structs, node_node);
            }
        }; break;
        default: {
            printf("Not handled oops!\n");
            break;
        };
        }
    }

    return make_root(parser->arena, declarations, functions, structs);
}


ParseResult parse(Arena *arena, Arena *lex_arena, char *input)
{
    Parser parser = {
        .arena = arena,
        .lex_arena = lex_arena,
        .n_errors = 0,
        .err_head = NULL,
        .err_tail = NULL,
    };
    lex_init(&parser.lexer, input);

    AstRoot *head = parse_root(&parser);
    return (ParseResult){
        .n_errors = parser.n_errors,
        .err_head = parser.err_head,
        .head = head,
        .str_list = parser.lexer.str_list,
    };
}

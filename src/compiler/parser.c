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
#include "base/sac_single.h"
#include "error.h"
#include "lex.h"
#include "parser.h"

TokenKind token_precedences[TOKEN_TYPE_ENUM_COUNT] = {
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
    0, // TOKEN_LBRACKET,
    0, // TOKEN_RBRACKET,
    15, // TOKEN_DOT,
    0, // TOKEN_COMMA,
    0, // TOKEN_AMPERSAND,
    0, // TOKEN_CARET,
    0, // TOKEN_AT,
    0, // TOKEN_EOF,
    0, // TOKEN_IDENTIFIER,
    0, // TOKEN_FUNC,
    0, // TOKEN_STRUCT,
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
    0, // TOKEN_NULL,
    0, // TOKEN_COMPILER,
};

/* Forwards */
static AstExpr *parse_expr(Parser *parser, u32 precedence);
static AstList *parse_expr_list(Parser *parser);
static AstStmt *parse_stmt(Parser *parser);
static TypedIdentList parse_local_decl_list(Parser *parser);

static Token next_token(Parser *parser)
{
    if (parser->unlex) {
        parser->unlex = false;
        return parser->previous;
    }
    Token token = lex_next(parser->lex_arena, &parser->lexer);
    parser->previous = token;
    /*
     * NOTE on error handling:
     * There is no proper strategy in the parser for error recovery. Once we detect a error lex
     * error, the strategy is to give a reasonable error message and then exit. On detecting a
     * parse error, we optimistically continue parsing the whole source. This leads to many false
     * positives. Down the line, it would be nice to have a proper error recovery strategy.
     */
    if (token.kind == TOKEN_ERR) {
        printf("%s\n ... Exiting ...\n", parser->lexer.e->tail->msg.str);
        exit(1);
    }
#ifdef DEBUG_PARSER
    printf("Consumed: %s\n", token_type_str_map[token.type]);
#endif
    return token;
}

static Token peek_token(Parser *parser)
{
    /* NOTE: if we need more than one token of lookahead, this must change */
    Token token = lex_peek(parser->lex_arena, &parser->lexer);
#ifdef DEBUG_PARSER
    printf("Peek: %s\n", token_type_str_map[token.type]);
#endif
    return token;
}

static bool match_token(Parser *parser, TokenKind kind)
{
    if (peek_token(parser).kind == kind) {
        next_token(parser);
        return true;
    }
    return false;
}

static bool is_bin_op(Token token)
{
    switch (token.kind) {
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_STAR:
    case TOKEN_SLASH:
    case TOKEN_LSHIFT:
    case TOKEN_RSHIFT:
    case TOKEN_DOT:
        return true;
    default:
        return false;
    }
}

static bool is_relation_op(Token token)
{
    switch (token.kind) {
    case TOKEN_EQ:
    case TOKEN_NEQ:
    case TOKEN_LESS:
    case TOKEN_GREATER:
        return true;
    default:
        return false;
    }
}

static Token consume_or_err(Parser *parser, TokenKind expected, char *msg)
{
    Token token = peek_token(parser);
    if (token.kind != expected) {
        error_parse(parser->lexer.e, msg, token);
        return (Token){ .kind = TOKEN_ERR };
    }
    next_token(parser);
    return token;
}

static AstTypeInfo parse_type(Parser *parser, bool allow_array_types)
{
    consume_or_err(parser, TOKEN_COLON, "Expected ':' after declaration to denote type");

    u32 pointer_indirection = 0;
    while (match_token(parser, TOKEN_CARET))
        pointer_indirection++;

    Token name = consume_or_err(parser, TOKEN_IDENTIFIER, "Expected typename after ':'");
    if (!match_token(parser, TOKEN_LBRACKET)) {
        return (AstTypeInfo){ .name = name.lexeme,
                              .is_array = false,
                              .pointer_indirection = pointer_indirection };
    }
    if (!allow_array_types) {
        consume_or_err(parser, TOKEN_RBRACKET, "Local arrays are not allowed");
        return (AstTypeInfo){ .name = name.lexeme,
                              .is_array = false,
                              .pointer_indirection = pointer_indirection };
    }

    s32 elements = -1;
    Token peek = peek_token(parser);
    if (peek.kind == TOKEN_NUM) {
        next_token(parser);
        bool parsed_success;
        elements = str_view_to_u32(peek.lexeme, &parsed_success);
        if (!parsed_success) {
            error_parse(parser->lexer.e, "Could not be parsed as a u32", peek);
        }
    }
    consume_or_err(parser, TOKEN_RBRACKET, "Expected ']' to terminate the array type");
    return (AstTypeInfo){ .name = name.lexeme,
                          .is_array = true,
                          .elements = elements,
                          .pointer_indirection = pointer_indirection };
}

static AstCall *parse_call(Parser *parser, Token identifier, bool is_comptime)
{
    /* Came from TOKEN_IDENTIFIER and then peeked TOKEN_LPAREN */
    next_token(parser);
    AstList *args = NULL;
    if (!match_token(parser, TOKEN_RPAREN)) {
        args = parse_expr_list(parser);
        consume_or_err(parser, TOKEN_RPAREN, "Expected ')' to end function call");
    }
    return make_call(parser->arena, is_comptime, identifier.lexeme, args);
}

static AstExpr *parse_primary(Parser *parser)
{
    Token token = next_token(parser);
    switch (token.kind) {
    case TOKEN_LPAREN: {
        AstExpr *expr = parse_expr(parser, 0);
        consume_or_err(parser, TOKEN_RPAREN, "Expected ')' to terminate the group expression");
        return expr;
    }

    /* Unary */
    case TOKEN_STAR:
    case TOKEN_AMPERSAND: {
        /*
         * We later typecheck dereference (*) and address-of (&) to ensure RHS is correct,
         * but already in the parse stage we can reject the RHS if it is not an expression
         * starting with an identifier.
         */
        Token next = peek_token(parser);
        if (next.kind != TOKEN_IDENTIFIER) {
            error_parse(parser->lexer.e, "RHS of unary * or & must start with an identifier", next);
        }
        /* Intentional fallthrough */
    }
    case TOKEN_MINUS: {
        AstExpr *expr = parse_expr(parser, 0);
        return (AstExpr *)make_unary(parser->arena, expr, token.kind);
    }
    case TOKEN_NUM:
    case TOKEN_IDENTIFIER: {
        Token next = peek_token(parser);
        if (next.kind == TOKEN_LPAREN) {
            return (AstExpr *)parse_call(parser, token, false);
        } else if (next.kind == TOKEN_LBRACKET) {
            /* Parse array indexing as a binary op */
            next_token(parser);
            AstExpr *left = (AstExpr *)make_literal(parser->arena, token);
            AstExpr *right = parse_expr(parser, 0);
            consume_or_err(parser, TOKEN_RBRACKET, "Expected ']' to terminate array indexing");
            return (AstExpr *)make_binary(parser->arena, left, next.kind, right);
        } else {
            /* Parse single identifier */
            return (AstExpr *)make_literal(parser->arena, token);
        }
    }
    case TOKEN_NULL: {
        return (AstExpr *)make_literal(parser->arena, token);
    };
    default:
        error_parse(parser->lexer.e, "Invalid start of a primary expression", token);
        return NULL;
    }
}

static AstExpr *parse_increasing_precedence(Parser *parser, AstExpr *left, u32 precedence)
{
    Token next = peek_token(parser);
    if (!is_bin_op(next))
        return left;

    u32 next_precedence = token_precedences[next.kind];
    if (precedence >= next_precedence)
        return left;

    next_token(parser);
    AstExpr *right;
    /* Struct member access is special because RHS must be an identifier */
    if (next.kind == TOKEN_DOT) {
        Token ident = consume_or_err(parser, TOKEN_IDENTIFIER, "Expected a struct member name");
        right = (AstExpr *)make_literal(parser->arena, ident);
    } else {
        right = parse_expr(parser, next_precedence);
    }
    return (AstExpr *)make_binary(parser->arena, left, next.kind, right);
}

static AstExpr *parse_expr(Parser *parser, u32 precedence)
{
    AstExpr *left = parse_primary(parser);
    AstExpr *expr;
    while (1) {
        expr = parse_increasing_precedence(parser, left, precedence);
        if (expr == left) /* pointer comparison */
            break;

        left = expr;
    }

    return left;
}

static AstBinary *parse_relation(Parser *parser)
{
    AstExpr *left = parse_expr(parser, 0);
    Token op = peek_token(parser);
    if (!is_relation_op(op)) {
        error_parse(parser->lexer.e, "Expected a relation operator", op);
    } else {
        next_token(parser);
    }
    AstExpr *right = parse_expr(parser, 0);
    return make_binary(parser->arena, left, op.kind, right);
}

static AstList *parse_expr_list(Parser *parser)
{
    /* Parsers a comma seperated list of expressions */
    AstExpr *expr;
    if (peek_token(parser).kind == TOKEN_STR) {
        expr = (AstExpr *)make_literal(parser->arena, next_token(parser));
    } else {
        expr = parse_expr(parser, 0);
    }

    AstList *list = make_list(parser->arena, (AstNode *)expr);

    if (peek_token(parser).kind != TOKEN_COMMA) {
        return list;
    }

    AstListNode *list_node;
    do {
        next_token(parser);
        if (peek_token(parser).kind == TOKEN_STR) {
            expr = (AstExpr *)make_literal(parser->arena, next_token(parser));
        } else {
            expr = parse_expr(parser, 0);
        }
        list_node = make_list_node(parser->arena, (AstNode *)expr);
        ast_list_push_back(list, list_node);
    } while (peek_token(parser).kind == TOKEN_COMMA);

    return list;
}


/* Parsing of statements */
static AstWhile *parse_while(Parser *parser)
{
    /* Came from TOKEN_WHILE */
    AstExpr *condition = (AstExpr *)parse_relation(parser);
    consume_or_err(parser, TOKEN_DO, "Expected 'do' keyword to start the while-loop");
    AstStmt *body = parse_stmt(parser);
    return make_while(parser->arena, condition, body);
}

static AstIf *parse_if(Parser *parser)
{
    /* Came from TOKEN_IF */
    AstExpr *condition = (AstExpr *)parse_relation(parser);
    consume_or_err(parser, TOKEN_THEN, "Expected 'then' keyword after if-statement condition");
    AstStmt *then = parse_stmt(parser);
    AstStmt *else_ = NULL;
    if (match_token(parser, TOKEN_ELSE)) {
        else_ = parse_stmt(parser);
    }
    return make_if(parser->arena, condition, then, else_);
}

static AstBlock *parse_block(Parser *parser)
{
    /* Came from TOKEN_BLOCK */
    TypedIdentList declarations = { 0 };
    if (match_token(parser, TOKEN_VAR)) {
        declarations = parse_local_decl_list(parser);
    }

    AstStmt *first = parse_stmt(parser);
    AstList *stmts = make_list(parser->arena, (AstNode *)first);

    Token next;
    while ((next = peek_token(parser)).kind != TOKEN_END) {
        if (next.kind == TOKEN_EOF) {
            error_parse(parser->lexer.e, "Found EOF inside a block. Expected END", next);
            break;
        }
        AstStmt *stmt = parse_stmt(parser);
        AstListNode *list_node = make_list_node(parser->arena, (AstNode *)stmt);
        ast_list_push_back(stmts, list_node);
    }

    /* Consume the END token */
    if (next.kind != TOKEN_EOF) {
        next_token(parser);
    }
    return make_block(parser->arena, declarations, stmts);
}

static AstAssignment *parse_assignment(Parser *parser, AstExpr *left)
{
    /* Already parsed the LHS, now we excpect the assignment operator ':=' */
    Token token = next_token(parser);
    if (token.kind != TOKEN_ASSIGNMENT) {
        error_parse(parser->lexer.e, "Expected assignment after expression", token);
        return NULL;
    }

    AstExpr *right = parse_expr(parser, 0);
    return make_assignment(parser->arena, left, right);
}

static AstStmt *parse_stmt(Parser *parser)
{
    Token token = next_token(parser);
    switch (token.kind) {
    case TOKEN_WHILE:
        return (AstStmt *)parse_while(parser);
    case TOKEN_IF:
        return (AstStmt *)parse_if(parser);
    case TOKEN_PRINT: {
        AstList *print_list = parse_expr_list(parser);
        print_list->kind = (AstNodeKind)STMT_PRINT;
        return (AstStmt *)print_list;
    }
    case TOKEN_RETURN: {
        AstExpr *expr = parse_expr(parser, 0);
        return (AstStmt *)make_single(parser->arena, STMT_RETURN, (AstNode *)expr);
    }
    case TOKEN_IDENTIFIER: {
        Token next = peek_token(parser);
        if (next.kind == TOKEN_LPAREN) {
            /* Function call, promoted to a statement */
            AstNode *call = (AstNode *)parse_call(parser, token, false);
            return (AstStmt *)make_single(parser->arena, STMT_EXPR, call);
        }
        break; /* Can still be the LHS of an assignment */
    }
    case TOKEN_BREAK:
        return (AstStmt *)make_single(parser->arena, STMT_BREAK, NULL);
    case TOKEN_CONTINUE:
        return (AstStmt *)make_single(parser->arena, STMT_CONTINUE, NULL);
    case TOKEN_BEGIN:
        return (AstStmt *)parse_block(parser);
    default:
        break;
    }

    /*
     * The only valid production at this point is an assignment.
     * An assignment must start with an identifier (literal, array indexing, member access) or
     * a star for dereference. If we want to parse these expressions for the LHS of the assignment,
     * we can call parse_expr(). However, parse_expr() expects to parse the from the start of the
     * expression, meaning we must unlex the identifier/star we consumed.
     */
    AstExpr *left;
    if (token.kind == TOKEN_IDENTIFIER) {
        left = (AstExpr *)make_literal(parser->arena, token);
        Token next = peek_token(parser);
        if (next.kind == TOKEN_DOT || next.kind == TOKEN_LBRACKET) {
            parser->unlex = true;
            left = parse_expr(parser, 0);
        }
    } else if (token.kind == TOKEN_STAR) {
        parser->unlex = true;
        left = parse_expr(parser, 0);
    } else {
        error_parse(parser->lexer.e, "Illegal first token in statement", token);
        return NULL;
    }
    return (AstStmt *)parse_assignment(parser, left);
}

static TypedIdentList parse_variable_list(Parser *parser, bool allow_array_types, bool typed)
{
    TypedIdentList typed_vars = { .vars = m_arena_alloc_struct(parser->arena, TypedIdent),
                                  .len = 0 };

    TypedIdent *indices_head = typed_vars.vars;
    do {
        /*
         * If not first iteration of loop then we need to consume the comma we already peeked and
         * allocate space for the next identifier.
         */
        if (typed_vars.len != 0) {
            next_token(parser);
            indices_head = m_arena_alloc_struct(parser->arena, TypedIdent);
        }
        Token identifier = consume_or_err(parser, TOKEN_IDENTIFIER, "Expected variable name");
        AstTypeInfo type_info = { 0 };
        if (typed) {
            type_info = parse_type(parser, allow_array_types);
        }
        TypedIdent new = { .name = identifier.lexeme, .ast_type_info = type_info };
        /* Alloc space for next TypedVar, store current, update len and head */
        *indices_head = new;
        typed_vars.len++;
    } while (peek_token(parser).kind == TOKEN_COMMA);

    return typed_vars;
}

static TypedIdentList parse_local_decl_list(Parser *parser)
{
    /* Came from TOKEN_VAR */
    TypedIdentList identifiers = parse_variable_list(parser, false, true);
    while (match_token(parser, TOKEN_VAR)) {
        TypedIdentList next_identifiers = parse_variable_list(parser, false, true);
        /*
         * Since parse_variable_list ensures the identifiers are stored contigiously, and we do no
         * other allocations on the parser arena, consequetive retain this contigious property.
         */
        identifiers.len += next_identifiers.len;
    }
    return identifiers;
}

static AstFunc *parse_func(Parser *parser, bool require_body)
{
    /* Came from TOKEN_FUNC */
    Token identifier = consume_or_err(parser, TOKEN_IDENTIFIER, "Expected function name");

    consume_or_err(parser, TOKEN_LPAREN, "Expected '(' to start function parameter list");
    TypedIdentList vars = { 0 };
    if (peek_token(parser).kind != TOKEN_RPAREN) {
        vars = parse_variable_list(parser, true, true);
    }
    consume_or_err(parser, TOKEN_RPAREN, "Expected ')' to terminate function parameter list");

    AstTypeInfo return_type = parse_type(parser, true);
    AstStmt *body = NULL;
    if (require_body) {
        body = parse_stmt(parser);
    }
    AstFunc *func = make_func(parser->arena, identifier.lexeme, vars, body, return_type);
    return func;
}

static AstRoot *parse_root(Parser *parser)
{
    AstList vars = { .kind = AST_LIST, .head = NULL, .tail = NULL };
    AstList funcs = { .kind = AST_LIST, .head = NULL, .tail = NULL };
    AstList structs = { .kind = AST_LIST, .head = NULL, .tail = NULL };
    AstList enums = { .kind = AST_LIST, .head = NULL, .tail = NULL };
    AstList calls = { .kind = AST_LIST, .head = NULL, .tail = NULL };

    Token next;
    while ((next = next_token(parser)).kind != TOKEN_EOF) {
        switch (next.kind) {
        case TOKEN_VAR: {
            /* Parse global declarations list */
            TypedIdentList v = parse_variable_list(parser, true, true);
            AstTypedIdentList *node_vars = make_typed_ident_list(parser->arena, v);
            AstListNode *node_node = make_list_node(parser->arena, (AstNode *)node_vars);
            ast_list_push_back(&vars, node_node);
        }; break;
        case TOKEN_COMPILER: {
            consume_or_err(parser, TOKEN_FUNC, "Expected a function");
            AstFunc *func = parse_func(parser, false);
            AstListNode *func_node = make_list_node(parser->arena, (AstNode *)func);
            ast_list_push_back(&funcs, func_node);
        }; break;
        case TOKEN_FUNC: {
            AstFunc *func = parse_func(parser, true);
            AstListNode *func_node = make_list_node(parser->arena, (AstNode *)func);
            ast_list_push_back(&funcs, func_node);
        }; break;
        case TOKEN_STRUCT: {
            Token name = consume_or_err(parser, TOKEN_IDENTIFIER, "Expected struct name");
            consume_or_err(parser, TOKEN_ASSIGNMENT, "Expected ':=' after struct name");
            TypedIdentList members = parse_variable_list(parser, true, true);
            AstStruct *struct_decl = make_struct(parser->arena, name.lexeme, members);
            AstListNode *node_node = make_list_node(parser->arena, (AstNode *)struct_decl);
            ast_list_push_back(&structs, node_node);
        }; break;
        case TOKEN_ENUM: {
            Token name = consume_or_err(parser, TOKEN_IDENTIFIER, "Expected enum name");
            consume_or_err(parser, TOKEN_ASSIGNMENT, "Expected ':=' after enum name");
            TypedIdentList values = parse_variable_list(parser, true, false);
            AstEnum *enum_decl = make_enum(parser->arena, name.lexeme, values);
            AstListNode *node_node = make_list_node(parser->arena, (AstNode *)enum_decl);
            ast_list_push_back(&enums, node_node);
        }; break;
        case TOKEN_AT: {
            Token function_identifier =
                consume_or_err(parser, TOKEN_IDENTIFIER, "Expected an function name after '@'");
            AstCall *call = parse_call(parser, function_identifier, true);
            AstListNode *call_node = make_list_node(parser->arena, (AstNode *)call);
            ast_list_push_back(&calls, call_node);
        }; break;
        default: {
            error_parse(parser->lexer.e, "Illegal first token. Expected var, struct or func", next);
            break;
        };
        }
    }

    return make_root(parser->arena, vars, funcs, structs, enums, calls);
}

AstRoot *parse(Arena *arena, Arena *lex_arena, ErrorHandler *e, char *input)
{
    Parser parser = {
        .arena = arena,
        .lex_arena = lex_arena,
        .unlex = false,
    };
    lex_init(&parser.lexer, e, input);

    return parse_root(&parser);
}

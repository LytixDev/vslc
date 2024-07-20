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
#ifndef AST_H
#define AST_H

#include "base/types.h"
#include "lex.h"

typedef enum {
    EXPR_UNARY = 0,
    EXPR_BINARY,
    EXPR_LITERAL,
    EXPR_LIST,
} AstExprType;

typedef enum {
    LIT_STR,
    LIT_NUM,
} LiteralType;

typedef struct expr_t {
    AstExprType type;
} AstExpr;

typedef struct {
    AstExprType type;
    LiteralType lit_type; // TOKEN_NUM, TOKEN_STR or TOKEN_IDENT
    union {
        u32 str_list_idx;
        s32 num_value;
    };
} AstExprLiteral;

typedef struct {
    AstExprType type;
    TokenType op;
    AstExpr *expr;
} AstExprUnary;

typedef struct {
    AstExprType type;
    AstExpr *left;
    TokenType op;
    AstExpr *right;
} AstExprBinary;


// TODO: Consider other structure than a linked-list
typedef struct ast_expr_list_node AstExprListNode;
struct ast_expr_list_node {
    /* The AstExprListNode's will never be used as regular Expr's, so we don't need a type here */
    AstExpr *this;
    AstExprListNode *next;
};
typedef struct {
    AstExprType type;
    AstExprListNode head;
    AstExprListNode *tail;
} AstExprList;

#define AS_LITERAL(___expr) ((AstExprLiteral *)(___expr))
#define AS_BINARY(___expr) ((AstExprBinary *)(___expr))
#define AS_LIST(___expr) ((AstExprList *)(___expr))


void ast_print(AstExpr *head, Str8 *str_list, u32 indent, bool print_newline);

#endif /* AST_H */

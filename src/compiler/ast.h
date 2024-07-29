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

/* Expressions */
typedef enum {
    EXPR_UNARY = 0,
    EXPR_BINARY,
    EXPR_LITERAL,
    EXPR_LIST,
    EXPR_TYPE_LEN,
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
    TokenType op;
    AstExpr *expr;
} AstExprUnary;

typedef struct {
    AstExprType type;
    AstExpr *left;
    TokenType op;
    AstExpr *right;
} AstExprBinary;

typedef struct {
    AstExprType type;
    LiteralType lit_type; // TOKEN_NUM, TOKEN_STR or TOKEN_IDENT
    union {
        u32 str_list_idx;
        s32 num_value;
    };
} AstExprLiteral;

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

/* Statements */
typedef enum {
    STMT_WHILE = 0,
    STMT_IF,
    STMT_ABRUPT, // Break, Continue, Return
    STMT_LIST,
    STMT_PRINT,
    STMT_BLOCK,
    STMT_ASSIGNMENT,
    STMT_FUNC,
    STMT_DECLARATION,
    STMT_TYPE_LEN,
} AstStmtType;

typedef struct stmt_t {
    AstStmtType type;
} AstStmt;

typedef struct {
    AstStmtType type;
    AstExpr *condition;
    AstStmt *body;
} AstStmtWhile;

typedef struct {
    AstStmtType type;
    AstExpr *condition;
    AstStmt *then;
    AstStmt *else_;
} AstStmtIf;

typedef struct ast_stmt_list_node AstStmtListNode;
struct ast_stmt_list_node {
    AstStmt *this;
    AstStmtListNode *next;
};
typedef struct {
    AstStmtType type;
    AstStmtListNode head;
    AstStmtListNode *tail;
} AstStmtList;

typedef struct {
    AstStmtType type;
    AstExpr *print_list;
} AstStmtPrint;

typedef struct {
    AstStmtType type;
    u32 *identifier_str_idxs; // Array of indexes into the str list
    u32 len; // How many identifiers
} AstStmtDeclaration;

typedef struct {
    AstStmtType type;
    AstStmtList *declarations; // @NULLABLE
    AstStmtList *stmts;
} AstStmtBlock;


#define AS_UNARY(___expr) ((AstExprUnary *)(___expr))
#define AS_BINARY(___expr) ((AstExprBinary *)(___expr))
#define AS_LITERAL(___expr) ((AstExprLiteral *)(___expr))
#define AS_LIST(___expr) ((AstExprList *)(___expr))

#define AS_WHILE(___stmt) ((AstStmtWhile *)(___stmt))
#define AS_IF(___stmt) ((AstStmtIf *)(___stmt))
#define AS_PRINT(___stmt) ((AstStmtPrint *)(___stmt))
#define AS_DECLARATION(___stmt) ((AstStmtDeclaration *)(___stmt))
#define AS_BLOCK(___stmt) ((AstStmtBlock *)(___stmt))

extern char *expr_type_str_map[EXPR_TYPE_LEN];
extern char *stmt_type_str_map[STMT_TYPE_LEN];

/* Expresions */
AstExprUnary *make_unary(Arena *arena, AstExpr *expr, TokenType op);
AstExprBinary *make_binary(Arena *arena, AstExpr *left, TokenType op, AstExpr *right);
AstExprLiteral *make_literal(Arena *arena, Token token);
AstExprListNode *make_list_node(Arena *arena, AstExpr *this);
AstExprList *make_list(Arena *arena, AstExpr *head);

/* Statements */
AstStmtListNode *make_stmt_list_node(Arena *arena, AstStmt *this);
AstStmtList *make_stmt_list(Arena *arena, AstStmt *head);
AstStmtWhile *make_while(Arena *arena, AstExpr *condition, AstStmt *body);
AstStmtIf *make_if(Arena *arena, AstExpr *condition, AstStmt *then, AstStmt *else_);
AstStmtPrint *make_print(Arena *arena, AstExpr *print_list);
AstStmtBlock *make_block(Arena *arena, AstStmtList *declarations, AstStmtList *statements);


void ast_print(AstStmt *head, Str8 *str_list, u32 indent);


#endif /* AST_H */

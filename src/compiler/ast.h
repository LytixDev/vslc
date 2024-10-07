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

/* Forwards */
// typedef struct ast_decl_t AstDecl;

/*
 * Holds indices of identifiers into the str_list.
 * iden_indice is stored in contiguous memory on an arena.
 */
typedef struct var_list_t {
    u32 *iden_indices;
    u32 len;
} VarList;

/* Expressions */
typedef enum {
    EXPR_UNARY = 0,
    EXPR_BINARY,
    EXPR_LITERAL,
    EXPR_LIST,
    EXPR_CALL,
    EXPR_TYPE_LEN,
} AstExprType;

typedef enum {
    LIT_STR,
    LIT_IDENT,
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

typedef struct {
    AstExprType type;
    u32 identifier; // Index into str_list
    AstExpr *args; // @NULLABLE. type should either be Literal or List
} AstExprCall;

/* Statements */
typedef enum {
    STMT_WHILE = 0,
    STMT_IF,

    STMT_ABRUPT,
    STMT_ABRUPT_BREAK,
    STMT_ABRUPT_CONTINUE,
    STMT_ABRUPT_RETURN,

    STMT_LIST,
    STMT_PRINT,
    STMT_EXPR,
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

typedef struct {
    AstStmtType type;
    AstExpr *expr; // @NULLABLE
} AstStmtAbrupt;

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
    AstStmtType type; // Print or expression promoted to statement
    AstExpr *print_list;
} AstStmtSingle;

typedef struct {
    AstStmtType type;
    u32 *identifier_str_idxs; // Array of indexes into the str list
    u32 len; // How many identifiers
} AstStmtDeclaration;

typedef struct {
    AstStmtType type;
    VarList declarations;
    AstStmtList *stmts;
} AstStmtBlock;

typedef struct {
    AstStmtType type;
    AstExpr *left; // Identifier literal or array indexing (BinaryExpr)
    AstExpr *right;
} AstStmtAssignment;

/* Declarations and other AST Nodes */
typedef enum {
    AST_FUNC,
    AST_GLOBAL_DECL,
    AST_LOCAL_DECL,

    AST_NODE_TYPE_LEN,
} AstNodeType;

typedef struct {
    AstNodeType type;
} AstNode;

typedef struct {
    AstNodeType type;
    u32 identifier; // Index into str_list
    VarList vars;
    AstStmt *body;
} AstFunction;

// AstDecl, forwared at the start of the file
// struct ast_decl_t {
//    AstNodeType type; // Global or Local
//    VarList identifiers;
//};


#define AS_UNARY(___expr) ((AstExprUnary *)(___expr))
#define AS_BINARY(___expr) ((AstExprBinary *)(___expr))
#define AS_LITERAL(___expr) ((AstExprLiteral *)(___expr))
#define AS_LIST(___expr) ((AstExprList *)(___expr))
#define AS_CALL(___expr) ((AstExprCall *)(___expr))

#define AS_WHILE(___stmt) ((AstStmtWhile *)(___stmt))
#define AS_IF(___stmt) ((AstStmtIf *)(___stmt))
#define AS_ABRUPT(___stmt) ((AstStmtAbrupt *)(___stmt))
#define AS_SINGLE(___stmt) ((AstStmtSingle *)(___stmt))
#define AS_DECLARATION(___stmt) ((AstStmtDeclaration *)(___stmt))
#define AS_BLOCK(___stmt) ((AstStmtBlock *)(___stmt))
#define AS_ASSIGNMENT(___stmt) ((AstStmtAssignment *)(___stmt))

#define AS_FUNC(___node) ((AstFunction *)(___node))

extern char *expr_type_str_map[EXPR_TYPE_LEN];
extern char *stmt_type_str_map[STMT_TYPE_LEN];
extern char *node_type_str_map[AST_NODE_TYPE_LEN];

/* Expresions */
AstExprUnary *make_unary(Arena *arena, AstExpr *expr, TokenType op);
AstExprBinary *make_binary(Arena *arena, AstExpr *left, TokenType op, AstExpr *right);
AstExprLiteral *make_literal(Arena *arena, Token token);
AstExprListNode *make_list_node(Arena *arena, AstExpr *this);
AstExprList *make_list(Arena *arena, AstExpr *head);
AstExprCall *make_call(Arena *arena, u32 identifier, AstExpr *args);

/* Statements */
AstStmtListNode *make_stmt_list_node(Arena *arena, AstStmt *this);
AstStmtList *make_stmt_list(Arena *arena, AstStmt *head);
AstStmtWhile *make_while(Arena *arena, AstExpr *condition, AstStmt *body);
AstStmtIf *make_if(Arena *arena, AstExpr *condition, AstStmt *then, AstStmt *else_);
AstStmtSingle *make_single(Arena *arena, AstStmtType single_type, AstExpr *print_list);
AstStmtAbrupt *make_abrupt(Arena *arena, AstStmtType abrupt_type, AstExpr *expr);
AstStmtBlock *make_block(Arena *arena, VarList declarations, AstStmtList *statements);
AstStmtAssignment *make_assignment(Arena *arena, AstExpr *left, AstExpr *right);

/* */
AstFunction *make_function(Arena *arena, u32 identifier, VarList vars, AstStmt *body);
// AstDecl *make_decl(Arena *arena, AstNodeType decl_type, VarList identifiers);


void ast_print_stmt(AstStmt *head, Str8 *str_list, u32 indent);
void ast_print(AstNode *head, Str8 *str_list);


#endif /* AST_H */

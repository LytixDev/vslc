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


typedef struct {
    u32 name; // index into the str_list
    bool is_pointer;
    bool is_array;
    s32 elements; // a -1 value means the array is dynamic
} AstTypeInfo;

typedef struct {
    u32 name; // index into the str_list
    AstTypeInfo ast_type_info;
} TypedVar;

typedef struct {
    TypedVar *vars; // contiguous array
    u32 len;
} TypedVarList;


/* Enums */
/* High-level AST: */
typedef enum {
    LIT_STR,
    LIT_IDENT,
    LIT_NUM,
} LiteralType;

typedef enum {
    EXPR_UNARY = 0,
    EXPR_BINARY,
    EXPR_LITERAL,
    EXPR_CALL,
    EXPR_TYPE_LEN,
} AstExprType;

typedef enum {
    STMT_WHILE = EXPR_TYPE_LEN,
    STMT_IF,

    STMT_ABRUPT,
    STMT_ABRUPT_BREAK,
    STMT_ABRUPT_CONTINUE,
    STMT_ABRUPT_RETURN,

    STMT_PRINT,
    STMT_EXPR,
    STMT_BLOCK,
    STMT_ASSIGNMENT,
    STMT_TYPE_LEN,
} AstStmtType;

typedef enum {
    /*
     * The way we set up the enum values means we always have space here for the EXPR and STMT vals.
     */
    AST_FUNC = STMT_TYPE_LEN,
    AST_STRUCT,
    AST_LIST,
    AST_NODE_VAR_LIST,
    AST_ROOT,

    AST_NODE_TYPE_LEN,
} AstNodeType;

/* Low-level AST: */
typedef enum {
    AST_LL_TYPE_LEN,
} AstLLType;

/*
 * Headers
 * The way everything is set up means we can always upcast AstExpr's and AstStmt's to AstNode's.
 */
typedef struct expr_t {
    AstExprType type;
} AstExpr;

typedef struct stmt_t {
    AstStmtType type;
} AstStmt;

typedef struct {
    AstNodeType type;
} AstNode;


/* Expressions */
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

typedef struct {
    AstExprType type;
    u32 identifier; // Index into str_list
    AstNode *args; // @NULLABLE. type should either be Literal or List
} AstExprCall;


/* Statements */
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

// TODO: Consider other structure than a linked-list
typedef struct ast_list_node AstListNode;
struct ast_list_node {
    AstNode *this;
    AstListNode *next;
};
typedef struct {
    AstNodeType type;
    AstListNode *head;
    AstListNode *tail;
} AstList;

typedef struct {
    AstStmtType type; // Abrupt, print or Expr promoted to an Stmt
    AstNode *node; // @NULLABLE
} AstStmtSingle;

typedef struct {
    AstStmtType type;
    TypedVarList declarations;
    AstList *stmts;
} AstStmtBlock;

typedef struct {
    AstStmtType type;
    AstExpr *left; // Identifier literal or array indexing (BinaryExpr)
    AstExpr *right;
} AstStmtAssignment;


/* Nodes */
typedef struct {
    AstNodeType type;
    TypedVarList vars;
} AstNodeVarList;

typedef struct {
    AstNodeType type;
    u32 name; // Index into str_list
    TypedVarList parameters;
    AstTypeInfo return_type;
    AstStmt *body;
} AstFunc;

typedef struct {
    AstNodeType type;
    u32 name; // Index into str_list
    TypedVarList members;
} AstStruct;

typedef struct {
    AstNodeType type;
    AstList declarations; // AstTypedVarList
    AstList structs; // AstStruct
    AstList functions; // AstFunction
} AstRoot;


#define AS_UNARY(___expr) ((AstExprUnary *)(___expr))
#define AS_BINARY(___expr) ((AstExprBinary *)(___expr))
#define AS_LITERAL(___expr) ((AstExprLiteral *)(___expr))
#define AS_CALL(___expr) ((AstExprCall *)(___expr))

#define AS_WHILE(___stmt) ((AstStmtWhile *)(___stmt))
#define AS_IF(___stmt) ((AstStmtIf *)(___stmt))
#define AS_ABRUPT(___stmt) ((AstStmtAbrupt *)(___stmt))
#define AS_SINGLE(___stmt) ((AstStmtSingle *)(___stmt))
#define AS_BLOCK(___stmt) ((AstStmtBlock *)(___stmt))
#define AS_ASSIGNMENT(___stmt) ((AstStmtAssignment *)(___stmt))

#define AS_NODE_VAR_LIST(___node) ((AstNodeVarList *)(___node))
#define AS_FUNC(___node) ((AstFunc *)(___node))
#define AS_STRUCT(___node) ((AstStruct *)(___node))
#define AS_LIST(___node) ((AstList *)(___node))
#define AS_ROOT(___node) ((AstRoot *)(___node))

extern char *node_type_str_map[AST_NODE_TYPE_LEN];

/* Expresions */
AstExprUnary *make_unary(Arena *arena, AstExpr *expr, TokenType op);
AstExprBinary *make_binary(Arena *arena, AstExpr *left, TokenType op, AstExpr *right);
AstExprLiteral *make_literal(Arena *arena, Token token);
AstExprCall *make_call(Arena *arena, u32 identifier, AstNode *args);

/* Statements */
AstStmtWhile *make_while(Arena *arena, AstExpr *condition, AstStmt *body);
AstStmtIf *make_if(Arena *arena, AstExpr *condition, AstStmt *then, AstStmt *else_);
AstStmtSingle *make_single(Arena *arena, AstStmtType single_type, AstNode *print_list);
AstStmtBlock *make_block(Arena *arena, TypedVarList declarations, AstList *statements);
AstStmtAssignment *make_assignment(Arena *arena, AstExpr *left, AstExpr *right);

/* */
AstFunc *make_function(Arena *arena, u32 name, TypedVarList parameters, AstStmt *body,
                       AstTypeInfo return_type);
AstStruct *make_struct(Arena *arena, u32 name, TypedVarList members);
AstListNode *make_list_node(Arena *arena, AstNode *this);
void ast_list_push_back(AstList *list, AstListNode *node);
AstList *make_list(Arena *arena, AstNode *head);
AstNodeVarList *make_node_var_list(Arena *arena, TypedVarList vars);
AstRoot *make_root(Arena *arena, AstList declarations, AstList functions, AstList structs);

void ast_print(AstNode *head, Str8 *str_list, u32 indent);


#endif /* AST_H */

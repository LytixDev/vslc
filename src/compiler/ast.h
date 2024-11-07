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

typedef struct symbol_t Symbol; // forward from type.h
typedef struct type_info_t TypeInfo; // forward from type.h


typedef struct {
    Str8 name;
    bool is_pointer;
    bool is_array;
    s32 elements; // a -1 value means the array is dynamic
} AstTypeInfo;

typedef struct {
    Str8 name;
    AstTypeInfo ast_type_info;
} AstTypedVar;

typedef struct {
    AstTypedVar *vars; // contiguous array
    u32 len;
} AstTypedVarList;


/* Enums */
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

    STMT_BREAK, // AstSingle
    STMT_CONTINUE, // AstSingle
    STMT_RETURN, // AstSingle
    STMT_EXPR, // AstSingle. AstCall promoted to a statement.

    STMT_PRINT, // AstList
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
    AST_ENUM,
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
    TypeInfo *t; // @NULLABLE. Only set after typechecking.
} AstExpr;

typedef struct stmt_t {
    AstStmtType type;
} AstStmt;

typedef struct {
    AstNodeType type;
} AstNode;

typedef struct ast_list_node AstListNode;
typedef struct ast_list AstList;

/* Expressions */
typedef struct {
    AstExprType type;
    TypeInfo *t; // @NULLABLE. Only set after typechecking.
    TokenType op;
    AstExpr *expr;
} AstUnary;

typedef struct {
    AstExprType type;
    TypeInfo *t; // @NULLABLE. Only set after typechecking.
    AstExpr *left;
    TokenType op;
    AstExpr *right;
} AstBinary;

typedef struct {
    AstExprType type;
    TypeInfo *t; // @NULLABLE. Only set after typechecking.
    LiteralType lit_type; // TOKEN_NUM, TOKEN_STR or TOKEN_IDENT
    Str8View literal; // Guranteed to be zero-terminated for STR and IDENT aka Str8
    // TODO: Unsure if this is how we want it going forward.
    Symbol *sym; // @NULLABLE. After type checking, each identifier is bound to a symbol
} AstLiteral;

typedef struct {
    AstExprType type;
    TypeInfo *t; // @NULLABLE. Only set after typechecking.
    Str8 identifier;
    AstList *args; // @NULLABLE.
} AstCall;

/* Statements */
typedef struct {
    AstStmtType type;
    AstExpr *condition;
    AstStmt *body;
} AstWhile;

typedef struct {
    AstStmtType type;
    AstExpr *condition;
    AstStmt *then;
    AstStmt *else_;
} AstIf;

typedef struct {
    AstStmtType type; // Abrupt, print or Expr promoted to an Stmt
    AstNode *node; // @NULLABLE
} AstSingle;

typedef struct {
    AstStmtType type;
    AstTypedVarList declarations;
    AstList *stmts;
} AstBlock;

typedef struct {
    AstStmtType type;
    AstExpr *left; // Identifier literal or array indexing (BinaryExpr)
    AstExpr *right;
} AstAssignment;


/* Nodes */
// TODO: Consider other structure than a linked-list
struct ast_list_node {
    AstNode *this;
    AstListNode *next;
};

struct ast_list {
    AstNodeType type;
    AstListNode *head;
    AstListNode *tail;
};

typedef struct {
    AstNodeType type;
    AstTypedVarList vars;
} AstNodeVarList;

typedef struct {
    AstNodeType type;
    Str8 name;
    AstTypedVarList parameters;
    AstTypeInfo return_type;
    AstStmt *body;
} AstFunc;

typedef struct {
    AstNodeType type;
    Str8 name;
    AstTypedVarList members;
} AstStruct;

typedef struct {
    AstNodeType type;
    Str8 name;
    AstTypedVarList members; // Untyped
} AstEnum;

typedef struct {
    AstNodeType type;
    AstList vars; // AstTypedVarList
    AstList funcs; // AstFunc
    AstList structs; // AstStruct
    AstList enums; // AstEnum
} AstRoot;


#define AS_UNARY(___expr) ((AstUnary *)(___expr))
#define AS_BINARY(___expr) ((AstBinary *)(___expr))
#define AS_LITERAL(___expr) ((AstLiteral *)(___expr))
#define AS_CALL(___expr) ((AstCall *)(___expr))

#define AS_WHILE(___stmt) ((AstWhile *)(___stmt))
#define AS_IF(___stmt) ((AstIf *)(___stmt))
#define AS_SINGLE(___stmt) ((AstSingle *)(___stmt))
#define AS_BLOCK(___stmt) ((AstBlock *)(___stmt))
#define AS_ASSIGNMENT(___stmt) ((AstAssignment *)(___stmt))

#define AS_NODE_VAR_LIST(___node) ((AstNodeVarList *)(___node))
#define AS_FUNC(___node) ((AstFunc *)(___node))
#define AS_STRUCT(___node) ((AstStruct *)(___node))
#define AS_ENUM(___node) ((AstEnum *)(___node))
#define AS_LIST(___node) ((AstList *)(___node))
#define AS_ROOT(___node) ((AstRoot *)(___node))

extern char *node_type_str_map[AST_NODE_TYPE_LEN];


/* Expresions */
AstUnary *make_unary(Arena *a, AstExpr *expr, TokenType op);
AstBinary *make_binary(Arena *a, AstExpr *left, TokenType op, AstExpr *right);
AstLiteral *make_literal(Arena *a, Token token);
AstCall *make_call(Arena *a, Str8View identifier, AstList *args);

/* Statements */
AstWhile *make_while(Arena *a, AstExpr *condition, AstStmt *body);
AstIf *make_if(Arena *a, AstExpr *condition, AstStmt *then, AstStmt *else_);
AstSingle *make_single(Arena *a, AstStmtType single_type, AstNode *node);
AstBlock *make_block(Arena *a, AstTypedVarList declarations, AstList *statements);
AstAssignment *make_assignment(Arena *a, AstExpr *left, AstExpr *right);

/* Declarations and other nodes*/
AstFunc *make_func(Arena *a, Str8 name, AstTypedVarList params, AstStmt *body,
                   AstTypeInfo return_type);
AstStruct *make_struct(Arena *a, Str8 name, AstTypedVarList members);
AstEnum *make_enum(Arena *a, Str8 name, AstTypedVarList values);
AstListNode *make_list_node(Arena *a, AstNode *this);
void ast_list_push_back(AstList *list, AstListNode *node);
AstList *make_list(Arena *a, AstNode *head);
AstNodeVarList *make_node_var_list(Arena *a, AstTypedVarList vars);
AstRoot *make_root(Arena *a, AstList vars, AstList funcs, AstList structs, AstList enums);

void ast_print(AstNode *head, u32 indent);


#endif /* AST_H */

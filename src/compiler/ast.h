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

#include "base/base.h"
#include "base/types.h"
#include "lex.h"

typedef struct symbol_t Symbol; // forward from type.h
typedef struct symbol_table_t SymbolTable; // forward from type.h
typedef struct type_info_t TypeInfo; // forward from type.h


typedef struct {
    Str8 name;
    bool is_array;
    s32 elements;
    s32 pointer_indirection; // 0 means that this is not a pointer
} AstTypeInfo;

typedef struct {
    Str8 name;
    AstTypeInfo ast_type_info;
} TypedIdent;

typedef struct {
    TypedIdent *vars; // contiguous array
    u32 len;
} TypedIdentList;


/*
 * Enums for the three categories of AST nodes: Expressions, Statements and Nodes.
 * The enum values are set up in a way that enables us to treat all categories
 * as part of a single continuous range, allowing for casting between node types.
 *
 * This setup allows us to:
 * 1. Determine a node's category by comparing its type value against the _LEN constants
 * 2. Use switch statements that handle all possible node types in a category
 */
typedef enum {
    LIT_STR,
    LIT_IDENT,
    LIT_NUM,
    LIT_NULL,
} LiteralType;

typedef enum {
    EXPR_UNARY = 0,
    EXPR_BINARY,
    EXPR_LITERAL,
    EXPR_CALL,
    EXPR_TYPE_LEN,
} AstExprKind;

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
} AstStmtKind;

typedef enum {
    AST_FUNC = STMT_TYPE_LEN,
    AST_STRUCT,
    AST_ENUM,
    AST_LIST,
    AST_TYPED_IDENT_LIST,
    AST_ROOT,

    AST_NODE_TYPE_LEN,
} AstNodeKind;

/*
 * Headers. As described above, the way we set up the neums  means we can always "upcast" AstExprs
 * and AstStmts to AstNodes.
 * NOTE: The actual node structs (AstExpr, AstStmt, AstNode) all start with their respective
 * type field, enabling safe pointer casting between them.
 */
typedef struct expr_t {
    AstExprKind kind;
    TypeInfo *type; // @NULLABLE. Only set after typechecking.
} AstExpr;

typedef struct stmt_t {
    AstStmtKind kind;
} AstStmt;

typedef struct {
    AstNodeKind kind;
} AstNode;

typedef struct ast_list_node AstListNode;
typedef struct ast_list AstList;

/* Expressions */
typedef struct {
    AstExprKind kind;
    TypeInfo *t; // @NULLABLE. Only set after typechecking.
    TokenKind op;
    AstExpr *expr;
} AstUnary;

typedef struct {
    AstExprKind kind;
    TypeInfo *type; // @NULLABLE. Only set after typechecking.
    AstExpr *left;
    TokenKind op;
    AstExpr *right;
} AstBinary;

typedef struct {
    AstExprKind kind;
    TypeInfo *type; // @NULLABLE. Only set after typechecking.
    Symbol *sym; // @NULLABLE. After type checking, each TOKEN_IDENT is bound to a symbol
    LiteralType lit_type; // TOKEN_NUM, TOKEN_STR or TOKEN_IDENT
    Str8View literal; // Guranteed to be zero-terminated for STR and IDENT aka Str8
} AstLiteral;

typedef struct {
    AstExprKind kind;
    bool is_comptime;
    TypeInfo *type; // @NULLABLE. Only set after typechecking.
    Str8 identifier;
    AstList *args; // @NULLABLE.
} AstCall;

/* Statements */
typedef struct {
    AstStmtKind kind;
    AstExpr *condition;
    AstStmt *body;
} AstWhile;

typedef struct {
    AstStmtKind kind;
    AstExpr *condition;
    AstStmt *then;
    AstStmt *else_;
} AstIf;

typedef struct {
    AstStmtKind kind;
    AstNode *node; // @NULLABLE
} AstSingle;

typedef struct {
    AstStmtKind kind;
    TypedIdentList declarations;
    AstList *stmts;
    // NOTE: Is this where this should be?
    SymbolTable *symt_local; // @NULLABLE. Set after bind_stmt(), just before typecheck
} AstBlock;

typedef struct {
    AstStmtKind kind;
    AstExpr *left; // Identifier literal, array indexing, dereference or struct member access
    AstExpr *right;
} AstAssignment;


/* Nodes */
// NOTE: Consider other structure than a linked-list?
struct ast_list_node {
    AstNode *this;
    AstListNode *next;
};

struct ast_list {
    AstNodeKind kind;
    AstListNode *head;
    AstListNode *tail;
};

// NOTE: Hacky solution so we can have a list of TypedIdentList
typedef struct {
    AstNodeKind kind;
    TypedIdentList idents;
} AstTypedIdentList;

typedef struct {
    AstNodeKind kind;
    Str8 name;
    TypedIdentList parameters;
    AstTypeInfo return_type;
    AstStmt *body; // @NULLABLE. If NULL then the function is a compiler intrinsic.
} AstFunc;

typedef struct {
    AstNodeKind kind;
    Str8 name;
    TypedIdentList members;
} AstStruct;

typedef struct {
    AstNodeKind kind;
    Str8 name;
    TypedIdentList members; // For enums, these are actually untyped
} AstEnum;

typedef struct {
    AstNodeKind kind;
    AstList vars; // AstTypedVarList wrapped inside
    AstList funcs; // AstFunc
    AstList structs; // AstStruct
    AstList enums; // AstEnum
    AstList calls; // AstCall - Compile time calls
} AstRoot;


#define AST_IS_EXPR(___node) \
    (IS_BETWEEN((___node)->kind, (AstNodeKind)EXPR_UNARY, (AstNodeKind)EXPR_TYPE_LEN - 1))
#define AST_IS_STMT(___node) \
    (IS_BETWEEN((___node)->kind, (AstNodeKind)STMT_WHILE, (AstNodeKind)STMT_TYPE_LEN - 1))

#define AS_UNARY(___expr) ((AstUnary *)(___expr))
#define AS_BINARY(___expr) ((AstBinary *)(___expr))
#define AS_LITERAL(___expr) ((AstLiteral *)(___expr))
#define AS_CALL(___expr) ((AstCall *)(___expr))

#define AS_WHILE(___stmt) ((AstWhile *)(___stmt))
#define AS_IF(___stmt) ((AstIf *)(___stmt))
#define AS_SINGLE(___stmt) ((AstSingle *)(___stmt))
#define AS_BLOCK(___stmt) ((AstBlock *)(___stmt))
#define AS_ASSIGNMENT(___stmt) ((AstAssignment *)(___stmt))

#define AS_TYPED_IDENT_LIST(___node) ((AstTypedIdentList *)(___node))
#define AS_FUNC(___node) ((AstFunc *)(___node))
#define AS_STRUCT(___node) ((AstStruct *)(___node))
#define AS_ENUM(___node) ((AstEnum *)(___node))
#define AS_LIST(___node) ((AstList *)(___node))
#define AS_ROOT(___node) ((AstRoot *)(___node))

extern char *node_kind_str_map[AST_NODE_TYPE_LEN];


/* Expresions */
AstUnary *make_unary(Arena *a, AstExpr *expr, TokenKind op);
AstBinary *make_binary(Arena *a, AstExpr *left, TokenKind op, AstExpr *right);
AstLiteral *make_literal(Arena *a, Token token);
AstCall *make_call(Arena *a, bool is_comptime_call, Str8View identifier, AstList *args);

/* Statements */
AstWhile *make_while(Arena *a, AstExpr *condition, AstStmt *body);
AstIf *make_if(Arena *a, AstExpr *condition, AstStmt *then, AstStmt *else_);
AstSingle *make_single(Arena *a, AstStmtKind single_type, AstNode *node);
AstBlock *make_block(Arena *a, TypedIdentList declarations, AstList *statements);
AstAssignment *make_assignment(Arena *a, AstExpr *left, AstExpr *right);

/* Declarations and other nodes*/
AstFunc *make_func(Arena *a, Str8 name, TypedIdentList params, AstStmt *body,
                   AstTypeInfo return_type);
AstStruct *make_struct(Arena *a, Str8 name, TypedIdentList members);
AstEnum *make_enum(Arena *a, Str8 name, TypedIdentList values);
AstListNode *make_list_node(Arena *a, AstNode *this);
void ast_list_push_back(AstList *list, AstListNode *node);
AstList *make_list(Arena *a, AstNode *head);
AstTypedIdentList *make_typed_ident_list(Arena *a, TypedIdentList vars);
AstRoot *make_root(Arena *a, AstList vars, AstList funcs, AstList structs, AstList enums,
                   AstList calls);

void ast_print(AstNode *head, u32 indent);


#endif /* AST_H */

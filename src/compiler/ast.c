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
#include "ast.h"
#include "base/str.h"
#include "lex.h"
#include "type.h"
#include <stdio.h>

char *node_kind_str_map[AST_NODE_TYPE_LEN] = {
    "EXPR_UNARY", "EXPR_BINARY", "EXPR_LITERAL",         "EXPR_CALL",   "STMT_WHILE",
    "STMT_IF",    "STMT_BREAK",  "STMT_CONTINUE",        "STMT_RETURN", "STMT_EXPR",
    "STMT_PRINT", "STMT_BLOCK",  "STMT_ASSIGNMENT",      "AST_FUNC",    "AST_STRUCT",
    "AST_ENUM",   "AST_LIST",    "AST_TYPED_IDENT_LIST", "AST_ROOT",
};

/* Expressions */
AstUnary *make_unary(Arena *a, AstExpr *expr, TokenKind op)
{
    AstUnary *unary = m_arena_alloc(a, sizeof(AstUnary));
    unary->kind = EXPR_UNARY;
    unary->op = op;
    unary->expr = expr;
    return unary;
}

AstBinary *make_binary(Arena *a, AstExpr *left, TokenKind op, AstExpr *right)
{
    AstBinary *binary = m_arena_alloc(a, sizeof(AstBinary));
    binary->kind = EXPR_BINARY;
    binary->op = op;
    binary->left = left;
    binary->right = right;
    return binary;
}

AstLiteral *make_literal(Arena *a, Token token)
{
    AstLiteral *literal = m_arena_alloc(a, sizeof(AstLiteral));
    literal->kind = EXPR_LITERAL;
    literal->literal = token.lexeme;
    if (token.kind == TOKEN_NUM) {
        literal->lit_type = LIT_NUM;
    } else if (token.kind == TOKEN_STR) {
        literal->lit_type = LIT_STR;
    } else if (token.kind == TOKEN_NULL) {
        literal->lit_type = LIT_NULL;
    } else {
        literal->lit_type = LIT_IDENT;
    }
    return literal;
}

AstCall *make_call(Arena *a, bool is_comptime, Str8View identifier, AstList *args)
{
    AstCall *call = m_arena_alloc(a, sizeof(AstCall));
    call->is_comptime = is_comptime;
    call->kind = EXPR_CALL;
    call->identifier = identifier;
    call->args = args;
    return call;
}

/* Statements */
AstWhile *make_while(Arena *a, AstExpr *condition, AstStmt *body)
{
    AstWhile *stmt = m_arena_alloc(a, sizeof(AstWhile));
    stmt->kind = STMT_WHILE;
    stmt->condition = condition;
    stmt->body = body;
    return stmt;
}

AstIf *make_if(Arena *a, AstExpr *condition, AstStmt *then, AstStmt *else_)
{
    AstIf *stmt = m_arena_alloc(a, sizeof(AstIf));
    stmt->kind = STMT_IF;
    stmt->condition = condition;
    stmt->then = then;
    stmt->else_ = else_;
    return stmt;
}

AstSingle *make_single(Arena *a, AstStmtKind single_type, AstNode *node)
{
    AstSingle *stmt = m_arena_alloc(a, sizeof(AstSingle));
    stmt->kind = single_type;
    stmt->node = node;
    return stmt;
}

AstBlock *make_block(Arena *a, TypedIdentList declarations, AstList *stmts)
{
    AstBlock *stmt = m_arena_alloc(a, sizeof(AstBlock));
    stmt->kind = STMT_BLOCK;
    stmt->declarations = declarations;
    stmt->stmts = stmts;
    return stmt;
}

AstAssignment *make_assignment(Arena *a, AstExpr *left, AstExpr *right)
{
    AstAssignment *stmt = m_arena_alloc(a, sizeof(AstAssignment));
    stmt->kind = STMT_ASSIGNMENT;
    stmt->left = left;
    stmt->right = right;
    return stmt;
}

/* Other nodes */
AstFunc *make_func(Arena *a, Str8View name, TypedIdentList params, AstStmt *body,
                   AstTypeInfo return_type)
{
    AstFunc *func = m_arena_alloc(a, sizeof(AstFunc));
    func->kind = AST_FUNC;
    func->name = name;
    func->parameters = params;
    func->return_type = return_type;
    func->body = body;
    return func;
}

AstStruct *make_struct(Arena *a, Str8View name, TypedIdentList members)
{
    AstStruct *struct_decl = m_arena_alloc(a, sizeof(AstStruct));
    struct_decl->kind = AST_STRUCT;
    struct_decl->name = name;
    struct_decl->members = members;
    return struct_decl;
}

AstEnum *make_enum(Arena *a, Str8View name, TypedIdentList values)
{
    AstEnum *enum_decl = m_arena_alloc(a, sizeof(AstEnum));
    enum_decl->kind = AST_ENUM;
    enum_decl->name = name;
    enum_decl->members = values;
    return enum_decl;
}

AstListNode *make_list_node(Arena *a, AstNode *this)
{
    AstListNode *node = m_arena_alloc(a, sizeof(AstListNode));
    node->this = this;
    node->next = NULL;
    return node;
}

AstList *make_list(Arena *a, AstNode *head)
{
    AstList *list = m_arena_alloc(a, sizeof(AstList));
    list->kind = AST_LIST;
    list->head = make_list_node(a, head);
    list->tail = list->head;
    return list;
}

void ast_list_push_back(AstList *list, AstListNode *node)
{
    if (list->head == NULL) {
        list->head = node;
        list->tail = node;
    } else {
        list->tail->next = node;
        list->tail = node;
    }
}

AstTypedIdentList *make_typed_ident_list(Arena *a, TypedIdentList vars)
{
    AstTypedIdentList *node_var_list = m_arena_alloc(a, sizeof(AstTypedIdentList));
    node_var_list->kind = AST_TYPED_IDENT_LIST;
    node_var_list->idents = vars;
    return node_var_list;
}

AstRoot *make_root(Arena *a, AstList vars, AstList funcs, AstList structs, AstList enums,
                   AstList calls)
{
    AstRoot *root = m_arena_alloc(a, sizeof(AstRoot));
    root->kind = AST_ROOT;
    root->vars = vars;
    root->funcs = funcs;
    root->structs = structs;
    root->enums = enums;
    root->calls = calls;
    return root;
}

/* AST Print */
static void print_indent(u32 indent)
{
    for (u32 i = 0; i < indent; i++) {
        putchar(' ');
    }
}

static void ast_print_typed_var_list(TypedIdentList vars)
{
    for (u32 i = 0; i < vars.len; i++) {
        TypedIdent var = vars.vars[i];
        printf("%.*s: ", STR8VIEW_PRINT(var.name));
        for (s32 j = 0; j < var.ast_type_info.pointer_indirection; j++) {
            printf("^");
        }
        printf("%.*s", STR8VIEW_PRINT(var.ast_type_info.name));
        if (var.ast_type_info.is_array) {
            printf("[%d]", var.ast_type_info.elements);
        }
        if (i != vars.len - 1) {
            printf(", ");
        }
    }
}

static void ast_print_expr(AstExpr *head, u32 indent)
{
    putchar('\n');
    print_indent(indent);
    printf("%s ", node_kind_str_map[head->kind]);

    switch (head->kind) {
    case EXPR_UNARY: {
        AstUnary *unary = AS_UNARY(head);
        char *op_text_repr = token_type_str_map[unary->op];
        printf("%s", op_text_repr);
        ast_print_expr(unary->expr, indent + 1);
    } break;
    case EXPR_BINARY: {
        AstBinary *binary = AS_BINARY(head);
        char *op_text_repr = token_type_str_map[binary->op];
        putchar('\n');
        print_indent(indent + 1);
        printf("op: %s", op_text_repr);
        ast_print_expr(binary->left, indent + 1);
        ast_print_expr(binary->right, indent + 1);
    } break;
    case EXPR_LITERAL: {
        AstLiteral *lit = AS_LITERAL(head);
        printf("%.*s", STR8VIEW_PRINT(lit->literal));
        if (lit->sym != NULL) {
            printf(":%d", lit->sym->seq_no);
        }
    } break;
    case EXPR_CALL: {
        AstCall *call = AS_CALL(head);
        if (call->is_comptime) {
            printf("@");
        }
        printf("%.*s", STR8VIEW_PRINT(call->identifier));
        if (call->args) {
            ast_print((AstNode *)call->args, indent + 1);
        }
    } break;
    default:
        ASSERT_NOT_REACHED;
    }
}

void ast_print_stmt(AstStmt *head, u32 indent)
{
    if (indent != 0) {
        putchar('\n');
    }
    print_indent(indent);
    printf("%s", node_kind_str_map[head->kind]);
    switch (head->kind) {
    case STMT_WHILE: {
        AstWhile *stmt = AS_WHILE(head);
        ast_print_expr(stmt->condition, indent + 1);
        ast_print_stmt(stmt->body, indent + 1);
    }; break;
    case STMT_IF: {
        AstIf *stmt = AS_IF(head);
        ast_print_expr(stmt->condition, indent + 1);
        ast_print_stmt(stmt->then, indent + 1);
        if (stmt->else_ != NULL) {
            ast_print_stmt(stmt->else_, indent + 1);
        }
    }; break;
    case STMT_BREAK:
    case STMT_CONTINUE:
    case STMT_RETURN:
    case STMT_EXPR: {
        AstSingle *stmt = AS_SINGLE(head);
        if (stmt->node != NULL) {
            ast_print(stmt->node, indent + 1);
        }
    }; break;
    case STMT_PRINT: {
        AstList *list = AS_LIST(head);
        for (AstListNode *node = list->head; node != NULL; node = node->next) {
            ast_print(node->this, indent + 1);
        }
    }; break;
    case STMT_BLOCK: {
        AstBlock *stmt = AS_BLOCK(head);
        printf(" vars=");
        ast_print_typed_var_list(stmt->declarations);
        printf(" syms=");
        for (u32 i = 0; i < stmt->symt_local->sym_len; i++) {
            Symbol *sym = stmt->symt_local->symbols[i];
            printf("(%s, %d) ", sym->name.str, sym->seq_no);
        }
        ast_print((AstNode *)stmt->stmts, indent + 1);
    }; break;
    case STMT_ASSIGNMENT: {
        AstAssignment *stmt = AS_ASSIGNMENT(head);
        ast_print_expr(stmt->left, indent + 1);
        ast_print_expr(stmt->right, indent + 1);
    }; break;
    default:
        ASSERT_NOT_REACHED;
    }
}

void ast_print(AstNode *head, u32 indent)
{
    if (AST_IS_EXPR(head)) {
        ast_print_expr((AstExpr *)head, indent);
        return;
    }
    if (AST_IS_STMT(head)) {
        ast_print_stmt((AstStmt *)head, indent);
        return;
    }

    if (indent != 0) {
        putchar('\n');
    }
    print_indent(indent);
    printf("%s ", node_kind_str_map[head->kind]);
    switch (head->kind) {
    case AST_ROOT: {
        AstRoot *root = AS_ROOT(head);
        ast_print((AstNode *)(&root->vars), indent + 1);
        ast_print((AstNode *)(&root->funcs), indent + 1);
        ast_print((AstNode *)(&root->structs), indent + 1);
        ast_print((AstNode *)(&root->enums), indent + 1);
        ast_print((AstNode *)(&root->calls), indent + 1);
    }; break;
    case AST_FUNC: {
        AstFunc *func = AS_FUNC(head);
        if (func->body == NULL) {
            printf("compiler internal ");
        }
        printf("name=%.*s", STR8VIEW_PRINT(func->name));
        printf(" parameters=");
        ast_print_typed_var_list(func->parameters);
        if (func->body != NULL) {
            ast_print_stmt(func->body, indent + 1);
        }
    }; break;
    case AST_STRUCT: {
        AstStruct *struct_decl = AS_STRUCT(head);
        printf("name=%.*s", STR8VIEW_PRINT(struct_decl->name));
        printf(" members=");
        ast_print_typed_var_list(struct_decl->members);
    }; break;
    case AST_ENUM: {
        AstEnum *enum_decl = AS_ENUM(head);
        printf("name=%.*s", STR8VIEW_PRINT(enum_decl->name));
        printf(" values=");
        for (u32 i = 0; i < enum_decl->members.len; i++) {
            TypedIdent var = enum_decl->members.vars[i];
            printf("%.*s", STR8VIEW_PRINT(var.name));
            if (i != enum_decl->members.len - 1) {
                printf(", ");
            }
        }
    }; break;
    case AST_LIST: {
        AstList *list = AS_LIST(head);
        for (AstListNode *node = list->head; node != NULL; node = node->next) {
            ast_print(node->this, indent + 1);
        }
    }; break;
    case AST_TYPED_IDENT_LIST: {
        AstTypedIdentList *node_var_list = AS_TYPED_IDENT_LIST(head);
        ast_print_typed_var_list(node_var_list->idents);
    }; break;
    default:
        ASSERT_NOT_REACHED;
    }
}

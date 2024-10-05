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
#include <stdio.h>


char *expr_type_str_map[EXPR_TYPE_LEN] = {
    "EXPR_UNARY",
    "EXPR_BINARY",
    "EXPR_LITERAL",
    "EXPR_LIST",
};

char *stmt_type_str_map[STMT_TYPE_LEN] = {
    "STMT_WHILE", "STMT_IF",         "STMT_ABRUPT", "STMT_LIST",        "STMT_PRINT",
    "STMT_BLOCK", "STMT_ASSIGNMENT", "STMT_FUNC",   "STMT_DECLARATION",
};


/* Expressions */
AstExprUnary *make_unary(Arena *arena, AstExpr *expr, TokenType op)
{
    AstExprUnary *unary = m_arena_alloc(arena, sizeof(AstExprUnary));
    unary->type = EXPR_UNARY;
    unary->op = op;
    unary->expr = expr;
    return unary;
}

AstExprBinary *make_binary(Arena *arena, AstExpr *left, TokenType op, AstExpr *right)
{
    AstExprBinary *binary = m_arena_alloc(arena, sizeof(AstExprBinary));
    binary->type = EXPR_BINARY;
    binary->op = op;
    binary->left = left;
    binary->right = right;
    return binary;
}

AstExprLiteral *make_literal(Arena *arena, Token token)
{
    AstExprLiteral *literal = m_arena_alloc(arena, sizeof(AstExprLiteral));
    literal->type = EXPR_LITERAL;
    if (token.type == TOKEN_NUM) {
        literal->lit_type = LIT_NUM;
        literal->num_value = token.num_value;
    } else {
        literal->lit_type = LIT_STR;
        literal->str_list_idx = token.str_list_idx;
    }
    return literal;
}

AstExprListNode *make_list_node(Arena *arena, AstExpr *this)
{
    AstExprListNode *node = m_arena_alloc(arena, sizeof(AstExprListNode));
    node->this = this;
    node->next = NULL;
    return node;
}

AstExprList *make_list(Arena *arena, AstExpr *head)
{
    AstExprList *list = m_arena_alloc(arena, sizeof(AstExprList));
    list->type = EXPR_LIST;
    AstExprListNode head_node = { .this = head, .next = NULL };
    list->head = head_node;
    list->tail = &list->head;
    return list;
}


/* Statements */
AstStmtListNode *make_stmt_list_node(Arena *arena, AstStmt *this)
{
    AstStmtListNode *node = m_arena_alloc(arena, sizeof(AstStmtListNode));
    node->this = this;
    node->next = NULL;
    return node;
}

AstStmtList *make_stmt_list(Arena *arena, AstStmt *head)
{
    AstStmtList *list = m_arena_alloc(arena, sizeof(AstStmtList));
    list->type = STMT_LIST;
    AstStmtListNode head_node = { .this = head, .next = NULL };
    list->head = head_node;
    list->tail = &list->head;
    return list;
}

AstStmtWhile *make_while(Arena *arena, AstExpr *condition, AstStmt *body)
{
    AstStmtWhile *stmt = m_arena_alloc(arena, sizeof(AstStmtWhile));
    stmt->type = STMT_WHILE;
    stmt->condition = condition;
    stmt->body = body;
    return stmt;
}

AstStmtIf *make_if(Arena *arena, AstExpr *condition, AstStmt *then, AstStmt *else_)
{
    AstStmtIf *stmt = m_arena_alloc(arena, sizeof(AstStmtIf));
    stmt->type = STMT_IF;
    stmt->condition = condition;
    stmt->then = then;
    stmt->else_ = else_;
    return stmt;
}

AstStmtPrint *make_print(Arena *arena, AstExpr *print_list)
{
    AstStmtPrint *stmt = m_arena_alloc(arena, sizeof(AstStmtPrint));
    stmt->type = STMT_PRINT;
    stmt->print_list = print_list;
    return stmt;
}

AstStmtDeclaration *make_declaration(Arena *arena);


AstStmtBlock *make_block(Arena *arena, AstStmtList *declarations, AstStmtList *stmts)
{
    AstStmtBlock *stmt = m_arena_alloc(arena, sizeof(AstStmtBlock));
    stmt->type = STMT_BLOCK;
    stmt->declarations = declarations;
    stmt->stmts = stmts;
    return stmt;
}

static void print_indent(u32 indent)
{
    for (u32 i = 0; i < indent; i++) {
        putchar(' ');
    }
}

static void ast_print_expr(AstExpr *head, Str8 *str_list, u32 indent)
{
    putchar('\n');
    print_indent(indent);
    printf("%s ", expr_type_str_map[head->type]);

    switch (head->type) {
    case EXPR_UNARY: {
        AstExprUnary *unary = AS_UNARY(head);
        char *op_text_repr = token_type_str_map[unary->op];
        printf("%s", op_text_repr);
        ast_print_expr(unary->expr, str_list, 1);
    } break;
    case EXPR_BINARY: {
        AstExprBinary *binary = AS_BINARY(head);
        char *op_text_repr = token_type_str_map[binary->op];
        putchar('\n');
        print_indent(indent + 1);
        printf("op: %s", op_text_repr);
        ast_print_expr(binary->left, str_list, indent + 1);
        ast_print_expr(binary->right, str_list, indent + 1);
    } break;
    case EXPR_LITERAL: {
        AstExprLiteral *lit = AS_LITERAL(head);
        if (lit->lit_type == LIT_NUM) {
            printf("%d", lit->num_value);
        } else {
            printf("\"%s\"", str_list[lit->str_list_idx].str);
        }
    } break;
    case EXPR_LIST: {
        AstExprList *list = AS_LIST(head);
        for (AstExprListNode *node = &list->head; node != NULL; node = node->next) {
            ast_print_expr(node->this, str_list, indent + 1);
        }
    } break;
    default:
        printf("Ast type handled ...\n");
    }
}

void ast_print(AstStmt *head, Str8 *str_list, u32 indent)
{
    if (indent != 0) {
        putchar('\n');
    }
    print_indent(indent);
    printf("%s", stmt_type_str_map[head->type]);
    switch (head->type) {
    case STMT_WHILE: {
        AstStmtWhile *stmt = AS_WHILE(head);
        ast_print_expr(stmt->condition, str_list, indent + 1);
        ast_print(stmt->body, str_list, indent + 1);
    }; break;
    case STMT_IF: {
        AstStmtIf *stmt = AS_IF(head);
        ast_print_expr(stmt->condition, str_list, indent + 1);
        ast_print(stmt->then, str_list, indent + 1);
        if (stmt->else_ != NULL) {
            ast_print(stmt->else_, str_list, indent + 1);
        }
    }; break;
    case STMT_PRINT: {
        AstStmtPrint *stmt = AS_PRINT(head);
        ast_print_expr(stmt->print_list, str_list, indent + 1);
    }; break;
    case STMT_BLOCK: {
        AstStmtBlock *stmt = AS_BLOCK(head);
        for (AstStmtListNode *node = &stmt->stmts->head; node != NULL; node = node->next) {
            ast_print(node->this, str_list, indent + 1);
        }
    }; break;
    default:
        printf("NOT HANDLED");
    }
}

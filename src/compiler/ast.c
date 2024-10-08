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

char *node_type_str_map[AST_NODE_TYPE_LEN] = {
    "EXPR_UNARY",
    "EXPR_BINARY",
    "EXPR_LITERAL",
    "EXPR_CALL",

    "EXPR_END",

    "STMT_WHILE",
    "STMT_IF",
    "STMT_ABRUPT",
    "STMT_ABRUPT_BREAK",
    "STMT_ABRUPT_CONTINUE",
    "STMT_BREAK_RETURN",
    "STMT_PRINT",
    "STMT_EXPR",
    "STMT_BLOCK",
    "STMT_ASSIGNMENT",
    "STMT_FUNC",
    "STMT_DECLARATION",

    "STMT_END",

    "AST_FUNC",
    "AST_LIST",
    "AST_GLOBAL_DECL",
    "AST_LOCAL_DECL",
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
        literal->lit_type = token.type == TOKEN_IDENTIFIER ? LIT_IDENT : LIT_STR;
        literal->str_list_idx = token.str_list_idx;
    }
    return literal;
}

AstExprCall *make_call(Arena *arena, u32 identifier, AstNode *args)
{
    AstExprCall *call = m_arena_alloc(arena, sizeof(AstExprCall));
    call->type = EXPR_CALL;
    call->identifier = identifier;
    call->args = args;
    return call;
}

/* Statements */
AstListNode *make_list_node(Arena *arena, AstNode *this)
{
    AstListNode *node = m_arena_alloc(arena, sizeof(AstListNode));
    node->this = this;
    node->next = NULL;
    return node;
}

AstList *make_list(Arena *arena, AstNode *head)
{
    AstList *list = m_arena_alloc(arena, sizeof(AstList));
    list->type = AST_LIST;
    AstListNode head_node = { .this = head, .next = NULL };
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

AstStmtSingle *make_single(Arena *arena, AstStmtType single_type, AstNode *print_list)
{
    AstStmtSingle *stmt = m_arena_alloc(arena, sizeof(AstStmtSingle));
    stmt->type = single_type;
    stmt->print_list = print_list;
    return stmt;
}

AstStmtAbrupt *make_abrupt(Arena *arena, AstStmtType abrupt_type, AstExpr *expr)
{
    AstStmtAbrupt *stmt = m_arena_alloc(arena, sizeof(AstStmtAbrupt));
    stmt->type = abrupt_type;
    stmt->expr = expr;
    return stmt;
}

AstStmtDeclaration *make_declaration(Arena *arena);


AstStmtBlock *make_block(Arena *arena, VarList declarations, AstList *stmts)
{
    AstStmtBlock *stmt = m_arena_alloc(arena, sizeof(AstStmtBlock));
    stmt->type = STMT_BLOCK;
    stmt->declarations = declarations;
    stmt->stmts = stmts;
    return stmt;
}

AstStmtAssignment *make_assignment(Arena *arena, AstExpr *left, AstExpr *right)
{
    AstStmtAssignment *stmt = m_arena_alloc(arena, sizeof(AstStmtAssignment));
    stmt->type = STMT_ASSIGNMENT;
    stmt->left = left;
    stmt->right = right;
    return stmt;
}

/* Other nodes */
AstFunction *make_function(Arena *arena, u32 identifier, VarList vars, AstStmt *body)
{
    AstFunction *func = m_arena_alloc(arena, sizeof(AstFunction));
    func->type = AST_FUNC;
    func->identifier = identifier;
    func->vars = vars;
    func->body = body;
    return func;
}

// AstDecl *make_decl(Arena *arena, AstNodeType decl_type, VarList identifiers)
// {
//     AstDecl *decl = m_arena_alloc(arena, sizeof(AstDecl));
//     decl->type = decl_type;
//     decl->identifiers = identifiers;
//     return decl;
// }


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
    printf("%s ", node_type_str_map[head->type]);

    switch (head->type) {
    case EXPR_UNARY: {
        AstExprUnary *unary = AS_UNARY(head);
        char *op_text_repr = node_type_str_map[unary->op];
        printf("%s", op_text_repr);
        ast_print_expr(unary->expr, str_list, indent + 1);
    } break;
    case EXPR_BINARY: {
        AstExprBinary *binary = AS_BINARY(head);
        char *op_text_repr = node_type_str_map[binary->op];
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
        } else if (lit->lit_type == LIT_IDENT) {
            printf("%s", str_list[lit->str_list_idx].str);
        } else {
            printf("\"%s\"", str_list[lit->str_list_idx].str);
        }
    } break;
    case EXPR_CALL: {
        AstExprCall *call = AS_CALL(head);
        printf("%s", str_list[call->identifier].str);
        if (call->args) {
            ast_print(call->args, str_list, indent + 1);
        }
    } break;
    default:
        printf("Ast type not handled ...\n");
    }
}

void ast_print_stmt(AstStmt *head, Str8 *str_list, u32 indent)
{
    if (indent != 0) {
        putchar('\n');
    }
    print_indent(indent);
    printf("%s", node_type_str_map[head->type]);
    switch (head->type) {
    case STMT_WHILE: {
        AstStmtWhile *stmt = AS_WHILE(head);
        ast_print_expr(stmt->condition, str_list, indent + 1);
        ast_print_stmt(stmt->body, str_list, indent + 1);
    }; break;
    case STMT_IF: {
        AstStmtIf *stmt = AS_IF(head);
        ast_print_expr(stmt->condition, str_list, indent + 1);
        ast_print_stmt(stmt->then, str_list, indent + 1);
        if (stmt->else_ != NULL) {
            ast_print_stmt(stmt->else_, str_list, indent + 1);
        }
    }; break;
    case STMT_EXPR:
    case STMT_PRINT: {
        AstStmtSingle *stmt = AS_SINGLE(head);
        ast_print(stmt->print_list, str_list, indent + 1);
    }; break;
    case STMT_BLOCK: {
        AstStmtBlock *stmt = AS_BLOCK(head);
        printf(" vars=");
        for (u32 i = 0; i < stmt->declarations.len; i++) {
            printf("%s ", str_list[stmt->declarations.iden_indices[i]].str);
        }
        ast_print((AstNode *)stmt->stmts, str_list, indent + 1);
        // for (AstListNode *node = &stmt->stmts->head; node != NULL; node = node->next) {
        //     ast_print_stmt(node->this, str_list, indent + 1);
        // }
    }; break;
    case STMT_ABRUPT:
    case STMT_ABRUPT_BREAK:
    case STMT_ABRUPT_CONTINUE:
    case STMT_ABRUPT_RETURN: {
        AstStmtAbrupt *stmt = AS_ABRUPT(head);
        if (stmt->expr != NULL) {
            ast_print_expr(stmt->expr, str_list, indent + 1);
        }
    }; break;
    case STMT_ASSIGNMENT: {
        AstStmtAssignment *stmt = AS_ASSIGNMENT(head);
        ast_print_expr(stmt->left, str_list, indent + 1);
        ast_print_expr(stmt->right, str_list, indent + 1);
    }; break;
    default:
        printf("NOT HANDLED");
    }
}

void ast_print(AstNode *head, Str8 *str_list, u32 indent)
{
    if ((u32)head->type < (u32)EXPR_TYPE_LEN) {
        ast_print_expr((AstExpr *)head, str_list, indent);
        return;
    } else if ((u32)head->type < (u32)STMT_TYPE_LEN) {
        ast_print_stmt((AstStmt *)head, str_list, indent);
        return;
    }

    if (indent != 0) {
        putchar('\n');
    }
    print_indent(indent);
    printf("%s ", node_type_str_map[head->type]);
    switch (head->type) {
    case AST_FUNC: {
        AstFunction *func = AS_FUNC(head);
        printf("name=%s", str_list[func->identifier].str);
        /* Print the VarList */
        printf(" vars=");
        for (u32 i = 0; i < func->vars.len; i++) {
            printf("%s ", str_list[func->vars.iden_indices[i]].str);
        }
        ast_print_stmt(func->body, str_list, indent + 1);
    }; break;
    case AST_LIST: {
        AstList *list = AS_LIST(head);
        for (AstListNode *node = &list->head; node != NULL; node = node->next) {
            ast_print(node->this, str_list, indent + 1);
        }
    }; break;
    default:
        printf("NOT HANDLED");
    }
}

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


void ast_print(AstExpr *head, Str8 *str_list, u32 indent, bool print_newline)
{
    if (print_newline) {
        putchar('\n');
    }
    for (u32 i = 0; i < indent; i++) {
        putchar(' ');
    }

    switch (head->type) {
    case EXPR_UNARY: {
        AstExprUnary *unary = AS_UNARY(head);
        char *op_text_repr = token_type_str_map[unary->op];
        printf("%s", op_text_repr);
        ast_print(unary->expr, str_list, indent, false);
        break;
    }
    case EXPR_BINARY: {
        AstExprBinary *binary = AS_BINARY(head);
        char *op_text_repr = token_type_str_map[binary->op];
        printf("%s", op_text_repr);
        ast_print(binary->left, str_list, indent + 1, print_newline);
        ast_print(binary->right, str_list, indent + 1, print_newline);
        break;
    }
    case EXPR_LITERAL: {
        AstExprLiteral *lit = AS_LITERAL(head);
        if (lit->lit_type == LIT_NUM) {
            printf("%d", lit->num_value);
        } else {
            printf("%s", str_list[lit->str_list_idx].str);
        }
        break;
    }
    case EXPR_LIST: {
        AstExprList *list = AS_LIST(head);
        for (AstExprListNode *node = &list->head; node != NULL; node = node->next) {
            ast_print(node->this, str_list, indent, false);
            putchar(',');
        }
    }
    default:
        printf("Ast type handled ...\n");
        break;
    }
}

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
#include <stdio.h>
#include "ast.h"


void ast_print(AstExpr *head, u32 indent)
{
    putchar('\n');
    for (u32 i = 0; i < indent; i++) {
        putchar(' ');
    }

    switch (head->type) {
        case EXPR_LITERAL:
            printf("%f", AS_LITERAL(head)->num_value);
            break;
        case EXPR_BINARY: {
            AstExprBinary *binary = AS_BINARY(head);
            if (binary->op == TOKEN_PLUS)
                printf("+");
            else
                printf("*");
            ast_print(binary->left, indent + 1);
            ast_print(binary->right, indent + 1);
            break;
        }
        default:
            break;
    }
}

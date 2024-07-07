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
#include "base/str.h"
#include "lex.h"

typedef enum {
	EXPR_UNARY = 0,
	EXPR_BINARY,
	EXPR_LITERAL,
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
    LiteralType lit_type;
    union {
        Str8 str_value;
        f64 num_value;
    };
} AstExprLiteral;

typedef struct {
    AstExprType type;
    AstExpr *left;
    TokenType op;
    AstExpr *right;
} AstExprBinary;

#define AS_LITERAL(___expr) ((AstExprLiteral *)(___expr))
#define AS_BINARY(___expr) ((AstExprBinary *)(___expr))


void ast_print(AstExpr *head, u32 indent);

#endif /* AST_H */

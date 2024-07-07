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
#include "base/base.h"
#include "base/sac_single.h"
#include "ast.h"
#include "lex.h"
#include "parser.h"

u32 prev_pri = 0;

TokenType token_priorities[TOKEN_TYPE_LEN] = {
    1,      /* TOKEN_NUM */
    10,     /* TOKEN_PLUS */
    20,     /* TOKEN_STAR */
    1,      /* TOKEN_SEMICOLON */
    1,      /* TOKEN_EOF */
};

static AstExprBinary *rearange(AstExprBinary *expr)
{
    assert(expr->right->type == EXPR_BINARY);

    /*
     *   OP
     *  / \
     * L  OP2
     *   /  \
     *  L2   R
     *
     *  becomes:
     *
     *     OP2
     *    /  \
     *   OP   R
     *  / \
     * L  L2
     */

    AstExprBinary *right = (AstExprBinary *)expr->right;
    expr->right = right->left;
    AstExprBinary *new_root = right;
    new_root->left = (AstExpr *)expr;
    return new_root;
}

static AstExpr *single_token_to_expr(Lexer *lexer, Token token)
{
     if (token.type == TOKEN_NUM) {
         AstExprLiteral *literal = m_arena_alloc(&lexer->arena, sizeof(AstExprLiteral));
         literal->type = EXPR_LITERAL;
         literal->lit_type = LIT_NUM;
         literal->num_value = token.num_value;
         return (AstExpr *)literal;
    }

     ASSERT_NOT_REACHED;
}

static AstExpr *parse_expr(Lexer *lexer)
{
    Token first_token = get_next_token(lexer);
    AstExpr *expr = single_token_to_expr(lexer, first_token);
    Token current_token = get_next_token(lexer);
    u32 current_pri = token_priorities[current_token.type];

    switch (current_token.type) {
        case TOKEN_SEMICOLON: {
            return expr;
        }
        case TOKEN_PLUS:
        case TOKEN_STAR: {
            prev_pri = current_pri;
            /* Binary expression */
            AstExprBinary *binary = m_arena_alloc(&lexer->arena, sizeof(AstExprBinary));
            binary->type = EXPR_BINARY;
            binary->op = current_token.type;
            binary->left = expr;
            binary->right = parse_expr(lexer);
            
            if (current_pri > prev_pri) {
                binary = rearange(binary);
            }

            return (AstExpr *)binary;
        }
        default: {
        }
    }
    ASSERT_NOT_REACHED;
}

AstExpr *parse(char *input)
{
    Arena lexer_arena;
    m_arena_init_dynamic(&lexer_arena, 2, 512);
    Lexer lexer = {
        .had_error = false,
        .input = input,
        .pos_start = 0,
        .pos_current = 0,
        .start = (Point){0},
        .current = (Point){0},
        .state = lex_any,
        .arena = lexer_arena,
    };

    return parse_expr(&lexer);
}

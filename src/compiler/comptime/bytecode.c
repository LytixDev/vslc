/*
 *  Copyright (C) 2024 Nicolai Brand (https://lytix.dev)
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
#include "compiler/comptime/bytecode.h"
#include "base/types.h"
#include "compiler/ast.h"
#include "compiler/type.h"
#include <assert.h>
#include <stdio.h>

char *op_code_str_map[OP_TYPE_LEN] = {
    "OP_CONSTANTW", "OP_ADDW", "OP_SUBW",  "OP_MULW",   "OP_DIVW",  "OP_LSHIFT", "OP_RSHIFT",
    "OP_PUSHN",     "OP_POPN", "OP_LOADL", "OP_STOREL", "OP_PRINT", "OP_RETURN",
};


/* Bytecode dissasembler */
static void disassemble_instruction(Bytecode *b)
{
    OpCode instruction = b->code[b->code_offset];
    printf("%04d %s", b->code_offset, op_code_str_map[instruction]);
    b->code_offset++;
    switch (instruction) {
    case OP_PRINT: {
        u8 n_args = b->code[b->code_offset];
        b->code_offset++;
        printf(" number of args %d", n_args);
    }; break;
    case OP_POPN:
    case OP_PUSHN:
    case OP_LOADL:
    case OP_STOREL: {
        BytecodeImm value = b->code[b->code_offset];
        b->code_offset += sizeof(BytecodeImm);
        printf(" %d", value);
    }; break;
    case OP_CONSTANTW: {
        BytecodeWord value = b->code[b->code_offset];
        b->code_offset += sizeof(BytecodeWord);
        printf(" %ld", value);
    }; break;
    default:
        break;
    }
}

void disassemble(Bytecode b)
{
    printf("--- bytecode ---\n");
    u32 code_offset_end = b.code_offset;
    b.code_offset = 0;
    while (b.code_offset < code_offset_end) {
        disassemble_instruction(&b);
        putchar('\n');
    }
}


/* Bytecode assembler */
static void writeu8(Bytecode *b, u8 byte)
{
    b->code[b->code_offset] = byte;
    b->code_offset++;
}

static void writew(Bytecode *b, BytecodeWord v)
{
    b->code[b->code_offset] = v;
    b->code_offset += sizeof(BytecodeWord);
}

static void writei(Bytecode *b, BytecodeImm v)
{
    b->code[b->code_offset] = v;
    b->code_offset += sizeof(BytecodeImm);
}

static void ast_expr_to_bytecode(Bytecode *b, AstExpr *head)
{
    switch (head->kind) {
    default:
        printf("Ast stmt not handled\n");
        break;
    case EXPR_LITERAL: {
        AstLiteral *expr = AS_LITERAL(head);
        assert(expr->lit_type == LIT_NUM);
        u32 literal = str_view_to_u32(expr->literal, NULL);
        writeu8(b, OP_CONSTANTW);
        writew(b, literal);
    }; break;
    };
}

static void ast_stmt_to_bytecode(Bytecode *b, AstStmt *head)
{
    switch (head->kind) {
    default:
        printf("Ast stmt %d not handled\n", head->kind);
        break;
    case STMT_BLOCK: {
        AstBlock *block = AS_BLOCK(head);

        /* Make space for each local variable */
        u32 var_space = 0;
        SymbolTable *symt = block->symt_local;
        for (u32 i = 0; i < symt->sym_len; i++) {
            Symbol *sym = symt->symbols[i];
            if (sym->kind == SYMBOL_LOCAL_VAR) {
                // TODO: align?
                var_space += type_info_bit_size(sym->type_info);
            }
        }
        u32 var_space_in_words = (var_space + sizeof(BytecodeWord) - 1) / sizeof(BytecodeWord);
        writeu8(b, OP_PUSHN);
        writei(b, (BytecodeImm)var_space_in_words);

        AstList *stmt = block->stmts;
        for (AstListNode *n = stmt->head; n != NULL; n = n->next) {
            ast_stmt_to_bytecode(b, (AstStmt *)n->this);
        }

        writeu8(b, OP_POPN);
        writei(b, (BytecodeImm)var_space_in_words);
    } break;
    case STMT_PRINT: {
        AstList *stmt = AS_LIST(head);
        u8 n_args = 0;
        for (AstListNode *n = stmt->head; n != NULL; n = n->next) {
            n_args++;
            ast_expr_to_bytecode(b, (AstExpr *)n->this);
        }
        writeu8(b, OP_PRINT);
        writeu8(b, n_args);
    } break;
    }
}

Bytecode ast_func_to_bytecode(AstFunc *func)
{
    Bytecode b = { 0 };
    // NOTE: right now we only compile the body
    assert(func->body != NULL);
    ast_stmt_to_bytecode(&b, func->body);
    writeu8(&b, OP_RETURN);
    return b;
}

Bytecode ast_to_bytecode(AstRoot *root)
{
    assert(OP_TYPE_LEN <= U8_MAX);
    assert(root->funcs.head != NULL);
    return ast_func_to_bytecode(AS_FUNC(root->funcs.head->this));
}

Bytecode bytecode_test(void)
{
    Bytecode b = { 0 };

    /*
     * var a: s32, b: s32, c: s32
     * a = 1
     * b = 2
     * c = a + b
     * print c
     */
    writeu8(&b, OP_PUSHN);
    writei(&b, 3);

    // a = 1
    writeu8(&b, OP_CONSTANTW);
    writew(&b, 1);
    writeu8(&b, OP_STOREL);
    writei(&b, 0);

    // b = 2
    writeu8(&b, OP_CONSTANTW);
    writew(&b, 2);
    writeu8(&b, OP_STOREL);
    writei(&b, 1);

    // c = a + b
    writeu8(&b, OP_LOADL);
    writei(&b, 1);
    writeu8(&b, OP_LOADL);
    writei(&b, 0);
    writeu8(&b, OP_ADDW);
    writeu8(&b, OP_STOREL);
    writei(&b, 2);

    // print c
    writeu8(&b, OP_LOADL);
    writei(&b, 2);
    writeu8(&b, OP_PRINT);
    writeu8(&b, 1);

    writeu8(&b, OP_RETURN);
    return b;
}

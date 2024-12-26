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
#include "compiler/bytecode.h"
#include "base/types.h"
#include "compiler/ast.h"
#include "compiler/type.h"
#include <assert.h>
#include <stdio.h>

char *op_code_str_map[OP_TYPE_LEN] = {
    "OP_CONSTANT", "OP_PLUS",   "OP_MINUS", "OP_STAR",   "OP_SLASH",
    "OP_LSHIFT",   "OP_RSHIFT", "OP_PRINT", "OP_RETURN",
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
    case OP_CONSTANTW: {
        BytecodeWord value = b->code[b->code_offset];
        b->code_offset += sizeof(BytecodeWord);
        printf(" %ld", value);
    }; break;
    default:
        break;
    }
}

static void disassemble(Bytecode *b)
{
    printf("--- bytecode ---\n");
    u32 code_offset_end = b->code_offset;
    b->code_offset = 0;
    while (b->code_offset < code_offset_end) {
        disassemble_instruction(b);
        putchar('\n');
    }
    b->code_offset = code_offset_end;
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

static BytecodeWord readw(MetagenVM *vm)
{
    BytecodeWord value = *vm->ip;
    vm->ip += sizeof(BytecodeWord);
    return value;
}

static u8 read_u8(MetagenVM *vm)
{
    u8 value = *vm->ip;
    vm->ip++;
    return value;
}

void ast_expr_to_bytecode(Bytecode *b, AstExpr *head)
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

void ast_stmt_to_bytecode(Bytecode *b, AstStmt *head)
{
    switch (head->kind) {
    default:
        printf("Ast stmt %d not handled\n", head->kind);
        break;
    case STMT_BLOCK: {
        AstBlock *block = AS_BLOCK(head);

        /* Make space for each local variable */
        // b->bp = b->code_offset;
        // SymbolTable *symt = block->symt_local;
        // for (u32 i = 0; i < symt->sym_len; i++) {
        // }

        AstList *stmt = block->stmts;
        for (AstListNode *n = stmt->head; n != NULL; n = n->next) {
            ast_stmt_to_bytecode(b, (AstStmt *)n->this);
        }
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


static Bytecode test(void)
{
    Bytecode b = { 0 };
    writeu8(&b, OP_CONSTANTW);
    writew(&b, 3);
    writeu8(&b, OP_CONSTANTW);
    writew(&b, 2);
    writeu8(&b, OP_ADDW);
    writeu8(&b, OP_CONSTANTW);
    writew(&b, 10);
    writeu8(&b, OP_MULW);
    writeu8(&b, OP_PRINT);
    writeu8(&b, 1);
    writeu8(&b, OP_RETURN);
    disassemble(&b);

    return b;
}


/* Stack operations */
static void pushw(MetagenVM *vm, BytecodeWord value)
{
    *vm->sp = value;
    vm->sp += sizeof(BytecodeWord);
}

static BytecodeWord popw(MetagenVM *vm)
{
    vm->sp -= sizeof(BytecodeWord);
    return *vm->sp;
}

u32 bytecode_run(Bytecode b)
{
    // b = test();
    MetagenVM vm = { .b = b, .ip = b.code };
    vm.sp = (u8 *)vm.stack;

    disassemble(&b);

    while (1) {
        // printf("%04ld\n", vm.ip - b.code);
        OpCode instruction;
        switch (instruction = *vm.ip++) {
        case OP_CONSTANTW: {
            BytecodeWord value = readw(&vm);
            pushw(&vm, value);
        }; break;

        /* Binary operators */
        case OP_ADDW:
            pushw(&vm, popw(&vm) + popw(&vm));
            break;
        case OP_SUBW:
            pushw(&vm, popw(&vm) - popw(&vm));
            break;
        case OP_MULW:
            pushw(&vm, popw(&vm) * popw(&vm));
            break;
        case OP_DIVW:
            pushw(&vm, popw(&vm) / popw(&vm));
            break;
        case OP_LSHIFT:
            pushw(&vm, popw(&vm) << popw(&vm));
            break;
        case OP_RSHIFT:
            pushw(&vm, popw(&vm) >> popw(&vm));
            break;

        case OP_PRINT: {
            u8 n_args = read_u8(&vm);
            /* Must first pop unto an array to maintain correct printing order */
            BytecodeWord args[n_args];
            for (u8 i = 0; i < n_args; i++) {
                BytecodeWord value = popw(&vm);
                args[i] = value;
            }
            for (u8 i = n_args; i > 0; i--) {
                printf("%ld ", args[i - 1]);
            }
            printf("\n");
        }; break;

        case OP_RETURN: {
            goto vm_loop_done;
        }
        default:
            printf("Unknown opcode %d\n", instruction);
            goto vm_loop_done;
        }
    }

vm_loop_done:
    // final = stack_pop(&vm);
    // printf("%d\n", final);
    return 0;
}

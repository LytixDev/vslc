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
#include "compiler/comptime/vm.h"
#include "compiler/comptime/bytecode.h"
#include "compiler/type.h"
#include <assert.h>
#include <stdio.h>


static BytecodeWord readw(MetagenVM *vm)
{
    BytecodeWord value = *vm->ip;
    vm->ip += sizeof(BytecodeWord);
    return value;
}

static BytecodeImm readi(MetagenVM *vm)
{
    BytecodeImm value = *vm->ip;
    vm->ip += sizeof(BytecodeImm);
    return value;
}

static u8 read_u8(MetagenVM *vm)
{
    u8 value = *vm->ip;
    vm->ip++;
    return value;
}

/* Stack operations */
static void pushn(MetagenVM *vm, BytecodeImm n)
{
    // TODO: zero init?
    vm->sp += sizeof(BytecodeWord) * n;
}

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

static BytecodeWord popn(MetagenVM *vm, BytecodeImm n)
{
    vm->sp -= sizeof(BytecodeWord) * n;
    return *vm->sp;
}

static BytecodeWord loadw(MetagenVM *vm, BytecodeImm bp_offset)
{
    return vm->stack[vm->bp + bp_offset];
}

static void storew(MetagenVM *vm, BytecodeImm bp_offset, BytecodeWord value)
{
    vm->stack[vm->bp + bp_offset] = value;
}

u32 run(Bytecode b)
{
    MetagenVM vm = { .b = b, .ip = b.code };
    vm.sp = (u8 *)vm.stack;
    vm.bp = 0;

    while (1) {
        // printf("%04ld\n", vm.ip - b.code);
        OpCode instruction;
        switch (instruction = *vm.ip++) {
        case OP_CONSTANTW: {
            BytecodeWord value = readw(&vm);
            pushw(&vm, value);
        }; break;

        /* Arithmetic */
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

        case OP_PUSHN: {
            BytecodeImm n_words = readi(&vm);
            pushn(&vm, n_words);
        }; break;
        case OP_POPN: {
            BytecodeImm n_words = readi(&vm);
            popn(&vm, n_words);
        }; break;
        case OP_STOREL: {
            BytecodeImm bp_offset = readi(&vm);
            BytecodeWord value = popw(&vm);
            storew(&vm, bp_offset, value);
        }; break;
        case OP_LOADL: {
            BytecodeImm bp_offset = readi(&vm);
            pushw(&vm, loadw(&vm, bp_offset));
        }; break;

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

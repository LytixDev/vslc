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
#include "base/nicc.h"
#include "base/base.h"
#include "base/str.h"
#include "symbol.h"
#include "compiler.h"
#include "compiler/ast.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static SymbolTable make_symbol_table(HashMap *parent)
{
    SymbolTable sym_table = { .sym_len = 0, .sym_cap = 16, .hashmap_parent = parent };
    sym_table.symbols = malloc(sizeof(Symbol) * sym_table.sym_cap);
    hashmap_init(&sym_table.hashmap);
    return sym_table;
}

// Returns the index of the newly added symbol
static u32 symbol_table_add(Compiler *compiler, SymbolTable *table, SymbolKind type, u32 name, AstNode *node)
{
    u32 seq_num = table->sym_len;
    Symbol symbol = { .type = type, .name = name, .seq_num = seq_num, .node = node };
    if (node != NULL && node->type == AST_FUNC) {
        symbol.function_symtable = make_symbol_table(&table->hashmap);
    }

    // TODO: A local symbol is not allowed to exist as a global func or type
    // if type == SYMBOL_LOCAL_VAR, traverse parents of hasmaps and ensure no exists
    Str8 symbol_name = compiler->str_list.strs[name];
    void *seq = hashmap_get(&table->hashmap, symbol_name.str, symbol_name.len);
    if (seq != NULL) {
        // TODO: report error
        printf("Symbol already exists\n");
        return SYM_ERROR;
    } 

    hashmap_put(&table->hashmap, symbol_name.str, symbol_name.len, (void *)(size_t)seq_num, sizeof(void *), false);
    table->symbols[seq_num] = symbol;
    table->sym_len += 1;
    //TODO: realloc table
    return seq_num;
}

// bind_symbols


SymbolTable symbol_generate(Compiler *compiler, AstRoot *root)
{
    SymbolTable sym_table_root = make_symbol_table(NULL);

    char *builtin_types[] = { "s32", "bool" };
    for (u32 i = 0; i < ARRAY_LENGTH(builtin_types); i++) {
        u32 name = str_list_push_cstr(compiler->persist_arena, &compiler->str_list, builtin_types[i]);
        symbol_table_add(compiler, &sym_table_root, SYMBOL_TYPE, name, NULL);
    }

    // str_list_print(&compiler->str_list);

    /* Find global symbols */
    for (AstListNode *node = root->structs.head; node != NULL; node = node->next) {
        u32 type_name = AS_STRUCT(node->this)->name;
        symbol_table_add(compiler, &sym_table_root, SYMBOL_TYPE, type_name, node->this);
    }
    for (AstListNode *node = root->functions.head; node != NULL; node = node->next) {
        u32 type_name = AS_FUNC(node->this)->name;
        symbol_table_add(compiler, &sym_table_root, SYMBOL_FUNC, type_name, node->this);
    }
    for (AstListNode *node = root->declarations.head; node != NULL; node = node->next) {
        TypedVarList vars = AS_NODE_VAR_LIST(node->this)->vars;
        for (u32 i = 0; i < vars.len; i++) {
            symbol_table_add(compiler, &sym_table_root, SYMBOL_GLOBAL_VAR, vars.vars[i].identifier, node->this);
        }
    }



    // Print symbols
    for (u32 i = 0; i < sym_table_root.sym_len; i++) {
        Symbol symbol = sym_table_root.symbols[i];
        printf("[%d] %s\n", symbol.seq_num, compiler->str_list.strs[symbol.name].str);
    }


 //3. Find all global symbols (global vars, func decls, struct decls, ... )
 //   While doing this, check for collisions and report any potential errors.
    return (SymbolTable){ 0 };
}

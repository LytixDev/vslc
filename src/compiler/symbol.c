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
#include "base/sac_single.h"
#include "symbol.h"
#include "compiler.h"
#include "compiler/ast.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static void *make_type_info(Arena *arena, TypeInfoKind kind)
{
    TypeInfo *info;
    switch (kind) {
    case TYPE_INTEGER:
        info = m_arena_alloc(arena, sizeof(TypeInfoInteger));
        break;
    case TYPE_BOOL:
        info = m_arena_alloc(arena, sizeof(TypeInfoBool));
        break;
    case TYPE_STRUCT:
        info = m_arena_alloc(arena, sizeof(TypeInfoStruct));
        break;
    case TYPE_FUNC:
        info = m_arena_alloc(arena, sizeof(TypeInfoFunc));
        break;
    default:
        ASSERT_NOT_REACHED;
    };

    info->kind = kind;
    info->is_resolved = false;
    return info;
}

static SeqNum symbol_lookup(HashMap *m, Str8 name)
{
    SeqNum *seq_num = hashmap_get(m, name.str, name.len);
    if (seq_num == NULL) {
        return SYMBOL_NOT_FOUND;
    }
    return (SeqNum)seq_num;
}

static void symbol_store(HashMap *m, Str8 name, SeqNum seq_num)
{
    hashmap_put(m, name.str, name.len, (void *)(size_t)(seq_num + 1), sizeof(void *), false);
}

static SymbolTable make_symbol_table(HashMap *parent)
{
    SymbolTable sym_table = { .sym_len = 0, .sym_cap = 16, .type_len = 0, .type_cap = 16,
                              .hashmap_parent = parent };
    sym_table.symbols = malloc(sizeof(Symbol) * sym_table.sym_cap);
    sym_table.types = malloc(sizeof(TypeInfo) * sym_table.type_cap);
    hashmap_init(&sym_table.hashmap);
    return sym_table;
}

static u32 symbol_table_add_symbol(SymbolTable *table, Symbol symbol)
{
    if (table->sym_len >= table->sym_cap) {
        table->sym_cap *= 2;
        table->symbols = realloc(table->symbols, sizeof(Symbol) * table->sym_cap);
    }

    table->symbols[table->sym_len] = symbol;
    table->sym_len++;
    return table->sym_len - 1;
}

static u32 symbol_table_add_type(SymbolTable *table, TypeInfo *type)
{
    if (table->type_len >= table->type_cap) {
        table->type_cap *= 2;
        table->types = realloc(table->types, sizeof(TypeInfo *) * table->type_cap);
    }

    table->types[table->type_len] = type;
    table->type_len++;
    return table->type_len - 1;
}

// Returns the index of the newly added symbol
static u32 symbol_table_add(Str8List str_list, SymbolTable *table, SymbolKind type, s64 name, s64 type_idx, AstNode *node)
{
    SeqNum seq_num = table->sym_len;
    Symbol symbol = { .kind = type, .name = name, .type_idx = type_idx, .seq_num = seq_num, .node = node };
    if (node != NULL && node->type == AST_FUNC) {
        symbol.function_symtable = make_symbol_table(&table->hashmap);
    }

    // TODO: A local symbol is not allowed to exist as a global func or type
    // if type == SYMBOL_LOCAL_VAR, traverse parents of hasmaps and ensure no exists
    Str8 symbol_name = str_list.strs[name];
    SeqNum lookup_seq = symbol_lookup(&table->hashmap, symbol_name);
    if (lookup_seq != SYMBOL_NOT_FOUND) {
        // TODO: report error
        printf("Symbol already exists\n");
        return SYMBOL_NOT_FOUND;
    }

    symbol_store(&table->hashmap, symbol_name, seq_num);
    symbol_table_add_symbol(table, symbol);
    return seq_num;
}


static TypeInfoStruct *struct_decl_to_type(Arena *arena, Str8List str_list, SymbolTable *sym_table, AstStruct *decl)
{
    TypeInfoStruct *type_info = make_type_info(arena, TYPE_STRUCT);
    type_info->members = m_arena_alloc(arena, sizeof(TypeInfoStructMember *) * decl->members.len);
    type_info->members_len = decl->members.len;
    type_info->info.is_resolved = true;
    for (u32 i = 0; i < decl->members.len; i++) {
        // TODO: not ignore: member->is_array, member->elements
        TypeInfoStructMember *member = m_arena_alloc(arena, sizeof(TypeInfoStructMember));
        type_info->members[i] = member;
        member->is_resolved = false;
        member->name = decl->name;

        TypedVar tv = decl->members.vars[i];
        Str8 type_str = str_list.strs[tv.type_info.name];
        SeqNum member_sym_seq = symbol_lookup(&sym_table->hashmap, type_str);
        if (member_sym_seq == SYMBOL_NOT_FOUND) {
            continue;
        }
        Symbol member_sym_type = sym_table->symbols[member_sym_seq - 1];
        if (member_sym_type.kind != SYMBOL_TYPE) {
            printf("Struct member of type X, but X is not type, but a Y...");
            continue;
        }

        member->type = sym_table->types[member_sym_type.type_idx];
        member->is_resolved = true;
    }

    for (u32 i = 0; i < type_info->members_len; i++) {
        if (!type_info->members[i]->is_resolved) {
            type_info->info.is_resolved = false;
            break;
        }
    }
    return type_info;
}



SymbolTable symbol_generate(Compiler *compiler, AstRoot *root)
{
    SymbolTable sym_table_root = make_symbol_table(NULL);

    /* Fill symbol table with builtin types */
    TypeInfoInteger *s32_builtin = make_type_info(compiler->persist_arena, TYPE_INTEGER);
    s32_builtin->info.is_resolved = true;
    s32_builtin->size = 32;
    s32_builtin->is_signed = false;
    u32 type_handle = symbol_table_add_type(&sym_table_root, (TypeInfo *)s32_builtin);
    u32 name = str_list_push_cstr(compiler->persist_arena, &compiler->str_list, "s32");
    symbol_table_add(compiler->str_list, &sym_table_root, SYMBOL_TYPE, name, type_handle, NULL);

    TypeInfoBool *bool_builtin = make_type_info(compiler->persist_arena, TYPE_BOOL);
    bool_builtin->info.is_resolved = true;
    type_handle = symbol_table_add_type(&sym_table_root, (TypeInfo *)bool_builtin);
    name = str_list_push_cstr(compiler->persist_arena, &compiler->str_list, "bool");
    symbol_table_add(compiler->str_list, &sym_table_root, SYMBOL_TYPE, name, type_handle, NULL);

    // str_list_print(&compiler->str_list);

    /* Find global symbols */
    for (AstListNode *node = root->structs.head; node != NULL; node = node->next) {
        AstStruct *struct_decl = AS_STRUCT(node->this);
        TypeInfoStruct *t = struct_decl_to_type(compiler->persist_arena, compiler->str_list, &sym_table_root, struct_decl);
        u32 type_idx = symbol_table_add_type(&sym_table_root, (TypeInfo *)t);
        symbol_table_add(compiler->str_list, &sym_table_root, SYMBOL_TYPE, struct_decl->name, type_idx, node->this);
    }
    /*
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
    */

    // Create new lower-level AST IR and bind globals to correct symbol
    // 4.



    // Print symbols
    for (u32 i = 0; i < sym_table_root.sym_len; i++) {
        Symbol symbol = sym_table_root.symbols[i];
        printf("[%d] %s", symbol.seq_num, compiler->str_list.strs[symbol.name].str);
        if (symbol.type_idx == -1) {
            putchar('\n');
            continue;
        }
        // Print type
        TypeInfo *type_info = sym_table_root.types[symbol.type_idx];
        printf(" -> %d, is_resolved %d\n", type_info->kind, type_info->is_resolved);
    }


 //3. Find all global symbols (global vars, func decls, struct decls, ... )
 //   While doing this, check for collisions and report any potential errors.
    return (SymbolTable){ 0 };
}

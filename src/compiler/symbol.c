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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


static void *make_type_info(Arena *arena, TypeInfoKind kind, u32 generated_by_name)
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
    case TYPE_ARRAY:
        info = m_arena_alloc(arena, sizeof(TypeInfoArray));
        break;
    default:
        ASSERT_NOT_REACHED;
    };

    info->kind = kind;
    info->is_resolved = false;
    info->generated_by_name = generated_by_name;
    return info;
}

static SymbolTable make_symt(SymbolTable *parent)
{
    SymbolTable symt = { .sym_len = 0, .sym_cap = 16, .type_len = 0, .type_cap = 16, .parent = parent };
    symt.symbols = malloc(sizeof(Symbol *) * symt.sym_cap);
    symt.types = malloc(sizeof(TypeInfo *) * symt.type_cap);
    hashmap_init(&symt.map);
    return symt;
}

static Symbol *symt_new_sym(Arena *arena, Str8List str_list, SymbolTable *symt, SymbolKind kind, u32 name, TypeInfo *type_info, AstNode *node)
{
    // TODO: check if symbol already exists
    // TODO: A local symbol is not allowed to exist as a global func or type
    // if type == SYMBOL_LOCAL_VAR, traverse parents of hasmaps and ensure no exists
    Symbol *sym = m_arena_alloc(arena, sizeof(Symbol));
    sym->kind = kind;
    sym->name = name;
    sym->type_info = type_info;
    sym->node = node;
    if (kind == SYMBOL_FUNC) {
        sym->function_symtable = make_symt(symt);
    }

    if (symt->sym_len >= symt->sym_cap) {
        symt->sym_cap *= 2;
        symt->symbols = realloc(symt->symbols, sizeof(Symbol *) * symt->sym_cap);
    }

    symt->symbols[symt->sym_len] = sym;
    symt->sym_len++;

    Str8 sym_name = str_list.strs[name];
    hashmap_put(&symt->map, sym_name.str, sym_name.len, sym, sizeof(Symbol *), false);

    return sym;
}

static Symbol *symt_find_sym(SymbolTable *symt, Str8 key)
{
    Symbol *sym = NULL;
    SymbolTable *t = symt;
    while (sym == NULL && t != NULL) {
        sym = hashmap_get(&t->map, key.str, key.len);
        t = t->parent;
    }
    return sym;
}

static TypeInfo *ast_type_resolve(Arena *arena, SymbolTable *symt, Str8List str_list, AstTypeInfo ati)
{
    Str8 key = str_list.strs[ati.name];
    Symbol *sym = symt_find_sym(symt, key);
    if (sym == NULL) {
        printf("ERR: Type not found\n");
        return NULL;
    }
    if (sym->kind != SYMBOL_TYPE) {
        printf("ERR: Expected symbol to be type, but found X\n");
        return NULL;
    }
    assert(sym->type_info != NULL);
    if (ati.is_array == false) {
        return sym->type_info;
    }

    TypeInfoArray *t = make_type_info(arena, TYPE_ARRAY, 0);
    t->elements = ati.elements;
    t->element_type = sym->type_info;
    t->info.is_resolved = true;
    return (TypeInfo *)t;
}


static TypeInfoStruct *struct_decl_to_type(Arena *arena, Str8List str_list, SymbolTable *symt, AstStruct *decl)
{
    TypeInfoStruct *type_info = make_type_info(arena, TYPE_STRUCT, decl->name);
    type_info->members = m_arena_alloc(arena, sizeof(TypeInfoStructMember *) * decl->members.len);
    type_info->members_len = decl->members.len;
    type_info->info.is_resolved = true;
    for (u32 i = 0; i < decl->members.len; i++) {
        TypedVar tv = decl->members.vars[i];
        TypeInfoStructMember *member = m_arena_alloc(arena, sizeof(TypeInfoStructMember));
        type_info->members[i] = member;
        member->is_resolved = false;
        member->name = tv.name;
        member->type_name = tv.ast_type_info.name;

        TypeInfo *t = ast_type_resolve(arena, symt, str_list, tv.ast_type_info);
        if (t != NULL) {
            member->is_resolved = true;
        }
        member->type = t;
    }

    for (u32 i = 0; i < type_info->members_len; i++) {
        if (!type_info->members[i]->is_resolved) {
            type_info->info.is_resolved = false;
            break;
        }
    }
    return type_info;
}

static TypeInfoFunc *func_decl_to_type(Arena *arena, Str8List str_list, SymbolTable *symt, AstFunc *decl)
{

    TypeInfoFunc *type_info = make_type_info(arena, TYPE_FUNC, decl->name);
    type_info->n_params = decl->parameters.len;
    type_info->param_names = m_arena_alloc(arena, sizeof(u32) * type_info->n_params);
    type_info->param_types = m_arena_alloc(arena, sizeof(TypeInfo *) * type_info->n_params);
    type_info->info.is_resolved = true;

    TypeInfo *return_type = ast_type_resolve(arena, symt, str_list, decl->return_type);
    type_info->return_type = return_type;
    if (return_type == NULL) {
        type_info->info.is_resolved = false;
    }

    for (u32 i = 0; i < decl->parameters.len; i++) {
        TypedVar tv = decl->parameters.vars[i];
        type_info->param_names[i] = tv.name;
        // type_info->param_types[i] = NULL;

        TypeInfo *t = ast_type_resolve(arena, symt, str_list, tv.ast_type_info);
        if (t == NULL) {
            type_info->info.is_resolved = false;
        }
        type_info->param_types[i] = t;
    }

    return type_info;
}

/* --- PRINT --- */
static void type_info_print(Str8List list, TypeInfo *info)
{
    printf("[%s] ", info->is_resolved ? "R" : "U");
    switch (info->kind) {
        case TYPE_INTEGER: {
            TypeInfoInteger *t = (TypeInfoInteger *)info;
            printf("int {size: %d, is_signed: %d}", t->size, t->is_signed);
        } break;
        case TYPE_BOOL:
            printf("bool");
            break;
        case TYPE_STRUCT: {
            TypeInfoStruct *t = (TypeInfoStruct *)info;
            printf("struct {");
            for (u32 i = 0; i < t->members_len; i++) {
                TypeInfoStructMember *member = t->members[i];
                Str8 name = list.strs[member->name];
                if (member->is_resolved) {
                    u32 generated_by = member->type->generated_by_name;
                    Str8 s = list.strs[generated_by];
                    printf("%s: %s, ", name.str, s.str);
                } else {
                    Str8 type_name = list.strs[member->type_name];
                    printf("%s: {unresolved : }, ", type_name.str); 
                }
            }
            putchar('}');
        } break;
        case TYPE_ARRAY: {
            TypeInfoArray *t = (TypeInfoArray *)info;
            Str8 element_type_name = list.strs[t->info.generated_by_name];
            printf("array %s[%d], type: ", element_type_name.str, t->elements);
        } break;
        case TYPE_FUNC: {
            TypeInfoFunc *t = (TypeInfoFunc *)info;
            printf("func {return_type: %s, ", list.strs[t->return_type->generated_by_name].str);
            printf("params: [");
            for (u32 i = 0; i < t->n_params; i++) {
                Str8 name = list.strs[t->param_names[i]];
                Str8 type_name = list.strs[t->param_types[i]->generated_by_name];
                printf("%s: %s, ", name.str, type_name.str);
            }
            printf("]}");
        } break;
        default:
            break;
    }
}



SymbolTable symbol_generate(Compiler *compiler, AstRoot *root)
{
    SymbolTable symt_root = make_symt(NULL);

    /* Fill symbol table with builtin types */
    {
        u32 name = str_list_push_cstr(compiler->persist_arena, &compiler->str_list, "s32");
        TypeInfoInteger *s32_builtin = make_type_info(compiler->persist_arena, TYPE_INTEGER, name);
        s32_builtin->info.is_resolved = true;
        s32_builtin->size = 32;
        s32_builtin->is_signed = true;
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_TYPE, name, (TypeInfo *)s32_builtin, NULL);

        name = str_list_push_cstr(compiler->persist_arena, &compiler->str_list, "bool");
        TypeInfoBool *bool_builtin = make_type_info(compiler->persist_arena, TYPE_BOOL, name);
        bool_builtin->info.is_resolved = true;
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_TYPE, name, (TypeInfo *)bool_builtin, NULL);
    }

    /* Find global symbols */
    for (AstListNode *node = root->structs.head; node != NULL; node = node->next) {
        AstStruct *struct_decl = AS_STRUCT(node->this);
        TypeInfoStruct *t = struct_decl_to_type(compiler->persist_arena, compiler->str_list, &symt_root, struct_decl);
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_TYPE, struct_decl->name, (TypeInfo *)t, node->this);
    }
    for (AstListNode *node = root->functions.head; node != NULL; node = node->next) {
        AstFunc *func_decl = AS_FUNC(node->this);
        TypeInfoFunc *t = func_decl_to_type(compiler->persist_arena, compiler->str_list, &symt_root, func_decl);
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_FUNC, func_decl->name, (TypeInfo *)t, node->this);
    }
    /*
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
    for (u32 i = 0; i < symt_root.sym_len; i++) {
        Symbol *sym = symt_root.symbols[i];
        printf("%s", compiler->str_list.strs[sym->name].str);
        if (sym->type_info == NULL) {
            putchar('\n');
            continue;
        }
        // Print type
        //printf(" -> %d, is_resolved %d\n", sym->type_info->kind, sym->type_info->is_resolved);
        printf("\t-> ");
        type_info_print(compiler->str_list, sym->type_info);
        putchar('\n');
    }


    return (SymbolTable){ 0 };
}

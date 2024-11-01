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
#include "type.h"
#include "ast.h"
#include "base/base.h"
#include "base/nicc.h"
#include "base/sac_single.h"
#include "base/str.h"
#include "compiler.h"
#include "error.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static void *make_type_info(Arena *arena, TypeInfoKind kind, Str8 generated_by_name)
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
    case TYPE_ENUM:
        info = m_arena_alloc(arena, sizeof(TypeInfoEnum));
        break;
    case TYPE_FUNC:
        info = m_arena_alloc(arena, sizeof(TypeInfoFunc));
        break;
    case TYPE_ARRAY:
        info = m_arena_alloc(arena, sizeof(TypeInfoArray));
        break;
    case TYPE_POINTER:
        info = m_arena_alloc(arena, sizeof(TypeInfoPointer));
        break;
    default:
        ASSERT_NOT_REACHED;
    };

    info->kind = kind;
    info->is_resolved = kind == TYPE_ENUM ? true : false; // Enums are always resolved
    info->generated_by_name = generated_by_name;
    return info;
}

static SymbolTable make_symt(SymbolTable *parent)
{
    SymbolTable symt = { .sym_len = 0,
                         .sym_cap = 16,
                         .type_len = 0,
                         .type_cap = 0, // Only the root symbol table can generate types
                         .struct_count = 0,
                         .parent = parent };

    /* More sensible defautls for the root symbol table */
    if (parent == NULL) {
        symt.sym_cap = 64;
        symt.type_cap = 64;
    }
    symt.symbols = malloc(sizeof(Symbol *) * symt.sym_cap);
    symt.types = malloc(sizeof(TypeInfo *) * symt.type_cap);
    hashmap_init(&symt.map);
    return symt;
}

static Symbol *symt_new_sym(Compiler *compiler, SymbolTable *symt, SymbolKind kind, Str8 name,
                            TypeInfo *type_info, AstNode *node)
{
    // TODO: check if symbol already exists
    // TODO: A local symbol is not allowed to exist as a global func or type
    // if type == SYMBOL_LOCAL_VAR, traverse parents of hasmaps and ensure no exists
    Symbol *sym_existing = hashmap_get(&symt->map, name.str, name.len);
    if (sym_existing != NULL) {
        error_sym(compiler->e, "Symbol already exists", name);
        /* We will continue with the OG symbol, but will stop compilation later */
        return sym_existing;
    }

    /* Create the new symbol */
    Symbol *sym = m_arena_alloc(compiler->persist_arena, sizeof(Symbol));
    sym->kind = kind;
    sym->name = name;
    sym->type_info = type_info;
    sym->node = node;
    if (kind == SYMBOL_FUNC) {
        sym->function_symtable = make_symt(symt);
    }

    /* Ensure space in the symbol table and add the new symbol */
    if (symt->sym_len >= symt->sym_cap) {
        symt->sym_cap *= 2;
        symt->symbols = realloc(symt->symbols, sizeof(Symbol *) * symt->sym_cap);
    }
    symt->symbols[symt->sym_len] = sym;
    symt->sym_len++;

    /* If symbol generated a type, ensure space in type table and add new type */
    if (type_info != NULL) {
        if (type_info->kind == TYPE_STRUCT) {
            ((TypeInfoStruct *)type_info)->struct_id = symt->struct_count;
            symt->struct_count++;
        }
        if (symt->type_len >= symt->type_cap) {
            symt->type_cap *= 2;
            symt->types = realloc(symt->types, sizeof(TypeInfo *) * symt->type_cap);
        }
        symt->types[symt->type_len] = type_info;
        symt->type_len++;
    }

    hashmap_put(&symt->map, name.str, name.len, sym, sizeof(Symbol *), false);
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

static TypeInfo *ast_type_resolve(Compiler *compiler, AstTypeInfo ati)
{
    // NOTE: This function uses the root symbol table. Okay as of now since we don't allow
    //       declarations that generate types that aren't global.
    Str8 sym_name = ati.name;
    Symbol *sym = symt_find_sym(&compiler->symt_root, sym_name);
    /* Not found */
    if (sym == NULL) {
        return NULL;
    }
    /* Symbol is not a type */
    if (sym->kind != SYMBOL_TYPE) {
        // Str8 sym_name = sym->name;
        error_sym(compiler->e, "Wants to be used as a type, but is in fact not :-(", sym_name);
        return NULL;
    }
    assert(sym->type_info != NULL);

    // TODO: Instead of creating new types when we see pointers and arrays, maybe we can check if
    //       they already exist and then reuse that?
    TypeInfo *type_info = sym->type_info;
    if (ati.is_pointer) {
        TypeInfoPointer *t = make_type_info(compiler->persist_arena, TYPE_POINTER, sym->name);
        t->pointer_to = type_info;
        t->info.is_resolved = true;
        type_info = (TypeInfo *)t;
    }
    if (ati.is_array) {
        TypeInfoArray *t = make_type_info(compiler->persist_arena, TYPE_ARRAY, sym->name);
        t->elements = ati.elements;
        t->element_type = type_info;
        t->info.is_resolved = true;
        type_info = (TypeInfo *)t;
    }
    return type_info;
}

static TypeInfoEnum *enum_decl_to_type(Compiler *compiler, AstEnum *decl)
{
    Arena *arena = compiler->persist_arena;
    TypeInfoEnum *t = make_type_info(arena, TYPE_ENUM, decl->name);
    t->member_names = m_arena_alloc(arena, sizeof(Str8) * decl->members.len);
    t->members_len = decl->members.len;

    for (u32 i = 0; i < decl->members.len; i++) {
        TypedVar tv = decl->members.vars[i];
        t->member_names[i] = tv.name;
    }

    return t;
}

static TypeInfoStruct *struct_decl_to_type(Compiler *compiler, AstStruct *decl)
{
    Arena *arena = compiler->persist_arena;
    TypeInfoStruct *t = make_type_info(arena, TYPE_STRUCT, decl->name);
    t->members = m_arena_alloc(arena, sizeof(TypeInfoStructMember *) * decl->members.len);
    t->members_len = decl->members.len;
    t->info.is_resolved = true;

    for (u32 i = 0; i < decl->members.len; i++) {
        TypedVar tv = decl->members.vars[i];
        TypeInfoStructMember *member = m_arena_alloc(arena, sizeof(TypeInfoStructMember));
        t->members[i] = member;
        member->is_resolved = false;
        member->name = tv.name;
        member->ati = tv.ast_type_info;

        TypeInfo *member_t = ast_type_resolve(compiler, tv.ast_type_info);
        if (member_t != NULL) {
            member->is_resolved = true;
            member->type = member_t;
        }
    }

    for (u32 i = 0; i < t->members_len; i++) {
        if (!t->members[i]->is_resolved) {
            t->info.is_resolved = false;
            break;
        }
    }
    return t;
}

static TypeInfoFunc *func_decl_to_type(Compiler *compiler, AstFunc *decl)
{
    TypeInfoFunc *t = make_type_info(compiler->persist_arena, TYPE_FUNC, decl->name);
    t->n_params = decl->parameters.len;
    t->param_names = m_arena_alloc(compiler->persist_arena, sizeof(Str8) * t->n_params);
    t->param_types = m_arena_alloc(compiler->persist_arena, sizeof(TypeInfo *) * t->n_params);
    t->info.is_resolved = true;

    TypeInfo *return_type = ast_type_resolve(compiler, decl->return_type);
    t->return_type = return_type;
    if (return_type == NULL) {
        t->info.is_resolved = false;
    }

    for (u32 i = 0; i < decl->parameters.len; i++) {
        TypedVar tv = decl->parameters.vars[i];
        t->param_names[i] = tv.name;
        TypeInfo *param_t = ast_type_resolve(compiler, tv.ast_type_info);
        if (param_t == NULL) {
            t->info.is_resolved = false;
        }
        t->param_types[i] = param_t;
    }

    return t;
}

/* --- PRINT --- */
static void type_info_print(TypeInfo *type_info)
{
    printf("[%s] ", type_info->is_resolved ? "R" : "U");
    switch (type_info->kind) {
    case TYPE_INTEGER: {
        TypeInfoInteger *t = (TypeInfoInteger *)type_info;
        printf("int {size: %d, is_signed: %d}", t->size, t->is_signed);
    } break;
    case TYPE_BOOL:
        printf("bool");
        break;
    case TYPE_STRUCT: {
        TypeInfoStruct *t = (TypeInfoStruct *)type_info;
        printf("struct {");
        for (u32 i = 0; i < t->members_len; i++) {
            TypeInfoStructMember *member = t->members[i];
            Str8 name = member->name;
            if (member->is_resolved) {
                if (member->type->kind == TYPE_ARRAY || member->type->kind == TYPE_POINTER) {
                    printf("%s: ", name.str);
                    type_info_print(member->type);
                    printf(", ");
                } else {
                    printf("%s: %s, ", name.str, member->type->generated_by_name.str);
                }
            } else {
                printf("%s: [U]%s, ", name.str, member->ati.name.str);
            }
        }
        putchar('}');
    } break;
    case TYPE_ENUM: {
        TypeInfoEnum *t = (TypeInfoEnum *)type_info;
        printf("enum {");
        for (u32 i = 0; i < t->members_len; i++) {
            printf("%s", t->member_names[i].str);
            if (i != t->members_len - 1) {
                printf(", ");
            }
        }
        putchar('}');
    }; break;
    case TYPE_FUNC: {
        TypeInfoFunc *t = (TypeInfoFunc *)type_info;
        // TODO: generated_by_name will not be set for arrays
        printf("func {return_type: %s, ", t->return_type->generated_by_name.str);
        printf("params: [");
        for (u32 i = 0; i < t->n_params; i++) {
            printf("%s: %s, ", t->param_names[i].str, t->return_type[i].generated_by_name.str);
        }
        printf("]}");
    } break;
    case TYPE_ARRAY: {
        TypeInfoArray *t = (TypeInfoArray *)type_info;
        if (t->element_type->kind == TYPE_POINTER) {
            type_info_print(t->element_type);
        } else {
            printf("%s", t->info.generated_by_name.str);
        }
        printf("[%d]", t->elements);
    } break;
    case TYPE_POINTER: {
        TypeInfoPointer *t = (TypeInfoPointer *)type_info;
        printf("^%s", t->info.generated_by_name.str);
    } break;
    default:
        break;
    }
}


/* --- Graph --- */
typedef struct graph_node_t GraphNode;
struct graph_node_t {
    u32 id;
    GraphNode *next;
};

typedef struct {
    u32 n_nodes;
    GraphNode **neighbor_list;
} Graph;


static Graph make_graph(Arena *arena, u32 n_nodes)
{
    Graph graph = { .n_nodes = n_nodes,
                    .neighbor_list = m_arena_alloc(arena, sizeof(GraphNode *) * n_nodes) };

    memset(graph.neighbor_list, 0, sizeof(GraphNode) * graph.n_nodes);
    return graph;
}

/*
static void graph_print(Graph *graph)
{
    for (u32 i = 0; i < graph->n_nodes; i++) {
        printf("[%d] -> ", i);
        GraphNode *node = graph->neighbor_list[i];
        while (node != NULL) {
            printf("%d, ", node->id);
            node = node->next;
        }
        putchar('\n');
    }
}
*/

static void graph_add_edge(Arena *arena, Graph *graph, u32 from, u32 to)
{
    assert(from <= graph->n_nodes);
    GraphNode *first = graph->neighbor_list[from];
    GraphNode *new_node = m_arena_alloc(arena, sizeof(GraphNode));
    new_node->id = to;
    new_node->next = first;
    graph->neighbor_list[from] = new_node;
}

static void graph_add_edges_from_struct_type(Arena *arena, Graph *graph, TypeInfoStruct *t)
{
    u32 *added_list = m_arena_alloc_zero(arena, sizeof(u32) * t->members_len);
    u32 added_count = 0;

    for (u32 i = 0; i < t->members_len; i++) {
        TypeInfoStructMember *m = t->members[i];
        TypeInfoStruct *member_type;
        /* Only care about arrays were the underlying type is a struct */
        if (m->type->kind == TYPE_ARRAY) {
            TypeInfoArray *member = (TypeInfoArray *)m->type;
            if (member->element_type->kind != TYPE_STRUCT) {
                continue;
            }
            member_type = (TypeInfoStruct *)member->element_type;
        } else if (m->type->kind == TYPE_STRUCT) {
            member_type = (TypeInfoStruct *)m->type;
        } else {
            continue;
        }

        u32 from = t->struct_id;
        u32 to = member_type->struct_id;

        /* Do not add duplicate edges */
        bool duplicate = false;
        for (u32 j = 0; j < added_count; j++) {
            if (added_list[j] == to) {
                duplicate = true;
                break;
            }
        }
        if (!duplicate) {
            graph_add_edge(arena, graph, from, to);
            added_list[added_count++] = to;
        }
    }
}

static bool find_cycles(Compiler *compiler, Arena *graph_arena, Graph *graph)
{
    // TODO: Trajan's algorithm would be better
    // TODO: return what caused the cycle so we can create an error message

    /* DFS-based cycle detector */
    u32 *visited = m_arena_alloc(graph_arena, sizeof(u32) * graph->n_nodes);
    memset(visited, false, sizeof(u32) * graph->n_nodes);
    u32 stack[1024]; // TODO: not-fixed width
    u32 recursion_stack[1024]; // TODO: not-fixed width


    for (u32 start_node = 0; start_node < graph->n_nodes; start_node++) {
        if (visited[start_node]) {
            continue;
        }

        stack[0] = start_node;
        u32 stack_len = 1;
        u32 recursion_idx = 0;

        while (stack_len != 0) {
            u32 current_node = stack[--stack_len];
            /* if node in recursion_stack then we have a cycle */
            for (u32 i = 0; i < recursion_idx; i++) {
                if (recursion_stack[i] == current_node) {
                    // TODO: We can backtrace the steps to give a really good error message, but
                    //       right now I just do the bare minimum.
                    Str8Builder sb = make_str_builder(&compiler->e->arena);
                    str_builder_sprintf(&sb, "A type cycle detected in struct with id %d", 1,
                                        (int)current_node);
                    Str8 msg = str_builder_end(&sb);
                    error_msg_str8(compiler->e, msg);
                    return true;
                }
            }

            if (visited[current_node]) {
                /* if node is visited, pop it from recursion_stack (backtrack) */
                if (recursion_idx > 0 && recursion_stack[recursion_idx - 1] == current_node) {
                    recursion_idx--;
                }
                continue;
            }

            /* mark node as visited and add it to recursion_stack */
            visited[current_node] = true;
            recursion_stack[recursion_idx++] = current_node;

            for (GraphNode *n = graph->neighbor_list[current_node]; n != NULL; n = n->next) {
                stack[stack_len++] = n->id;
            }
        }
    }

    return false;
}

static void fill_builtin_types(Compiler *compiler)
{
    /* s32 */
    Str8Builder sb = make_str_builder(compiler->persist_arena);
    str_builder_append_cstr(&sb, "s32", 3);
    sb.str.len++; // TODO: fix HACK
    Str8 name = str_builder_end(&sb);
    TypeInfoInteger *s32_builtin = make_type_info(compiler->persist_arena, TYPE_INTEGER, name);
    s32_builtin->info.is_resolved = true;
    s32_builtin->size = 32;
    s32_builtin->is_signed = true;
    symt_new_sym(compiler, &compiler->symt_root, SYMBOL_TYPE, name, (TypeInfo *)s32_builtin, NULL);

    /* bool */
    sb = make_str_builder(compiler->persist_arena);
    str_builder_append_cstr(&sb, "bool", 4);
    sb.str.len++; // TODO: fix HACK
    name = str_builder_end(&sb);
    TypeInfoBool *bool_builtin = make_type_info(compiler->persist_arena, TYPE_BOOL, name);
    bool_builtin->info.is_resolved = true;
    symt_new_sym(compiler, &compiler->symt_root, SYMBOL_TYPE, name, (TypeInfo *)bool_builtin, NULL);
}


void symbol_generate(Compiler *compiler, AstRoot *root)
{
    compiler->symt_root = make_symt(NULL);

    /* Fill symbol table with builtin types */
    fill_builtin_types(compiler);

    /* Create symbols and types for global declarations */
    for (AstListNode *node = root->enums.head; node != NULL; node = node->next) {
        AstEnum *enum_decl = AS_ENUM(node->this);
        TypeInfoEnum *t = enum_decl_to_type(compiler, enum_decl);
        symt_new_sym(compiler, &compiler->symt_root, SYMBOL_TYPE, enum_decl->name, (TypeInfo *)t,
                     node->this);
    }
    for (AstListNode *node = root->structs.head; node != NULL; node = node->next) {
        AstStruct *struct_decl = AS_STRUCT(node->this);
        TypeInfoStruct *t = struct_decl_to_type(compiler, struct_decl);
        symt_new_sym(compiler, &compiler->symt_root, SYMBOL_TYPE, struct_decl->name, (TypeInfo *)t,
                     node->this);
    }
    for (AstListNode *node = root->functions.head; node != NULL; node = node->next) {
        AstFunc *func_decl = AS_FUNC(node->this);
        TypeInfoFunc *t = func_decl_to_type(compiler, func_decl);
        symt_new_sym(compiler, &compiler->symt_root, SYMBOL_FUNC, func_decl->name, (TypeInfo *)t,
                     node->this);
    }
    /*
    for (AstListNode *node = root->declarations.head; node != NULL; node = node->next) {
        TypedVarList vars = AS_NODE_VAR_LIST(node->this)->vars;
        for (u32 i = 0; i < vars.len; i++) {
            symbol_table_add(compiler, &sym_table_root, SYMBOL_GLOBAL_VAR, vars.vars[i].identifier,
    node->this);
        }
    }
    */

    /* Resolve all unsresolved types */
    for (u32 i = 0; i < compiler->symt_root.type_len; i++) {
        TypeInfo *t = compiler->symt_root.types[i];
        if (t->is_resolved) {
            continue;
        }
        /*
         * Right now, everything other than struct member will be wholly resolved if the type it
         * references exists.
         */
        if (t->kind != TYPE_STRUCT) {
            error_sym(compiler->e, "A type used here is not declared", t->generated_by_name);
            continue;
        }

        TypeInfoStruct *t_struct = (TypeInfoStruct *)t;
        t_struct->info.is_resolved = true;
        for (u32 j = 0; j < t_struct->members_len; j++) {
            TypeInfoStructMember *m = t_struct->members[j];
            if (m->is_resolved) {
                continue;
            }
            TypeInfo *m_t = ast_type_resolve(compiler, m->ati);
            if (m_t == NULL) {
                error_sym(compiler->e, "Type of struct member is never declared", m->ati.name);
                t_struct->info.is_resolved = false;
                continue;
            }
            m->type = m_t;
            m->is_resolved = true;
        }
    }
    /* Early return if any types are unresolved */
    if (compiler->e->n_errors != 0) {
        return;
    }

    /* Check for cirular type dependencies */
    ArenaTmp tmp = m_arena_tmp_init(compiler->persist_arena);
    Graph graph = make_graph(tmp.arena, compiler->symt_root.struct_count);
    for (u32 i = 0; i < compiler->symt_root.type_len; i++) {
        TypeInfo *t = compiler->symt_root.types[i];
        if (t->kind == TYPE_STRUCT) {
            graph_add_edges_from_struct_type(tmp.arena, &graph, (TypeInfoStruct *)t);
        }
    }

    find_cycles(compiler, tmp.arena, &graph);
    m_arena_tmp_release(tmp);
    /* If any cycles were found, early return */
    if (compiler->e->n_errors != 0) {
        return;
    }

    // Print symbols
    for (u32 i = 0; i < compiler->symt_root.sym_len; i++) {
        Symbol *sym = compiler->symt_root.symbols[i];
        printf("%.*s", STR8VIEW_PRINT(sym->name));
        if (sym->type_info == NULL) {
            putchar('\n');
            continue;
        }
        // Print type
        // printf(" -> %d, is_resolved %d\n", sym->type_info->kind, sym->type_info->is_resolved);
        printf("\t\t-> ");
        type_info_print(sym->type_info);
        putchar('\n');
    }
}

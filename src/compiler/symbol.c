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
#include "symbol.h"
#include "base/base.h"
#include "base/nicc.h"
#include "base/sac_single.h"
#include "base/str.h"
#include "compiler.h"
#include "compiler/ast.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


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
    SymbolTable symt = { .sym_len = 0,
                         .sym_cap = 16,
                         .type_len = 0,
                         .type_cap = 16,
                         .struct_count = 0,
                         .parent = parent };
    symt.symbols = malloc(sizeof(Symbol *) * symt.sym_cap);
    symt.types = malloc(sizeof(TypeInfo *) * symt.type_cap);
    hashmap_init(&symt.map);
    return symt;
}

static Symbol *symt_new_sym(Arena *arena, Str8List str_list, SymbolTable *symt, SymbolKind kind,
                            u32 name, TypeInfo *type_info, AstNode *node)
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

    // Ensure space for Symbol
    if (symt->sym_len >= symt->sym_cap) {
        symt->sym_cap *= 2;
        symt->symbols = realloc(symt->symbols, sizeof(Symbol *) * symt->sym_cap);
    }
    symt->symbols[symt->sym_len] = sym;
    symt->sym_len++;

    if (type_info != NULL) {
        if (type_info->kind == TYPE_STRUCT) {
            ((TypeInfoStruct *)type_info)->struct_id = symt->struct_count;
            symt->struct_count++;
        }

        // Ensure space for Type
        if (symt->type_len >= symt->type_cap) {
            symt->type_cap *= 2;
            symt->types = realloc(symt->types, sizeof(TypeInfo *) * symt->type_cap);
        }
        symt->types[symt->type_len] = type_info;
        symt->type_len++;
    }

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

static TypeInfo *ast_type_resolve(Arena *arena, SymbolTable *symt, Str8List str_list,
                                  AstTypeInfo ati)
{
    Str8 key = str_list.strs[ati.name];
    Symbol *sym = symt_find_sym(symt, key);
    if (sym == NULL) {
        // printf("LOG: Type not found\n");
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


static TypeInfoStruct *struct_decl_to_type(Arena *arena, Str8List str_list, SymbolTable *symt,
                                           AstStruct *decl)
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
        member->ati = tv.ast_type_info;

        TypeInfo *t = ast_type_resolve(arena, symt, str_list, tv.ast_type_info);
        if (t != NULL) {
            member->is_resolved = true;
            member->type = t;
        }
    }

    for (u32 i = 0; i < type_info->members_len; i++) {
        if (!type_info->members[i]->is_resolved) {
            type_info->info.is_resolved = false;
            break;
        }
    }
    return type_info;
}

static TypeInfoFunc *func_decl_to_type(Arena *arena, Str8List str_list, SymbolTable *symt,
                                       AstFunc *decl)
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
static void type_info_print(Str8List list, TypeInfo *type_info)
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
            Str8 name = list.strs[member->name];
            if (member->is_resolved) {
                if (member->type->kind == TYPE_ARRAY) {
                    printf("%s: ", name.str);
                    type_info_print(list, member->type);
                } else {
                    u32 generated_by = member->type->generated_by_name;
                    Str8 s = list.strs[generated_by];
                    printf("%s: %s, ", name.str, s.str);
                }
            } else {
                u32 expected_type_name = member->ati.name;
                Str8 s = list.strs[expected_type_name];
                printf("%s: [U]%s, ", name.str, s.str);
            }
        }
        putchar('}');
    } break;
    case TYPE_ARRAY: {
        TypeInfoArray *t = (TypeInfoArray *)type_info;
        Str8 element_type_name = list.strs[t->info.generated_by_name];
        printf("array %s[%d]", element_type_name.str, t->elements);
    } break;
    case TYPE_FUNC: {
        TypeInfoFunc *t = (TypeInfoFunc *)type_info;
        // TODO: generated_by_name will not be set for arrays
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

    // TODO: memset
    for (u32 i = 0; i < n_nodes; i++) {
        graph.neighbor_list[i] = NULL;
    }

    return graph;
}

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

static void graph_add_edge(Arena *arena, Graph *graph, u32 from, u32 to)
{
    assert(from <= graph->n_nodes);
    GraphNode *first = graph->neighbor_list[from];
    GraphNode *new_node = m_arena_alloc(arena, sizeof(GraphNode));
    new_node->id = to;
    new_node->next = first;
    graph->neighbor_list[from] = new_node;
}

static void graph_add_struct_edges(Arena *arena, Graph *graph, TypeInfoStruct *t)
{
    u32 *added_list = m_arena_alloc_zero(arena, sizeof(u32) * t->members_len);
    u32 added_count = 0;
    for (u32 i = 0; i < t->members_len; i++) {
        TypeInfoStructMember *m = t->members[i];
        /* Only care about struct and array types */
        if (!(m->type->kind == TYPE_STRUCT || m->type->kind == TYPE_ARRAY)) {
            continue;
        }

        TypeInfoStruct *member_type = (TypeInfoStruct *)m->type;
        /* We are interested in arrays if the element type is a struct */
        if (m->type->kind == TYPE_ARRAY) {
            TypeInfoArray *member_as_array = (TypeInfoArray *)m->type;
            if (member_as_array->element_type->kind == TYPE_STRUCT) {
                member_type = (TypeInfoStruct *)member_as_array->element_type;
            }
        }
        u32 from = t->struct_id;
        u32 to = member_type->struct_id;

        // Only add new edges
        bool skip_add = false;
        for (u32 j = 0; j < added_count; j++) {
            if (added_list[j] == to) {
                skip_add = true;
                break;
            }
        }
        if (!skip_add) {
            graph_add_edge(arena, graph, from, to);
            added_list[added_count++] = to;
        }
    }
}

static bool find_cycles(Arena *arena, Graph *graph)
{
    // TODO: Trajan's algorithm would be better
    // TODO: return what caused the cycle so we can create an error message

    /* DFS-based cycle detector */
    u32 *visited = m_arena_alloc(arena, sizeof(u32) * graph->n_nodes);
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
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_TYPE, name,
                     (TypeInfo *)s32_builtin, NULL);

        name = str_list_push_cstr(compiler->persist_arena, &compiler->str_list, "bool");
        TypeInfoBool *bool_builtin = make_type_info(compiler->persist_arena, TYPE_BOOL, name);
        bool_builtin->info.is_resolved = true;
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_TYPE, name,
                     (TypeInfo *)bool_builtin, NULL);
    }

    /* Find global symbols */
    for (AstListNode *node = root->structs.head; node != NULL; node = node->next) {
        AstStruct *struct_decl = AS_STRUCT(node->this);
        TypeInfoStruct *t = struct_decl_to_type(compiler->persist_arena, compiler->str_list,
                                                &symt_root, struct_decl);
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_TYPE,
                     struct_decl->name, (TypeInfo *)t, node->this);
    }
    for (AstListNode *node = root->functions.head; node != NULL; node = node->next) {
        AstFunc *func_decl = AS_FUNC(node->this);
        TypeInfoFunc *t =
            func_decl_to_type(compiler->persist_arena, compiler->str_list, &symt_root, func_decl);
        symt_new_sym(compiler->persist_arena, compiler->str_list, &symt_root, SYMBOL_FUNC,
                     func_decl->name, (TypeInfo *)t, node->this);
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
    for (u32 i = 0; i < symt_root.type_len; i++) {
        TypeInfo *t = symt_root.types[i];
        if (t->is_resolved) {
            continue;
        }
        if (t->kind != TYPE_STRUCT) {
            printf("ERR: Some type here is unknown ...\n");
            continue;
        }

        TypeInfoStruct *t_struct = (TypeInfoStruct *)t;
        t_struct->info.is_resolved = true;
        for (u32 j = 0; j < t_struct->members_len; j++) {
            TypeInfoStructMember *m = t_struct->members[j];
            if (m->is_resolved) {
                continue;
            }
            TypeInfo *m_t =
                ast_type_resolve(compiler->persist_arena, &symt_root, compiler->str_list, m->ati);
            if (m_t == NULL) {
                printf("ERR: Struct member type is unknown ...\n");
                t_struct->info.is_resolved = false;
                continue;
            }
            m->type = m_t;
            m->is_resolved = true;
        }
    }

    /* Check for cirular type dependencies */
    ArenaTmp tmp = m_arena_tmp_init(compiler->persist_arena);
    Graph graph = make_graph(tmp.arena, symt_root.struct_count);
    for (u32 i = 0; i < symt_root.type_len; i++) {
        TypeInfo *t = symt_root.types[i];
        if (t->kind == TYPE_STRUCT) {
            graph_add_struct_edges(tmp.arena, &graph, (TypeInfoStruct *)t);
        } else if (t->kind == TYPE_ARRAY) {
            // TypeInfoArray *ta = (TypeInfoArray *)t;
            // graph_add_struct_edges(tmp.arena, &graph, (TypeInfoStruct *)ta->element_type);
        }
    }

    bool found_cycle = find_cycles(tmp.arena, &graph);
    if (found_cycle) {
        printf("Error: Cycle detecetd struct types \n");
    }
    m_arena_tmp_release(tmp);


    // Print symbols
    for (u32 i = 0; i < symt_root.sym_len; i++) {
        Symbol *sym = symt_root.symbols[i];
        printf("%s", compiler->str_list.strs[sym->name].str);
        if (sym->type_info == NULL) {
            putchar('\n');
            continue;
        }
        // Print type
        // printf(" -> %d, is_resolved %d\n", sym->type_info->kind, sym->type_info->is_resolved);
        printf("\t-> ");
        type_info_print(compiler->str_list, sym->type_info);
        putchar('\n');
    }


    return (SymbolTable){ 0 };
}

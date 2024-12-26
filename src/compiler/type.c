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
#include "base/nag.h"
#include "base/nicc.h"
#include "base/sac_single.h"
#include "base/str.h"
#include "compiler.h"
#include "error.h"

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


static void *make_type_info(Arena *arena, TypeInfoKind kind, Str8 generated_by)
{
    struct type_info_properties {
        size_t size;
        bool resolved_by_default;
    };

    static struct type_info_properties type_info_table[] = {
        [TYPE_INTEGER] = { sizeof(TypeInfoInteger), true },
        [TYPE_BOOL] = { sizeof(TypeInfoBool), true },
        [TYPE_STRUCT] = { sizeof(TypeInfoStruct), false },
        [TYPE_ENUM] = { sizeof(TypeInfoEnum), true },
        [TYPE_FUNC] = { sizeof(TypeInfoFunc), false },
        [TYPE_ARRAY] = { sizeof(TypeInfoArray), false },
        [TYPE_POINTER] = { sizeof(TypeInfoPointer), false }
    };

    TypeInfo *info = m_arena_alloc(arena, type_info_table[kind].size);
    info->is_resolved = type_info_table[kind].resolved_by_default;
    info->kind = kind;
    info->generated_by = generated_by;
    return info;
}

static bool type_info_equal(TypeInfo *a, TypeInfo *b)
{
    /*
     * We use name equivalence to determine if two types are the same.
     */
    if (a->kind != b->kind) {
        return false;
    }
    if (a->kind == TYPE_ARRAY) {
        TypeInfoArray *aa = (TypeInfoArray *)a;
        TypeInfoArray *bb = (TypeInfoArray *)b;
        return aa->elements == bb->elements && type_info_equal(aa->element_type, bb->element_type);
    }
    if (a->kind == TYPE_POINTER) {
        TypeInfoPointer *aa = (TypeInfoPointer *)a;
        TypeInfoPointer *bb = (TypeInfoPointer *)b;
        if (aa->pointer_to == NULL || bb->pointer_to == NULL) {
            return true;
        }
        return aa->level_of_indirection == bb->level_of_indirection &&
               type_info_equal(aa->pointer_to, bb->pointer_to);
    }
    return STR8VIEW_EQUAL(a->generated_by, b->generated_by);
}

// TODO: we could move this into the generic TypeInfo struct
u32 type_info_bit_size(TypeInfo *type_info)
{
    switch (type_info->kind) {
    case TYPE_ARRAY: {
        TypeInfoArray *array_info = (TypeInfoArray *)type_info;
        return array_info->elements * type_info_bit_size(array_info->element_type);
    }
    case TYPE_INTEGER:
        return ((TypeInfoInteger *)type_info)->bit_size;
    case TYPE_STRUCT:
        return ((TypeInfoStruct *)type_info)->bit_size;
    case TYPE_ENUM:
    case TYPE_BOOL:
        return 4;
    case TYPE_POINTER:
    case TYPE_FUNC:
        return 8;
    default:
        assert(false && "type_info_bit_size not implemented");
    }
}

static SymbolTable symt_init(SymbolTable *parent)
{
    SymbolTable symt = { .sym_len = 0, .sym_cap = 16, .parent = parent };

    /* More sensible default for the root symbol table */
    if (parent == NULL) {
        symt.sym_cap = 64;
    }
    symt.symbols = malloc(sizeof(Symbol *) * symt.sym_cap);
    hashmap_init(&symt.map);
    return symt;
}

static bool sym_generates_type(Symbol *sym)
{
    TypeInfo *type_info = sym->type_info;
    if (type_info == NULL) {
        return false;
    }
    if (sym->kind == SYMBOL_FUNC) {
        return true;
    }
    if (sym->kind == SYMBOL_TYPE &&
        (type_info->kind == TYPE_ENUM || type_info->kind == TYPE_STRUCT)) {
        return true;
    }
    return false;
}

static Symbol *symt_new_sym(Compiler *c, SymbolTable *symt, SymbolKind sym_kind, Str8 name,
                            TypeInfo *type_info, AstNode *node)
{
    Symbol *existing_sym = hashmap_get(&symt->map, name.str, name.len);
    if (existing_sym == NULL && sym_kind == SYMBOL_LOCAL_VAR) {
        /* Local symbols can not have the same names as GLOBAL symbols */
        existing_sym = hashmap_get(&c->symt_root.map, name.str, name.len);
    }
    if (existing_sym != NULL) {
        error_sym(c->e, "Symbol already exists", name);
        /* Continue with the OG symbol, but stop compilation later */
        return existing_sym; // Continue
    }

    /* Create the new symbol */
    Symbol *sym = m_arena_alloc(c->persist_arena, sizeof(Symbol));
    sym->kind = sym_kind;
    sym->seq_no = symt->sym_len;
    sym->name = name;
    sym->type_info = type_info;
    sym->node = node;
    if (sym_generates_type(sym)) {
        sym->symt_local = symt_init(symt);
        /* Structs and enums have completely isolated scopes */
        if (type_info->kind == TYPE_STRUCT || type_info->kind == TYPE_ENUM) {
            sym->symt_local.parent = NULL;
        }
    }

    /* Ensure space in the symbol table and add the new symbol */
    if (symt->sym_len >= symt->sym_cap) {
        symt->sym_cap *= 2;
        symt->symbols = realloc(symt->symbols, sizeof(Symbol *) * symt->sym_cap);
    }
    symt->symbols[symt->sym_len] = sym;
    symt->sym_len++;

    /* If symbol generated a new type, add it to the type table */
    if (sym_generates_type(sym)) {
        // NOTE: double pointer ?
        arraylist_append(&c->all_types, &type_info);
        if (type_info->kind == TYPE_STRUCT) {
            ((TypeInfoStruct *)type_info)->struct_id = c->struct_types.size;
            arraylist_append(&c->struct_types, &type_info);
        }
    }

    hashmap_put(&symt->map, name.str, name.len, sym, sizeof(Symbol *), false);
    return sym;
}

Symbol *symt_find_sym(SymbolTable *symt, Str8 key)
{
    Symbol *sym = NULL;
    SymbolTable *symt_current = symt;
    while (sym == NULL && symt_current != NULL) {
        sym = hashmap_get(&symt_current->map, key.str, key.len);
        symt_current = symt_current->parent;
    }
    return sym;
}

static TypeInfo *ast_type_resolve(Compiler *c, AstTypeInfo ati, bool err_if_not_resolved)
{
    // NOTE: This function uses the root symbol table. Okay as of now since we don't allow
    //       declarations that generate types that aren't global.
    Str8 sym_name = ati.name;
    Symbol *sym = symt_find_sym(&c->symt_root, sym_name);
    if (sym == NULL) {
        if (err_if_not_resolved) {
            error_sym(c->e, "Type not found", sym_name);
        }
        return NULL;
    }
    /* Symbol is found, but it is not a type */
    if (sym->kind != SYMBOL_TYPE) {
        error_sym(c->e, "Wants to be used as a type, but is in fact not :-(", sym_name);
        return NULL;
    }
    assert(sym->type_info != NULL);

    // NOTE: Instead of creating new types when we see pointers and arrays, maybe we can check if
    //       they already exist and then reuse that?
    TypeInfo *type_info = sym->type_info;
    if (ati.pointer_indirection > 0) {
        TypeInfoPointer *t = make_type_info(c->persist_arena, TYPE_POINTER, sym->name);
        t->info.is_resolved = true;
        t->pointer_to = type_info;
        t->level_of_indirection = ati.pointer_indirection;
        type_info = (TypeInfo *)t;
    }
    if (ati.is_array) {
        TypeInfoArray *t = make_type_info(c->persist_arena, TYPE_ARRAY, sym->name);
        t->info.is_resolved = true;
        t->elements = ati.elements;
        t->element_type = type_info;
        type_info = (TypeInfo *)t;
    }
    return type_info;
}

static void typegen_from_enum_decl(Compiler *c, AstEnum *decl)
{
    TypeInfoEnum *t = make_type_info(c->persist_arena, TYPE_ENUM, decl->name);
    // NOTE: Could this be done better? Do we really need the member names here?
    t->member_names = m_arena_alloc(c->persist_arena, sizeof(Str8) * decl->members.len);
    t->members_len = decl->members.len;

    Symbol *sym =
        symt_new_sym(c, &c->symt_root, SYMBOL_TYPE, decl->name, (TypeInfo *)t, (AstNode *)decl);
    SymbolTable *symt_local = &sym->symt_local;
    for (u32 i = 0; i < decl->members.len; i++) {
        TypedIdent ident = decl->members.vars[i];
        t->member_names[i] = ident.name;
        symt_new_sym(c, symt_local, SYMBOL_ENUM_MEMBER, ident.name, (TypeInfo *)t, (AstNode *)decl);
    }
}

static void typegen_from_struct_decl(Compiler *c, AstStruct *decl)
{
    Arena *arena = c->persist_arena;
    TypeInfoStruct *t = make_type_info(arena, TYPE_STRUCT, decl->name);
    t->members = m_arena_alloc(arena, sizeof(TypeInfoStructMember *) * decl->members.len);
    t->members_len = decl->members.len;
    symt_new_sym(c, &c->symt_root, SYMBOL_TYPE, decl->name, (TypeInfo *)t, (AstNode *)decl);

    for (u32 i = 0; i < decl->members.len; i++) {
        TypedIdent ident = decl->members.vars[i];
        TypeInfoStructMember *member = m_arena_alloc(arena, sizeof(TypeInfoStructMember));
        t->members[i] = member;
        member->is_resolved = false;
        member->name = ident.name;
        member->ast_type_info = ident.ast_type_info;
        /* Eagerly attempt to resolve the type of the member */
        TypeInfo *member_type = ast_type_resolve(c, ident.ast_type_info, false);
        if (member_type != NULL) {
            member->is_resolved = true;
            member->type = member_type;
        }
    }
}

static void typegen_from_func_decl(Compiler *c, AstFunc *decl)
{
    TypeInfoFunc *t = make_type_info(c->persist_arena, TYPE_FUNC, decl->name);
    t->n_params = decl->parameters.len;
    t->param_names = m_arena_alloc(c->persist_arena, sizeof(Str8) * t->n_params);
    t->param_types = m_arena_alloc(c->persist_arena, sizeof(TypeInfo *) * t->n_params);
    t->info.is_resolved = true; // We will error in this function if it does not resovle
    symt_new_sym(c, &c->symt_root, SYMBOL_FUNC, decl->name, (TypeInfo *)t, (AstNode *)decl);

    TypeInfo *return_type = ast_type_resolve(c, decl->return_type, true);
    t->return_type = return_type;

    for (u32 i = 0; i < decl->parameters.len; i++) {
        TypedIdent ident = decl->parameters.vars[i];
        t->param_names[i] = ident.name;
        TypeInfo *param_type = ast_type_resolve(c, ident.ast_type_info, true);
        t->param_types[i] = param_type;
    }
}

static void bind_expr(Compiler *c, SymbolTable *symt_local, AstExpr *head)
{
    /* Creates and binds symbols to expressions */
    switch (head->kind) {
    case EXPR_UNARY:
        bind_expr(c, symt_local, AS_UNARY(head)->expr);
        break;
    case EXPR_BINARY: {
        AstBinary *expr = AS_BINARY(head);
        bind_expr(c, symt_local, expr->left);
        /* Member access has to be bound after typechecking */
        if (expr->op != TOKEN_DOT) {
            bind_expr(c, symt_local, expr->right);
        }
    } break;
    case EXPR_LITERAL: {
        AstLiteral *lit = AS_LITERAL(head);
        /* The null constant will get the null symbol */
        if (lit->lit_type == LIT_NULL) {
            lit->sym = c->sym_null;
            break;
        }
        /* Constant strings and numbers do not need symbols */
        if (lit->lit_type != LIT_IDENT) {
            break;
        }
        Symbol *sym = symt_find_sym(symt_local, lit->literal);
        lit->sym = sym;
        if (sym == NULL) {
            error_sym(c->e, "Symbol never declared", lit->literal);
        }
    } break;
    case EXPR_CALL: {
        AstCall *call = AS_CALL(head);
        if (call->args == NULL) {
            break;
        }
        AstList *args = (AstList *)call->args;
        for (AstListNode *node = args->head; node != NULL; node = node->next) {
            bind_expr(c, symt_local, (AstExpr *)node->this);
        }
    } break;
    default:
        ASSERT_NOT_REACHED;
    }
}

static void bind_stmt(Compiler *c, SymbolTable *symt_local, AstStmt *head)
{
    switch (head->kind) {
    case STMT_WHILE:
        bind_expr(c, symt_local, AS_WHILE(head)->condition);
        bind_stmt(c, symt_local, AS_WHILE(head)->body);
        break;
    case STMT_IF:
        bind_expr(c, symt_local, AS_IF(head)->condition);
        bind_stmt(c, symt_local, AS_IF(head)->then);
        if (AS_IF(head)->else_ != NULL) {
            bind_stmt(c, symt_local, AS_IF(head)->else_);
        }
        break;
    case STMT_BREAK:
    case STMT_CONTINUE:
    case STMT_RETURN:
    case STMT_EXPR: {
        AstSingle *stmt = AS_SINGLE(head);
        if (stmt->node != NULL) {
            bind_expr(c, symt_local, (AstExpr *)stmt->node);
        }
    }; break;
    case STMT_PRINT: {
        AstList *stmt = AS_LIST(head);
        for (AstListNode *node = stmt->head; node != NULL; node = node->next) {
            bind_expr(c, symt_local, (AstExpr *)node->this);
        }
    }; break;
    case STMT_BLOCK: {
        AstBlock *stmt = AS_BLOCK(head);
        /* Blocks create new scopes */
        // TODO: If there are no declarations in this scope, we don't need to create a new one?
        stmt->symt_local = m_arena_alloc(c->persist_arena, sizeof(SymbolTable));
        *stmt->symt_local = symt_init(symt_local);
        symt_local = stmt->symt_local;
        /* Create symbols for declarations */
        for (u32 i = 0; i < stmt->declarations.len; i++) {
            TypedIdent decl = stmt->declarations.vars[i];
            TypeInfo *decl_type = ast_type_resolve(c, decl.ast_type_info, true);
            symt_new_sym(c, symt_local, SYMBOL_LOCAL_VAR, decl.name, decl_type, (AstNode *)stmt);
        }
        for (AstListNode *node = stmt->stmts->head; node != NULL; node = node->next) {
            bind_stmt(c, symt_local, (AstStmt *)node->this);
        }
    }; break;
    case STMT_ASSIGNMENT:
        bind_expr(c, symt_local, AS_ASSIGNMENT(head)->left);
        bind_expr(c, symt_local, AS_ASSIGNMENT(head)->right);
        break;
    default:
        ASSERT_NOT_REACHED;
    }
}

static void bind_function(Compiler *c, AstFunc *func)
{
    Symbol *func_sym = symt_find_sym(&c->symt_root, func->name);
    assert(func_sym != NULL && "Internal Error: Could not find symbol for function !?");

    /* Create symbols for function parameters */
    for (u32 i = 0; i < func->parameters.len; i++) {
        TypedIdent param = func->parameters.vars[i];
        TypeInfo *param_t = ast_type_resolve(c, param.ast_type_info, true);
        symt_new_sym(c, &func_sym->symt_local, SYMBOL_PARAM, param.name, param_t, (AstNode *)func);
    }

    if (func->body != NULL) {
        bind_stmt(c, &func_sym->symt_local, func->body);
    }
}

static TypeInfo *typecheck_expr(Compiler *c, SymbolTable *symt_local, AstExpr *head)
{
    /* Bind the type to the expression and bubble it up */
    switch (head->kind) {
    case EXPR_UNARY: {
        AstUnary *expr = AS_UNARY(head);
        TypeInfo *t = typecheck_expr(c, symt_local, expr->expr);
        if (expr->op == TOKEN_AMPERSAND) {
            TypeInfoPointer *tp = make_type_info(c->persist_arena, TYPE_POINTER, t->generated_by);
            tp->info.is_resolved = true;
            tp->pointer_to = t;
            tp->level_of_indirection = 1;
            if (t->kind == TYPE_POINTER) {
                tp->level_of_indirection += ((TypeInfoPointer *)t)->level_of_indirection;
            }
            head->type = (TypeInfo *)tp;
        } else if (expr->op == TOKEN_STAR) {
            if (t->kind != TYPE_POINTER) {
                error_node(c->e, "Can not dereference x", (AstNode *)head);
            } else {
                head->type = ((TypeInfoPointer *)t)->pointer_to;
            }
        } else {
            head->type = t;
        }
    }; break;
    case EXPR_BINARY: {
        AstBinary *expr = AS_BINARY(head);
        TypeInfo *left = typecheck_expr(c, symt_local, expr->left);

        /* Member access */
        if (expr->op == TOKEN_DOT) {
            if (left->kind == TYPE_POINTER) {
                left = ((TypeInfoPointer *)left)->pointer_to;
            }
            if (!(left->kind == TYPE_STRUCT || left->kind == TYPE_ENUM)) {
                error_typecheck_binary(c->e, "Has no members", (AstNode *)head, left, left);
                head->type = left;
                break;
            }
            /*
             * For member accesses, the RHS could not be bound before we knew the type of the LHS,
             * meaning we have to bind it now. If bind_expr fails, an error is generated. We don't
             * need to check the type of the RHS since struct and enum scopes are isolated, meaning
             * if bind_expr succeeds, it has been bound to the correct type, and we shall return
             * this type.
             */
            Symbol *type_sym = symt_find_sym(symt_local, left->generated_by);
            bind_expr(c, &type_sym->symt_local, expr->right);
            head->type = typecheck_expr(c, &type_sym->symt_local, expr->right);
            break;
        }

        /* Regular binary operator */
        TypeInfo *right = typecheck_expr(c, symt_local, expr->right);
        if (!type_info_equal(left, right)) {
            error_typecheck_binary(c->e, "bin", (AstNode *)head, left, right);
        }

        // TODO: some binary ops have a limited number of types that are allowed
        //       f.ex. we don't allow addition of structs
        head->type = left;
    } break;
    case EXPR_LITERAL: {
        AstLiteral *lit = AS_LITERAL(head);
        if (lit->lit_type == LIT_IDENT || lit->lit_type == LIT_NULL) {
            head->type = lit->sym->type_info;
        } else {
            // TODO: temporary assumption that every constant literal that is not an ident is a s32
            Symbol *sym = symt_find_sym(symt_local, (Str8){ .len = 3, .str = (u8 *)"s32" });
            head->type = sym->type_info;
        }
    } break;
    case EXPR_CALL: {
        AstCall *call = AS_CALL(head);
        Symbol *sym = symt_find_sym(symt_local, call->identifier);
        TypeInfoFunc *callee = (TypeInfoFunc *)sym->type_info;
        if (call->args == NULL && callee->n_params == 0) {
            head->type = callee->return_type;
            break;
        }

        /* Check that enough args were supplied */
        if (call->args == NULL && callee->n_params != 0) {
            error_node(c->e, "Expected n args, but got 0", (AstNode *)call);
            head->type = callee->return_type;
            break;
        }

        /* If args is a list */
        AstList *args = (AstList *)call->args;
        u32 n_args = 0;
        for (AstListNode *node = args->head; node != NULL; node = node->next) {
            n_args += 1;
        }
        if (n_args != callee->n_params) {
            error_node(c->e, "Expected x args, but got y", (AstNode *)head);
            head->type = callee->return_type;
            break;
        }
        /* Typecheck params vs args */
        u32 i = 0;
        for (AstListNode *node = args->head; node != NULL; node = node->next) {
            TypeInfo *t_arg = typecheck_expr(c, symt_local, (AstExpr *)node->this);
            TypeInfo *t_param = callee->param_types[i];
            if (!type_info_equal(t_arg, t_param)) {
                error_typecheck_binary(c->e, "Argument mismatch", (AstNode *)head, t_arg, t_param);
            }
            i += 1;
        }
        head->type = callee->return_type;
    } break;
    default:
        ASSERT_NOT_REACHED;
    }
    return head->type;
}

static void typecheck_stmt(Compiler *c, SymbolTable *symt_local, TypeInfoFunc *parent_func,
                           AstStmt *head)
{
    switch (head->kind) {
    case STMT_WHILE:
        typecheck_expr(c, symt_local, AS_WHILE(head)->condition);
        typecheck_stmt(c, symt_local, parent_func, AS_WHILE(head)->body);
        break;
    case STMT_IF:
        typecheck_expr(c, symt_local, AS_IF(head)->condition);
        typecheck_stmt(c, symt_local, parent_func, AS_IF(head)->then);
        if (AS_IF(head)->else_) {
            typecheck_stmt(c, symt_local, parent_func, AS_IF(head)->else_);
        }
        break;
    case STMT_RETURN: {
        TypeInfo *ret = typecheck_expr(c, symt_local, AS_IF(head)->condition);
        if (!type_info_equal(ret, parent_func->return_type)) {
            error_typecheck_binary(c->e, "wrong return type", (AstNode *)head, ret,
                                   parent_func->return_type);
        }
    }; break;
    case STMT_EXPR: {
        typecheck_expr(c, symt_local, (AstExpr *)AS_SINGLE(head)->node);
    }; break;
    case STMT_PRINT: {
        AstList *stmt = AS_LIST(head);
        for (AstListNode *node = stmt->head; node != NULL; node = node->next) {
            typecheck_expr(c, symt_local, (AstExpr *)node->this);
        }
    }; break;
    case STMT_BLOCK: {
        AstBlock *stmt = AS_BLOCK(head);
        for (AstListNode *node = stmt->stmts->head; node != NULL; node = node->next) {
            typecheck_stmt(c, stmt->symt_local, parent_func, (AstStmt *)node->this);
        }
    }; break;
    case STMT_ASSIGNMENT: {
        TypeInfo *l = typecheck_expr(c, symt_local, AS_ASSIGNMENT(head)->left);
        TypeInfo *r = typecheck_expr(c, symt_local, AS_ASSIGNMENT(head)->right);
        // TODO: enums type can not be the LHS of an assignment
        if (!type_info_equal(l, r)) {
            error_typecheck_binary(c->e, "Typecheck error in assignment", (AstNode *)head, l, r);
        }
    } break;
    case STMT_BREAK:
    case STMT_CONTINUE:
        break;
    default:
        ASSERT_NOT_REACHED;
    }
}

/* --- PRINT --- */
static void symt_print(SymbolTable symt)
{
    for (u32 i = 0; i < symt.sym_len; i++) {
        Symbol *sym = symt.symbols[i];
        TypeInfo *sym_type = sym->type_info;
        if (sym_type == NULL) {
            printf("[T%d:no%d] - %s\n", sym->kind, sym->seq_no, sym->name.str);
        } else {
            bool is_ptr = sym_type->kind == TYPE_POINTER;
            bool is_array = false;
            if (sym_type->kind == TYPE_ARRAY) {
                is_array = true;
                is_ptr = ((TypeInfoArray *)sym_type)->element_type->kind == TYPE_POINTER;
            }
            printf("[T%d:no%d] - %s: %s%s%s\n", sym->kind, sym->seq_no, sym->name.str,
                   is_ptr ? "^" : "", sym_type->generated_by.str, is_array ? "[]" : "");
        }
        if (sym_generates_type(sym)) {
            printf("local syms:\n");
            symt_print(sym->symt_local);
            putchar('\n');
        }
    }
}

static void graph_add_edges_from_struct_type(Arena *pass_arena, NAG_Graph *graph, TypeInfoStruct *t)
{
    NAG_Idx *added_list = m_arena_alloc_zero(pass_arena, sizeof(NAG_Idx) * t->members_len);
    NAG_Idx added_count = 0;

    for (NAG_Idx i = 0; i < t->members_len; i++) {
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

        NAG_Idx from = t->struct_id;
        NAG_Idx to = member_type->struct_id;

        /* Do not add duplicate edges */
        bool duplicate = false;
        for (NAG_Idx j = 0; j < added_count; j++) {
            if (added_list[j] == to) {
                duplicate = true;
                break;
            }
        }
        if (!duplicate) {
            nag_add_edge(graph, from, to);
            added_list[added_count++] = to;
        }
    }
}

static void add_builtin_integral_type(Compiler *c, bool is_signed, u32 bit_size)
{
    Str8Builder sb = make_str_builder(c->persist_arena);
    str_builder_append_u8(&sb, is_signed ? 's' : 'u');
    str_builder_sprintf(&sb, "%d", 1, bit_size);
    Str8 name = str_builder_end(&sb, true);
    TypeInfoInteger *T = make_type_info(c->persist_arena, TYPE_INTEGER, name);
    T->is_signed = is_signed;
    T->bit_size = bit_size;
    symt_new_sym(c, &c->symt_root, SYMBOL_TYPE, name, (TypeInfo *)T, NULL);
}

static void fill_builtin_types(Compiler *c)
{
    /* Integers */
    // add_builtin_integral_type(c, true, 8);
    // add_builtin_integral_type(c, true, 16);
    add_builtin_integral_type(c, true, 32);
    // add_builtin_integral_type(c, true, 64);
    // add_builtin_integral_type(c, false, 8);
    // add_builtin_integral_type(c, false, 16);
    // add_builtin_integral_type(c, false, 32);
    // add_builtin_integral_type(c, false, 64);

    /* Bool */
    Str8Builder sb = make_str_builder(c->persist_arena);
    str_builder_append_cstr(&sb, "bool", 4);
    Str8 name = str_builder_end(&sb, true);
    TypeInfoBool *bool_builtin = make_type_info(c->persist_arena, TYPE_BOOL, name);
    bool_builtin->info.is_resolved = true;
    symt_new_sym(c, &c->symt_root, SYMBOL_TYPE, name, (TypeInfo *)bool_builtin, NULL);
}

static void resolve_global_types(Compiler *c)
{
    /*
     * Right now we frst create types for enums, structs and then functions.
     * Enums do not reference other types, so they already resolved. Function types are
     * created after structs, so they should be wholly resolved. Structs however may need
     * to be resolved.
     */
    for (u32 i = 0; i < c->all_types.size; i++) {
        TypeInfo *t = *(TypeInfo **)arraylist_get(&c->all_types, i);
        if (t->is_resolved) {
            continue;
        }
        if (t->kind != TYPE_STRUCT) {
            error_sym(c->e, "A type used here is not declared", t->generated_by);
            continue;
        }
        TypeInfoStruct *t_struct = (TypeInfoStruct *)t;
        t_struct->info.is_resolved = true;
        for (u32 j = 0; j < t_struct->members_len; j++) {
            TypeInfoStructMember *m = t_struct->members[j];
            if (m->is_resolved) {
                continue;
            }
            TypeInfo *member_type = ast_type_resolve(c, m->ast_type_info, false);
            if (member_type == NULL) {
                error_sym(c->e, "Type does not exist", m->ast_type_info.name);
                t_struct->info.is_resolved = false;
                continue;
            }
            m->type = member_type;
            m->is_resolved = true;
        }
        /* Don't bother creating a symbol if we could not resolve the type */
        if (t_struct->info.is_resolved == false) {
            continue;
        }
        /* Generate symbols for struct members */
        Symbol *struct_sym = symt_find_sym(&c->symt_root, t_struct->info.generated_by);
        assert(struct_sym && struct_sym->type_info && struct_sym->type_info->kind == TYPE_STRUCT);
        for (u32 j = 0; j < t_struct->members_len; j++) {
            TypeInfoStructMember *m = t_struct->members[j];
            // TODO: Attach the correct AST node
            symt_new_sym(c, &struct_sym->symt_local, SYMBOL_ENUM_MEMBER, m->name, m->type, NULL);
        }
    }
}

void typegen(Compiler *c, AstRoot *root)
{
    c->symt_root = symt_init(NULL);

    /* Fill symbol table with builtin types */
    fill_builtin_types(c);

    /* Create symbols and types for global declarations */
    for (AstListNode *node = root->enums.head; node != NULL; node = node->next) {
        typegen_from_enum_decl(c, AS_ENUM(node->this));
    }
    for (AstListNode *node = root->structs.head; node != NULL; node = node->next) {
        typegen_from_struct_decl(c, AS_STRUCT(node->this));
    }
    for (AstListNode *node = root->funcs.head; node != NULL; node = node->next) {
        typegen_from_func_decl(c, AS_FUNC(node->this));
    }
    for (AstListNode *node = root->vars.head; node != NULL; node = node->next) {
        TypedIdentList global_vars = AS_TYPED_IDENT_LIST(node->this)->idents;
        for (u32 i = 0; i < global_vars.len; i++) {
            TypedIdent v = global_vars.vars[i];
            TypeInfo *t = ast_type_resolve(c, v.ast_type_info, true);
            symt_new_sym(c, &c->symt_root, SYMBOL_GLOBAL_VAR, v.name, (TypeInfo *)t, node->this);
        }
    }

    /* Resolve all types for the global declarations */
    resolve_global_types(c);
    /* Early return if any types are unresolved */
    if (c->e->n_errors != 0) {
        return;
    }

    /* Check for cirular type dependencies */
    ArenaTmp persist_arena_tmp = m_arena_tmp_init(c->persist_arena);

    NAG_Graph graph =
        nag_make_graph(c->persist_arena, c->pass_arena, (NAG_Idx)c->struct_types.size);
    for (NAG_Idx i = 0; i < (NAG_Idx)c->struct_types.size; i++) {
        TypeInfoStruct **t = arraylist_get(&c->struct_types, i);
        graph_add_edges_from_struct_type(c->pass_arena, &graph, *t);
    }

    NAG_OrderList sccs = nag_scc(&graph);
    /*
     * Give appropritate error message for strongly connected component found
     * NOTE: nag_scc() only return sccs > 1, which is the behavior we want.
     */
    for (u32 i = 0; i < sccs.n; i++) {
        NAG_Order scc = sccs.orders[i];
        Str8Builder sb = make_str_builder(&c->e->arena);
        str_builder_append_str8(&sb, STR8_LIT("Circular dependency between structs: "));
        str_builder_sprintf(&sb, "%d", 1, scc.nodes[0]);
        for (u32 j = 0; j < scc.n_nodes; j++) {
            str_builder_sprintf(&sb, " <- %d", 1, scc.nodes[j]);
        }
        Str8 error_msg = str_builder_end(&sb, true);
        error_msg_str8(c->e, error_msg);
    }
    free(sccs.orders);

    /*
     * Reversed topological sort over the custom types so we know what order to generate structs and
     * infer struct sizes.
     * NOTE: Actually, Trajan's algorithm used in nag_scc does topological sorting as a by-product,
     *       so using that would save some compute.
     */
    NAG_Order rev_toposort = nag_rev_toposort(&graph);
    ArrayList structs_sorted;
    arraylist_init(&structs_sorted, sizeof(TypeInfoStruct *));
    for (u32 i = 0; i < rev_toposort.n_nodes; i++) {
        TypeInfoStruct **s = arraylist_get(&c->struct_types, rev_toposort.nodes[i]);
        arraylist_append(&structs_sorted, s);
    }
    arraylist_free(&c->struct_types);
    c->struct_types = structs_sorted;

    /* Calculate the size of each struct */
    for (u32 i = 0; i < c->struct_types.size; i++) {
        TypeInfoStruct *s = *(TypeInfoStruct **)arraylist_get(&c->struct_types, i);
        s->bit_size = 0;
        for (u32 j = 0; j < s->members_len; j++) {
            TypeInfoStructMember *member = s->members[j];
            member->offset = s->bit_size; // TODO: align?
            s->bit_size += type_info_bit_size(member->type);
        }
    }

    m_arena_tmp_release(persist_arena_tmp);
}

void infer(Compiler *c, AstRoot *root)
{
    /* Create the symbol for the null pointer */
    Str8Builder sb = make_str_builder(c->persist_arena);
    str_builder_append_cstr(&sb, "null", 4);
    Str8 name = str_builder_end(&sb, true);
    TypeInfoPointer *t = make_type_info(c->persist_arena, TYPE_POINTER, name);
    t->info.is_resolved = true;
    t->pointer_to = NULL;
    c->sym_null = symt_new_sym(c, &c->symt_root, SYMBOL_NULL_PTR, name, (TypeInfo *)t, NULL);

    /* Bind symbols */
    for (AstListNode *node = root->funcs.head; node != NULL; node = node->next) {
        bind_function(c, AS_FUNC(node->this));
    }
}

void typecheck(Compiler *c, AstRoot *root)
{
    /* Typecheck each function */
    for (AstListNode *node = root->funcs.head; node != NULL; node = node->next) {
        AstFunc *func = AS_FUNC(node->this);
        Symbol *func_sym = symt_find_sym(&c->symt_root, func->name);
        assert(func_sym != NULL && "Could not find symbol for function in bind_and_check!?!?");
        if (func->body != NULL) {
            typecheck_stmt(c, &func_sym->symt_local, (TypeInfoFunc *)func_sym->type_info,
                           func->body);
        }
    }
    // symt_print(c->symt_root);
}

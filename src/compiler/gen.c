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

#include "ast.h"
#include "base/str.h"
#include "compiler.h"
#include "error.h"
#include "type.h"

#include <stdio.h>

FILE *f = NULL;

static void write_base(void)
{
    char *base = "/*\n"
                 " * --- File generated by metagen compiler ---\n"
                 " * 2024 Nicolai Brand (https://lytix.dev)\n"
                 " */\n"
                 "#include <stdbool.h>\n"
                 "#include <stdint.h>\n"
                 "#include <stdio.h>\n"
                 "#include <stdlib.h> // for size_t and ssize_t\n\n"
                 "typedef int8_t s8;\n"
                 "typedef int16_t s16;\n"
                 "typedef int32_t s32;\n"
                 "typedef int64_t s64;\n"
                 "typedef uint8_t u8;\n"
                 "typedef uint16_t u16;\n"
                 "typedef uint32_t u32;\n"
                 "typedef uint64_t u64;\n"
                 "typedef float f32;\n"
                 "typedef double f64;\n";

    fprintf(f, "%s", base);
}

static void write_newline_and_indent(u32 indent)
{
    fprintf(f, "\n");
    for (u32 i = 0; i < indent; i++) {
        fprintf(f, " ");
    }
}

static Str8 type_info_to_c_type_name(Compiler *compiler, TypeInfo *t)
{
    Str8Builder sb = make_str_builder(compiler->persist_arena); // TODO: use pass arena

    s32 pointer_indirection = 0;
    if (t->kind == TYPE_POINTER) {
        pointer_indirection = ((TypeInfoPointer *)t)->level_of_indirection;
    }
    u32 arr_elements = 0;
    if (t->kind == TYPE_ARRAY) {
        TypeInfoArray *type_as_array = (TypeInfoArray *)t;
        if (type_as_array->element_type->kind == TYPE_POINTER) {
            pointer_indirection = ((TypeInfoPointer *)type_as_array)->level_of_indirection;
        }
        arr_elements = type_as_array->elements;
    }

    if (t->kind == TYPE_ENUM) {
        str_builder_append_cstr(&sb, "enum ", 5);
    }
    str_builder_append_cstr(&sb, (char *)t->generated_by.str, t->generated_by.len);
    if (t->kind == TYPE_ARRAY) {
        str_builder_sprintf(&sb, "[%d]", 1, arr_elements);
    }
    if (pointer_indirection > 0) {
        str_builder_append_u8(&sb, ' ');
        for (s32 i = 0; i < pointer_indirection; i++) {
            str_builder_append_u8(&sb, '*');
        }
    }
    return str_builder_end(&sb, false);
}

static u8 type_info_to_printf_format(TypeInfo *t)
{
    switch (t->kind) {
    case TYPE_ENUM:
    case TYPE_BOOL:
    case TYPE_INTEGER:
        return 'd';
    default:
        return '?';
    }
}

static void gen_enum(Compiler *compiler, Symbol *sym)
{
    fprintf(f, "enum %s {\n", sym->name.str);

    TypeInfoEnum *t = (TypeInfoEnum *)sym->type_info;
    for (u32 i = 0; i < t->members_len; i++) {
        Str8 member_name = t->member_names[i];
        if (i == 0) {
            fprintf(f, "\t%s_%s = 0,\n", sym->name.str, member_name.str);
        } else {
            fprintf(f, "\t%s_%s,\n", sym->name.str, member_name.str);
        }
    }

    fprintf(f, "};\n");
}

static void gen_struct(Compiler *compiler, Symbol *sym)
{
    fprintf(f, "struct %s_t {\n", sym->name.str);

    TypeInfoStruct *t = (TypeInfoStruct *)sym->type_info;
    for (u32 i = 0; i < t->members_len; i++) {
        TypeInfoStructMember *m = t->members[i];
        TypeInfo *m_t = m->type;
        char *ptr_modifier = m_t->kind == TYPE_POINTER ? "*" : "";
        if (m_t->kind == TYPE_ARRAY) {
            TypeInfoArray *type_as_array = (TypeInfoArray *)m_t;
            if (type_as_array->element_type->kind == TYPE_POINTER) {
                ptr_modifier = "*";
            }
            fprintf(f, "\t%s %s%s[%d];\n", m_t->generated_by.str, ptr_modifier, m->name.str,
                    type_as_array->elements);
        } else {
            fprintf(f, "\t%s %s%s;\n", m_t->generated_by.str, ptr_modifier, m->name.str);
        }
    }

    fprintf(f, "};\n");
}

static void gen_expr(Compiler *compiler, AstExpr *head)
{
    switch (head->kind) {
    // case EXPR_UNARY:
    //     bind_expr(compiler, symt_local, AS_UNARY(head)->expr);
    //     break;
    case EXPR_BINARY: {
        AstBinary *expr = AS_BINARY(head);

        if (expr->op == TOKEN_DOT && expr->type->kind == TYPE_ENUM) {
            TypeInfoEnum *t = (TypeInfoEnum *)expr->type;
            fprintf(f, "%s_", t->info.generated_by.str);
            gen_expr(compiler, expr->right);
            break;
        }

        fprintf(f, "(");
        if (expr->left->type->kind == TYPE_POINTER && expr->op == TOKEN_DOT) {
            fprintf(f, "*");
        }
        gen_expr(compiler, expr->left);
        fprintf(f, ")");

        switch (expr->op) {
        case TOKEN_PLUS:
            fprintf(f, " + ");
            break;
        case TOKEN_MINUS:
            fprintf(f, " - ");
            break;
        case TOKEN_STAR:
            fprintf(f, " * ");
            break;
        case TOKEN_SLASH:
            fprintf(f, " / ");
            break;
        case TOKEN_LSHIFT:
            fprintf(f, " << ");
            break;
        case TOKEN_RSHIFT:
            fprintf(f, " >> ");
            break;
        case TOKEN_DOT:
            fprintf(f, ".");
            break;
        case TOKEN_EQ:
            fprintf(f, "==");
            break;
        case TOKEN_NEQ:
            fprintf(f, "!=");
            break;
        case TOKEN_LESS:
            fprintf(f, "<");
            break;
        case TOKEN_GREATER:
            fprintf(f, "<");
            break;
        default:
            ASSERT_NOT_REACHED;
        }
        gen_expr(compiler, expr->right);
        // /* Member access are bound in after typechecking */
        // if (expr->op != TOKEN_DOT) {
        //     bind_expr(compiler, expr->right);
        // }
    } break;
    case EXPR_LITERAL: {
        AstLiteral *lit = AS_LITERAL(head);
        if (lit->lit_type == LIT_NUM) {
            bool success;
            u32 num = str_view_to_u32(lit->literal, &success);
            if (!success) {
                error_node(compiler->e, "Could not convert number literal to u32", (AstNode *)head);
                fprintf(f, "RIP");
            } else {
                fprintf(f, "%d", num);
            }
        } else {
            fprintf(f, "%s", lit->literal.str);
        }
    } break;
    case EXPR_CALL: {
        AstCall *call = AS_CALL(head);
        fprintf(f, "%s(", call->identifier.str);
        if (call->args != NULL) {
            if ((u32)call->args->kind == (u32)EXPR_LITERAL) {
                gen_expr(compiler, (AstExpr *)call->args);
            } else {
                AstList *args = (AstList *)call->args;
                for (AstListNode *node = args->head; node != NULL; node = node->next) {
                    gen_expr(compiler, (AstExpr *)node->this);
                    if (node->next != NULL) {
                        fprintf(f, ", ");
                    }
                }
            }
        }
        fprintf(f, ")");
    } break;
    default:
        (void)0;
        // ASSERT_NOT_REACHED;
    }
}


static void gen_stmt(Compiler *compiler, AstStmt *head, u32 indent)
{
    write_newline_and_indent(indent);
    switch (head->kind) {
    case STMT_WHILE: {
        AstWhile *stmt = AS_WHILE(head);
        fprintf(f, "while (");
        gen_expr(compiler, stmt->condition);
        fprintf(f, ")");
        gen_stmt(compiler, stmt->body, indent + 2);
    }; break;
    case STMT_IF: {
        AstIf *stmt = AS_IF(head);
        fprintf(f, "if (");
        gen_expr(compiler, stmt->condition);
        fprintf(f, ") {");
        gen_stmt(compiler, stmt->then, indent + 2);
        if (stmt->else_) {
            write_newline_and_indent(indent);
            fprintf(f, "} else ");
            gen_stmt(compiler, stmt->else_, indent + 2);
        }
        write_newline_and_indent(indent);
        fprintf(f, "}");
    } break;
    // case STMT_ABRUPT_BREAK:
    // case STMT_ABRUPT_CONTINUE:
    case STMT_RETURN: {
        AstSingle *stmt = AS_SINGLE(head);
        fprintf(f, "return ");
        gen_expr(compiler, (AstExpr *)stmt->node);
        fprintf(f, ";");
    }; break;
    // case STMT_EXPR: {
    //     AstStmtSingle *stmt = AS_SINGLE(head);
    //     if (stmt->node) {
    //         bind_expr(compiler, (AstExpr *)stmt->node);
    //     }
    // }; break;
    case STMT_PRINT: {
        AstList *stmt = AS_LIST(head);
        Str8Builder sb = make_str_builder(compiler->persist_arena); // TODO: pass arena
        /* Build printf format */
        for (AstListNode *node = stmt->head; node != NULL; node = node->next) {
            str_builder_append_u8(&sb, '%');
            str_builder_append_u8(&sb, type_info_to_printf_format(((AstExpr *)node->this)->type));
            if (node->next != NULL) {
                str_builder_append_u8(&sb, ',');
                str_builder_append_u8(&sb, ' ');
            }
        }
        Str8 fmt = str_builder_end(&sb, false);

        fprintf(f, "printf(\"%s\\n\", ", fmt.str);

        for (AstListNode *node = stmt->head; node != NULL; node = node->next) {
            gen_expr(compiler, (AstExpr *)node->this);
            if (node->next != NULL) {
                fprintf(f, ", ");
            }
        }
        fprintf(f, ");");
    }; break;
    case STMT_BLOCK: {
        AstBlock *stmt = AS_BLOCK(head);

        write_newline_and_indent(indent);
        fprintf(f, "{");
        indent += 2;
        write_newline_and_indent(indent);

        /* Declarations */
        for (u32 i = 0; i < stmt->declarations.len; i++) {
            TypedIdent decl = stmt->declarations.vars[i];
            Symbol *sym = symt_find_sym(stmt->symt_local, decl.name);
            Str8 type_name = type_info_to_c_type_name(compiler, sym->type_info);
            /*
             * var x: s32, y: pair
             * s32 x;
             */
            fprintf(f, "%s %s;", type_name.str, sym->name.str);
            write_newline_and_indent(indent);
        }
        /* Statement */
        for (AstListNode *node = stmt->stmts->head; node != NULL; node = node->next) {
            gen_stmt(compiler, (AstStmt *)node->this, indent);
        }

        indent -= 2;
        write_newline_and_indent(indent);
        fprintf(f, "}");
    }; break;
    case STMT_ASSIGNMENT: {
        AstAssignment *stmt = AS_ASSIGNMENT(head);
        gen_expr(compiler, stmt->left);
        fprintf(f, " = ");
        gen_expr(compiler, stmt->right);
        fprintf(f, ";");
    }; break;
    default:
        (void)0;
        // ASSERT_NOT_REACHED;
    }
}

static void gen_func(Compiler *compiler, Symbol *sym)
{
    /* Generate function head */
    TypeInfoFunc *t = (TypeInfoFunc *)sym->type_info;
    Str8 return_type_name = type_info_to_c_type_name(compiler, t->return_type);
    fprintf(f, "%s %s(", return_type_name.str, sym->name.str);

    for (u32 i = 0; i < t->n_params; i++) {
        Str8 param_type_name = type_info_to_c_type_name(compiler, t->param_types[i]);
        Str8 param_name = t->param_names[i];
        fprintf(f, "%s %s", param_type_name.str, param_name.str);
        if (i != t->n_params - 1) {
            fprintf(f, ", ");
        }
    }
    if (t->n_params == 0) {
        fprintf(f, "void");
    }

    fprintf(f, ")\n{");

    /* Generate function body */
    AstFunc *func_node = (AstFunc *)sym->node;
    gen_stmt(compiler, func_node->body, 2);

    fprintf(f, "\n}");
}


/*
 * Syntax-directed translation to C
 */
void transpile_to_c(Compiler *compiler)
{
    // gloabl symbols !
    // enums
    // structs
    // globals
    // functions

    f = fopen("out.c", "w");
    if (f == NULL) {
        Str8 msg = STR8_LIT("Could not open file out.c");
        error_msg_str8(compiler->e, msg);
        return;
    }

    /* Prelude */
    write_base();
    fprintf(f, "\n\n");

    SymbolTable *symt_root = &compiler->symt_root;

    /* Generate enums */
    for (u32 i = 0; i < symt_root->sym_len; i++) {
        Symbol *sym = symt_root->symbols[i];
        if (sym->kind == SYMBOL_TYPE) {
            if (sym->type_info->kind == TYPE_ENUM) {
                gen_enum(compiler, sym);
            }
        }
    }

    fprintf(f, "\n");

    /* Forward declare structs */
    for (u32 i = 0; i < symt_root->sym_len; i++) {
        Symbol *sym = symt_root->symbols[i];
        if (sym->kind == SYMBOL_TYPE) {
            if (sym->type_info->kind == TYPE_STRUCT) {
                fprintf(f, "typedef struct %s_t %s;\n", sym->name.str, sym->name.str);
            }
        }
    }

    fprintf(f, "\n");

    /* Generate structs */
    for (u32 i = 0; i < symt_root->sym_len; i++) {
        Symbol *sym = symt_root->symbols[i];
        if (sym->kind == SYMBOL_TYPE) {
            if (sym->type_info->kind == TYPE_STRUCT) {
                gen_struct(compiler, sym);
            }
        }
    }

    fprintf(f, "\n");

    /* Generate functions */
    for (u32 i = 0; i < symt_root->sym_len; i++) {
        Symbol *sym = symt_root->symbols[i];
        if (sym->kind == SYMBOL_FUNC) {
            gen_func(compiler, sym);
            fprintf(f, "\n\n");
        }
    }

    fprintf(f, "\n");
    fclose(f);
}

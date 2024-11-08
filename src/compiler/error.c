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

#include "error.h"
#include "ast.h"
#include "base/sac_single.h"
#include "base/str.h"
#include "lex.h"
#include <string.h>

void error_handler_init(ErrorHandler *e, char *input, char *file_name)
{
    m_arena_init_dynamic(&e->arena, 1, 100);
    e->input = input;
    e->file_name = file_name;
    e->n_errors = 0;
    e->head = NULL;
    e->tail = NULL;
}

void error_handler_release(ErrorHandler *e)
{
    m_arena_release(&e->arena);
}

void error_handler_reset(ErrorHandler *e)
{
    m_arena_clear(&e->arena);
    e->n_errors = 0;
    e->head = NULL;
    e->tail = NULL;
}


static void append_err(ErrorHandler *e, Str8 msg)
{
    CompilerError *error = m_arena_alloc(&e->arena, sizeof(CompilerError));
    error->next = NULL;
    error->msg = msg;
    if (e->head == NULL) {
        e->head = error;
    } else {
        e->tail->next = error;
    }
    e->tail = error;
    e->n_errors += 1;
}

void error_msg_str8(ErrorHandler *e, Str8 msg)
{
    append_err(e, msg);
}

void error_lex(ErrorHandler *e, char *msg, Point start, Point end)
{
    Str8Builder sb = make_str_builder(&e->arena);
    str_builder_sprintf(&sb, "[%s @ line %d] ", 2, e->file_name, end.l);
    str_builder_append_cstr(&sb, msg, strlen(msg));
    Str8 str = str_builder_end(&sb, false);
    append_err(e, str);
}

void error_parse(ErrorHandler *e, char *msg, Token guilty)
{
    Str8Builder sb = make_str_builder(&e->arena);
    str_builder_sprintf(&sb, "[%s @ line %d] ", 2, e->file_name, guilty.end);
    str_builder_append_cstr(&sb, msg, strlen(msg));
    Str8 str = str_builder_end(&sb, false);
    append_err(e, str);
}


void error_node(ErrorHandler *e, char *msg, AstNode *guilty)
{
    Str8Builder sb = make_str_builder(&e->arena);
    str_builder_sprintf(&sb, "[%s @ line %d] ", 2, e->file_name, -1);
    str_builder_append_cstr(&sb, msg, strlen(msg));
    Str8 str = str_builder_end(&sb, false);
    append_err(e, str);
}

void error_sym(ErrorHandler *e, char *msg, Str8 name)
{
    // TODO: print occurence of this type
    Str8Builder sb = make_str_builder(&e->arena);
    str_builder_sprintf(&sb, "[%s:%d] ", 2, e->file_name, -1);
    str_builder_append_cstr(&sb, (char *)name.str, name.len);
    str_builder_append_u8(&sb, ':');
    str_builder_append_u8(&sb, ' ');
    str_builder_append_cstr(&sb, msg, strlen(msg));
    Str8 str = str_builder_end(&sb, false);
    append_err(e, str);
}

void error_typecheck_binary(ErrorHandler *e, char *msg, AstNode *guilty, TypeInfo *l, TypeInfo *r)
{
    error_node(e, msg, guilty);
}

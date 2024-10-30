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
#ifndef SYMBOL_H
#define SYMBOL_H

#include "ast.h"
#include "base/nicc.h"
#include "compiler.h"

typedef enum {
    TYPE_INTEGER = 0,
    TYPE_BOOL,
    TYPE_STRUCT,
    TYPE_ARRAY,
    TYPE_FUNC,
    TYPE_INFO_KIND_LEN
} TypeInfoKind;

typedef struct {
    TypeInfoKind kind;
    u32 generated_by_name; // Not used by TYPE_ARRAY
    bool is_resolved;
} TypeInfo;

typedef struct {
    TypeInfo info;
    u32 size;
    bool is_signed;
} TypeInfoInteger;

typedef struct {
    TypeInfo info;
} TypeInfoBool;

// NOTE: Not an actual TypeInfo kind
typedef struct {
    bool is_resolved;
    u32 name; // Index into the string list
    u32 member_offset;
    union {
        TypeInfo *type;
        u32 type_name;
    };
} TypeInfoStructMember;

typedef struct {
    TypeInfo info;
    u32 members_len;
    TypeInfoStructMember **members;
} TypeInfoStruct;

typedef struct {
    TypeInfo info;
    TypeInfo *element_type;
    s32 elements; // -1 then array is dynamic
} TypeInfoArray;

typedef struct {
    TypeInfo info;
    u32 n_params;
    u32 *param_names;
    TypeInfo **param_types;
    TypeInfo *return_type;
} TypeInfoFunc;


/* Symbol tuff */

typedef enum {
    // SYMBOL_GLOBAL_ARRAY,
    SYMBOL_TYPE,
    SYMBOL_FUNC,
    SYMBOL_GLOBAL_VAR,
    SYMBOL_LOCAL_VAR,
    SYMBOL_PARAM,

    SYMBOL_TYPE_LEN,
} SymbolKind;


typedef struct symbol_table_t SymbolTable;
typedef struct symbol_t Symbol;

struct symbol_table_t {
    Symbol **symbols;
    u32 sym_len;
    u32 sym_cap;

    TypeInfo **types;
    u32 type_len;
    u32 type_cap;

    HashMap map; // Key: string (u8 *), Value: *Symbol
    SymbolTable *parent; // @NULLABLE
};

struct symbol_t {
    SymbolKind kind;
    u32 name;
    TypeInfo *type_info; // @NULLABLE
    AstNode *node; // @NULLABLE. Node which defined this symbol. If NULL then defined by compiler.
    SymbolTable function_symtable; // Only used when node is AST_FUNC
};



void symbol_table_init(SymbolTable *table, HashMap *parent);

SymbolTable symbol_generate(Compiler *compiler, AstRoot *root);


/*
 * 1. Create symbol table
 * 2. Add all default types (s32, f32, ...) as types and symbols
 * 3. Find all global symbols, mostly interested in the types here.
 *    While doing this, check for collisions and report any potential errors.
 *
 * 4. Resolve all types. Check for circular dependencies in struct.
 *
 *
 * 5. Iteratively create new low-level AST:
 *     >Get rid of token information
 *     >Bind correct symbols to the nodes.
 *    Traverse the symbols in the table.
 *    When encountering a function:
 *     >go into it
 *     >generate local symbols
 *     >bind correct types.
 *   While doing this, check if each function call and type bound exists, and report potential errs.
 *   OBS: Can deduce here if call is actually a struct initialization
 *
 * Will be left with a lower-level IR where every Type is bound to its correct symbol.
 * >Function declarations will have the symbol it generates connected to it and the types
 *  return type and parameter has.
 * >Function calls will point to the symbol of the function it calls.
 *
 * >Struct declarations will have the Type it generates as well as pointers to the type of its
 *   members.
 * >Var declarations (local and global) will point the Type symbol it has.
 *
 *
 * 6. Typecheck !
 *
 * Test:
 * --------------------------------------------------------
    struct Pair :: a: s32, b: bool      // Generates Pair

    func foo(a: s32, b: bool): Pair     // Generates foo
    begin
        var pair: Pair
        pair := Pair(a, b)
        return pair
    end

    func main(): s32                    // Generates main
    begin
        var a: s32, b: Pair
        a := 10
        b := foo(a, true)
    end
 * --------------------------------------------------------
 *  1. [], []
 *
 *  2. [s32, bool], [{int, 32}, {bool}]
 *
 *  3. [s32, bool, Pair, foo, main], 
 *     [{int, 32}, {bool}, {struct, [a: s32, b: bool]},
 *      {func, [a: s32, b: bool], Pair}, {func, [], s32}]
 *
 *  5. [s32, bool, Pair, foo, main, a, b, pair, a]
 *     [{int, 32}, {bool}, {struct, [a: s32, b: bool]},
 *      {func, [a: s32, b: bool], Pair}, {func, [], s32}]
 *
 *
Statically check:
>no circular dependencies: struct Foo <-> struct Bar
>do not attempt to use variable or call func which does not exist
>do not have two funcs or two vars in the same scope with same name
 */
#endif /* SYMBOL_H */

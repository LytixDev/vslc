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
#include "base/nicc.h"
#include "compiler.h"

typedef enum {
    SYMBOL_GLOBAL_ARRAY,
    SYMBOL_GLOBAL_VAR,
    SYMBOL_LOCAL_VAR,
    SYMBOL_PARAM,
    SYMBOL_FUNC,
    SYMBOL_TYPE,

    SYMBOL_TYPE_LEN,
} SymbolKind;


typedef struct symbol_table_t SymbolTable;

typedef struct {
    SymbolKind type;
    u32 name; // Index into str_list
    u32 sequence_number;
    AstNode *node; // Node which defined this symbol

    SymbolTable *function_symtable; // @NULLABLE
} Symbol;


struct symbol_table_t {
    Symbol *symbols;
    u32 sym_len;
    u32 sym_cap;
    HashMap *hashmap;
    HashMap *hashmap_parent; // @NULLABLE
};

SymbolTable make_table(HashMap *parent);
void symbol_table_init(SymbolTable *table);

// Returns the index of the newly added symbol
u32 symbol_table_add(SymbolTable *table, SymbolKind type, u32 name, AstNode *node);
u32 symbol_table_get(SymbolTable *table, u32 name);


/*
 * 1. Create symbol table
 * 2. Add all default types (s32, f32, ...) as SYMBOL_TYPE
 * 3. Find all global symbols (global vars, func decls, struct decls, ... )
 *    While doing this, check for collisions and report any potential errors.
 *
 * At this point we will not find any more new functions or new types.
 *
 * Check for circular dependencies within a struct !
 *
 * 4. Iteratively create new low-level AST:
 *     >Get rid of token information
 *     >Bind correct symbols to the nodes.
 *    Traverse the symbols in the table.
 *    When encountering a function or struct:
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
    struct Pair :: a: s32, b: bool

    func foo(a: s32, b: bool): Pair
    begin
        var pair: Pair
        pair := Pair(a, b)
        return pair
    end

    func main(): s32
    begin
        var a: s32
        a := 10
        foo(a, true)
    end
 * --------------------------------------------------------
 *  1. []
 *  2. [s32, bool]
 *  3. [s32, bool, Pair, foo, main]
 *  4. [s32, bool, Pair, foo, main, a, b, pair, a]
 *
 *  How the functions and structs will look like
 *
 *  Pair : gen type 2
 *    a : type 0
 *    b : type 1
 *
 * foo : gen func 3, return type 2
 *   a : gen param 5, type 0
 *   b : gen param 6, type 1
 *   pair : gen local 7, type 2
 *
 * main : gen func 4, return type 0
 *   a : gen local 8, type 0
 */

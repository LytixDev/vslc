/*
 * From https://github.com/lytixDev/nag
 */

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
#ifndef NAG_H
#define NAG_H

#include "base/sac_single.h"
#include "base/types.h"

/*
 * The index type.
 * If u16 does not suffice, just change this typedef.
 */
typedef u16 NAG_Idx;

#define NAG_STACK_GROW_SIZE (NAG_Idx)256 // at least 8
#define NAG_QUEUE_GROW_SIZE \
    (NAG_Idx)32 // at least 8
                //
#define NAG_MIN(a, b) ((a) < (b) ? (a) : (b))

#define NAG_UNDISCOVERED U16_MAX

typedef struct nag_graph_node_t NAG_GraphNode;
struct nag_graph_node_t {
    NAG_Idx id;
    NAG_GraphNode *next;
};

typedef struct {
    NAG_Idx n_nodes;
    NAG_GraphNode **neighbor_list;
    Arena *scratch_arena;
    Arena *persist_arena;
} NAG_Graph;

typedef struct {
    NAG_Idx n_nodes;
    NAG_Idx *nodes; // of n_nodes len
} NAG_Order;

typedef struct {
    u32 n; // how many orders
    NAG_Order *orders; // NOTE: Heap allocated!
} NAG_OrderList;


NAG_Graph nag_make_graph(Arena *persist, Arena *scratch, NAG_Idx n_nodes);
/* Expects node indices between 0 and graph->n_nodes - 1 */
void nag_add_edge(NAG_Graph *graph, NAG_Idx from, NAG_Idx to);
void nag_print(NAG_Graph *graph);

NAG_OrderList nag_dfs(NAG_Graph *graph);
NAG_Order nag_dfs_from(NAG_Graph *graph, NAG_Idx start_node);

NAG_OrderList nag_bfs(NAG_Graph *graph);
NAG_Order nag_bfs_from(NAG_Graph *graph, NAG_Idx start_node);

/* Assumes graph contains no cycles */
NAG_Order nag_rev_toposort(NAG_Graph *graph);

NAG_OrderList nag_scc(NAG_Graph *graph);


#endif /* NAG_H */

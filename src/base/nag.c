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
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h> // why the hell is memset here

#include "base/str.h"
#include "nag.h"

typedef NAG_Order (*GraphTraverse)(NAG_Graph *graph, NAG_Idx start_node, u8 *visited);


NAG_Graph nag_make_graph(Arena *persist, Arena *scratch, NAG_Idx n_nodes)
{
    NAG_Graph graph = {
        .n_nodes = n_nodes,
        .scratch_arena = scratch,
        .persist_arena = persist,
        .neighbor_list = m_arena_alloc(persist, sizeof(NAG_GraphNode) * n_nodes),
    };
    return graph;
}

void nag_add_edge(NAG_Graph *graph, NAG_Idx from, NAG_Idx to)
{
    assert(from <= graph->n_nodes);
    // NOTE: This is the most naive way we can add eges and probably quite poor for performance
    //       I will improve this if/when it becomes noticable.
    NAG_GraphNode *first = graph->neighbor_list[from];
    NAG_GraphNode *new_node = m_arena_alloc(graph->persist_arena, sizeof(NAG_GraphNode));
    new_node->id = to;
    new_node->next = first;
    graph->neighbor_list[from] = new_node;
}

void nag_print(NAG_Graph *graph)
{
    for (NAG_Idx i = 0; i < graph->n_nodes; i++) {
        printf("[%d] -> ", i);
        NAG_GraphNode *node = graph->neighbor_list[i];
        while (node != NULL) {
            printf("%d, ", node->id);
            node = node->next;
        }
        putchar('\n');
    }
}

static NAG_OrderList nag_traverse_all(NAG_Graph *graph, GraphTraverse traverse_func)
{
    u8 *visited = m_arena_alloc(graph->scratch_arena, sizeof(bool) * graph->n_nodes);
    memset(visited, false, sizeof(NAG_Idx) * graph->n_nodes);

    NAG_OrderList result = { 0 };
    u32 n_orders_allocated = 8;
    result.orders = malloc(sizeof(NAG_Order) * n_orders_allocated);

    for (NAG_Idx i = 0; i < graph->n_nodes; i++) {
        if (visited[i]) {
            continue;
        }
        NAG_Order order = traverse_func(graph, i, visited);
        result.orders[result.n++] = order;
        if (result.n == n_orders_allocated) {
            n_orders_allocated += 8;
            result.orders = realloc(result.orders, sizeof(NAG_Order) * n_orders_allocated);
        }
    }

    m_arena_clear(graph->scratch_arena);
    return result;
}

static inline bool linear_alloc_nodes(Arena *arena, u32 n)
{
    void *r = m_arena_alloc_internal(arena, sizeof(NAG_Idx) * n, sizeof(NAG_Idx), false);
    return r != NULL;
}

static NAG_Order nag_dfs_internal(NAG_Graph *graph, NAG_Idx start_node, u8 *visited)
{
    /* This will grow linearly on the persist arena as we add nodes to the order */
    NAG_Idx *ordered = m_arena_alloc(graph->persist_arena, sizeof(NAG_Idx) * 1);
    NAG_Idx ordered_len = 0;

    /* Everything we allocate on the scratch arena will be released before we returned */
    ArenaTmp tmp_arena = m_arena_tmp_init(graph->scratch_arena);
    NAG_Idx stack_size = NAG_STACK_GROW_SIZE;
    NAG_Idx stack_top = 1;
    /* Similar to ordered. Will grow linearly on the scratch arena as we add nodes to the stack */
    NAG_Idx *stack = m_arena_alloc_internal(graph->scratch_arena, sizeof(NAG_Idx) * stack_size,
                                            sizeof(NAG_Idx), false);
    stack[0] = start_node;

    while (stack_top != 0) {
        NAG_Idx current_node = stack[--stack_top];
        if (visited[current_node]) {
            continue;
        }
        visited[current_node] = true;
        ordered[ordered_len++] = current_node;
        if (!linear_alloc_nodes(graph->persist_arena, 1)) {
            /* Persist arena is full. Report error. */
        }

        for (NAG_GraphNode *n = graph->neighbor_list[current_node]; n != NULL; n = n->next) {
            stack[stack_top++] = n->id;
            if (stack_top == stack_size) {
                if (!linear_alloc_nodes(graph->scratch_arena, NAG_STACK_GROW_SIZE)) {
                    /* Scratch arena is full. Report error. */
                }
                stack_size += NAG_STACK_GROW_SIZE;
            }
        }
    }
    m_arena_tmp_release(tmp_arena); // Reclaims the memory to the arena, not to the OS
    return (NAG_Order){ .n_nodes = ordered_len, .nodes = ordered };
}

NAG_Order nag_dfs_from(NAG_Graph *graph, NAG_Idx start_node)
{
    u8 *visited = m_arena_alloc(graph->scratch_arena, sizeof(bool) * graph->n_nodes);
    memset(visited, false, sizeof(NAG_Idx) * graph->n_nodes);
    NAG_Order dfs_order = nag_dfs_internal(graph, start_node, visited);
    m_arena_clear(graph->scratch_arena);
    return dfs_order;
}

NAG_OrderList nag_dfs(NAG_Graph *graph)
{
    return nag_traverse_all(graph, nag_dfs_internal);
}

static NAG_Order nag_bfs_internal(NAG_Graph *graph, NAG_Idx start_node, u8 *visited)
{
    /* This will grow linearly on the persist arena as we add nodes to the order */
    NAG_Idx *ordered = m_arena_alloc(graph->persist_arena, sizeof(NAG_Idx) * 1);
    NAG_Idx ordered_len = 0;

    /* Everything we allocate on the scratch arena will be released before we returned */
    ArenaTmp tmp_arena = m_arena_tmp_init(graph->scratch_arena);

    NAG_Idx queue_size = NAG_QUEUE_GROW_SIZE;
    NAG_Idx queue_low = 0;
    NAG_Idx queue_high = 1;
    /* Similar to ordered. Will grow linearly on the scratch arena if we need to increase the size
     */
    NAG_Idx *queue = m_arena_alloc_internal(
        graph->scratch_arena, sizeof(NAG_Idx) * NAG_QUEUE_GROW_SIZE, sizeof(NAG_Idx), false);
    queue[0] = start_node;

    while (queue_low != queue_high) {
        NAG_Idx current_node = queue[queue_low++];
        if (visited[current_node]) {
            continue;
        }
        visited[current_node] = true;
        ordered[ordered_len++] = current_node;
        if (!linear_alloc_nodes(graph->persist_arena, 1)) {
            /* Persist arena is full. Report error. */
        }

        for (NAG_GraphNode *n = graph->neighbor_list[current_node]; n != NULL; n = n->next) {
            queue[queue_high++] = n->id;
            /*
             * Queue is full.
             * If we have a lot of unused space to the left, we shift the entire queue
             * downards. If not, we increase the allocation.
             */
            if (queue_high == queue_size) {
                /* Shift left */
                if (queue_low > queue_size / 2) {
                    memmove(queue, queue + queue_low, queue_high - queue_low + 1);
                    queue_high -= queue_low;
                    queue_low = 0;
                }
                /* Increase the allocation for the queue */
                if (!linear_alloc_nodes(graph->scratch_arena, NAG_QUEUE_GROW_SIZE)) {
                    /* Scratch arena is full. Report error. */
                }
                queue_size += NAG_QUEUE_GROW_SIZE;
            }
        }
    }
    m_arena_tmp_release(tmp_arena); // Reclaims the memory to the arena, not to the OS
    return (NAG_Order){ .n_nodes = ordered_len, .nodes = ordered };
}

NAG_Order nag_bfs_from(NAG_Graph *graph, NAG_Idx start_node)
{
    u8 *visited = m_arena_alloc(graph->scratch_arena, sizeof(bool) * graph->n_nodes);
    memset(visited, false, sizeof(NAG_Idx) * graph->n_nodes);
    NAG_Order bfs_order = nag_bfs_internal(graph, start_node, visited);
    m_arena_clear(graph->scratch_arena);
    return bfs_order;
}

NAG_OrderList nag_bfs(NAG_Graph *graph)
{
    return nag_traverse_all(graph, nag_bfs_internal);
}

static NAG_Order nag_toposort_from_internal(NAG_Graph *graph, NAG_Idx start_node, u8 *visited)
{
    /* This will grow linearly on the persist arena as we add nodes to the order */
    NAG_Idx *ordered = m_arena_alloc(graph->persist_arena, sizeof(NAG_Idx) * 1);
    NAG_Idx ordered_len = 0;

    /* NOTE: Most of the code below here is just DFS + some backtracking */

    /* Everything we allocate on the scratch arena will be released before we returned */
    ArenaTmp tmp_arena = m_arena_tmp_init(graph->scratch_arena);
    NAG_Idx stack_size = NAG_STACK_GROW_SIZE;
    NAG_Idx stack_top = 1;
    /* Similar to ordered. Will grow linearly on the scratch arena as we add nodes to the stack */
    NAG_Idx *stack = m_arena_alloc_internal(graph->scratch_arena, sizeof(NAG_Idx) * stack_size,
                                            sizeof(NAG_Idx), false);
    stack[0] = start_node;

    while (stack_top != 0) {
        NAG_Idx current_node = stack[--stack_top];
        if (visited[current_node]) {
            ordered[ordered_len++] = current_node;
            if (!linear_alloc_nodes(graph->persist_arena, 1)) {
                /* Persist arena is full. Report error. */
            }
            continue;
        }

        visited[current_node] = true;
        stack[stack_top++] =
            current_node; /* Next time we pop this node all neighbours have been visited */
        if (stack_top == stack_size) {
            if (!linear_alloc_nodes(graph->scratch_arena, NAG_STACK_GROW_SIZE)) {
                /* Scratch arena is full. Report error. */
            }
            stack_size += NAG_STACK_GROW_SIZE;
        }

        for (NAG_GraphNode *n = graph->neighbor_list[current_node]; n != NULL; n = n->next) {
            stack[stack_top++] = n->id;
            if (stack_top == stack_size) {
                if (!linear_alloc_nodes(graph->scratch_arena, NAG_STACK_GROW_SIZE)) {
                    /* Scratch arena is full. Report error. */
                }
                stack_size += NAG_STACK_GROW_SIZE;
            }
        }
    }
    m_arena_tmp_release(tmp_arena); /* Reclaims the memory to the arena, not to the OS */
    return (NAG_Order){ .n_nodes = ordered_len, .nodes = ordered };
}

NAG_Order nag_rev_toposort(NAG_Graph *graph)
{
    NAG_OrderList all = nag_traverse_all(graph, nag_toposort_from_internal);
    bool *included = m_arena_alloc(graph->scratch_arena, sizeof(NAG_Idx) * graph->n_nodes);
    memset(included, 0, sizeof(NAG_Idx) * graph->n_nodes);

    NAG_Order final;
    final.n_nodes = 0;
    final.nodes = m_arena_alloc(graph->persist_arena, sizeof(NAG_Idx) * graph->n_nodes);

    for (u32 i = 0; i < all.n; i++) {
        NAG_Order current = all.orders[i];
        for (u32 j = 0; j < current.n_nodes; j++) {
            NAG_Idx current_idx = current.nodes[j];
            if (!included[current_idx]) {
                included[current_idx] = true;
                final.nodes[final.n_nodes++] = current_idx;
            }
        }
    }

    return final;
}

typedef struct {
    NAG_Idx *stack;
    bool *on_stack;
    NAG_Idx *low_link;
    NAG_Idx *discovery_time;
    NAG_Idx time;
    NAG_Idx stack_top;
    Arena *scratch_arena;
} NAG_TarjanContext;

static void nag_tarjan_scc_dfs(NAG_Graph *graph, NAG_Idx node, NAG_TarjanContext *ctx,
                               NAG_OrderList *sccs)
{
    ctx->discovery_time[node] = ctx->time;
    ctx->low_link[node] = ctx->time;
    ctx->time++;
    ctx->stack[ctx->stack_top++] = node;
    ctx->on_stack[node] = true;

    for (NAG_GraphNode *neighbor = graph->neighbor_list[node]; neighbor != NULL;
         neighbor = neighbor->next) {
        NAG_Idx neighbor_id = neighbor->id;
        if (ctx->discovery_time[neighbor_id] == NAG_UNDISCOVERED) {
            /* If neighbor is not yet visited, recurse on it */
            nag_tarjan_scc_dfs(graph, neighbor_id, ctx, sccs);
            ctx->low_link[node] = NAG_MIN(ctx->low_link[node], ctx->low_link[neighbor_id]);
        } else if (ctx->on_stack[neighbor_id]) {
            /* Update low-link value if the neighbor is on the stack */
            ctx->low_link[node] = NAG_MIN(ctx->low_link[node], ctx->discovery_time[neighbor_id]);
        }
    }

    /* If node is a root node, pop the stack and form an SCC */
    if (ctx->low_link[node] == ctx->discovery_time[node]) {
        NAG_Order scc = { 0 };
        /* This will grow linearly on the persist arena as we add nodes to the order */
        scc.nodes = m_arena_alloc(graph->persist_arena, sizeof(NAG_Idx) * 1);

        while (1) {
            NAG_Idx top = ctx->stack[--ctx->stack_top];
            ctx->on_stack[top] = false;
            scc.nodes[scc.n_nodes++] = top;
            if (!linear_alloc_nodes(graph->persist_arena, 1)) {
                /* Persist arena is full. Report error. */
            }
            if (top == node)
                break;
        }

        /* This implemention does not care about trivial scc's */
        if (scc.n_nodes != 1) {
            if (sccs->n == 0 || sccs->n % 8 == 0) { // TODO: this is hacky
                sccs->orders = realloc(sccs->orders, sizeof(NAG_Order) * (sccs->n + 8));
            }
            sccs->orders[sccs->n++] = scc;
        }
    }
}

NAG_OrderList nag_scc(NAG_Graph *graph)
{
    NAG_OrderList sccs;
    sccs.n = 0;
    sccs.orders = malloc(sizeof(NAG_Order) * sccs.n);

    NAG_TarjanContext ctx;
    ctx.stack = m_arena_alloc(graph->scratch_arena, sizeof(NAG_Idx) * graph->n_nodes);
    ctx.on_stack = m_arena_alloc(graph->scratch_arena, sizeof(bool) * graph->n_nodes);
    ctx.low_link = m_arena_alloc(graph->scratch_arena, sizeof(NAG_Idx) * graph->n_nodes);
    ctx.discovery_time = m_arena_alloc(graph->scratch_arena, sizeof(NAG_Idx) * graph->n_nodes);
    ctx.time = 0;
    ctx.stack_top = 0;
    ctx.scratch_arena = graph->scratch_arena;

    memset(ctx.on_stack, false, sizeof(bool) * graph->n_nodes);
    memset(ctx.discovery_time, NAG_UNDISCOVERED, sizeof(NAG_Idx) * graph->n_nodes);

    for (NAG_Idx i = 0; i < graph->n_nodes; i++) {
        if (ctx.discovery_time[i] == NAG_UNDISCOVERED) {
            nag_tarjan_scc_dfs(graph, i, &ctx, &sccs);
        }
    }

    m_arena_clear(graph->scratch_arena);
    return sccs;
}

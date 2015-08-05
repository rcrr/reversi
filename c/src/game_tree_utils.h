/**
 * @file
 *
 * @brief Game tree logger utility definitions.
 * @details This module defines the functions used to log the game tree.
 *
 * @par game_tree_utils.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#ifndef GAME_TREE_UTILS_H
#define GAME_TREE_UTILS_H

#define PV_MAX_LENGTH 128

/*
 * Game tree stack size.
 *
 * It give the size of the static stack used to pile-up the info
 * computed by deepening the game tree.
 * The value is given by the 60 plus 12 moves added to take into account the possibility
 * to pass. Real tree depth are smaller because the number of pass is little,
 * and because there is no capability currently to search so deep.
 */
#define GAME_TREE_MAX_DEPTH 72

/*
 * Max number of legal moves hosted in the stack.
 *
 * A game has 60 moves, pass moves are not consuming the stack.
 * Every game stage has been assessed with the random game generator, and a distribution of legal moves
 * has been computed. The maximum for each game stage has been summed up totalling 981.
 * the value 1024 is a further safety added in order to prevent running out of space.
 */
#define MAX_LEGAL_MOVE_STACK_COUNT 1024

#define PVE_LINES_SEGMENTS_SIZE 28
#define PVE_LINES_FIRST_SIZE 4

#include <stdbool.h>
#include <glib.h>

#include "board.h"



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @brief A set of 64 binary switches.
 */
typedef uint64_t switches_t;

/**
 * @brief An exact solution is an entity that holds the result of a #game_position_solve run.
 */
typedef struct {
  GamePosition *solved_game_position;        /**< @brief The game position given as input. */
  int           outcome;                     /**< @brief The final endgame score. */
  Square        pv[PV_MAX_LENGTH];           /**< @brief The sequence of best moves, or principal variation. */
  int           pv_length;                   /**< @brief The number of moves in the principal variation line. */
  Board        *final_board;                 /**< @brief The final board state. */
  uint64_t      leaf_count;                  /**< @brief The count of leaf nodes searched by the solver. */
  uint64_t      node_count;                  /**< @brief The count of all nodes touched by the solver. */
} ExactSolution;

/**
 * @brief A principal variation cell.
 */
typedef struct PVCell_ {
  Square           move;           /**< @brief The current move. */
  gboolean         is_active;      /**< @brief True when the cell is used. */
  struct PVCell_  *next;           /**< @brief The next move. */
  struct PVCell_ **variant;        /**< @brief A variant move. */
} PVCell;

/**
 * @brief A principal variation environment.
 *
 * @details Fields `cells_size` and `lines_size` are assigned by the #pve_new function based on the empty count parameter.
 *          Pointers `cells`, `cells_stack`, `lines`, and `lines_stack` are also assigned in the #pve_new function
 *          by calls to malloc. All these fields are immutable.
 *
 *          Fields `cells_stack_head` and `lines_stack_head` are mutable.
 *
 *          Field `state` is a bitfield of flags collecting the structure state.
 *           - Bit zero (least significant bit) describes the lines stack state. When `0` the stack is not ordered
 *             and the used pointers must be `NULL`. When `1` the unused lines are sorted in ascending order, as well as the
 *             used pointers, no `NULL` pointers occurs.
 */
typedef struct {
  switches_t state;                         /**< @brief The condition of the structure. */
  size_t     cells_size;                    /**< @brief The count of cells contained by the cells array. */
  size_t     cells_segments_size;           /**< @brief The count of cells segments. */
  size_t     cells_first_size;              /**< @brief The number of cells contained by the first segment. */
  PVCell   **cells_segments;                /**< @brief Segments are pointers to array of cells. */
  PVCell   **cells_segments_head;           /**< @brief The next cells segment to be used. */
  size_t    *cells_segments_sorted_sizes;   /**< @brief Sizes of cells segments in the sorted order. */
  PVCell   **cells_segments_sorted;         /**< @brief Sorted cells segments, by means of the natural order of the memory adress. */
  PVCell   **cells_stack;                   /**< @brief The pointer to the array of pointers used to manage the cells. */
  PVCell   **cells_stack_head;              /**< @brief The pointer to the next, free to be assigned, pointer in the stack. */
  size_t     cells_max_usage;               /**< @brief The maximum number of cells in use. */
  size_t     lines_size;                    /**< @brief The total count of lines contained by the lines segments. */
  size_t     lines_segments_size;           /**< @brief The count of lines segments. */
  size_t     lines_first_size;              /**< @brief The number of lines contained by the first segment. */
  PVCell  ***lines_segments;                /**< @brief Segments are pointers to array of lines. */
  PVCell  ***lines_segments_head;           /**< @brief The next lines segment to be used. */
  size_t    *lines_segments_sorted_sizes;   /**< @brief Sizes of lines segments in the sorted order. */
  PVCell  ***lines_segments_sorted;         /**< @brief Sorted lines segments, by means of the natural order of the memory adress. */
  PVCell  ***lines_stack;                   /**< @brief The pointer to an array of pointers used to manage the lines. */
  PVCell  ***lines_stack_head;              /**< @brief The pointer to the next, free to be assigned, pointer in the lines array. */
  size_t     lines_max_usage;               /**< @brief The maximum number of lines in use. */
} PVEnv;

/**
 * @brief A search node is the most simple structure returned by the implementations of the search function.
 */
typedef struct {
  Square move;       /**< @brief The move to play. */
  int    value;      /**< @brief The move's value. */
} SearchNode;

/**
 * @brief The info collected on each node.
 */
typedef struct {
  GamePositionX  gpx;                         /**< @brief The game position related to the game tree node. */
  uint64_t       hash;                        /**< @brief The hash value of the game position. */
  SquareSet      move_set;                    /**< @brief The set of legal moves. */
  int            move_count;                  /**< @brief The count of legal moves. */
  uint8_t       *head_of_legal_move_list;     /**< @brief A poiter to the first legal move. */
  Square         best_move;                   /**< @brief The best move for the node. */
  int            alpha;                       /**< @brief The node value. */
  int            beta;                        /**< @brief The node cutoff value. */
} NodeInfo;

/**
 * @brief The info collected by deepening the game tree.
 *
 * @details The stack uses 5 kbytes of memory.
 */
typedef struct {
  int        fill_index;                                     /**< @brief The index of the current entry into the stack, at the beginning of game_position_solve_impl. */
  NodeInfo   nodes[GAME_TREE_MAX_DEPTH];                     /**< @brief The stack of node info. */
  uint8_t    legal_move_stack[MAX_LEGAL_MOVE_STACK_COUNT];   /**< @brief The stack hosting the legal moves for each node. */
} GameTreeStack;



/**********************************************/
/* Global constants.                          */
/**********************************************/

/**
 * @brief The invalid outcome.
 */
static const int invalid_outcome = 65;

/**
 * @brief An out of range defeat score is a value lesser than the worst case.
 */
static const int out_of_range_defeat_score = -65;

/**
 * @brief An out of range win score is a value greater than the best case.
 */
static const int out_of_range_win_score = +65;

/**
 * @brief The best score achievable.
 */
static const int best_score = +64;

/**
 * @brief The worst score achievable.
 */
static const int worst_score = -64;

/**
 * @brief The PVE internals header section switch mask.
 */
static const switches_t pve_internals_header_section                = 0x0001;

/**
 * @brief The PVE internals index section switch mask.
 */
static const switches_t pve_internals_index_section                 = 0x0002;

/**
 * @brief The PVE internals properties section switch mask.
 */
static const switches_t pve_internals_properties_section            = 0x0004;

/**
 * @brief The PVE internals structure  section switch mask.
 */
static const switches_t pve_internals_structure_section             = 0x0008;

/**
 * @brief The PVE internals computed properties section switch mask.
 */
static const switches_t pve_internals_computed_properties_section   = 0x0010;

/**
 * @brief The PVE internals active lines section switch mask.
 */
static const switches_t pve_internals_active_lines_section          = 0x0020;

/**
 * @brief The PVE internals cells segments section switch mask.
 */
static const switches_t pve_internals_cells_segments_section        = 0x0040;

/**
 * @brief The PVE internals sorted cells segments section switch mask.
 */
static const switches_t pve_internals_sorted_cells_segments_section = 0x0080;

/**
 * @brief The PVE internals cells section switch mask.
 */
static const switches_t pve_internals_cells_section                 = 0x0100;

/**
 * @brief The PVE internals sorted cells section switch mask.
 */
static const switches_t pve_internals_cells_stack_section           = 0x0200;

/**
 * @brief The PVE internals lines segments section switch mask.
 */
static const switches_t pve_internals_lines_segments_section        = 0x0400;

/**
 * @brief The PVE internals sorted lines segments section switch mask.
 */
static const switches_t pve_internals_sorted_lines_segments_section = 0x0800;

/**
 * @brief The PVE internals lines section switch mask.
 */
static const switches_t pve_internals_lines_section                 = 0x1000;

/**
 * @brief The PVE internals sorted lines section switch mask.
 */
static const switches_t pve_internals_lines_stack_section           = 0x2000;

/**
 * @brief The PVE internals header section switch mask.
 */
static const switches_t pve_state_lines_stack_sorted                = 0x0001;



/*****************************************************/
/* Function prototypes for the ExactSolution entity. */
/*****************************************************/

extern ExactSolution *
exact_solution_new (void);

extern void
exact_solution_free (ExactSolution *es);

extern char *
exact_solution_to_string (const ExactSolution *const es);

extern void
exact_solution_compute_final_board (ExactSolution *const es);



/*********************************************/
/* Function prototypes for the PVEnv entity. */
/*********************************************/

extern PVEnv *
pve_new (void);

extern void
pve_free (PVEnv *pve);

extern bool
pve_is_invariant_satisfied (const PVEnv *const pve,
                            int *const error_code,
                            const switches_t checked_invariants);

extern PVCell **
pve_line_create (PVEnv *pve);

extern void
pve_line_delete (PVEnv *pve,
                 PVCell **line);

extern void
pve_line_add_move (PVEnv *pve,
                   PVCell **line,
                   Square move);

extern void
pve_line_add_variant (PVEnv *pve,
                      PVCell **line,
                      PVCell **line_variant);

extern void
pve_internals_to_stream (PVEnv *const pve,
                         FILE *const stream,
                         const switches_t shown_sections);

extern void
pve_line_internals_to_stream (const PVEnv *const pve,
                              const PVCell **const line,
                              FILE *const stream);

extern void
pve_line_with_variants_to_stream (const PVEnv *const pve,
                                  const PVCell **const line,
                                  FILE *const stream);

extern void
pve_line_copy_to_exact_solution (const PVEnv *const pve,
                                 const PVCell **const line,
                                 ExactSolution *const es);



/**************************************************/
/* Function prototypes for the SearchNode entity. */
/**************************************************/

extern SearchNode *
search_node_new (const Square move,
                 const int value);

extern void
search_node_free (SearchNode *sn);

extern SearchNode *
search_node_negated (SearchNode *sn);



/*****************************************************/
/* Function prototypes for the GameTreeStack entity. */
/*****************************************************/

extern GameTreeStack *
game_tree_stack_new (void);

extern void
game_tree_stack_free (GameTreeStack *stack);

extern void
game_tree_stack_init (const GamePosition *const root,
                      GameTreeStack *const stack);

extern void
legal_move_list_from_set (const SquareSet legal_move_set,
                          NodeInfo *const current_node_info,
                          NodeInfo *const next_node_info);



#endif /* GAME_TREE_UTILS_H */

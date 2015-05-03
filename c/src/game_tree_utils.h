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

#include <glib.h>

#include "board.h"



/**********************************************/
/* Type declarations.                         */
/**********************************************/

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
 */
typedef struct {
  size_t    cells_size;           /**< @brief The count of cells contained by the cells array. */
  PVCell   *cells;                /**< @brief The pointer to the array of cells. */
  PVCell  **cells_stack;          /**< @brief The pointer to the array of pointers used to manage the cells. */
  PVCell  **cells_stack_head;     /**< @brief The pointer to the next, free to be assigned, pointer in the stack. */
  size_t    lines_size;           /**< @brief The count of lines contained by the lines array. */
  size_t    lines_segments_size;
  size_t    lines_first_size;
  PVCell ***lines_segments;       /**< @brief Segments are pointers to array of lines. */
  PVCell ***lines_segments_head;  /**< @brief The next segment to be used. */
  //  PVCell  **lines;                /**< @brief The pointer to the array of pointers used as a reference of the head of a cell-list. */
  PVCell ***lines_stack;          /**< @brief The pointer to an array of pointers used to manage the lines. */
  PVCell ***lines_stack_head;     /**< @brief The pointer to the next, free to be assigned, pointer in the lines array. */
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
pve_new (const int empty_count);

extern void
pve_free (PVEnv *pve);

extern gboolean
pve_verify_consistency (const PVEnv *const pve,
                        int *const error_code,
                        gchar **const error_message);

extern char *
pve_internals_to_string (const PVEnv *const pve);

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

extern gchar *
pve_line_print_internals (const PVEnv *const pve,
                          const PVCell **const line);

extern char *
pve_line_to_string (const PVEnv *const pve,
                    const PVCell **const line);

extern char *
pve_line_with_variants_to_string (const PVEnv *const pve,
                                  const PVCell **const line);

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

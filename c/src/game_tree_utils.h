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
 * @copyright 2014, 2015, 2016 Roberto Corradini. All rights reserved.
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

#include <stdbool.h>

#include "board.h"
#include "red_black_tree.h"



/**
 * @cond
 */

/*
 * These constants should be internal, but are in the header file for testing purposes.
 */

#define PVE_CELLS_SEGMENTS_SIZE 26
#define PVE_CELLS_FIRST_SIZE 64
#define PVE_LINES_SEGMENTS_SIZE 28
#define PVE_LINES_FIRST_SIZE 8

/**
 * @endcond
 */



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @enum pve_error_code_t
 * @brief Error codes returned by the #pve_is_invariant_satisfied function.
 */
typedef enum {
  PVE_ERROR_CODE_OK,                                              /**< No error detected. */
  PVE_ERROR_CODE_LINES_SEGMENTS_SIZE_IS_INCORRECT,                /**< Field `lines_segments_size` has a value different from `PVE_LINES_SEGMENTS_SIZE`. */
  PVE_ERROR_CODE_LINES_FIRST_SIZE_IS_INCORRECT,                   /**< Field `lines_first_size` has a value different from `PVE_LINES_FIRST_SIZE`. */
  PVE_ERROR_CODE_LINES_SEGMENTS_HEAD_IS_NULL,                     /**< Field `lines_segments_head` is `NULL`. */
  PVE_ERROR_CODE_LINES_SEGMENTS_IS_NULL,                          /**< Field `lines_segments` is `NULL`. */
  PVE_ERROR_CODE_LINES_SEGMENTS_HEADS_PRECEDES_ARRAY_INDEX_0,     /**< Active lines segments count is negative. */
  PVE_ERROR_CODE_ACTIVE_LINES_SEGMENTS_COUNT_EXCEEDS_BOUND,       /**< Active lines segments count is too big. */
  PVE_ERROR_CODE_LINES_SIZE_MISMATCH,                             /**< Lines size doesn't match with allocated lines segments. */
  PVE_ERROR_CODE_LINES_SEGMENTS_HAS_AN_INVALID_NULL_VALUE,        /**< Array `lines_segments` has an invalid `NULL` value. */
  PVE_ERROR_CODE_LINES_SEGMENTS_HAS_AN_INVALID_NOT_NULL_VALUE,    /**< Array `lines_segments` has an invalid `NOT NULL` value. */
  PVE_ERROR_CODE_LINES_SEGMENT_COMPUTED_INDEX_OUT_OF_RANGE,       /**< The index of a lines segment, computed from field `lines_segments_sorted_sizes`, is out of range. */
  PVE_ERROR_CODE_LINES_SEGMENTS_POS_0_AND_1_ANOMALY,              /**< Array `lines_segments` has an anomaly on positions zero and one, computed from field `lines_segments_sorted_sizes`. */
  PVE_ERROR_CODE_LINES_SEGMENTS_SORTED_AND_UNSORTED_DO_NOT_MATCH, /**< Array `lines_segments` does not match with array `lines_segments_sorted`. */
  PVE_ERROR_CODE_LINES_SEGMENTS_ARE_NOT_PROPERLY_SORTED,          /**< Array `lines_segments_sorted` is not in the proper ascending order. */
  PVE_ERROR_CODE_LINES_SIZE_DOESNT_MATCH_WITH_CUMULATED,          /**< Field `lines_size` differs from the cumulated values in `lines_segments_sorted_sizes`. */
  PVE_ERROR_CODE_LINES_SEGMENT_POSITION_0_NOT_FOUND,              /**< No position found having minimum size in array `lines_segments_sorted_sizes`. */
  PVE_ERROR_CODE_LINES_SEGMENT_POSITION_0_OR_1_NOT_FOUND,         /**< Two positions not found having minimum size in array `lines_segments_sorted_sizes`. */
  PVE_ERROR_CODE_LINES_SEGMENTS_UNUSED_SEGMENT_HAS_SIZE           /**< Unused positions in array `lines_segments_sorted_sizes` has size different from zero. */
} pve_error_code_t;

/**
 * @brief A set of 64 binary switches.
 */
typedef uint64_t switches_t;

/**
 * @brief A principal variation cell.
 */
typedef struct PVCell_ {
  Square           move;           /**< @brief The current move. */
  bool             is_active;      /**< @brief True when the cell is used. */
  GamePositionX    gpx;            /**< @brief Game position. */
  size_t           ref_count;      /**< @brief Number of entries into the dictionary. */
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
  switches_t      state;                         /**< @brief The internal state of the structure. */
  GamePositionX  *root_game_position;            /**< @brief A pointer to the root game position. */
  PVCell        **root_line;                     /**< @brief A reference to the root line. */
  size_t          cells_size;                    /**< @brief The count of cells contained by the cells array. */
  size_t          cells_segments_size;           /**< @brief The count of cells segments. */
  size_t          cells_first_size;              /**< @brief The number of cells contained by the first segment. */
  PVCell        **cells_segments;                /**< @brief Segments are pointers to array of cells. */
  PVCell        **cells_segments_head;           /**< @brief The next cells segment to be used. */
  size_t         *cells_segments_sorted_sizes;   /**< @brief Sizes of cells segments in the sorted order. */
  PVCell        **cells_segments_sorted;         /**< @brief Sorted cells segments, by means of the natural order of the memory adress. */
  PVCell        **cells_stack;                   /**< @brief The pointer to the array of pointers used to manage the cells. */
  PVCell        **cells_stack_head;              /**< @brief The pointer to the next, free to be assigned, pointer in the stack. */
  size_t          cells_max_usage;               /**< @brief The maximum number of cells in use. */
  size_t          lines_size;                    /**< @brief The total count of lines contained by the lines segments. */
  size_t          lines_segments_size;           /**< @brief The count of lines segments. */
  size_t          lines_first_size;              /**< @brief The number of lines contained by the first segment. */
  PVCell       ***lines_segments;                /**< @brief Segments are pointers to array of lines. */
  PVCell       ***lines_segments_head;           /**< @brief The next lines segment to be used. */
  size_t         *lines_segments_sorted_sizes;   /**< @brief Sizes of lines segments in the sorted order. */
  PVCell       ***lines_segments_sorted;         /**< @brief Sorted lines segments, by means of the natural order of the memory adress. */
  PVCell       ***lines_stack;                   /**< @brief The pointer to an array of pointers used to manage the lines. */
  PVCell       ***lines_stack_head;              /**< @brief The pointer to the next, free to be assigned, pointer in the lines array. */
  size_t          lines_max_usage;               /**< @brief The maximum number of lines in use. */
  size_t          line_create_count;             /**< @brief The number of time the pve_line_create() function has been called. */
  size_t          line_delete_count;             /**< @brief The number of time the pve_line_delete() function has been called. */
  size_t          line_add_move_count;           /**< @brief The number of time the pve_line_add_move() function has been called. */
  size_t          line_release_cell_count;       /**< @brief The number of times a cell is released in the pve_line_delete() function. */
} PVEnv;

/**
 * @brief An exact solution is an entity that holds the result of a #endgame_solver_f run.
 */
typedef struct {
  GamePositionX  root_gpx;                    /**< @brief The game position to be solved. */
  int            outcome;                     /**< @brief The final endgame score. */
  Square         best_move;                   /**< @brief The first move of the main principal variation. */
  GamePositionX  final_state;                 /**< @brief The final board state. */
  uint64_t       leaf_count;                  /**< @brief The count of leaf nodes searched by the solver. */
  uint64_t       node_count;                  /**< @brief The count of all nodes touched by the solver. */
  PVEnv         *pve;                         /**< @brief A reference to the principal variation env. */
} ExactSolution;

/**
 * @brief The info collected on each node.
 */
typedef struct {
  GamePositionX  gpx;                         /**< @brief The game position related to the game tree node. */
  uint64_t       hash;                        /**< @brief The hash value of the game position. */
  SquareSet      move_set;                    /**< @brief The set of legal moves. */
  Square         best_move;                   /**< @brief The best move for the node. */
  uint8_t        move_count;                  /**< @brief The count of legal moves. */
  uint8_t       *head_of_legal_move_list;     /**< @brief A poiter to the first legal move. */
  uint8_t       *move_cursor;                 /**< @brief Legal move iterator. */
  int            alpha;                       /**< @brief The node value. */
  int            beta;                        /**< @brief The node cutoff value. */
} NodeInfo;

/**
 * @brief The info collected by deepening the game tree.
 *
 * @details The stack uses 5 kbytes of memory.
 */
typedef struct {
  NodeInfo  *active_node;                                    /**< @brief The active node on the stack. */
  NodeInfo   nodes[GAME_TREE_MAX_DEPTH];                     /**< @brief The stack of node info. */
  uint8_t    legal_move_stack[MAX_LEGAL_MOVE_STACK_COUNT];   /**< @brief The stack hosting the legal moves for each node. */
  uint8_t    flip_count;                                     /**< @brief Number of flips plus one. */
  Square     flips[20];                                      /**< @brief Flips generated by the move. The move is stored in position 0, at most ther are 19 flips. */
  bool       hash_is_on;                                     /**< @brief True when hash has to be computed. */
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
 *
 * @details This switch mask identifies the header section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_header_section                = 0x0001;

/**
 * @brief The PVE internals index section switch mask.
 *
 * @details This switch mask identifies the index section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_index_section                 = 0x0002;

/**
 * @brief The PVE internals properties section switch mask.
 *
 * @details This switch mask identifies the properties section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_properties_section            = 0x0004;

/**
 * @brief The PVE internals structure  section switch mask.
 *
 * @details This switch mask identifies the structure section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_structure_section             = 0x0008;

/**
 * @brief The PVE internals computed properties section switch mask.
 *
 * @details This switch mask identifies the computed properties section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_computed_properties_section   = 0x0010;

/**
 * @brief The PVE internals active lines section switch mask.
 *
 * @details This switch mask identifies the active lines section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_active_lines_section          = 0x0020;

/**
 * @brief The PVE internals cells segments section switch mask.
 *
 * @details This switch mask identifies the cells segments section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_cells_segments_section        = 0x0040;

/**
 * @brief The PVE internals sorted cells segments section switch mask.
 *
 * @details This switch mask identifies the sorted cells segments section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_sorted_cells_segments_section = 0x0080;

/**
 * @brief The PVE internals cells section switch mask.
 *
 * @details This switch mask identifies the cells section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_cells_section                 = 0x0100;

/**
 * @brief The PVE internals sorted cells section switch mask.
 *
 * @details This switch mask identifies the cells stack section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_cells_stack_section           = 0x0200;

/**
 * @brief The PVE internals lines segments section switch mask.
 *
 * @details This switch mask identifies the lines segments section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_lines_segments_section        = 0x0400;

/**
 * @brief The PVE internals sorted lines segments section switch mask.
 *
 * @details This switch mask identifies the sorted lines segments section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_sorted_lines_segments_section = 0x0800;

/**
 * @brief The PVE internals lines section switch mask.
 *
 * @details This switch mask identifies the lines section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_lines_section                 = 0x1000;

/**
 * @brief The PVE internals lines stack section switch mask.
 *
 * @details This switch mask identifies the lines stack section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_lines_stack_section           = 0x2000;

/**
 * @brief The PVE internals root game position switch mask.
 *
 * @details This switch mask identifies the root game position section when calling the function #pve_internals_to_stream.
 */
static const switches_t pve_internals_root_game_position            = 0x4000;



/**
 * @brief A mask associated with ::PVEnv::state field that identifies if the stack is sorted.
 */
static const switches_t pve_state_lines_stack_sorted = 0x0001;



/**
 * @brief The PVE mask that activates basic lines checks in #pve_is_invariant_satisfied.
 */
static const switches_t pve_chk_inv_lines_basic = 0x0001;



/*****************************************************/
/* Function prototypes for the ExactSolution entity. */
/*****************************************************/

extern ExactSolution *
exact_solution_new (void);

extern void
exact_solution_free (ExactSolution *es);

extern void
exact_solution_to_stream (FILE *const fp,
                          const ExactSolution *const es);

extern void
exact_solution_compute_final_state (ExactSolution *const es);

extern void
exact_solution_set_root (ExactSolution *const es,
                         const GamePositionX *const gpx);



/*********************************************/
/* Function prototypes for the PVEnv entity. */
/*********************************************/

extern PVEnv *
pve_new (const GamePositionX *const root_game_position);

extern void
pve_free (PVEnv *pve);

extern bool
pve_is_invariant_satisfied (const PVEnv *const pve,
                            pve_error_code_t *const error_code,
                            const switches_t checked_invariants);

extern PVCell **
pve_line_create (PVEnv *pve);

extern void
pve_line_delete (PVEnv *pve,
                 PVCell **line);

extern void
pve_line_add_move (PVEnv *pve,
                   PVCell **line,
                   Square move,
                   GamePositionX *gpx);

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
                                  FILE *const stream);

extern void
pve_root_line_as_table_to_stream (const PVEnv *const pve,
                                  FILE *const stream);

extern void
pve_dump_to_binary_file (const PVEnv *const pve,
                         const char *const out_file_path);

extern PVEnv *
pve_load_from_binary_file (const char *const in_file_path);

extern void
pve_summary_from_binary_file_to_stream (const char *const in_file_path,
                                        FILE *const stream);

extern void
pve_transform_to_standard_form(PVEnv *const pve);



/*****************************************************/
/* Function prototypes for the GameTreeStack entity. */
/*****************************************************/

extern GameTreeStack *
game_tree_stack_new (void);

extern void
game_tree_stack_free (GameTreeStack *stack);

extern void
game_tree_stack_init (const GamePositionX *const root,
                      GameTreeStack *const stack);

extern void
game_tree_move_list_from_set (const SquareSet move_set,
                              NodeInfo *const current_node_info);

inline static bool
gts_is_terminal_node (GameTreeStack *const stack)
{
  NodeInfo* const c = stack->active_node;
  if (c->move_set) return false;
  const GamePositionX *const current_gpx = &c->gpx;
  const SquareSet empties = ~(current_gpx->blacks | current_gpx->whites);
  if (!empties) return true;
  game_position_x_pass(current_gpx, &(c + 1)->gpx);
  (c + 1)->move_set = game_position_x_legal_moves(&(c + 1)->gpx);
  if ((c + 1)->move_set) return false;
  return true;
}

inline static void
gts_compute_hash (const GameTreeStack *const stack)
{
  NodeInfo* const c = stack->active_node;
  c->hash = game_position_x_delta_hash((c - 1)->hash,
                                       stack->flips,
                                       stack->flip_count,
                                       c->gpx.player);
}

inline static void
gts_make_move (GameTreeStack *const stack)
{
  NodeInfo* const c = stack->active_node;
  Square *flip_cursor = stack->flips;
  *flip_cursor++ = *c->move_cursor;
  game_position_x_make_move(&c->gpx, *c->move_cursor, &(c + 1)->gpx);
  if (stack->hash_is_on) {
    const SquareSet bitmove = 1ULL << *c->move_cursor;
    const SquareSet cu_p = game_position_x_get_player(&c->gpx);
    const SquareSet up_o = game_position_x_get_opponent(&(c + 1)->gpx);
    SquareSet flip_set = up_o & ~(cu_p | bitmove);
    while (flip_set) {
      *flip_cursor++ = bitw_bit_scan_forward_64(flip_set);
      flip_set = bitw_reset_lowest_set_bit_64(flip_set);
    }
    stack->flip_count = flip_cursor - stack->flips;
  }
}

inline static void
gts_generate_moves (GameTreeStack *const stack)
{
  NodeInfo* const c = stack->active_node;
  uint8_t *const holml = c->head_of_legal_move_list;
  c->move_set = game_position_x_legal_moves(&c->gpx);
  c->move_cursor = holml;
  SquareSet remaining_moves = c->move_set;
  c->move_count = 0;
  if (!remaining_moves) {
    *(c->move_cursor)++ = pass_move;
  } else {
    while (remaining_moves) {
      *(c->move_cursor)++ = bitw_bit_scan_forward_64_bsf(remaining_moves);
      remaining_moves = bitw_reset_lowest_set_bit_64(remaining_moves);
    }
  }
  c->move_count = c->move_cursor - holml;
  (c + 1)->head_of_legal_move_list = c->move_cursor;
  c->move_cursor = holml;
}



#endif /* GAME_TREE_UTILS_H */

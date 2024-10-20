/**
 * @file
 *
 * @brief Game tree utilities module implementation.
 *
 * @details Provides functions to support the game tree expansion.
 *
 *          The function pve_is_invariant_satisfied is quite inefficient, but the check can
 *          be diseabled by a macro.
 *
 *          A consideration: should we introduce a typedef for PVLine and so get rid of
 *          the three star sin?
 *
 * @par game_tree_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014, 2015, 2016, 2017, 2020 Roberto Corradini. All rights reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>

#include "game_tree_utils.h"
#include "sort_utils.h"
#include "time_utils.h"



/**
 * @cond
 */

#define PVE_LOAD_DUMP_LINES_SEGMENTS_SIZE 64
#define PVE_LOAD_DUMP_CELLS_SEGMENTS_SIZE 64

#define PVE_VERIFY_INVARIANT false
#define PVE_VERIFY_INVARIANT_MASK 0xFFFF
#define pve_verify_invariant(chk_mask)                                  \
  if (PVE_VERIFY_INVARIANT) do {                                        \
      pve_error_code_t error_code = 0;                                  \
      pve_is_invariant_satisfied(pve, &error_code, chk_mask);           \
      assert(!error_code);                                              \
    } while (0);



/*
 * Local types.
 */

typedef struct {
  PVCell **line;
  unsigned int dist_lev_0;
  unsigned int rel_distance;
  GamePositionX gp;
} pve_row_t;



/*
 * Prototypes for internal functions.
 */

static void
pve_double_lines_size (PVEnv *const pve);

static void
pve_double_cells_size (PVEnv *const pve);

static void
pve_sort_lines_in_place (PVEnv *const pve);

static void
pve_state_unset_lines_stack_sorted (PVEnv *const pve);

static void
pve_sort_cells_segments (PVEnv *const pve);

static void
pve_sort_lines_segments (PVEnv *const pve);

static void *
pve_translate_ref (size_t active_segments_count,
                   size_t *segments_sizes,
                   size_t element_size,
                   size_t *from_to_segments_sorted_map,
                   void **from_segments,
                   void **to_segments,
                   void *from_element);

static void
pve_populate_segments_sorted_map (size_t *const segments_sorted_map,
                                  const size_t active_segments_count,
                                  const void **const unsorted_segments,
                                  const void **const sorted_segments);

static void
pve_tree_walker (const PVEnv *const pve,
                 FILE *const stream,
                 const bool compute_game_positions,
                 void (* begin_of_line_action) (const PVEnv *const pve,
                                                FILE *const stream,
                                                pve_row_t *const row),
                 void (* end_of_line_action) (const PVEnv *const pve,
                                              FILE *const stream,
                                              pve_row_t *const row),
                 void (* cell_action) (const PVEnv *const pve,
                                       FILE *const stream,
                                       pve_row_t *const row,
                                       const PVCell *const cell));

static void pve_twa_eol (const PVEnv *const pve,
                         FILE *const stream,
                         pve_row_t *const row);

static void pve_twa_bol (const PVEnv *const pve,
                         FILE *const stream,
                         pve_row_t *const row);

static void pve_twa_cell (const PVEnv *const pve,
                          FILE *const stream,
                          pve_row_t *const row,
                          const PVCell *const cell);

static void pve_twa_cell_csv (const PVEnv *const pve,
                              FILE *const stream,
                              pve_row_t *const row,
                              const PVCell *const cell);



/*
 * Internal variables and constants.
 */

/**
 * @endcond
 */



/**********************************************************/
/* Function implementations for the ExactSolution entity. */
/**********************************************************/

/**
 * @brief Exact solution structure constructor.
 *
 * @return a pointer to a new exact solution structure
 */
ExactSolution *
exact_solution_new (void)
{
  ExactSolution * es;
  static const size_t size_of_exact_solution = sizeof(ExactSolution);

  es = (ExactSolution*) malloc(size_of_exact_solution);
  assert(es);

  es->outcome = invalid_outcome;
  es->best_move = invalid_move;
  es->node_count = 0;
  es->leaf_count = 0;
  es->pve = NULL;

  return es;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #exact_solution_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] es the pointer to be deallocated
 */
void
exact_solution_free (ExactSolution *es)
{
  if (es) {
    if (es->pve) pve_free(es->pve);
    free(es);
  }
}

/**
 * @brief Initialize the exact solution structure.
 *
 * @invariant Parameter `es` must be not `NULL`.
 *            Invariant is guarded by an assertion.
 *
 * @param [in]  es a pointer to the exact solution structure
 */
void
exact_solution_init (ExactSolution *es)
{
  assert(es);

  es->root_gpx.blacks = empty_square_set;
  es->root_gpx.whites = empty_square_set;
  es->root_gpx.player = BLACK_PLAYER;
  es->outcome = invalid_outcome;
  es->best_move = unknown_move;
  es->final_state.blacks = empty_square_set;
  es->final_state.whites = empty_square_set;
  es->final_state.player = BLACK_PLAYER;
  es->leaf_count = 0;
  es->node_count = 0;
  es->pve = NULL;
  es->all_legal_moves_solved = false;
  es->legal_move_count = 0;
  for (int i = 0; i < 64; i++) {
    move_value_t *mv = &es->legal_move_values[i];
    mv->move = invalid_move;
    mv->value = invalid_outcome;
  }
}

/**
 * @brief Sorts the legal moves by value.
 *
 * @invariant Parameter `es` must be not `NULL`.
 *            Invariant is guarded by an assertion.
 *
 * @param [in]  es a pointer to the exact solution structure
 */
void
exact_solution_sort_legal_moves_by_value (ExactSolution *es)
{
  assert(es);

  if (!es->all_legal_moves_solved) return;

  for (int i = 1; i < es->legal_move_count; i++) {
    int j = i;
    for (;;) {
      move_value_t tmp;
      move_value_t *ej0 = &es->legal_move_values[j];
      move_value_t *ej1 = &es->legal_move_values[j - 1];
      if (j == 0 || ej1->move <= ej0->move) break;
      tmp.move = ej0->move, tmp.value = ej0->value;
      ej0->move = ej1->move, ej0->value = ej1->value;
      ej1->move = tmp.move, ej1->value = tmp.value;
      j--;
    }
  }

  for (int i = 1; i < es->legal_move_count; i++) {
    int j = i;
    for (;;) {
      move_value_t tmp;
      move_value_t *ej0 = &es->legal_move_values[j];
      move_value_t *ej1 = &es->legal_move_values[j - 1];
      if (j == 0 || ej1->value >= ej0->value) break;
      tmp.move = ej0->move, tmp.value = ej0->value;
      ej0->move = ej1->move, ej0->value = ej1->value;
      ej1->move = tmp.move, ej1->value = tmp.value;
      j--;
    }
  }
}

/**
 * @brief Outputs to `fp` a formatted string describing the exact solution structure.
 *
 * @invariant Parameter `fp` must be not `NULL`.
 *            Invariant is guarded by an assertion.
 *
 * @invariant Parameter `es` must be not `NULL`.
 *            Invariant is guarded by an assertion.
 *
 * @param [out] fp a file pointer
 * @param [in]  es a pointer to the exact solution structure
 */
void
exact_solution_to_stream (FILE *const fp,
                          const ExactSolution *const es)
{
  assert(fp);
  assert(es);

  char buf[512];
  size_t buf_len;

  buf_len = game_position_x_print(buf, &es->root_gpx);
  assert(buf_len < 512);
  (void) buf_len;

  fprintf(fp, "%s\n", buf);
  fprintf(fp, "[node_count=%" PRIu64 ", leaf_count=%" PRIu64 "]\n",
          es->node_count,
          es->leaf_count);
  fprintf(fp, "Final outcome: best move=%s, position value=%d\n",
          square_as_move_to_string(es->best_move),
          es->outcome);

  if (es->all_legal_moves_solved) {
    fprintf(fp, "Legal move count: %02d - [Index:Move:Value]: ", es->legal_move_count);
    for (int i = 0; i < es->legal_move_count; i++) {
      const move_value_t *const mv = &es->legal_move_values[i];
      fprintf(fp, "[%02d:%2s:%+03d] ", i, square_as_move_to_string(mv->move), mv->value);
    }
    fprintf(fp, "\n");
  }

  if (es->pve) {
    PVCell **rl  = es->pve->root_line;
    assert(rl);
    fprintf(fp, "PV: ");
    for (PVCell *c = *rl; c != NULL; c = c->next) {
      fprintf(fp, "%s ", square_as_move_to_string(c->move));
    }
    fprintf(fp, "\n");

    buf_len = game_position_x_print(buf, &es->final_state);
    assert(buf_len < 512);
    fprintf(fp, "\nFinal state configuration playing main PV:\n%s\n", buf);
  }
}

/**
 * @brief Computes the final board when the pv component is properly populated.
 *
 * @details Executes all the moves stored into the pv field up the the final
 *          board configuration, then stores it into the final_board field.
 *
 * @param [in] es a pointer to the exact solution structure
 */
void
exact_solution_compute_final_state (ExactSolution *const es)
{
  assert(es);

  if (!es->pve) return;

  game_position_x_copy(&es->root_gpx, &es->final_state);

  PVCell **rl  = es->pve->root_line;
  assert (rl);

  for (PVCell *c = *rl; c != NULL; c = c->next) {
    game_position_x_make_move(&es->final_state, c->move, &es->final_state);
  }
}

/**
 * @brief Assigns the `root_gpx` field.
 *
 * @param [in,out] es  a pointer to the exact solution structure
 * @param [in]     gpx a pointer to the game position structure
 */
void
exact_solution_set_root (ExactSolution *const es,
                         const GamePositionX *const gpx)
{
  es->root_gpx.blacks = gpx->blacks;
  es->root_gpx.whites = gpx->whites;
  es->root_gpx.player = gpx->player;
}



/**************************************************/
/* Function implementations for the PVEnv entity. */
/**************************************************/

/**
 * @brief PVEnv structure constructor.
 *
 * @details The sizing of the structure's components is done taking into account
 *          a worst case scenario, where every disc put on the board can cost two search
 *          levels, one for the move and one for a potential pass, plus two extra slots
 *          are reserved if the minimax algorithm check the leaf condition consuming two
 *          consecutive pass moves.
 *          These assumptions can be unrealistic, but are the only one that fit all the conditions.
 *
 *          Assertions check that the received pointers to the allocated
 *          structures are not `NULL`.
 *
 * @invariant Parameter `empty_count` cannot be negative.
 *            The invariant is guarded by an assertion.
 *
 * @return a pointer to a new principal variation env structure
 */
PVEnv *
pve_new (const GamePositionX *const root_game_position)
{
  /*
   * Sixteen segments, having the first hosting 32 lines, leads to a maximum
   * of 1,048,576 lines.
   * The total memory required is 8,388,608 bytes for lines and the same for the stack,
   * leading to a total of 16 MBytes.
   */
  static const size_t lines_segments_size = PVE_LINES_SEGMENTS_SIZE;
  static const size_t lines_first_size = PVE_LINES_FIRST_SIZE;

  /*
   * TBD
   */
  static const size_t cells_segments_size = PVE_CELLS_SEGMENTS_SIZE;
  static const size_t cells_first_size = PVE_CELLS_FIRST_SIZE;

  /* Allocates the PVE structure. */
  PVEnv *const pve = (PVEnv*) malloc(sizeof(PVEnv));
  assert(pve);

  /*
   * Clones the game position structure into a newly allocated one, and assigns the reference
   * to the root_game_position field.
   */
  pve->root_game_position = game_position_x_clone(root_game_position);

  /* Sets the state switches. */
  pve->state = 0x0000000000000000;

  /* Sets to zero the max number of cells, and lines ever used. */
  pve->cells_max_usage = 0;
  pve->lines_max_usage = 0;

  /* Prepares the cells segments. */
  pve->cells_segments_size = cells_segments_size;
  pve->cells_first_size = cells_first_size;
  pve->cells_segments = (PVCell **) malloc(sizeof(PVCell *) * cells_segments_size);
  assert(pve->cells_segments);
  for (size_t i = 0; i < cells_segments_size; i++) {
    *(pve->cells_segments + i) = NULL;
  }
  pve->cells_segments_head = pve->cells_segments;

  /* Prepares the first segment of cells. */
  pve->cells_size = cells_first_size;
  PVCell *cells = (PVCell *) malloc(cells_first_size * sizeof(PVCell));
  assert(cells);
  *(pve->cells_segments_head) = cells;
  pve->cells_segments_head++;
  for (size_t i = 0; i < cells_first_size; i++) {
    (cells + i)->move = invalid_move;
    (cells + i)->is_active = false;
    (cells + i)->next = NULL;
    (cells + i)->variant = NULL;
  }

  /* Prepares the sorted cells segments and the sorted sizes. */
  pve->cells_segments_sorted_sizes = (size_t *) malloc(sizeof(size_t) * cells_segments_size);
  pve->cells_segments_sorted = (PVCell **) malloc(sizeof(PVCell *) * cells_segments_size);
  for (size_t i = 0; i < cells_segments_size; i++) {
    *(pve->cells_segments_sorted_sizes + i) = 0;
    *(pve->cells_segments_sorted + i) = NULL;
  }
  *(pve->cells_segments_sorted_sizes + 0) = cells_first_size;
  *(pve->cells_segments_sorted + 0) = cells;

  /* Creates the cells stack and load it with the cells held in the first segment. */
  pve->cells_stack = (PVCell **) malloc(cells_first_size * sizeof(PVCell *));
  assert(pve->cells_stack);
  for (size_t i = 0; i < cells_first_size; i++) {
    *(pve->cells_stack + i) = cells + i;
  }
  pve->cells_stack_head = pve->cells_stack;

  /* Prepares the lines segments. */
  pve->lines_segments_size = lines_segments_size;
  pve->lines_first_size = lines_first_size;
  pve->lines_segments = (PVCell ***) malloc(sizeof(PVCell **) * lines_segments_size);
  assert(pve->lines_segments);
  for (size_t i = 0; i < lines_segments_size; i++) {
    *(pve->lines_segments + i) = NULL;
  }
  pve->lines_segments_head = pve->lines_segments;

  /* Prepares the first segment of lines. */
  pve->lines_size = lines_first_size;
  PVCell **lines = (PVCell **) malloc(lines_first_size * sizeof(PVCell *));
  assert(lines);
  *(pve->lines_segments_head) = lines;
  pve->lines_segments_head++;
  for (size_t i = 0; i < lines_first_size; i++) {
    *(lines + i) = NULL;
  }

  /* Prepares the sorted lines segments and the sorted sizes. */
  pve->lines_segments_sorted_sizes = (size_t *) malloc(sizeof(size_t) * lines_segments_size);
  pve->lines_segments_sorted = (PVCell ***) malloc(sizeof(PVCell **) * lines_segments_size);
  for (size_t i = 0; i < lines_segments_size; i++) {
    *(pve->lines_segments_sorted_sizes + i) = 0;
    *(pve->lines_segments_sorted + i) = NULL;
  }
  *(pve->lines_segments_sorted_sizes + 0) = lines_first_size;
  *(pve->lines_segments_sorted + 0) = lines;

  /* Creates the lines stack and load it with the lines held in the first segment. */
  pve->lines_stack = (PVCell ***) malloc(lines_first_size * sizeof(PVCell **));
  assert(pve->lines_stack);
  for (size_t i = 0; i < lines_first_size; i++) {
    *(pve->lines_stack + i) = lines + i;
  }
  pve->lines_stack_head = pve->lines_stack;

  /* Sets the statistical counters to zero. */
  pve->line_create_count = 0;
  pve->line_delete_count = 0;
  pve->line_add_move_count = 0;
  pve->line_release_cell_count = 0;

  /* Creates the root line and assigns the reference to the dedicated field. */
  pve->root_line = pve_line_create(pve);

  assert(pve_is_invariant_satisfied(pve, NULL, 0xFF));

  return pve;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #pve_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] pve the pointer to be deallocated
 */
void
pve_free (PVEnv *pve)
{
  if (pve) {

    free(pve->cells_stack);

    /* Frees cells_segments. */
    size_t cells_segments_size = pve->cells_segments_head - pve->cells_segments;
    while (cells_segments_size) {
      pve->cells_segments_head--;
      free(*(pve->cells_segments_head));
      cells_segments_size--;
    }
    free(pve->cells_segments);
    free(pve->cells_segments_sorted_sizes);
    free(pve->cells_segments_sorted);

    free(pve->lines_stack);

    /* Frees lines_segments. */
    size_t lines_segments_size = pve->lines_segments_head - pve->lines_segments;
    while (lines_segments_size) {
      pve->lines_segments_head--;
      free(*(pve->lines_segments_head));
      lines_segments_size--;
    }
    free(pve->lines_segments);
    free(pve->lines_segments_sorted_sizes);
    free(pve->lines_segments_sorted);

    game_position_x_free(pve->root_game_position);

    free(pve);
  }
}

/**
 * @brief Verifies that the PVE invariant is satisfied.
 *
 * @details Invariants verified are:
 *          - Lines stack must have NULL values from the
 *
 * Tests that free lines and free cells are unique has to be added.
 * Tests that active lines and active cells are unique has to be added.
 * Tests that all active lines have active cells, and then that the
 *   active lines and cells count are consistent with their stack pointers.
 *
 *
 * @param [in]  pve                a pointer to the principal variation environment
 * @param [out] error_code         a pointer to the error code
 * @param [in]  checked_invariants switches that turn checks on
 * @return                         true when checked invariants are satisfied, otherwise false
 */
bool
pve_is_invariant_satisfied (const PVEnv *const pve,
                            pve_error_code_t *const error_code,
                            const switches_t checked_invariants)
{
  const ptrdiff_t active_lines_segments_count = pve->lines_segments_head - pve->lines_segments;

  /*
   * Lines basic checks.
   *
   * A set of very basic checks on lines storage.
   * Pointers boundaries are checked to be consistent with sizes and the first position
   * in each array.
   */
  if (pve_chk_inv_lines_basic & checked_invariants) {
    if (pve->lines_segments_size != PVE_LINES_SEGMENTS_SIZE) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_SIZE_IS_INCORRECT;
      return false;
    }
    if (pve->lines_first_size != PVE_LINES_FIRST_SIZE) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_FIRST_SIZE_IS_INCORRECT;
      return false;
    }
    if (!pve->lines_segments_head) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_HEAD_IS_NULL;
      return false;
    }
    if (!pve->lines_segments) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_IS_NULL;
      return false;
    }
    if (active_lines_segments_count < 0) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_HEADS_PRECEDES_ARRAY_INDEX_0;
      return false;
    }
    if (active_lines_segments_count > pve->lines_segments_size) {
      if (error_code) *error_code = PVE_ERROR_CODE_ACTIVE_LINES_SEGMENTS_COUNT_EXCEEDS_BOUND;
      return false;
    }
    const size_t expected_lines_size = (size_t) (active_lines_segments_count == 0
                                                 ? 0 : (1ULL << (active_lines_segments_count - 1)) * PVE_LINES_FIRST_SIZE);
    if (expected_lines_size != pve->lines_size) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SIZE_MISMATCH;
      return false;
    }

    for (size_t i = 0; i < active_lines_segments_count; i++) {
      PVCell **ls = *(pve->lines_segments + i);
      if (!ls) {
        if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_HAS_AN_INVALID_NULL_VALUE;
        return false;
      }
    }

    for (size_t i = active_lines_segments_count; i < pve->lines_segments_size; i++) {
      PVCell **ls = *(pve->lines_segments + i);
      if (ls) {
        if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_HAS_AN_INVALID_NOT_NULL_VALUE;
        return false;
      }
    }

    size_t cumulated_lines_size_from_sorted_segments = 0;
    bool ls_p0_found = false;
    bool ls_p1_found = false;
    PVCell **ls_position_0 = *(pve->lines_segments + 0);
    PVCell **ls_position_1 = *(pve->lines_segments + 1);
    for (size_t i = 0; i < active_lines_segments_count; i++) {
      const size_t segment_size = (size_t) *(pve->lines_segments_sorted_sizes + i);
      cumulated_lines_size_from_sorted_segments += segment_size;
      PVCell **ls = *(pve->lines_segments_sorted + i);
      const size_t ls_index = bitw_bit_scan_reverse_64((size_t) (segment_size / PVE_LINES_FIRST_SIZE)) + 1;
      if (ls_index > active_lines_segments_count) {
        if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENT_COMPUTED_INDEX_OUT_OF_RANGE;
        return false;
      }
      if (ls_index == 1) {
        if (!ls_p0_found && ls == ls_position_0) {
          ls_p0_found = true;
          goto positions_1_and_2_found;
        }
        if (!ls_p1_found && ls == ls_position_1) {
          ls_p1_found = true;
          goto positions_1_and_2_found;
        }
        if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_POS_0_AND_1_ANOMALY;
        return false;
      } else {
        PVCell **ls_unsorted = *(pve->lines_segments + ls_index);
        if (ls != ls_unsorted) {
          if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_SORTED_AND_UNSORTED_DO_NOT_MATCH;
          return false;
        }
      }
    positions_1_and_2_found:
      if (i > 0) {
        PVCell **ls_prec = *(pve->lines_segments_sorted + i - 1);
        ls_prec++;
        if (ls <= ls_prec) {
          if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_ARE_NOT_PROPERLY_SORTED;
          return false;
        }
      }
    }
    if (expected_lines_size != cumulated_lines_size_from_sorted_segments) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SIZE_DOESNT_MATCH_WITH_CUMULATED;
      return false;
    }
    if (active_lines_segments_count == 1 && !ls_p0_found) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENT_POSITION_0_NOT_FOUND;
      return false;
    }
    if (active_lines_segments_count > 1 && !ls_p0_found && !ls_p1_found) {
      if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENT_POSITION_0_OR_1_NOT_FOUND;
      return false;
    }

    for (size_t i = active_lines_segments_count; i < pve->lines_segments_size; i++) {
      const size_t segment_size = *(pve->lines_segments_sorted_sizes + i);
      if (segment_size != 0) {
        if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_UNUSED_SEGMENT_HAS_SIZE;
        return false;
      }
    }
  }

  /*
   * Lines stack invariants checks.
   */

  /* Lines stack pointers are in the proper range. */
  if (!pve->lines_stack_head) {
    if (error_code) *error_code = 1001;
    return false;
  }
  if (!pve->lines_stack) {
    if (error_code) *error_code = 1002;
    return false;
  }
  const size_t used_lines_count = pve->lines_stack_head - pve->lines_stack;
  if (used_lines_count < 0) {
    if (error_code) *error_code = 1003;
    return false;
  }
  if (used_lines_count > pve->lines_size) {
    if (error_code) *error_code = 1004;
    return false;
  }

  /* Verifies that the line reference is contained into a segment. */
  if (pve->state & pve_state_lines_stack_sorted) { // Lines stack is sorted.
    PVCell ***lines_stack_cursor_prev;
    PVCell ***lines_stack_cursor;
    for (lines_stack_cursor = pve->lines_stack, lines_stack_cursor_prev = NULL;
         lines_stack_cursor < pve->lines_stack_head;
         lines_stack_cursor++) {
      if (*lines_stack_cursor == NULL) {
        if (error_code) *error_code = 1005;
        printf("lines_stack_cursor=%p\n", (void *) lines_stack_cursor);
        return false;
      } else {
        if (lines_stack_cursor_prev && *lines_stack_cursor <= *lines_stack_cursor_prev) {
          if (error_code) *error_code = 1006;
          return false;
        }
        lines_stack_cursor_prev = lines_stack_cursor;
      }
    }
    for (lines_stack_cursor = pve->lines_stack_head, lines_stack_cursor_prev = NULL;
         lines_stack_cursor < pve->lines_stack + pve->lines_size;
         lines_stack_cursor++) {
      if (*lines_stack_cursor == NULL) {
        if (error_code) *error_code = 1007;
        printf("lines_stack_cursor=%p\n", (void *) lines_stack_cursor);
        return false;
      } else {
        if (lines_stack_cursor_prev && *lines_stack_cursor <= *lines_stack_cursor_prev) {
          if (error_code) *error_code = 1008;
          return false;
        }
        lines_stack_cursor_prev = lines_stack_cursor;
      }
    }
  } else { // Lines stack is not sorted.
    for (size_t i = 0; i < used_lines_count; i++) {
      const PVCell **line = (const PVCell **) *(pve->lines_stack + i);
      if (line) {
        if (error_code) *error_code = 1009;
        return false;
      }
    }

    for (size_t i = used_lines_count; i < pve->lines_size; i++) {
      const PVCell **line = (const PVCell **) *(pve->lines_stack + i);
      if (!line) {
        if (error_code) *error_code = 1010;
        return false;
      }
      // Moving on lines_segments is linear, a bisection approach would be better, but a lot more sophisticated .....
      for (size_t lsi = active_lines_segments_count - 1; ; lsi--) { // lsi: lines_segment_index
        const PVCell **first_line_in_segment = (const PVCell **) *(pve->lines_segments_sorted + lsi);
        if (line >= first_line_in_segment) {
          if (line - first_line_in_segment < *(pve->lines_segments_sorted_sizes + lsi)) {
            break;
          } else {
            if (error_code) *error_code = 1011;
            return false;
          }
        }
        if (lsi == 0) {
          if (error_code) *error_code = 1012;
          return false;
        }
      }
    }
  }

  if (pve->line_create_count - pve->line_delete_count != used_lines_count) {
    if (error_code) *error_code = 1100;
    return false;
  }

  const size_t used_cells_count = pve->cells_stack_head - pve->cells_stack;

  if (pve->line_add_move_count - pve->line_release_cell_count != used_cells_count) {
    if (error_code) *error_code = 1101;
    return false;
  }

  return true;
}

/**
 * @brief Prints the `pve` internals into the given `stream`.
 *
 * @details The text is structured into an header and the following sections:
 *          - The index of sections
 *          - A list of properties
 *          - A header with the fields of the pve structure
 *          - A list of computed properties
 *          - A detail of active lines
 *          - A csv table reporting cells segments
 *          - A csv table reporting sorted cells segments
 *          - A csv table reporting cells
 *          - A csv table reporting the stack of pointers to cells
 *          - A csv table reporting lines segments
 *          - A csv table reporting sorted lines segments
 *          - A csv table reporting lines
 *          - A csv table reporting the stack of pointers to lines
 *          - The root game position
 *
 *          The block on active lines makes a call to the function `pve_sort_lines_in_place`
 *          that sort, and so modify, the lines stack.
 *
 *          Sections are turned on and off according to the `shown_sections` parameter.
 *          The header file defines this list of constants, taht document the relationship
 *          between the switches and the sections:
 *          - #pve_internals_header_section                `0x0001`
 *          - #pve_internals_index_section                 `0x0002`
 *          - #pve_internals_properties_section            `0x0004`
 *          - #pve_internals_structure_section             `0x0008`
 *          - #pve_internals_computed_properties_section   `0x0010`
 *          - #pve_internals_active_lines_section          `0x0020`
 *          - #pve_internals_cells_segments_section        `0x0040`
 *          - #pve_internals_sorted_cells_segments_section `0x0080`
 *          - #pve_internals_cells_section                 `0x0100`
 *          - #pve_internals_cells_stack_section           `0x0200`
 *          - #pve_internals_lines_segments_section        `0x0400`
 *          - #pve_internals_sorted_lines_segments_section `0x0800`
 *          - #pve_internals_lines_section                 `0x1000`
 *          - #pve_internals_lines_stack_section           `0x2000`
 *          - #pve_internals_root_game_position            `0x4000`
 *
 *
 * @param [in] pve            a pointer to the principal variation environment
 * @param [in] stream         the file handler destination of the report of the pve internals
 * @param [in] shown_sections switches that turn on/off the sections
 */
void
pve_internals_to_stream (PVEnv *const pve,
                         FILE *const stream,
                         const switches_t shown_sections)
{
  assert(pve);

  if (shown_sections & pve_internals_header_section) {
    time_t current_time = (time_t) -1;
    char* c_time_string = NULL;

    /* Obtain current time as seconds elapsed since the Epoch. */
    current_time = time(NULL);
    assert(current_time != ((time_t) -1));

    /* Convert to local time format. */
    c_time_string = ctime(&current_time);
    assert(c_time_string);

    fprintf(stream, "# PVE HEADER\n");

    /* ctime() has already added a terminating newline character. */
    fprintf(stream, "Current time is %s", c_time_string);

    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_index_section) {
    fprintf(stream, "# PVE INDEX OF SECTIONS\n");
    fprintf(stream, " -00- PVE HEADER ... ... ... ... ... ... 0x0001, or ... 1\n");
    fprintf(stream, " -01- PVE INDEX  ... ... ... ... ... ... 0x0002, or ... 2\n");
    fprintf(stream, " -03- PVE PROPERTIES ... ... ... ... ... 0x0004, or ... 4\n");
    fprintf(stream, " -03- PVE STRUCTURE HEADER   ... ... ... 0x0008, or ... 8\n");
    fprintf(stream, " -04- PVE COMPUTED PROPERTIES    ... ... 0x0010, or .. 16\n");
    fprintf(stream, " -05- PVE ACTIVE LINES   ... ... ... ... 0x0020, or .. 32\n");
    fprintf(stream, " -06- PVE CELLS SEGMENTS ... ... ... ... 0x0040, or .. 64\n");
    fprintf(stream, " -07- PVE SORTED CELLS SEGMENTS  ... ... 0x0080, or . 128\n");
    fprintf(stream, " -08- PVE CELLS  ... ... ... ... ... ... 0x0100, or . 256\n");
    fprintf(stream, " -09- PVE CELLS STACK    ... ... ... ... 0x0200, or . 512\n");
    fprintf(stream, " -10- PVE LINES SEGMENTS ... ... ... ... 0x0400, or  1024\n");
    fprintf(stream, " -11- PVE SORTED LINES SEGMENTS  ... ... 0x0800, or  2048\n");
    fprintf(stream, " -12- PVE LINES  ... ... ... ... ... ... 0x1000, or  4096\n");
    fprintf(stream, " -13- PVE LINES STACK    ... ... ... ... 0x2000, or  8192\n");
    fprintf(stream, " -14- PVE ROOT GAME POSITION ... ... ... 0x4000, or 16384\n");
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_properties_section) {
    fprintf(stream, "# PVE PROPERTIES\n");
    fprintf(stream, "pve address:                 %20p  --  Address of the PVEnv structure.\n", (void *) pve);
    fprintf(stream, "size of pve:                 %20zu  --  Bytes used by a PVEnv structure.\n", sizeof(PVEnv));
    fprintf(stream, "size of pv cell:             %20zu  --  Bytes used by a PVCell structure.\n", sizeof(PVCell));
    fprintf(stream, "size of pv cell pointer:     %20zu  --  Bytes used by a PVCell pointer, or a line.\n", sizeof(PVCell *));
    fprintf(stream, "size of line pointer:        %20zu  --  Bytes used by a line pointer.\n", sizeof(PVCell **));
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_structure_section) {
    fprintf(stream, "# PVE STRUCTURE HEADER\n");
    fprintf(stream, "state:                         0x%016lx  --  The internal state of the structure.\n", pve->state);
    fprintf(stream, "root_game_position:          %20p  --  Root game position.\n", (void *) pve->root_game_position);
    fprintf(stream, "root_line:                   %20p  --  Root line.\n", (void *) pve->root_line);
    fprintf(stream, "cells_size:                  %20zu  --  Count of cells contained by the cells segments.\n", pve->cells_size);
    fprintf(stream, "cells_segments_size:         %20zu  --  Count of cells segments.\n", pve->cells_segments_size);
    fprintf(stream, "cells_first_size:            %20zu  --  Number of cells contained by the first segment.\n", pve->cells_first_size);
    fprintf(stream, "cells_segments:              %20p  --  Segments are pointers to array of cells.\n", (void *) pve->cells_segments);
    fprintf(stream, "cells_segments_head:         %20p  --  Next cells segment to be used.\n", (void *) pve->cells_segments_head);
    fprintf(stream, "cells_segments_sorted_sizes: %20p  --  Array of sizes of cells segments in the sorted order.\n", (void *) pve->cells_segments_sorted_sizes);
    fprintf(stream, "cells_segments_sorted:       %20p  --  Sorted cells segments, by means of the natural order of the memory adress.\n", (void *) pve->cells_segments_sorted);
    fprintf(stream, "cells_stack:                 %20p  --  Array of pointers used to manage the cells.\n", (void *) pve->cells_stack);
    fprintf(stream, "cells_stack_head:            %20p  --  Next, free to be assigned, pointer in the cells stack.\n", (void *) pve->cells_stack_head);
    fprintf(stream, "cells_max_usage:             %20zu  --  The maximum number of cells in use.\n", pve->cells_max_usage);
    fprintf(stream, "lines_size:                  %20zu  --  Count of lines contained by the lines segments.\n", pve->lines_size);
    fprintf(stream, "lines_segments_size:         %20zu  --  Count of lines segments.\n", pve->lines_segments_size);
    fprintf(stream, "lines_first_size:            %20zu  --  Number of lines contained by the first segment.\n", pve->lines_first_size);
    fprintf(stream, "lines_segments:              %20p  --  Segments are pointers to array of lines.\n", (void *) pve->lines_segments);
    fprintf(stream, "lines_segments_head:         %20p  --  Next lines segment to be used.\n", (void *) pve->lines_segments_head);
    fprintf(stream, "lines_segments_sorted_sizes: %20p  --  Array of sizes of lines segments in the sorted order.\n", (void *) pve->lines_segments_sorted_sizes);
    fprintf(stream, "lines_segments_sorted:       %20p  --  Sorted lines segments, by means of the natural order of the memory adress.\n", (void *) pve->lines_segments_sorted);
    fprintf(stream, "lines_stack:                 %20p  --  Array of pointers used to manage the lines.\n", (void *) pve->lines_stack);
    fprintf(stream, "lines_stack_head:            %20p  --  Next, free to be assigned, pointer in the lines stack.\n", (void *) pve->lines_stack_head);
    fprintf(stream, "lines_max_usage:             %20zu  --  The maximum number of lines in use.\n", pve->lines_max_usage);
    fprintf(stream, "line_create_count:           %20zu  --  The number of calls to the function pve_line_create().\n", pve->line_create_count);
    fprintf(stream, "line_delete_count:           %20zu  --  The number of calls to the function pve_line_delete().\n", pve->line_delete_count);
    fprintf(stream, "line_add_move_count:         %20zu  --  The number of calls to the function pve_line_add_move().\n", pve->line_add_move_count);
    fprintf(stream, "line_release_cell_count:     %20zu  --  The number of times a cell is released in the pve_line_delete() function.\n", pve->line_release_cell_count);
    fprintf(stream, "\n");
  }

  assert(pve->cells_segments_head >= pve->cells_segments);
  assert(pve->lines_segments_head >= pve->lines_segments);
  assert(pve->cells_stack_head >= pve->cells_stack);
  assert(pve->lines_stack_head >= pve->lines_stack);
  const size_t lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const size_t cells_in_use_count = pve->cells_stack_head - pve->cells_stack;
  const size_t lines_segments_in_use_count = pve->lines_segments_head - pve->lines_segments;
  const size_t cells_segments_in_use_count = pve->cells_segments_head - pve->cells_segments;
  const size_t lines_max_size = (1ULL << (pve->lines_segments_size - 1)) * pve->lines_first_size;
  const size_t cells_max_size = (1ULL << (pve->cells_segments_size - 1)) * pve->cells_first_size;
  const size_t lines_actual_max_size = (1ULL << (lines_segments_in_use_count - 1)) * pve->lines_first_size;
  const size_t cells_actual_max_size = (1ULL << (cells_segments_in_use_count - 1)) * pve->cells_first_size;
  const size_t pve_max_allowed_mem_consum =
    sizeof(PVEnv) +
    sizeof(PVCell *) * pve->cells_segments_size +
    sizeof(PVCell **) * pve->lines_segments_size +
    (sizeof(size_t) + sizeof(PVCell *)) * pve->cells_segments_size +
    (sizeof(size_t) + sizeof(PVCell **)) * pve->lines_segments_size +
    (sizeof(PVCell) + sizeof(PVCell *)) * cells_max_size +
    (sizeof(PVCell *) + sizeof(PVCell **)) * lines_max_size;
  const size_t pve_current_mem_consum =
    sizeof(PVEnv) +
    sizeof(PVCell *) * pve->cells_segments_size +
    sizeof(PVCell **) * pve->lines_segments_size +
    (sizeof(size_t) + sizeof(PVCell *)) * pve->cells_segments_size +
    (sizeof(size_t) + sizeof(PVCell **)) * pve->lines_segments_size +
    (sizeof(PVCell) + sizeof(PVCell *)) * cells_actual_max_size +
    (sizeof(PVCell *) + sizeof(PVCell **)) * lines_actual_max_size;
  if (shown_sections & pve_internals_computed_properties_section) {
    fprintf(stream, "# PVE COMPUTED PROPERTIES\n");
    fprintf(stream, "cells_segments_in_use_count: %20zu  --  Cells segments being allocated.\n", cells_segments_in_use_count);
    fprintf(stream, "cells_in_use_count:          %20zu  --  Cells actively assigned to lines.\n", cells_in_use_count);
    fprintf(stream, "cells_actual_max_size:       %20zu  --  Actual maximum number of cells without allocating new segments.\n", cells_actual_max_size);
    fprintf(stream, "cells_max_size:              %20zu  --  Overall maximum number of cells.\n", cells_max_size);
    fprintf(stream, "lines_segments_in_use_count: %20zu  --  Lines segments being allocated.\n", lines_segments_in_use_count);
    fprintf(stream, "lines_in_use_count:          %20zu  --  Active lines.\n", lines_in_use_count);
    fprintf(stream, "lines_actual_max_size:       %20zu  --  Actual maximum number of lines without allocating new segments.\n", lines_actual_max_size);
    fprintf(stream, "lines_max_size:              %20zu  --  Overall maximum number of lines.\n", lines_max_size);
    fprintf(stream, "pve_max_allowed_mem_consum:  %20zu  --  PV environment max allowed memory consumption.\n", pve_max_allowed_mem_consum);
    fprintf(stream, "pve_current_mem_consum:      %20zu  --  PV environment current memory consumption.\n", pve_current_mem_consum);
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_active_lines_section) {
    pve_sort_lines_in_place(pve);
    fprintf(stream, "# PVE ACTIVE LINES\n");
    fprintf(stream, "   ORDINAL; L_COUNT; C_POS;      LINE_ADDRESS;        FIRST_CELL;              CELL; MOVE;              NEXT;           VARIANT\n");
    size_t ordinal = 0;
    size_t line_counter = 0;
    for (PVCell ***line_p = pve->lines_stack; line_p < pve->lines_stack_head; line_p++) {
      PVCell **line = *line_p;
      PVCell *first_cell = *line;
      size_t cell_position = 0;
      for (PVCell *c = first_cell; c != NULL; c = c->next) {
        fprintf(stream, "%10zu;%8zu;%6zu;%18p;%18p;%18p;%5s;%18p;%18p\n",
                ordinal,
                line_counter,
                cell_position,
                (void *) line,
                (void *) first_cell,
                (void *) c,
                square_as_move_to_string(c->move),
                (void *) c->next,
                (void *) c->variant);
        cell_position++;
        ordinal++;
      }
      line_counter++;
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_cells_segments_section) {
    fprintf(stream, "# PVE CELLS SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;      SIZE\n");
    for (size_t i = 0; i < pve->cells_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%10zu\n",
              i,
              (void *) (pve->cells_segments + i),
              (void *) *(pve->cells_segments + i),
              (size_t) ((i == 0) ? 1 : (1ULL << (i - 1))) * pve->cells_first_size);
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_sorted_cells_segments_section) {
    fprintf(stream, "# PVE SORTED CELLS SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;      SIZE\n");
    for (size_t i = 0; i < pve->cells_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%10zu\n",
              i,
              (void *) (pve->cells_segments_sorted + i),
              (void *) *(pve->cells_segments_sorted + i),
              *(pve->cells_segments_sorted_sizes + i));
    }
    fprintf(stream, "\n");
  }

  size_t cells_segment_size = pve->cells_first_size;
  size_t cells_segment_size_incr = 0;
  if (shown_sections & pve_internals_cells_section) {
    fprintf(stream, "# PVE CELLS\n");
    fprintf(stream, "SEGMENT; ORDINAL;             ADDRESS; MOVE; IS_ACTIVE;                NEXT;             VARIANT\n");
    for (size_t i = 0; i < cells_segments_in_use_count; i++, cells_segment_size += cells_segment_size_incr, cells_segment_size_incr = cells_segment_size) {
      for (size_t j = 0; j < cells_segment_size; j++) {
        PVCell *cell = (PVCell *) (*(pve->cells_segments + i) + j);
        fprintf(stream, "%7zu;%8zu;%20p;%5s;%10d;%20p;%20p\n",
                i,
                j,
                (void *) cell,
                square_as_move_to_string(cell->move),
                cell->is_active,
                (void *) cell->next,
                (void *) cell->variant);
      }
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_cells_stack_section) {
    fprintf(stream, "# PVE CELLS STACK\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < pve->cells_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p\n",
              i,
              (void *) (pve->cells_stack + i),
              (void *) *(pve->cells_stack + i));
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_lines_segments_section) {
    fprintf(stream, "# PVE LINES SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;      SIZE\n");
    for (size_t i = 0; i < pve->lines_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%10zu\n",
              i,
              (void *) (pve->lines_segments + i),
              (void *) *(pve->lines_segments + i),
              (size_t) ((i == 0) ? 1 : (1ULL << (i - 1))) * pve->lines_first_size);
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_sorted_lines_segments_section) {
    fprintf(stream, "# PVE SORTED LINES SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;      SIZE\n");
    for (size_t i = 0; i < pve->lines_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%10zu\n",
              i,
              (void *) (pve->lines_segments_sorted + i),
              (void *) *(pve->lines_segments_sorted + i),
              *(pve->lines_segments_sorted_sizes + i));
    }
    fprintf(stream, "\n");
  }

  size_t lines_segment_size = pve->lines_first_size;
  size_t lines_segment_size_incr = 0;
  if (shown_sections & pve_internals_lines_section) {
    fprintf(stream, "# PVE LINES\n");
    fprintf(stream, "SEGMENT; ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < lines_segments_in_use_count; i++, lines_segment_size += lines_segment_size_incr, lines_segment_size_incr = lines_segment_size) {
      for (size_t j = 0; j < lines_segment_size; j++) {
        fprintf(stream, "%7zu;%8zu;%20p;%20p\n",
                i,
                j,
                (void *) (*(pve->lines_segments + i) + j),
                (void *) *(*(pve->lines_segments + i) + j));
      }
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_lines_stack_section) {
    fprintf(stream, "# PVE LINES STACK\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < pve->lines_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p\n",
              i,
              (void *) (pve->lines_stack + i),
              (void *) *(pve->lines_stack + i));
    }
    fprintf(stream, "\n");
  }

  char root_game_position_to_string[512];
  size_t length;

  if (shown_sections & pve_internals_root_game_position) {
    fprintf(stream, "# PVE ROOT GAME POSITION\n");
    length = game_position_x_print(root_game_position_to_string, pve->root_game_position);
    assert(length < 512);
    (void) length;
    fprintf(stream, "\n%s\n", root_game_position_to_string);
  }
}

/**
 * @brief Returns a free line pointer.
 *
 * @details The environment is modified because the line stack fill pointer,
 *          lines_stack_head, is incremented.
 *
 * @param [in,out] pve a pointer to the principal variation environment
 * @return             a pointer to the next free line
 */
PVCell **
pve_line_create (PVEnv *pve)
{
  pve_verify_invariant(PVE_VERIFY_INVARIANT_MASK);
  pve_state_unset_lines_stack_sorted(pve);
  pve->line_create_count++;
  PVCell **line_p = *(pve->lines_stack_head);
  *(line_p) = NULL;
  *(pve->lines_stack_head) = NULL; /* Set to NULL the stack cell. */
  pve->lines_stack_head++;
  if (pve->lines_stack_head - pve->lines_stack > pve->lines_max_usage) pve->lines_max_usage++;
  if (pve->lines_stack_head - pve->lines_stack == pve->lines_size) pve_double_lines_size(pve);
  return line_p;
}

/**
 * @brief Adds the `move` to the given `line`.
 *
 * @details The function inserts a new cell at the front of the linked list of cells.
 *          A cell is retrieved from the stack, the stack fill pointer is then decremented.
 *          The cell is filled with the move and the previous first cell as next.
 *          The line is updated referring to the new added cell.
 *
 *          The `line` must be active, it is an error to call the function on free lines.
 *
 * @param [in,out] pve  a pointer to the principal variation environment
 * @param [in,out] line the line to be updated
 * @param [in]     move the move value to add to the line
 * @param [in]     gpx  the game position after the move
 */
void
pve_line_add_move (PVEnv *pve,
                   PVCell **line,
                   Square move,
                   GamePositionX *gpx)
{
  pve_verify_invariant(PVE_VERIFY_INVARIANT_MASK);
  pve->line_add_move_count++;
  PVCell *added_cell = *(pve->cells_stack_head);
  *(pve->cells_stack_head) = NULL; /* Set to NULL the stack element. */
  pve->cells_stack_head++;
  if (pve->cells_stack_head - pve->cells_stack > pve->cells_max_usage) pve->cells_max_usage++;
  if (pve->cells_stack_head - pve->cells_stack == pve->cells_size) pve_double_cells_size(pve);
  added_cell->move = move;
  added_cell->is_active = true;
  added_cell->next = *line;
  added_cell->ref_count = 0;
  added_cell->gpx.blacks = gpx->blacks;
  added_cell->gpx.whites = gpx->whites;
  added_cell->gpx.player = gpx->player;
  *line = added_cell;
}

void
pve_line_add_variant (PVEnv *pve,
                      PVCell **line,
                      PVCell **line_variant)
{
  assert(pve);
  assert(line);
  assert(*line);
  assert(line_variant);
  assert(*line_variant);
  PVCell **tmp_line = (*line)->variant;
  (*line)->variant = line_variant;
  (*line_variant)->variant = tmp_line;
}

/**
 * @brief Deletes the `line`.
 *
 * @details Traverses the linked list of cells and returns them to the cell stack.
 *          For each cell traverse recurvively all the variants.
 *          Finally returns the line to the line stack.
 *
 * @param [in,out] pve  a pointer to the principal variation environment
 * @param [in,out] line the line to be deleted
 */
void
pve_line_delete (PVEnv *pve,
                 PVCell **line)
{
  pve_verify_invariant(PVE_VERIFY_INVARIANT_MASK);
  pve_state_unset_lines_stack_sorted(pve);
  pve->line_delete_count++;
  PVCell *cell = *line; // A poiter to the first cell, or null if the line is empty.
  while (cell) {
    PVCell **v_line = cell->variant;
    if (v_line) pve_line_delete(pve, v_line);
    pve->cells_stack_head--;
    *(pve->cells_stack_head) = cell;
    cell->is_active = false;
    cell->variant = NULL;
    pve->line_release_cell_count++;
    cell->move = invalid_move;
    PVCell *next = cell->next;
    cell->next = NULL;
    cell = next;
  }
  pve->lines_stack_head--;
  *(pve->lines_stack_head) = line;
}

/**
 * @brief Prints the `line` internals into the given `stream`.
 *
 * @param [in] pve    a pointer to the principal variation environment
 * @param [in] line   the line to be printed
 * @param [in] stream the stream collecting the output
 */
void
pve_line_internals_to_stream (const PVEnv *const pve,
                              const PVCell **const line,
                              FILE *const stream)
{
  if (line) {
    fprintf(stream, "line_address=%p, first_cell=%p", (void *) line, (void *) *line);
    if (*line) fprintf(stream, ", chain: ");
    for (const PVCell *c = *line; c != NULL; c = c->next) {
      fprintf(stream, "(c=%p, m=%s, n=%p, v=%p)",
              (void *) c,
              square_as_move_to_string(c->move),
              (void *) c->next,
              (void *) c->variant);
    }
  } else {
    fprintf(stream, "Line is NULL, it shouldn't.\n");
  }
}

/**
 * @brief Prints the `line` with variants into the given stream.
 *
 * @param [in] pve    a pointer to the principal variation environment
 * @param [in] stream the stream collecting the output
 */
void
pve_line_with_variants_to_stream (const PVEnv *const pve,
                                  FILE *const stream)
{
  assert(pve);
  assert(stream);
  pve_tree_walker(pve, stream, false, pve_twa_bol, pve_twa_eol, pve_twa_cell);
}

/**
 * @brief Prints the `pve` root line with variants as a table into the given stream.
 *
 * @param [in] pve    a pointer to the principal variation environment
 * @param [in] stream the stream collecting the output
 */
void
pve_root_line_as_table_to_stream (const PVEnv *const pve,
                                  FILE *const stream)
{
  assert(pve);
  assert(stream);

  fprintf(stream, "%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s\n",
          "LINE_ID",
          "MOVE_ID",
          "VARIANT_ID",
          "NEXT_ID",
          "MOVE",
          "HEAD_LEVEL",
          "REL_LEVEL",
          "GP_HASH",
          "GP_B",
          "GP_W",
          "GP_P");

  pve_tree_walker(pve, stream, true, NULL, NULL, pve_twa_cell_csv);
}

/**
 * @brief Dumps the pve structure to a binary file.
 *
 * @details TBD.
 *
 * @param [in]     pve           a pointer to the principal variation environment
 * @param [in]     out_file_path the path of the output file
 */
void
pve_dump_to_binary_file (const PVEnv *const pve,
                         const char *const out_file_path)
{
  assert(pve);
  assert(out_file_path);

  FILE *fp = fopen(out_file_path, "w");
  assert(fp);

  fwrite(pve, sizeof(PVEnv), 1, fp);
  fwrite(pve->root_game_position, sizeof(GamePositionX), 1, fp);
  fwrite(pve->cells_segments, sizeof(PVCell *), pve->cells_segments_size, fp);
  fwrite(pve->cells_segments_sorted, sizeof(PVCell *), pve->cells_segments_size, fp);
  fwrite(pve->cells_segments_sorted_sizes, sizeof(size_t), pve->cells_segments_size, fp);
  for (PVCell **segment_p = pve->cells_segments; segment_p < pve->cells_segments_head; segment_p++) {
    const ptrdiff_t i = segment_p - pve->cells_segments;
    PVCell *segment = *segment_p;
    size_t segment_size = (size_t) (((i == 0) ? 1 : (1ULL << (i - 1))) * pve->cells_first_size);
    fwrite(segment, sizeof(PVCell), segment_size, fp);
  }
  fwrite(pve->cells_stack, sizeof(PVCell **), pve->cells_size, fp);
  fwrite(pve->lines_segments, sizeof(PVCell **), pve->lines_segments_size, fp);
  fwrite(pve->lines_segments_sorted, sizeof(PVCell **), pve->lines_segments_size, fp);
  fwrite(pve->lines_segments_sorted_sizes, sizeof(size_t), pve->lines_segments_size, fp);
  for (PVCell ***segment_p = pve->lines_segments; segment_p < pve->lines_segments_head; segment_p++) {
    const ptrdiff_t i = segment_p - pve->lines_segments;
    PVCell **segment = *segment_p;
    size_t segment_size = (size_t) (((i == 0) ? 1 : (1ULL << (i - 1))) * pve->lines_first_size);
    fwrite(segment, sizeof(PVCell *), segment_size, fp);
  }
  fwrite(pve->lines_stack, sizeof(PVCell ***), pve->lines_size, fp);

  int fclose_ret = fclose(fp);
  assert(fclose_ret == 0);
  (void) fclose_ret; /* Suppress the warning "unused variable" rised when compiling without assertions. */
}

/**
 * @brief Loads the pve structure from a binary file.
 *
 * @details Reads a binary file previously written by calling the function #pve_dump_to_binary_file.
 *
 * @param [in] in_file_path the path of the input file
 * @return                  the pve stucture loaded from file
 */
PVEnv *
pve_load_from_binary_file (const char *const in_file_path)
{
  assert(in_file_path);

  int fread_result;
  PVEnv from_file_pve;

  PVCell **from_file_lines_segments[PVE_LOAD_DUMP_LINES_SEGMENTS_SIZE];
  PVCell **from_file_lines_segments_sorted[PVE_LOAD_DUMP_LINES_SEGMENTS_SIZE];
  size_t from_file_lines_segments_sorted_sizes[PVE_LOAD_DUMP_LINES_SEGMENTS_SIZE];

  PVCell *from_file_cells_segments[PVE_LOAD_DUMP_CELLS_SEGMENTS_SIZE];
  PVCell *from_file_cells_segments_sorted[PVE_LOAD_DUMP_CELLS_SEGMENTS_SIZE];
  size_t from_file_cells_segments_sorted_sizes[PVE_LOAD_DUMP_CELLS_SEGMENTS_SIZE];

  /* Opens the binary file for reading. */
  FILE *fp = fopen(in_file_path, "r");
  assert(fp);

  /* Reads the pve structure from the file. */
  fread_result = fread(&from_file_pve, sizeof(PVEnv), 1, fp);
  assert(fread_result == 1);
  (void) fread_result;
  assert(from_file_pve.cells_segments_size <= PVE_LOAD_DUMP_CELLS_SEGMENTS_SIZE);

  /* Computes usefull pve properties and dimensions. */
  const ptrdiff_t active_cells_segments_count = from_file_pve.cells_segments_head - from_file_pve.cells_segments;
  const ptrdiff_t active_lines_segments_count = from_file_pve.lines_segments_head - from_file_pve.lines_segments;
  const ptrdiff_t cells_in_use_count = from_file_pve.cells_stack_head - from_file_pve.cells_stack;
  const ptrdiff_t lines_in_use_count = from_file_pve.lines_stack_head - from_file_pve.lines_stack;

  /* Allocates the space for the new pve structure. */
  PVEnv *const pve = (PVEnv *) malloc(sizeof(PVEnv));
  assert(pve);

  /* Sets fields from the read structure. */
  pve->state = from_file_pve.state;
  pve->root_line = from_file_pve.root_line;
  pve->cells_size = from_file_pve.cells_size;
  pve->cells_segments_size = from_file_pve.cells_segments_size;
  pve->cells_first_size = from_file_pve.cells_first_size;
  pve->cells_max_usage = from_file_pve.cells_max_usage;
  pve->lines_size = from_file_pve.lines_size;
  pve->lines_segments_size = from_file_pve.lines_segments_size;
  pve->lines_first_size = from_file_pve.lines_first_size;
  pve->lines_max_usage = from_file_pve.lines_max_usage;
  pve->line_create_count = from_file_pve.line_create_count;
  pve->line_delete_count = from_file_pve.line_delete_count;
  pve->line_add_move_count = from_file_pve.line_add_move_count;
  pve->line_release_cell_count = from_file_pve.line_release_cell_count;

  /* Allocates the space for the new game position structure. */
  pve->root_game_position = (GamePositionX *) malloc(sizeof(GamePositionX));
  assert(pve->root_game_position);

  /* Reads the root_game_position structure. */
  fread_result = fread(pve->root_game_position, sizeof(GamePositionX), 1, fp);
  assert(fread_result == 1);

  /*
   * *************************************************************************
   * CELLS_START: reads and sets up cells from here up to the CELLS_END label.
   * *************************************************************************
   */

  /*
   * First it reads the cells segments array from the file.
   * Then it reads the cells segments sorted array from the file.
   * Finally it reads the cells segments sorted sizes array from the file.
   */
  fread_result = fread(&from_file_cells_segments, sizeof(PVCell *), pve->cells_segments_size, fp);
  assert(fread_result == pve->cells_segments_size);
  fread_result = fread(&from_file_cells_segments_sorted, sizeof(PVCell *), pve->cells_segments_size, fp);
  assert(fread_result == pve->cells_segments_size);
  fread_result = fread(&from_file_cells_segments_sorted_sizes, sizeof(size_t), pve->cells_segments_size, fp);
  assert(fread_result == pve->cells_segments_size);

  /*
   * - Allocates space for the cells segments array.
   * - Set the head pointer at the beginning of the array.
   * - For the active segments, allocates the proper space for cells,
   *   read the element data from file, and increment the head pointer.
   * - Sets the the cells segments element to point to the segment, or null if unsused.
   */
  pve->cells_segments = (PVCell **) malloc(sizeof(PVCell *) * pve->cells_segments_size);
  assert(pve->cells_segments);
  pve->cells_segments_head = pve->cells_segments;
  for (size_t i = 0; i < pve->cells_segments_size; i++) {
    PVCell *segment = NULL;
    if (i < active_cells_segments_count) {
      size_t segment_size = (size_t) (((i == 0) ? 1 : (1ULL << (i - 1))) * pve->cells_first_size);
      segment = (PVCell *) malloc(segment_size * sizeof(PVCell));
      assert(segment);
      fread_result = fread(segment, sizeof(PVCell), segment_size, fp);
      assert(fread_result == segment_size);
      pve->cells_segments_head++;
    }
    *(pve->cells_segments + i) = segment;
  }

  /* Prepares the sorted cells segments and the sorted sizes. */
  pve->cells_segments_sorted_sizes = (size_t *) malloc(sizeof(size_t) * pve->cells_segments_size);
  assert(pve->cells_segments_sorted_sizes);
  pve->cells_segments_sorted = (PVCell **) malloc(sizeof(PVCell *) * pve->cells_segments_size);
  assert(pve->cells_segments_sorted);
  for (size_t i = 0; i < pve->cells_segments_size; i++) {
    *(pve->cells_segments_sorted_sizes + i) = 0;
    *(pve->cells_segments_sorted + i) = NULL;
  }
  pve_sort_cells_segments(pve);

  /*
   * Allocates the cells stack and load it with the cells read from file.
   */
  pve->cells_stack = (PVCell **) malloc(pve->cells_size * sizeof(PVCell *));
  assert(pve->cells_stack);
  fread_result = fread(pve->cells_stack, sizeof(PVCell *), pve->cells_size, fp);
  assert(fread_result == pve->cells_size);
  pve->cells_stack_head = pve->cells_stack + cells_in_use_count;

  /*
   * ****************************************
   * CELLS_END: cells reading completes here.
   * ****************************************
   */

  /*
   * *************************************************************************
   * LINES_START: reads and sets up lines from here up to the LINES_END label.
   * *************************************************************************
   */

  /*
   * First it reads the lines segments array from the file.
   * Then it reads the lines segments sorted array from the file.
   * Finally it reads the lines segments sorted sizes array from the file.
   */
  fread_result = fread(&from_file_lines_segments, sizeof(PVCell **), pve->lines_segments_size, fp);
  assert(fread_result == pve->lines_segments_size);
  fread_result = fread(&from_file_lines_segments_sorted, sizeof(PVCell **), pve->lines_segments_size, fp);
  assert(fread_result == pve->lines_segments_size);
  fread_result = fread(&from_file_lines_segments_sorted_sizes, sizeof(size_t), pve->lines_segments_size, fp);
  assert(fread_result == pve->lines_segments_size);

  /*
   * - Allocates space for the lines segments array.
   * - Set the head pointer at the beginning of the array.
   * - For the active segments, allocates the proper space for lines,
   *   read the element data from file, and increment the head pointer.
   * - Sets the the lines segments element to point to the segment, or null if unsused.
   */
  pve->lines_segments = (PVCell ***) malloc(sizeof(PVCell **) * pve->lines_segments_size);
  assert(pve->lines_segments);
  pve->lines_segments_head = pve->lines_segments;
  for (size_t i = 0; i < pve->lines_segments_size; i++) {
    PVCell **segment = NULL;
    if (i < active_lines_segments_count) {
      size_t segment_size = (size_t) (((i == 0) ? 1 : (1ULL << (i - 1))) * pve->lines_first_size);
      segment = (PVCell **) malloc(segment_size * sizeof(PVCell *));
      assert(segment);
      fread_result = fread(segment, sizeof(PVCell *), segment_size, fp);
      assert(fread_result == segment_size);
      pve->lines_segments_head++;
    }
    *(pve->lines_segments + i) = segment;
  }

  /* Prepares the sorted lines segments and the sorted sizes. */
  pve->lines_segments_sorted_sizes = (size_t *) malloc(sizeof(size_t) * pve->lines_segments_size);
  assert(pve->lines_segments_sorted_sizes);
  pve->lines_segments_sorted = (PVCell ***) malloc(sizeof(PVCell **) * pve->lines_segments_size);
  assert(pve->lines_segments_sorted);
  for (size_t i = 0; i < pve->lines_segments_size; i++) {
    *(pve->lines_segments_sorted_sizes + i) = 0;
    *(pve->lines_segments_sorted + i) = NULL;
  }
  pve_sort_lines_segments(pve);

  /*
   * Allocates the lines stack and load it with the lines read from file.
   */
  pve->lines_stack = (PVCell ***) malloc(pve->lines_size * sizeof(PVCell **));
  assert(pve->lines_stack);
  fread_result = fread(pve->lines_stack, sizeof(PVCell **), pve->lines_size, fp);
  assert(fread_result == pve->lines_size);
  pve->lines_stack_head = pve->lines_stack + lines_in_use_count;

  /*
   * ****************************************
   * LINES_END: lines reading completes here.
   * ****************************************
   */

  /*
   * *************************************************************************************************
   * ADDRESS_TRANSLATION_START: pointer translation from here up to the ADDRESS_TRANSLATION_END label.
   * *************************************************************************************************
   */

  /*
   * First step is about generating the mappings between sorted and unsorted segments.
   * Cells first and then lines.
   */
  size_t from_file_cells_segments_sorted_map[PVE_LOAD_DUMP_CELLS_SEGMENTS_SIZE];
  size_t from_file_lines_segments_sorted_map[PVE_LOAD_DUMP_LINES_SEGMENTS_SIZE];

  pve_populate_segments_sorted_map(from_file_cells_segments_sorted_map,
                                   active_cells_segments_count,
                                   (const void ** const) from_file_cells_segments,
                                   (const void ** const) from_file_cells_segments_sorted);

  pve_populate_segments_sorted_map(from_file_lines_segments_sorted_map,
                                   active_lines_segments_count,
                                   (const void ** const) from_file_lines_segments,
                                   (const void ** const) from_file_lines_segments_sorted);

  /*
   * There are six address translation to execute:
   *
   * - The root_line field.
   * - The field next in all cells. It is a pointer to another cell.
   * - The field variant in all cells. It is a pointer to a line.
   * - The "field" first_cell in all lines (lines are just pointers, not structures, ut conceptually ...).
   *   It is a pointer to a cell.
   * - The cells stack is composed by cell references.
   * - The lines stack is composed by line references.
   */

  /* Translates the root_line field. */
  PVCell **translated_rl = pve_translate_ref(active_lines_segments_count,
                                             from_file_lines_segments_sorted_sizes,
                                             sizeof(PVCell *),
                                             from_file_lines_segments_sorted_map,
                                             (void **) from_file_lines_segments_sorted,
                                             (void **) pve->lines_segments,
                                             pve->root_line);
  if (translated_rl) {
    pve->root_line = translated_rl;
  } else {
    fprintf(stderr, "Error in translating address element for line address: pve->root_line=%p\n", (void *) pve->root_line);
    abort();
  }

  /*
   * Computes and executes address translation for cell->next and cell->variant fields in all cell segments.
   */
  for (PVCell **segment_p = pve->cells_segments; segment_p < pve->cells_segments_head; segment_p++) {
    PVCell *const segment = *segment_p;
    const ptrdiff_t i = segment_p - pve->cells_segments;
    const size_t segment_size = ((i == 0) ? 1 : (1ULL << (i - 1))) * pve->cells_first_size;
    for (size_t j = 0; j < segment_size; j++) {
      PVCell *c = segment + j;
      assert(c);
      if (c->next) {
        PVCell *translated_element = pve_translate_ref(active_cells_segments_count,
                                                       from_file_cells_segments_sorted_sizes,
                                                       sizeof(PVCell),
                                                       from_file_cells_segments_sorted_map,
                                                       (void **) from_file_cells_segments_sorted,
                                                       (void **) pve->cells_segments,
                                                       c->next);
        if (translated_element) {
          c->next = translated_element;
        } else {
          fprintf(stderr, "Error in translating address element for cell address: c->next=%p\n", (void *) c->next);
          abort();
        }
      }
      if (c->variant) {
        PVCell **translated_element = pve_translate_ref(active_lines_segments_count,
                                                        from_file_lines_segments_sorted_sizes,
                                                        sizeof(PVCell *),
                                                        from_file_lines_segments_sorted_map,
                                                        (void **) from_file_lines_segments_sorted,
                                                        (void **) pve->lines_segments,
                                                        c->variant);
        if (translated_element) {
          c->variant = translated_element;
        } else {
          fprintf(stderr, "Error in translating address element for line address: c->variant=%p\n", (void *) c->variant);
          abort();
        }
      }
    }
  }

  /*
   * Computes and executes address translation for "first cell" fields in all line segments.
   */
  for (PVCell ***segment_p = pve->lines_segments; segment_p < pve->lines_segments_head; segment_p++) {
    PVCell **const segment = *segment_p;
    const ptrdiff_t i = segment_p - pve->lines_segments;
    const size_t segment_size = ((i == 0) ? 1 : (1ULL << (i - 1))) * pve->lines_first_size;
    for (size_t j = 0; j < segment_size; j++) {
      PVCell **line = segment + j;
      assert(line);
      if (*line) {
        PVCell *translated_element = pve_translate_ref(active_cells_segments_count,
                                                       from_file_cells_segments_sorted_sizes,
                                                       sizeof(PVCell),
                                                       from_file_cells_segments_sorted_map,
                                                       (void **) from_file_cells_segments_sorted,
                                                       (void **) pve->cells_segments,
                                                       *line);
        if (translated_element) {
          *line = translated_element;
        } else {
          fprintf(stderr, "Error in translating address element for cell address: *line=%p\n", (void *) *line);
          abort();
        }
      }
    }
  }

  /*
   * Executes address translation for cells stack elements.
   */
  for (size_t i = 0; i < pve->cells_size; i++) {
    PVCell **element_ptr = pve->cells_stack + i;
    if (*element_ptr) {
      PVCell *translated_element = pve_translate_ref(active_cells_segments_count,
                                                     from_file_cells_segments_sorted_sizes,
                                                     sizeof(PVCell),
                                                     from_file_cells_segments_sorted_map,
                                                     (void **) from_file_cells_segments_sorted,
                                                     (void **) pve->cells_segments,
                                                     *element_ptr);
      if (translated_element) {
        *element_ptr = translated_element;
      } else {
        fprintf(stderr, "Error in translating address ref in cells stack: *element_ptr=%p\n", (void *) *element_ptr);
        abort();
      }
    }
  }

  /*
   * Executes address translation for lines stack elements.
   */
  for (size_t i = 0; i < pve->lines_size; i++) {
    PVCell ***element_ptr = pve->lines_stack + i;
    if (*element_ptr) {
      PVCell **translated_element = pve_translate_ref(active_lines_segments_count,
                                                      from_file_lines_segments_sorted_sizes,
                                                      sizeof(PVCell *),
                                                      from_file_lines_segments_sorted_map,
                                                      (void **) from_file_lines_segments_sorted,
                                                      (void **) pve->lines_segments,
                                                      *element_ptr);
      if (translated_element) {
        *element_ptr = translated_element;
      } else {
        fprintf(stderr, "Error in translating address ref in lines stack: *element_ptr=%p\n", (void *) *element_ptr);
        abort();
      }
    }
  }

  /*
   * ************************************************************
   * ADDRESS_TRANSLATION_END: pointer translation completes here.
   * ************************************************************
   */

  int fclose_ret = fclose(fp);
  assert(fclose_ret == 0);
  (void) fclose_ret;

  return pve;
}

/**
 * @brief Reads summary info from pve structure saved in a binary file, and prints them to stream.
 *
 * @details Reads header info from a binary file previously written by calling
 *          the function #pve_dump_to_binary_file, and prints them to `stream`.
 *
 * @param [in] in_file_path the path of the input file
 * @param [in] stream       the file handler destination of the report of the pve summary
 */
void
pve_summary_from_binary_file_to_stream (const char *const in_file_path,
                                        FILE *const stream)
{
  assert(in_file_path);

  int fread_result;
  PVEnv pve;

  /* Opens the binary file for reading. */
  FILE *fp = fopen(in_file_path, "r");
  assert(fp);

  /* Reads the pve structure from the file. */
  fread_result = fread(&pve, sizeof(PVEnv), 1, fp);
  assert(fread_result == 1);
  (void) fread_result;

  fprintf(stream, "# PVE STRUCTURE HEADER\n");
  fprintf(stream, "state:                         0x%016lx  --  The internal state of the structure.\n", pve.state);
  fprintf(stream, "root_game_position:          %20p  --  Root game position.\n", (void *) pve.root_game_position);
  fprintf(stream, "root_line:                   %20p  --  Root line.\n", (void *) pve.root_line);
  fprintf(stream, "cells_size:                  %20zu  --  Count of cells contained by the cells segments.\n", pve.cells_size);
  fprintf(stream, "cells_segments_size:         %20zu  --  Count of cells segments.\n", pve.cells_segments_size);
  fprintf(stream, "cells_first_size:            %20zu  --  Number of cells contained by the first segment.\n", pve.cells_first_size);
  fprintf(stream, "cells_segments:              %20p  --  Segments are pointers to array of cells.\n", (void *) pve.cells_segments);
  fprintf(stream, "cells_segments_head:         %20p  --  Next cells segment to be used.\n", (void *) pve.cells_segments_head);
  fprintf(stream, "cells_segments_sorted_sizes: %20p  --  Array of sizes of cells segments in the sorted order.\n", (void *) pve.cells_segments_sorted_sizes);
  fprintf(stream, "cells_segments_sorted:       %20p  --  Sorted cells segments, by means of the natural order of the memory adress.\n", (void *) pve.cells_segments_sorted);
  fprintf(stream, "cells_stack:                 %20p  --  Array of pointers used to manage the cells.\n", (void *) pve.cells_stack);
  fprintf(stream, "cells_stack_head:            %20p  --  Next, free to be assigned, pointer in the cells stack.\n", (void *) pve.cells_stack_head);
  fprintf(stream, "cells_max_usage:             %20zu  --  The maximum number of cells in use.\n", pve.cells_max_usage);
  fprintf(stream, "lines_size:                  %20zu  --  Count of lines contained by the lines segments.\n", pve.lines_size);
  fprintf(stream, "lines_segments_size:         %20zu  --  Count of lines segments.\n", pve.lines_segments_size);
  fprintf(stream, "lines_first_size:            %20zu  --  Number of lines contained by the first segment.\n", pve.lines_first_size);
  fprintf(stream, "lines_segments:              %20p  --  Segments are pointers to array of lines.\n", (void *) pve.lines_segments);
  fprintf(stream, "lines_segments_head:         %20p  --  Next lines segment to be used.\n", (void *) pve.lines_segments_head);
  fprintf(stream, "lines_segments_sorted_sizes: %20p  --  Array of sizes of lines segments in the sorted order.\n", (void *) pve.lines_segments_sorted_sizes);
  fprintf(stream, "lines_segments_sorted:       %20p  --  Sorted lines segments, by means of the natural order of the memory adress.\n", (void *) pve.lines_segments_sorted);
  fprintf(stream, "lines_stack:                 %20p  --  Array of pointers used to manage the lines.\n", (void *) pve.lines_stack);
  fprintf(stream, "lines_stack_head:            %20p  --  Next, free to be assigned, pointer in the lines stack.\n", (void *) pve.lines_stack_head);
  fprintf(stream, "lines_max_usage:             %20zu  --  The maximum number of lines in use.\n", pve.lines_max_usage);
  fprintf(stream, "line_create_count:           %20zu  --  The number of calls to the function pve_line_create().\n", pve.line_create_count);
  fprintf(stream, "line_delete_count:           %20zu  --  The number of calls to the function pve_line_delete().\n", pve.line_delete_count);
  fprintf(stream, "line_add_move_count:         %20zu  --  The number of calls to the function pve_line_add_move().\n", pve.line_add_move_count);
  fprintf(stream, "line_release_cell_count:     %20zu  --  The number of times a cell is released in the pve_line_delete() function.\n", pve.line_release_cell_count);
  fprintf(stream, "\n");

  /* Closes the input file. */
  int fclose_ret = fclose(fp);
  assert(fclose_ret == 0);
  (void) fclose_ret;
}

/**
 * @brief Transforms the PV to standard form.
 *
 * @details All the variants are sorted in natural order, [A1, B1, C1...H8].
 *
 * @param [in,out] pve a pointer to the principal variation environment
 */
void
pve_transform_to_standard_form (PVEnv *const pve)
{
  assert(pve);

  /*
   * The board has 64 squares, each move can have a pass, so
   * keeping it simple, 128 is the theoretical upper bound.
   */
  static const size_t max_recursion_depth = 128;

  /* The stack of pve lines to be traversed. */
  PVCell **line_stack[max_recursion_depth];

  /* Initializes the first element in the line array. */
  line_stack[0] = pve->root_line;

  /* Sets the header pointer on the second element. */
  PVCell ***line_stack_header = line_stack + 1;

  /* Loops until there are unprocessed lines on the stack. */
  while (line_stack_header > line_stack) {
    PVCell **l = *--line_stack_header;
    PVCell **p = l;

    int move_count = 0;
    int pass_move_at_tail_count = 0;

    while (*p) {

      PVCell *c = *p;

      move_count++;
      if (c->move == pass_move) pass_move_at_tail_count++;
      else pass_move_at_tail_count = 0;

      if (c->variant) {
        Square min = c->move;
        PVCell ***variant_ref = NULL;
        PVCell *cv0 = c;
        PVCell *cv1 = c;
        while (cv0->variant) {
          PVCell **lv = cv0->variant;
          cv0 = *lv;
          if (cv0->move < min) {
            min = cv0->move;
            variant_ref = &(cv1->variant);
          }
          cv1 = cv0;
        }
        if (variant_ref) {
          PVCell *c0 = c;
          PVCell *c1 = **variant_ref;
          PVCell **l = *variant_ref;
          c0->variant = c1->variant;
          c1->variant = l;
          *l = c0;
          c = c1;
          *p = c;
        }
        *line_stack_header++ = c->variant;
      }
      p = &(c->next);
    }

    /* Removes trailing pass moves. */
    p = l;
    for (int i = 0; i < move_count - pass_move_at_tail_count - 1; i++) {
      p = &((*p)->next);
    }
    PVCell *c = (*p)->next;
    (*p)->next = NULL;
    while (c) {
      pve->cells_stack_head--;
      *(pve->cells_stack_head) = c;
      c->is_active = false;
      c->variant = NULL;
      pve->line_release_cell_count++;
      c->move = invalid_move;
      PVCell *n = c->next;
      c->next = NULL;
      c = n;
    }
  }
}



/**********************************************************/
/* Function implementations for the GameTreeStack entity. */
/**********************************************************/

/**
 * @brief GameTreeStack structure constructor.
 *
 * @return a pointer to a new game tree stack structure
 */
GameTreeStack *
game_tree_stack_new (void)
{
  GameTreeStack* stack;
  static const size_t size_of_stack = sizeof(GameTreeStack);

  stack = (GameTreeStack*) malloc(size_of_stack);
  assert(stack);

  return stack;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #game_tree_stack_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] stack the pointer to be deallocated
 */
void
game_tree_stack_free (GameTreeStack *stack)
{
  free(stack);
}

/**
 * @brief Initializes the stack structure.
 */
void
game_tree_stack_init (const GamePositionX *const root,
                      GameTreeStack *const stack)
{
  for (int i = 0; i < MAX_LEGAL_MOVE_STACK_COUNT; i++) {
    stack->legal_move_stack[i] = &stack->legal_move_array[i];
  }

  NodeInfo* ground_node_info = &stack->nodes[0];
  game_position_x_copy(root, &ground_node_info->gpx);
  ground_node_info->gpx.player = player_opponent(ground_node_info->gpx.player);
  ground_node_info->hash = game_position_x_hash(&ground_node_info->gpx);
  ground_node_info->move_set = 0x0000008181000000;
  ground_node_info->best_move = invalid_move;
  ground_node_info->move_count = 4;
  ground_node_info->pv_first_line_created = false;
  ground_node_info->head_of_legal_move_list = &stack->legal_move_stack[0];
  ground_node_info->move_cursor = ground_node_info->head_of_legal_move_list;
  ground_node_info->pve_line = NULL;
  ground_node_info->alpha = out_of_range_defeat_score;
  ground_node_info->beta = - out_of_range_defeat_score;

  (*ground_node_info->move_cursor)->move = unknown_move;

  NodeInfo* first_node_info  = &stack->nodes[1];
  game_position_x_copy(root, &first_node_info->gpx);
  first_node_info->head_of_legal_move_list = ground_node_info->head_of_legal_move_list + ground_node_info->move_count;
  first_node_info->move_cursor = first_node_info->head_of_legal_move_list;
  first_node_info->pve_line = NULL;
  first_node_info->alpha = out_of_range_defeat_score;
  first_node_info->beta = - out_of_range_defeat_score;

  stack->active_node = ground_node_info;

  stack->flips[0] = pass_move;
  stack->flip_count = 1;

  stack->hash_is_on = false;
}

/**
 * To be documented.
 */
extern bool
gts_is_terminal_node (GameTreeStack *const stack);

/**
 * To be documented.
 */
extern void
gts_compute_hash (const GameTreeStack *const stack);

/**
 * To be documented.
 */
extern void
gts_make_move (GameTreeStack *const stack);


/**
 * To be documented.
 */
extern void
gts_generate_moves (GameTreeStack *const stack);



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Doubles the cell count in the PV environment.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_double_cells_size (PVEnv *const pve)
{
  assert(pve);

  /* Doubleing cells_size can occur only if the stack is fully used. */
  assert(pve->cells_size == pve->cells_stack_head - pve->cells_stack);

  /* The number of cells segments cannot grow further than the limit. */
  size_t cells_segments_used = pve->cells_segments_head - pve->cells_segments;
  assert(pve->cells_segments_size > cells_segments_used);

  const size_t actual_cells_size = pve->cells_size;
  const size_t cells_size_extension = pve->cells_size;

  /* Prepares the extension segment of cells. */
  PVCell *cells_extension = (PVCell *) malloc(cells_size_extension * sizeof(PVCell));
  assert(cells_extension);
  *(pve->cells_segments_head) = cells_extension;
  pve->cells_segments_head++;
  cells_segments_used++;
  pve->cells_size += cells_size_extension;
  for (size_t i = 0; i < cells_size_extension; i++) {
    (cells_extension + i)->move = invalid_move;
    (cells_extension + i)->is_active = false;
    (cells_extension + i)->next = NULL;
    (cells_extension + i)->variant = NULL;
  }

  /* Creates the new cells stack and load it with the cells held in the extension segment. */
  PVCell **new_cells_stack = (PVCell **) malloc(pve->cells_size * sizeof(PVCell *));
  assert(new_cells_stack);
  free(pve->cells_stack);
  pve->cells_stack = new_cells_stack;
  for (size_t i = 0; i < actual_cells_size; i++) {
    *(pve->cells_stack + i) = NULL;
  }
  for (size_t i = 0; i < cells_size_extension; i++) {
    *(pve->cells_stack + actual_cells_size + i) = cells_extension + i;
  }
  pve->cells_stack_head = pve->cells_stack + actual_cells_size;

  /* Re-compute the sorted cells segments array, and respective sizes. */
  pve_sort_cells_segments(pve);
}

/**
 * @brief Doubles the line count in the PV environment.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_double_lines_size (PVEnv *const pve)
{
  assert(pve);

  /* Doubleing lines_size can occur only if the stack is fully used. */
  assert(pve->lines_size == pve->lines_stack_head - pve->lines_stack);

  /* The number of lines segments cannot grow further than the limit. */
  size_t lines_segments_used = pve->lines_segments_head - pve->lines_segments;
  assert(pve->lines_segments_size > lines_segments_used);

  const size_t actual_lines_size = pve->lines_size;
  const size_t lines_size_extension = pve->lines_size;

  /* Prepares the extension segment of lines. */
  PVCell **lines_extension = (PVCell **) malloc(lines_size_extension * sizeof(PVCell *));
  assert(lines_extension);
  *(pve->lines_segments_head) = lines_extension;
  pve->lines_segments_head++;
  lines_segments_used++;
  pve->lines_size += lines_size_extension;
  for (size_t i = 0; i < lines_size_extension; i++) {
    *(lines_extension + i) = NULL;
  }

  /* Creates the new lines stack and load it with the lines held in the extension segment. */
  PVCell ***new_lines_stack = (PVCell ***) malloc(pve->lines_size * sizeof(PVCell **));
  assert(new_lines_stack);
  free(pve->lines_stack);
  pve->lines_stack = new_lines_stack;
  for (size_t i = 0; i < actual_lines_size; i++) {
    *(pve->lines_stack + i) = NULL;
  }
  for (size_t i = 0; i < lines_size_extension; i++) {
    *(pve->lines_stack + actual_lines_size + i) = lines_extension + i;
  }
  pve->lines_stack_head = pve->lines_stack + actual_lines_size;

  /* Re-compute the sorted lines segments array, and respective sizes. */
  pve_sort_lines_segments(pve);
}

/**
 * @brief Sorts the stack of lines in natural order.
 *
 * @details The lines_stack array is sorted in place in two parts.
 *          The first part of the array is filled with the used lines,
 *          the second part is holding the free lines.
 *          The two sub-arrays are sorted in their natural order.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_sort_lines_in_place (PVEnv *const pve)
{
  PVCell ***index = pve->lines_stack;

  /* Here the index preparation. */
  const size_t lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const size_t lines_not_in_use_count = pve->lines_size - lines_in_use_count;

  PVCell ***lines_not_in_use_head = index +  lines_in_use_count;
  sort_utils_insertionsort_asc_p ((void **) lines_not_in_use_head, lines_not_in_use_count);

  PVCell ***free_lines_stack_cursor = pve->lines_stack_head;
  PVCell ***used_lines_stack_cursor = pve->lines_stack;
  size_t lines_segments_count = pve->lines_segments_head - pve->lines_segments;
  for (size_t lines_segments_sorted_index = 0;
       lines_segments_sorted_index < lines_segments_count;
       lines_segments_sorted_index++) {
    PVCell **first_line_in_segment = *(pve->lines_segments_sorted + lines_segments_sorted_index);
    PVCell **segment_boundary = first_line_in_segment + *(pve->lines_segments_sorted_sizes + lines_segments_sorted_index);
    for (PVCell **line = first_line_in_segment; line < segment_boundary; line++) {
      if (line == *free_lines_stack_cursor) {
        free_lines_stack_cursor++;
      } else {
        *used_lines_stack_cursor++ = line;
      }
    }
  }

  pve->state |= pve_state_lines_stack_sorted;
}

static void
pve_state_unset_lines_stack_sorted (PVEnv *const pve)
{
  if (pve->state & pve_state_lines_stack_sorted) { // Lines stack is sorted.
    for (PVCell ***lines_stack_cursor = pve->lines_stack_head - 1; lines_stack_cursor >= pve->lines_stack; lines_stack_cursor--) {
      *lines_stack_cursor = NULL;
    }
    pve->state &= ~pve_state_lines_stack_sorted;
    return;
  } else {
    return;
  }
}

/**
 * @brief Re-computes the sorted cells segments array, and respective sizes.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_sort_cells_segments (PVEnv *const pve)
{
  const size_t cells_segments_used = pve->cells_segments_head - pve->cells_segments;
  for (size_t i = 0; i < cells_segments_used; i++) {
    *(pve->cells_segments_sorted + i) = *(pve->cells_segments + i);
  }
  sort_utils_insertionsort_asc_p ((void **) pve->cells_segments_sorted, cells_segments_used);
  for (size_t i = 0; i < cells_segments_used; i++) {
    PVCell *segment = *(pve->cells_segments_sorted + i);
    size_t segment_size = pve->cells_first_size;
    size_t segment_size_incr = 0;
    for (size_t j = 0; j < cells_segments_used; j++, segment_size += segment_size_incr, segment_size_incr = segment_size) {
      if (segment == *(pve->cells_segments + j)) break;
    }
    *(pve->cells_segments_sorted_sizes + i) = segment_size;
  }
}

/**
 * @brief Re-computes the sorted lines segments array, and respective sizes.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_sort_lines_segments (PVEnv *const pve)
{
  const size_t lines_segments_used = pve->lines_segments_head - pve->lines_segments;
  for (size_t i = 0; i < lines_segments_used; i++) {
    *(pve->lines_segments_sorted + i) = *(pve->lines_segments + i);
  }
  sort_utils_insertionsort_asc_p ((void **) pve->lines_segments_sorted, lines_segments_used);
  for (size_t i = 0; i < lines_segments_used; i++) {
    PVCell **segment = *(pve->lines_segments_sorted + i);
    size_t segment_size = pve->lines_first_size;
    size_t segment_size_incr = 0;
    for (size_t j = 0; j < lines_segments_used; j++, segment_size += segment_size_incr, segment_size_incr = segment_size) {
      if (segment == *(pve->lines_segments + j)) break;
    }
    *(pve->lines_segments_sorted_sizes + i) = segment_size;
  }
}

/**
 * @brief Translate a reference from one env to another.
 *
 * @details The reference given by the `from_element` parameter is translated and returned
 *          as reference in a new environment.
 *
 *          A `NULL` pointer is returned on error.
 *
 * @param [in] active_segments_count       the number of active segments
 * @param [in] segments_sizes              an array of segment sizes
 * @param [in] element_size                the element size in bytes
 * @param [in] from_to_segments_sorted_map the array of indexes that map from-to sorted segments
 * @param [in] from_segments               the array of segments from which translate
 * @param [in] to_segments                 the array of segments to which translate
 * @param [in] from_element                the reference of the element in the from env
 * @return                                 the translated pointer
 */
static void *
pve_translate_ref (size_t active_segments_count,
                   size_t *segments_sizes,
                   size_t element_size,
                   size_t *from_to_segments_sorted_map,
                   void **from_segments,
                   void **to_segments,
                   void *from_element)
{
  assert(from_segments);
  assert(to_segments);
  assert(from_element);

  for (size_t k1 = active_segments_count; k1 > 0; k1--) {
    const size_t k = k1 - 1;
    const char *segment_base_adress = (char *) *(from_segments + k);
    const size_t segment_size = *(segments_sizes + k);
    if ((char *) from_element >= segment_base_adress) {
      const ptrdiff_t d = (char *) from_element - segment_base_adress;
      const size_t kk = *(from_to_segments_sorted_map + k);
      if (d > segment_size * element_size) return NULL;
      return ((char *) *(to_segments + kk)) + d;
    }
  }
  return NULL;
}


/**
 * @brief Populates the array of index mappings between sorted and unsorted segments.
 *
 * @param [out] segments_sorted_map   the result is stored in this array
 * @param [in]  active_segments_count the size of the active segments array
 * @param [in]  unsorted_segments     the unsorted segments array
 * @param [in]  sorted_segments       the sorted segments array
 */
static void
pve_populate_segments_sorted_map (size_t *const segments_sorted_map,
                                  const size_t active_segments_count,
                                  const void **const unsorted_segments,
                                  const void **const sorted_segments)
{
  for (size_t i = 0; i < active_segments_count; i++) {
    const void *const sorted_segment = *(sorted_segments + i);
    for (size_t j = 0; j < active_segments_count; j++) {
      const void *const unsorted_segment = *(unsorted_segments + j);
      if (unsorted_segment == sorted_segment) {
        segments_sorted_map[i] = j;
        break;
      }
    }
  }
}


/**
 * @brief Traverses the PVE structure.
 *
 * @details All lines and cells are touches once.
 *
 * @param [in] pve    a pointer to the principal variation environment
 * @param [in] stream the stream collecting the output
 */
static void
pve_tree_walker (const PVEnv *const pve,
                 FILE *const stream,
                 const bool compute_game_positions,
                 void (* begin_of_line_action) (const PVEnv *const pve,
                                                FILE *const stream,
                                                pve_row_t *const row),
                 void (* end_of_line_action) (const PVEnv *const pve,
                                              FILE *const stream,
                                              pve_row_t *const row),
                 void (* cell_action) (const PVEnv *const pve,
                                       FILE *const stream,
                                       pve_row_t *const row,
                                       const PVCell *const cell))
{
  assert(pve);
  assert(stream);

  /*
   * The board has 64 squares, each move can have a pass, so
   * keeping it simple, 128 is the theoretical upper bound.
   */
  static const size_t max_recursion_depth = 128;

  /* The stack of pve lines to be traversed, organized in a wrapper structure (pve_row_t). */
  pve_row_t row_stack[max_recursion_depth];

  /* Initializes the first element in the row array. */
  row_stack->line = pve->root_line;
  row_stack->dist_lev_0 = 0;
  if (compute_game_positions) game_position_x_copy(pve->root_game_position, &row_stack->gp);

  /* Sets the header pointer on the second element. */
  pve_row_t *row_stack_header = row_stack + 1;

  /* Loops until there are unprocessed lines on the stack. */
  while (row_stack_header > row_stack) {
    row_stack_header--;

    /*
     * Each iteration of the while loop works on a line.
     * The current line cannot be a pointer on the row stack, becouse it can be overwritten
     * during the iteration.
     * The current row copy is set at the beginning of each iteration and is then used safely.
     */
    pve_row_t current_row_copy;

    /* Copies the current row fields from the stack element just discarded.*/
    current_row_copy.line = row_stack_header->line;
    current_row_copy.dist_lev_0 = row_stack_header->dist_lev_0;
    current_row_copy.rel_distance = 0;
    if (compute_game_positions) game_position_x_copy(&row_stack_header->gp, &current_row_copy.gp);

    if (begin_of_line_action) begin_of_line_action(pve, stream, &current_row_copy);

    /* Cycles over the moves of the line. If the move has a variant it is pushed on the stack of rows. */
    for (const PVCell *c = *current_row_copy.line; c != NULL; c = c->next) {
      if (c->variant) {
        pve_row_t *const v = row_stack_header++;
        v->line = c->variant;
        v->dist_lev_0 = current_row_copy.dist_lev_0 + current_row_copy.rel_distance;
        if (compute_game_positions) game_position_x_copy(&current_row_copy.gp, &v->gp);
      }
      if (compute_game_positions) game_position_x_make_move(&current_row_copy.gp, c->move, &current_row_copy.gp);
      if (cell_action) cell_action(pve, stream, &current_row_copy, c);
      if (c->move != pass_move) current_row_copy.rel_distance++;
    }

    if (end_of_line_action) end_of_line_action(pve, stream, &current_row_copy);
  }
}

static void
pve_twa_eol (const PVEnv *const pve,
             FILE *const stream,
             pve_row_t *const row)
{
  fprintf(stream, "\n");
}

static void
pve_twa_bol (const PVEnv *const pve,
             FILE *const stream,
             pve_row_t *const row)
{
  for (size_t i = 0; i < row->dist_lev_0; i++) {
    fprintf(stream, "    ");
  }
}

static void
pve_twa_cell (const PVEnv *const pve,
              FILE *const stream,
              pve_row_t *const row,
              const PVCell *const cell)
{
  if (cell->move == pass_move) return;
  fprintf(stream, "%s", square_as_move_to_string(cell->move));
  const PVCell *const next = cell->next;
  if (next) {
    if (next->move == pass_move) {
      if (cell->variant) fprintf(stream, ".-");
      else fprintf(stream, "--");
    } else {
      if (cell->variant) fprintf(stream, ". ");
      else fprintf(stream, "  ");
    }
  } else {
    if (cell->variant) fprintf(stream, ".");
  }
}

static void
pve_twa_cell_csv (const PVEnv *const pve,
                  FILE *const stream,
                  pve_row_t *const row,
                  const PVCell *const cell)
{
  int64_t line_id = (int64_t) row->line;
  int64_t cell_id = (int64_t) cell;
  int64_t variant_id = (int64_t) cell->variant;
  int64_t next_id = (int64_t) cell->next;
  unsigned int head_level = row->dist_lev_0;
  unsigned int rel_level = row->rel_distance;
  uint64_t gp_hash = game_position_x_hash(&row->gp);
  uint64_t gp_b = row->gp.blacks;
  uint64_t gp_w = row->gp.whites;
  Player gp_p = row->gp.player;
  fprintf(stream, "%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%2s;%2u;%2u;%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%1d\n",
          line_id,
          cell_id,
          variant_id,
          next_id,
          square_as_move_to_string(cell->move),
          head_level,
          rel_level,
          gp_hash,
          gp_b,
          gp_w,
          gp_p);
}

/**
 * @endcond
 */

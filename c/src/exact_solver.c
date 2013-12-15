/**
 * @file
 *
 * @brief Exact solver module implementation.
 * @details It searches the end of the game for an exact outcome..
 *
 * @par exact_solver.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013 Roberto Corradini. All rights reserved.
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "exact_solver.h"
#include "board.h"
#include "bit_works.h"
#include "advanced_square_set.h"

/**
 * @brief Elements of a doubly linked list that collects moves.
 *
 * Elements have the square and mobility fields.
 */
typedef struct MoveListElement_ {
  Square                   sq;           /**< @brief The square field. */
  uint8                    mobility;     /**< @brief The mobility field. */
  struct MoveListElement_ *pred;         /**< @brief A pointer to the predecesor element. */
  struct MoveListElement_ *succ;         /**< @brief A pointer to the successor element. */
} MoveListElement;

/**
 * @brief Move list, having head, tail, and elements fields.
 *
 * Head and tail are not part of the list.
 */
typedef struct {
  MoveListElement elements[64];
  MoveListElement head;
  MoveListElement tail;
} MoveList;



/*
 * Prototypes for internal functions.
 */

static void
sort_moves_by_mobility_count (      MoveList     *       move_list,
                              const GamePosition * const gp);

static SearchNode *
game_position_solve_impl (      ExactSolution * const result,
                          const GamePosition  * const gp,
                          const int                   achievable,
                          const int                   cutoff);

static int
final_value (const GamePosition * const gp);

static void
move_list_init (MoveList *ml);

static gchar *
move_list_print (MoveList *ml);



/*
 * Internal variables and constants.
 */

static const uint64 _legal_moves_priority_mask[] = {
  0xFFFFFFFFFFFFFFFF,0,0,0,0,0,0,0,0,0
};

static const uint64 legal_moves_priority_mask[] = {
  /* D4, E4, E5, D5 */                 0x0000001818000000,
  /* A1, H1, H8, A8 */                 0x8100000000000081,
  /* C1, F1, F8, C8, A3, H3, H6, A6 */ 0x2400810000810024,
  /* C3, F3, F6, C6 */                 0x0000240000240000,
  /* D1, E1, E8, D8, A4, H4, H5, A5 */ 0x1800008181000018,
  /* D3, E3, E6, D6, C4, F4, F5, C5 */ 0x0000182424180000,
  /* D2, E2, E7, D7, B4, G4, G5, B5 */ 0x0018004242001800,
  /* C2, F2, F7, C7, B3, G3, G6, B6 */ 0x0024420000422400,
  /* B1, G1, G8, B8, A2, H2, H7, A7 */ 0x4281000000008142,
  /* B2, G2, G7, B7 */                 0x0042000000004200
};


/*******************************************************/
/* Function implementations for the SearchNode entity. */ 
/*******************************************************/

/**
 * @brief Search node structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * search node structure is not `NULL`.
 *
 * @return a pointer to a new search node structure
 */
SearchNode *
search_node_new (const Square move, const int value)
{
  SearchNode * sn;
  static const size_t size_of_search_node = sizeof(SearchNode);

  sn = (SearchNode*) malloc(size_of_search_node);
  g_assert(sn);

  sn->move = move;
  sn->value = value;

  return sn;
}

/**
 * @brief Search node structure destructor.
 *
 * @invariant Parameter `sn` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] sn the pointer to be deallocated
 * @return        always the NULL pointer
 */
SearchNode *
search_node_free (SearchNode *sn)
{
  g_assert(sn);

  free(sn);
  sn = NULL;

  return sn;
}

/**
 * @brief To de documented.
 *
 * Details to be documented.
 */
SearchNode *
search_node_negated (SearchNode *sn)
{
  SearchNode *result;
  result = search_node_new(sn->move, -sn->value);
  search_node_free(sn);
  return result;
}



/**********************************************************/
/* Function implementations for the ExactSolution entity. */ 
/**********************************************************/

/**
 * @brief Exact solution structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * exact solution structure is not `NULL`.
 *
 * @return a pointer to a new exact solution structure
 */
ExactSolution *
exact_solution_new (void)
{
  ExactSolution * es;
  static const size_t size_of_exact_solution = sizeof(ExactSolution);

  es = (ExactSolution*) malloc(size_of_exact_solution);
  g_assert(es);

  es->solved_game_position = NULL;
  es->outcome = 65;
  for (int i = 0; i < 60; i++) {
    es->principal_variation[i] = -1;
  }
  es->final_board = NULL;
  es->node_count = 0;
  es->leaf_count = 0;

  return es;
}

/**
 * @brief Exact solution structure destructor.
 *
 * @invariant Parameter `es` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] es the pointer to be deallocated
 * @return        always the NULL pointer
 */
ExactSolution *
exact_solution_free (ExactSolution *es)
{
  g_assert(es);

  if (es->solved_game_position) game_position_free(es->solved_game_position);
  if (es->final_board) board_free(es->final_board);

  free(es);
  es = NULL;

  return es;
}

/**
 * @brief Returns a formatted string describing the exact solution structure.
 *
 * @todo MUST BE COMPLETED!
 *
 * The returned string has a dynamic extent set by a call to malloc. It must then properly
 * garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameter `es` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] es a pointer to the exact solution structure
 * @return        a string being a representation of the structure
 */
gchar *
exact_solution_print (const ExactSolution * const es)
{
  g_assert(es);

  gchar *es_to_string;

  es_to_string = game_position_print(es->solved_game_position);

  return es_to_string;
}



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

ExactSolution *
game_position_solve (const GamePosition * const root)
{
  ExactSolution *result; 
  SearchNode    *sn;

  result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  sn = game_position_solve_impl(result, result->solved_game_position, -64, +64);

  if (sn) {
    result->principal_variation[0] = sn->move;
    result->outcome = sn->value;
  }
  sn = search_node_free(sn);

  return result;
}



/*
 * Internal functions.
 */

static void
sort_moves_by_mobility_count (MoveList *move_list, const GamePosition * const gp)
{
  MoveListElement *curr = NULL;
  int move_index = 0;
  const SquareSet moves = game_position_legal_moves(gp);
  SquareSet moves_to_search = moves;
  for (int i = 0; i < 10; i++) {
    moves_to_search = legal_moves_priority_mask[i] & moves;
    while (moves_to_search) {
      curr = &move_list->elements[move_index];
      const Square move = bit_works_bitscanLS1B_64(moves_to_search);
      moves_to_search &= ~(1ULL << move);
      GamePosition *next_gp = game_position_make_move(gp, move);
      const SquareSet next_moves = game_position_legal_moves(next_gp);
      next_gp = game_position_free(next_gp);
      const int next_move_count = bit_works_popcount(next_moves);
      curr->sq = move;
      curr->mobility = next_move_count;
      for (MoveListElement *element = move_list->head.succ; element != NULL; element = element->succ) {
        if (curr->mobility < element->mobility) { /* Insert current before element. */
          MoveListElement *left  = element->pred;
          MoveListElement *right = element;
          curr->pred  = left;
          curr->succ  = right;
          left->succ  = curr;
          right->pred = curr;
          goto out;
        }
      }
    out:
      move_index++;
    }
  }
  return;
}

static SearchNode *
game_position_solve_impl (      ExactSolution * const result,
                          const GamePosition  * const gp,
                          const int                   achievable,
                          const int                   cutoff)
{
  SearchNode *node;
  SearchNode *node2;

  node  = NULL;
  node2 = NULL;
  result->node_count++;

  // - Debug On
  // SquareSet empties = board_empties(gp->board);
  // uint64 hash = game_position_hash(gp);
  // - Debug Off

  const SquareSet moves = game_position_legal_moves(gp);
  if (0ULL == moves) {
    // - Debug On
    // p: player, e: empties, ml: ordere legal moves
    /*
    printf("p=%c, e=%46s, ml=%20s, a-b=[%+2d %+2d] [%016llx];\n",
           (gp->player == BLACK_PLAYER) ? 'B' : 'W',
           square_set_print_as_moves(empties),
           "",
           achievable, cutoff,
           hash);
    */
    // - Debug Off
    GamePosition *flipped_players = game_position_pass(gp);
    if (game_position_has_any_legal_move(flipped_players)) {
      node = search_node_negated(game_position_solve_impl(result, flipped_players, -cutoff, -achievable));
    } else {
      result->leaf_count++;
      node = search_node_new((Square) -1, final_value(gp));
      // - Debug On
      /*
      printf("p=%c, leaf_value=%+2d;\n",
             (gp->player == BLACK_PLAYER) ? 'B' : 'W',
             node->value);
      */
      // - Debug Off
    }
    flipped_players = game_position_free(flipped_players);
  } else {
    MoveList move_list;
    move_list_init(&move_list);
    sort_moves_by_mobility_count(&move_list, gp);
    // - Debug On
    /*
    gchar *move_list_to_s = move_list_print(&move_list);
    printf("p=%c, e=%46s, ml=%20s, a-b=[%+3d %+3d] [%016llx];\n",
           (gp->player == BLACK_PLAYER) ? 'B' : 'W',
           square_set_print_as_moves(empties),
           move_list_to_s,
           achievable, cutoff,
           hash);
    g_free(move_list_to_s);
    */
    // - Debug Off
    for (MoveListElement *element = move_list.head.succ; element != &move_list.tail; element = element->succ) {
      const Square move = element->sq;
      if (!node) node = search_node_new(move, achievable);
      GamePosition *gp2 = game_position_make_move(gp, move);
      node2 = search_node_negated(game_position_solve_impl(result, gp2, -cutoff, -node->value));
      gp2 = game_position_free(gp2);
      if (node2->value > node->value) {
        search_node_free(node);
        node = node2;
        node->move = move;
        node2 = NULL;
        if (node->value >= cutoff) { /* printf("-cut-"); */ goto out; }
      } else {
        node2 = search_node_free(node2);
      }
    }
  }
  // printf("     ");
 out:
  ;
  //gchar* move_to_s = square_to_string(node->move);
  //printf("return node: n.move=%3s n.value=%+3d [%016llx]\n", move_to_s, node->value, hash);
  //g_free(move_to_s);
  return node;
}

static int
final_value (const GamePosition * const gp)
{
  return board_count_diff_winner_get_empties(gp->board, gp->player);
}

static void
move_list_init (MoveList *ml)
{
  for (int i = 0; i < 64; i++) {
    ml->elements[i].sq = -1;
    ml->elements[i].mobility = -1;
    ml->elements[i].pred = NULL;
    ml->elements[i].succ = NULL;
  }
  ml->head.sq = -1;
  ml->head.mobility = -1;
  ml->head.pred = NULL;
  ml->head.succ = &ml->tail;
  ml->tail.sq = -1;
  ml->tail.mobility = -1;
  ml->tail.pred = &ml->head;
  ml->tail.succ = NULL;
}

static gchar *
move_list_print (MoveList *ml)
{
  gchar *ml_to_s;
  gchar space[] = {' ', '\0'};

  static const size_t size_of_ml_to_s = (3 * 64 + 1) * sizeof(gchar);
  ml_to_s = (gchar*) malloc(size_of_ml_to_s);

  *ml_to_s = '\0';
  gchar *cursor = ml_to_s;
  for (MoveListElement *element = ml->head.succ; element != &ml->tail; element = element->succ) {
    gchar *move_to_s = square_to_string(element->sq);
    cursor = g_stpcpy(cursor, move_to_s);
    g_free(move_to_s);
    cursor = g_stpcpy(cursor, &space[0]);
  }
  if (ml_to_s != cursor) {
    *--cursor = '\0';
  }
  return ml_to_s;
}

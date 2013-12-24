/**
 * @file
 *
 * @brief Improved fast endgame solver.
 * @details Solver derived from the Gunnar Andersson work.
 *
 * @par improved_fast_endgame_solver.c
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


#include <stdio.h>
#include <stdlib.h>

#include "improved_fast_endgame_solver.h"

#define GAME_TREE_DEBUG

/**
 * @brief An empty list collects a set of ordered squares.
 *
 * Details to be documented.
 */
typedef struct EmList_ {
  uint8           square;     /**< @brief One square on the board. */
  uint64          hole_id;    /**< @brief Id of the hole to which the square belongs to. */
  struct EmList_ *pred;       /**< @brief Predecessor element, or NULL if missing. */
  struct EmList_ *succ;       /**< @brief Successor element, or NULL if missing. */
} EmList;

/**
 * @brief A node in the search tree.
 *
 * Details to be documented.
 */
typedef struct {
  uint8 square;    /**< @brief One square on the board. */
  sint8 value;     /**< @brief The game value of moving into the square. */
} Node;


/*
 * Prototypes for internal functions.
 */

inline static void
directional_flips (uint8 *sq, int inc, int color, int oppcol);

static int
do_flips (uint8 *board, int sqnum, int color, int oppcol);

inline static int
ct_directional_flips (uint8 *sq, int inc, int color, int oppcol);

static int
count_flips (uint8 *board, int sqnum, int color, int oppcol);

inline static int
any_directional_flips (uint8 *sq, int inc, int color, int oppcol);

static int
any_flips (uint8 *board, int sqnum, int color, int oppcol);

inline static void
undo_flips (int flip_count, int oppcol);

inline static uint64
minu (uint64 a, uint64 b);

static int
count_mobility (uint8 *board, int color);

static void
prepare_to_solve (uint8 *board);

inline static Node
no_parity_end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
                     int color, int empties, int discdiff, int prevmove);

static Node
parity_end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
                  int color, int empties, int discdiff, int prevmove);

static Node
fastest_first_end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
                         int color, int empties, int discdiff, int prevmove);

static Node
end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
           int color, int empties, int discdiff, int prevmove);

static void
game_position_to_ifes_board_ (const GamePosition * const gp, int *p_emp, int *p_wc, int *p_bc);

static char *
ifes_square_to_string (const int sq);

static Square
ifes_square_to_square (const int sq);

inline static Node
init_node (void);

inline static Node
node_negate (Node n);

inline static int
opponent_color (int color);

static char *
square_list_print (const EmList * const sl);



/*
 * Internal constants.
 */

/*
 * This is the best/worst case board value.
 * Any value greather than 64 can be adopted.
 */
static const sint8 infinity = 65;

/*
 * The selection of the variant of end end_solver() function follows these rules.
 *
 * Let's take as an example the two values: use_parity = 4, and fastest_first = 7.
 *
 * And lets's drow the empties axis:
 *
 * 0         1         2
 * 012345678901234567890123456789..... -> [empties]
 *     |  |
 *     |  fastest_first
 *     |
 *     use_parity
 *
 * When empties are less than or equal to use_parity, then no_parity_end_solve() is used,
 * in our case the range is [0...4].
 * Within the range use_parity + 1 and fastest_first, in the example [5...7], the
 * parity_end_solve() is applied.
 * When empties is greather then fastest_first, range [8...], fastest_first_end_solve()
 * is selected.
 *
 * Corner cases must be investigation: the standard case is (4,7), where 4 is assigned to
 * use_parity, and 7 is assigned to fastest_first.
 *
 * (a)
 *   use_parity = 0;
 *   fastest_first > 0;
 *   Test case (0,7). It runs fine, 36% more time is consumed compared with the standard case.
 *   When empties is 0 no_parity_end_solve() is used.
 *   Range [1...7] parity_end_solve() is called.
 *   Range [8...] fastest_first_end_solve is selected.
 *
 * (b)
 *   use_parity = 0;
 *   fastest_first = 0;
 *   Test case (0,0). It runs fine, 127% more time is consumed compared with the standard case.
 *   When empties is 0 parity_end_solve() is used.
 *   Range [1...] fastest_first_end_solve is selected.
 *
 * (c)
 *   use_parity > 0;
 *   fastest_first = use_parity;
 *   Test case (4,4). It runs fine, 23% more time is consumed compared with the standard case.
 *   [0...3] no_parity_end_solve().
 *   [4]     parity_end_solve().
 *   [5...]  fastest_first_end_solve().
 *   
 * (d)
 *   use_parity > 0;
 *   fastest_first = 0;
 *   Test case (4,0). It runs fine, 124% more time is consumed compared with the standard case.
 *   When empties is 0 parity_end_solve() is used.
 *   Range [1...] fastest_first_end_solve is selected.
 *   
 * (e)
 *   use_parity > 0;
 *   fastest_first != 0 && fastest_first < use_parity;
 *   Test case (6,4). It runs fine, 25% more time is consumed compared with the standard case.
 *   [0...3] no_parity_end_solve().
 *   [4]     parity_end_solve().
 *   [5...]  fastest_first_end_solve().
 */

/*
 * The parameter use_parity. See above for explanation.
 *
 * Best setting so far is 4.
 */
static const uint8 use_parity = 0;

/*
 * The parameter fastest_first. See above for explanation.
 *
 * Best setting so far is 7.
 */
static const uint8 fastest_first = 0;

/*
 * The 8 legal directions, plus no direction an ninth value.
 */
static const sint8 dir_inc[] = {1, -1, 8, -8, 9, -9, 10, -10, 0};

/*
 * Fixed square ordering.
 */
static const uint8 _worst_to_best[64] =
{
  /*B2*/      20, 25, 65, 70,
  /*B1*/      11, 16, 19, 26, 64, 71, 74, 79,
  /*C2*/      21, 24, 29, 34, 56, 61, 66, 69,
  /*D2*/      22, 23, 38, 43, 47, 52, 67, 68,
  /*D3*/      31, 32, 39, 42, 48, 51, 58, 59,
  /*D1*/      13, 14, 37, 44, 46, 53, 76, 77,
  /*C3*/      30, 33, 57, 60,
  /*C1*/      12, 15, 28, 35, 55, 62, 75, 78,
  /*A1*/      10, 17, 73, 80, 
  /*D4*/      40, 41, 49, 50
};

static const uint8 worst_to_best[64] =
{
  /*B2*/      70, 65, 25, 20,
  /*B1*/      79, 74, 71, 64, 26, 19, 16, 11,
  /*C2*/      69, 66, 61, 56, 34, 29, 24, 21,
  /*D2*/      68, 67, 52, 47, 43, 38, 23, 22,
  /*D3*/      59, 58, 51, 48, 42, 39, 32, 31,
  /*D1*/      77, 76, 53, 46, 44, 37, 14, 13,
  /*C3*/      60, 57, 33, 30,
  /*C1*/      78, 75, 62, 55, 35, 28, 15, 12,
  /*A1*/      80, 73, 17, 10, 
  /*D4*/      50, 49, 41, 40
};

/*
 * The bit mask for direction i is 1<<i
 *
 * Bit masks for the directions squares can flip in,
 * for example flipping_dir_mask_table[10]=81=64+16+1=(1<<6)+(1<<4)+(1<<0)
 * hence square 10 (A1) can flip in directions dir_inc[0]=1,
 * dir_inc[4]=9, and dir_inc[6]=10:
 */
static const uint8 flipping_dir_mask_table[91] = {
  0,   0,   0,   0,   0,   0,   0,   0,   0,
  0,  81,  81,  87,  87,  87,  87,  22,  22,
  0,  81,  81,  87,  87,  87,  87,  22,  22,
  0, 121, 121, 255, 255, 255, 255, 182, 182,
  0, 121, 121, 255, 255, 255, 255, 182, 182,
  0, 121, 121, 255, 255, 255, 255, 182, 182,
  0, 121, 121, 255, 255, 255, 255, 182, 182,
  0,  41,  41, 171, 171, 171, 171, 162, 162,
  0,  41,  41, 171, 171, 171, 171, 162, 162,
  0,   0,   0,   0,   0,   0,   0,   0,   0, 0
};



/*
 * Internal variables.
 */

#ifdef GAME_TREE_DEBUG
static uint64 call_count = 0;
static FILE *game_tree_debug_file = NULL;
#endif

/*
 * Inside this fast endgame solver, the board is represented by
 * a 1D array of 91 uint8s board[0..90]:
 * ddddddddd
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx       where A1 is board[10], H8 is board[80].
 * dxxxxxxxx       square(a,b) = board[10+a+b*9] for 0 <= a, b <= 7.
 * dddddddddd   
 * where d (dummy) squares contain DUMMY, x are EMPTY, BLACK, or WHITE:
 */
static uint8 board[91];

/*
 * Also there is a doubly linked list of the empty squares.
 * em_head points to the first empty square in the list (or NULL if none).
 * The list in maintained in a fixed best-to-worst order.
 */
static EmList em_head, ems[64];

/*
 * Also, and finally, each empty square knows the region it is in
 * and knows the directions you can flip in via some bit masks.
 * There are up to 32 regions. The parities of the regions are in
 * the region_parity bit vector:
 */
static uint64 region_parity;

/*
 * Stores the pointers to the board element that are flipped
 * by each move during the search tree expansion.
 *
 * An upper bound of the size of the stack is:
 * number_of_moves_in_a_game * max_flips_per_move = 60 * (3*6) = 1080.
 * But, first move flips always one discs (not sixteen), second the same,
 * so in a game 1024 is a trusted upper bound.
 */
static uint8 *global_flip_stack[1024];

/*
 * A global pointer to one element of `global_flip_stack`,
 * it identify the next empty position in the stack.
 */
static uint8 **flip_stack = &(global_flip_stack[0]);



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

/**
 * @brief Solves the game position defined by the `root` parameter,
 *        applying the ifes solver.
 *
 * The "Improved Fast Endgame Solver" is described by the module documentation.
 *
 * @invariant Parameters `root` must be not `NULL`.
 * The invariants are guarded by assertions.
 *
 * @param root the game position to be solved
 * @return     the exact solution is the collector for results
 */
ExactSolution *
game_position_ifes_solve (const GamePosition * const root)
{
  ExactSolution *result;    /* The solution structure returned by the function. */
  int            emp;       /* Empty discs count. */
  int            wc, bc;    /* White and Black discs count. */
  int            discdiff;  /* Disc difference between player and opponent. */
  Node           n;         /* Best node returned by the search. */

  g_assert(root);

#ifdef GAME_TREE_DEBUG
  game_tree_debug_file = fopen("ifes_log.csv", "w");
  fprintf(game_tree_debug_file, "%s;%s;%s;%s\n", "COUNTER", "GAME_POSITION_HASH", "GAME_POSITION", "EMPTY_COUNT");
#endif

  result = exact_solution_new();
  result->solved_game_position = game_position_clone(root);

  game_position_to_ifes_board_(root, &emp, &wc, &bc);

  IFES_SquareState player = game_position_get_ifes_player(root);

  discdiff = player == IFES_BLACK ? bc - wc : wc - bc;

  prepare_to_solve(board);

  /** Debug info **/
  if (FALSE) {
    printf("\nEmpty Square Doubly linked List debug info:\n");
    printf("em_head: address=%p [square=%2d (%s), hole_id=%llu] pred=%p succ=%p\n",
           (void*) &em_head, em_head.square, ifes_square_to_string(em_head.square), em_head.hole_id,
           (void*) em_head.pred, (void*) em_head.succ);
    for (int k = 0; k < 64; k++) {
      if (ems[k].square != 0)
        printf("ems[%2d]: address=%p [square=%2d (%s), hole_id=%llu] pred=%p succ=%p\n",
               k, (void*) &ems[k], ems[k].square, ifes_square_to_string(ems[k].square), ems[k].hole_id,
               (void*) ems[k].pred, (void*) ems[k].succ);
    }
    printf("region_parity=%llu\n", region_parity);
    printf("\n");
    printf("use_parity=%d. fastest_first=%d.\n",
           use_parity, fastest_first);
  }
  /** **/

  n = end_solve(result, board, -64, 64, player, emp, discdiff, 1);

  result->outcome = n.value;
  result->principal_variation[0] = ifes_square_to_square(n.square);

#ifdef GAME_TREE_DEBUG
  fclose(game_tree_debug_file);
#endif

  return result;
}

/**
 * @brief Prepares the board structure and return it, the empties, blacks, and whites counts.
 *
 * @param [in]  gp    the given game position
 * @param [out] b     a pointer to the board array
 * @param [out] p_emp a pointer to the empties count
 * @param [out] p_wc  a pointer to the whites count
 * @param [out] p_bc  a pointer to the blacks count
 *
 */
void
game_position_to_ifes_board (const GamePosition * const gp, uint8 *b, int *p_emp, int *p_wc, int *p_bc)
{
  /* Sets to IFES_DUMMY all the board squares. */
  for (int board_index = 0; board_index < 91; board_index++) b[board_index] = IFES_DUMMY;

  int emp = 0;
  int wc  = 0;
  int bc  = 0;
  for (int square_index = 0; square_index < 64; square_index++) {
    const int column = square_index & 7;
    const int row = (square_index >> 3) & 7;
    const int board_index = column + 10 + 9 * row;
    if      ((gp->board->whites & (1ULL << square_index)) != 0ULL) { b[board_index] = IFES_WHITE; wc++; }
    else if ((gp->board->blacks & (1ULL << square_index)) != 0ULL) { b[board_index] = IFES_BLACK; bc++; }
    else                                                           { b[board_index] = IFES_EMPTY; emp++; }
  }

  /* Sets the empties, whites, and blacks counts. */
  *p_emp = emp;
  *p_wc  = wc;
  *p_bc  = bc;
}

/**
 * @brief Returns the ifes player from the game position.
 *
 * @param [in] gp the game position
 * @return        the player having the move
 */
IFES_SquareState
game_position_get_ifes_player (const GamePosition * const gp)
{
  IFES_SquareState ifes_player;
  ifes_player = gp->player == BLACK_PLAYER ? IFES_BLACK : IFES_WHITE;
  return ifes_player;
}

/**
 * MUST BE DEVELOPPED !!!
 */
GamePosition *
ifes_game_position_translation (uint8 *board, int color)
{
  SquareSet blacks = 0ULL;
  SquareSet whites = 0ULL;
  Player    p      = (color == IFES_WHITE) ? WHITE_PLAYER : BLACK_PLAYER;

  for (int i = 0; i < 64; i++) {
    const int col = i % 8;
    const int row = i / 8;
    const int index = 10 + col + (row * 9);
    const SquareSet mask = 1ULL << i;
    switch (board[index]) {
    case IFES_WHITE:
      whites |= mask;
      break;
    case IFES_BLACK:
      blacks |= mask;
      break;
    default:
      break;
    }
  }

  return game_position_new(board_new(blacks, whites), p);
}



/*
 * Internal functions.
 */

/**
 * @brief Prepares the board global structure and return the empties, blacks, and whites counts.
 *
 * @param [in]  gp    the given game position
 * @param [out] p_emp a pointer to the empties count
 * @param [out] p_wc  a pointer to the whites count
 * @param [out] p_bc  a pointer to the blacks count
 *
 */
static void
game_position_to_ifes_board_ (const GamePosition * const gp, int *p_emp, int *p_wc, int *p_bc)
{
  /* Sets to IFES_DUMMY all the board squares. */
  for (int board_index = 0; board_index < 91; board_index++) board[board_index] = IFES_DUMMY;

  int emp = 0;
  int wc  = 0;
  int bc  = 0;
  for (int square_index = 0; square_index < 64; square_index++) {
    const int column = square_index & 7;
    const int row = (square_index >> 3) & 7;
    const int board_index = column + 10 + 9 * row;
    if      ((gp->board->whites & (1ULL << square_index)) != 0ULL) { board[board_index] = IFES_WHITE; wc++; }
    else if ((gp->board->blacks & (1ULL << square_index)) != 0ULL) { board[board_index] = IFES_BLACK; bc++; }
    else                                                           { board[board_index] = IFES_EMPTY; emp++; }
  }

  /* Sets the empties, whites, and blacks counts. */
  *p_emp = emp;
  *p_wc  = wc;
  *p_bc  = bc;
}

/**
 * @brief Translates the ifes square into an enum square.
 *
 * @param [in] sq the ifes square
 * @return        the translated square enum
 */
static Square
ifes_square_to_square (const int sq)
{
  const uint8 col = (sq % 9) - 1;
  const uint8 row = (sq / 9) - 1;
  return row * 8 + col;
}

/**
 * @brief Returns a string representation for the given square.
 *
 * The returned string must be freed when no longer used by the client function.
 *
 * @param [in] sq the square
 * @return        a string representation for the square
 */
static char *
ifes_square_to_string (const int sq)
{
  char *symbol;

  static const size_t size_of_square_to_string = 3 * sizeof(char);
  symbol = (char*) malloc(size_of_square_to_string);

  if (sq < 9 || sq > 80 || (sq % 9) == 0) {
    *symbol       = 'N';
    *(symbol + 1) = 'A';
    *(symbol + 2) = '\0';
  } else {
    const uint8 col = (sq % 9) - 1;
    const uint8 row = (sq / 9) - 1;
    *symbol = 'A' + col;
    *(symbol + 1) = '1' + row;
    *(symbol + 2) = '\0';
  }

  return symbol;
}

/**
 * @brief Executes board flips from a square `sq` in the `inc` direction.
 *
 * @param [in] sq     a pointer to the square the move is to
 * @param [in] inc    the increment to go in some direction
 * @param [in] color  the color of the mover
 * @param [in] oppcol the opposite color
 */
inline static void
directional_flips (uint8 *sq, int inc, int color, int oppcol)
{
  uint8 *pt = sq + inc;
  if (*pt == oppcol) {
    pt += inc;
    if (*pt == oppcol) {
      pt += inc;
      if (*pt == oppcol) {
        pt += inc;
        if (*pt == oppcol) {
          pt += inc;
          if (*pt == oppcol) {
            pt += inc;
            if (*pt == oppcol) {
              pt += inc;
            }
          }
        }
      }
    }
    if (*pt == color) {
      pt -= inc;
      do {
        *pt = color;
        *(flip_stack++) = pt;
        pt -= inc;
      } while (pt != sq);
    }
  }
}

/**
 * @brief Does all flips involved in making a move to square `sqnum` of board,
 * and return their count.
 *
 * If the move is not legal the returned value is zero.
 *
 * @param [in,out] board  a pointer to the board to modify
 * @param [in]     sqnum  move square number 
 * @param [in]     color  player color
 * @param [in]     oppcol opponent color
 * @return                the flip count
 */
static int
do_flips (uint8 *board, int sqnum, int color, int oppcol)
{
  const uint8 flipping_dir_mask = flipping_dir_mask_table[sqnum];
  uint8 **previous_flip_stack = flip_stack;
  uint8 *sq = sqnum + board;

  if (flipping_dir_mask & (1 << 7))
    directional_flips(sq, dir_inc[7], color, oppcol);
  if (flipping_dir_mask & (1 << 6))
    directional_flips(sq, dir_inc[6], color, oppcol);
  if (flipping_dir_mask & (1 << 5))
    directional_flips(sq, dir_inc[5], color, oppcol);
  if (flipping_dir_mask & (1 << 4))
    directional_flips(sq, dir_inc[4], color, oppcol);
  if (flipping_dir_mask & (1 << 3))
    directional_flips(sq, dir_inc[3], color, oppcol);
  if (flipping_dir_mask & (1 << 2))
    directional_flips(sq, dir_inc[2], color, oppcol);
  if (flipping_dir_mask & (1 << 1))
    directional_flips(sq, dir_inc[1], color, oppcol);
  if (flipping_dir_mask & (1 << 0))
    directional_flips(sq, dir_inc[0], color, oppcol);

  return flip_stack - previous_flip_stack;
}

/**
 * @brief Used by function count_flips().
 *
 * @param [in] sq     the square where to move
 * @param [in] inc    the increment to give to square to move to the neighbor
 * @param [in] color  player's color
 * @param [in] oppcol opponent's color
 * @return            the count of flipped discs in this direction
 */
inline static int
ct_directional_flips (uint8 *sq, int inc, int color, int oppcol)
{
  uint8 *pt = sq + inc;
  if (*pt == oppcol) {
    int count = 1;
    pt += inc;
    if (*pt == oppcol) {
      count++;                /* count = 2 */
      pt += inc;
      if (*pt == oppcol) {
        count++;              /* count = 3 */
        pt += inc;
        if (*pt == oppcol) {
          count++;            /* count = 4 */
          pt += inc;
          if (*pt == oppcol) {
            count++;          /* count = 5 */
            pt += inc;
            if (*pt == oppcol) {
              count++;        /* count = 6 */
              pt += inc;
            }
          }
        }
      }
    }
    if (*pt == color) return count;
  }
  return 0;
}

/**
 * @brief Counts the number of disc flips generated by the move.
 *
 * @param [in] board  the game board
 * @param [in] sqnum  the move
 * @param [in] color  player's color
 * @param [in] oppcol opponent's color
 * @return            the flip count
 */
static int
count_flips (uint8 *board, int sqnum, int color, int oppcol)
{
  int ct = 0;
  const uint8 flipping_dir_mask = flipping_dir_mask_table[sqnum];
  uint8 *sq = sqnum + board;

  if (flipping_dir_mask & (1 << 7))
    ct += ct_directional_flips(sq, dir_inc[7], color, oppcol);
  if (flipping_dir_mask & (1 << 6))
    ct += ct_directional_flips(sq, dir_inc[6], color, oppcol);
  if (flipping_dir_mask & (1 << 5))
    ct += ct_directional_flips(sq, dir_inc[5], color, oppcol);
  if (flipping_dir_mask & (1 << 4))
    ct += ct_directional_flips(sq, dir_inc[4], color, oppcol);
  if (flipping_dir_mask & (1 << 3))
    ct += ct_directional_flips(sq, dir_inc[3], color, oppcol);
  if (flipping_dir_mask & (1 << 2))
    ct += ct_directional_flips(sq, dir_inc[2], color, oppcol);
  if (flipping_dir_mask & (1 << 1))
    ct += ct_directional_flips(sq, dir_inc[1], color, oppcol);
  if (flipping_dir_mask & (1 << 0))
    ct += ct_directional_flips(sq, dir_inc[0], color, oppcol);

  return ct;
}

/**
 * @brief Returns if the move generates flips in a given direction.
 *
 * Sometimes we only want to know if a move is legal, not how
 * many discs it flips.
 *
 * @param [in] sq     a square pointer
 * @param [in] inc    a given direction
 * @param [in] color  player color
 * @param [in] oppcol opponent color
 * @return            `1` if there are legal flips, `0` otherwise
 */
inline static int
any_directional_flips (uint8 *sq, int inc, int color, int oppcol)
{
  uint8 *pt = sq + inc;
  if (*pt == oppcol) {
    pt += inc;
    if (*pt == oppcol) {
      pt += inc;
      if (*pt == oppcol) {
        pt += inc;
        if (*pt == oppcol) {
          pt += inc;
          if (*pt == oppcol) {
            pt += inc;
            if (*pt == oppcol) {
              pt += inc;
            }
          }
        }
      }
    }
    if (*pt == color) return 1;
  }
  return 0;
}

/**
 * @brief Verify if the move is legal.
 *
 * Parameter `sqnum` must be an empty square.
 *
 * @param [in] board  the game board
 * @param [in] sqnum  the move
 * @param [in] color  the player going to move
 * @param [in] oppcol the opponent
 * @return            the number of flipped discs
 */
static int
any_flips (uint8 *board, int sqnum, int color, int oppcol)
{
  int any = 0;
  const uint8 flipping_dir_mask = flipping_dir_mask_table[sqnum];
  uint8 *sq = sqnum + board;

  /*
   * Unrolling the loop brings a quite sensible gain.
   *
   * for (int dir = 0; dir < 8; dir++) {
   *   if (flipping_dir_mask & (1 < dir))
   *     any += any_directional_flips(sq, dir_inc[dir], color, oppcol);
   *  }
  */

  if (flipping_dir_mask & (1 << 7))
    any += any_directional_flips(sq, dir_inc[7], color, oppcol);
  if (flipping_dir_mask & (1 << 6))
    any += any_directional_flips(sq, dir_inc[6], color, oppcol);
  if (flipping_dir_mask & (1 << 5))
    any += any_directional_flips(sq, dir_inc[5], color, oppcol);
  if (flipping_dir_mask & (1 << 4))
    any += any_directional_flips(sq, dir_inc[4], color, oppcol);
  if (flipping_dir_mask & (1 << 3))
    any += any_directional_flips(sq, dir_inc[3], color, oppcol);
  if (flipping_dir_mask & (1 << 2))
    any += any_directional_flips(sq, dir_inc[2], color, oppcol);
  if (flipping_dir_mask & (1 << 1))
    any += any_directional_flips(sq, dir_inc[1], color, oppcol);
  if (flipping_dir_mask & (1 << 0))
    any += any_directional_flips(sq, dir_inc[0], color, oppcol);

  return any;
}

/**
 * @brief Call this function right after `flip_count = do_flips()` to undo those flips!
 *
 * @param [in] flip_count number of disc flipped
 * @param [in] oppcol     opponent color
 */
inline static void
undo_flips (int flip_count, int oppcol)
{
  while (flip_count) { flip_count--; *(*(--flip_stack)) = oppcol; }
}

/**
 * @brief Return the minimum amon parameters.
 *
 * @param [in] a first element to compare
 * @param [in] b second element to compare
 * @return       the lesser element
 */
inline static uint64
minu (uint64 a, uint64 b)
{
  if (a < b) return a;
  return b;
}

/**
 * @brief Returns the number of available legal moves.
 *
 * @param [in] board the game board
 * @param [in] color the player having the move
 * @return           the legal move count
 */
static int
count_mobility (uint8 *board, int color)
{
  int     mobility;
  int     square;
  EmList *em;

  const int oppcol = opponent_color(color);

  mobility = 0;
  for (em = em_head.succ; em != NULL; em = em->succ) {
    square = em->square;
    if (any_flips(board, square, color, oppcol))
      mobility++;
  }

  return mobility;
}

/**
 * @brief Set up the data structures, other than board array,
 * which will be used by solver.
 *
 * As a side effect it computes the `hole_id_map` table, the `region_parity` field,
 * and prepares the linked list `em_head`, hosted by the arry `ems` having the
 * list of empty squares.
 *
 * @param [in] board a given board
 */
static void
prepare_to_solve (uint8 *board)
{
  uint64 hole_id_map[91];
  uint8 sqnum;
  int i;
  uint64 k;
  EmList *pt;
  int z;

  /* find hole IDs: */
  k = 1;
  for (i = 10; i <= 80; i++) {
    if (board[i] == IFES_EMPTY) {
      if      (board[i - 10] == IFES_EMPTY) hole_id_map[i] = hole_id_map[i - 10];
      else if (board[i -  9] == IFES_EMPTY) hole_id_map[i] = hole_id_map[i -  9];
      else if (board[i -  8] == IFES_EMPTY) hole_id_map[i] = hole_id_map[i -  8];
      else if (board[i -  1] == IFES_EMPTY) hole_id_map[i] = hole_id_map[i -  1];
      else                                { hole_id_map[i] = k; k <<= 1; }
    }
    else hole_id_map[i] = 0;
  }

  /* In some sense this is wrong, since you
   * ought to keep doing iters until reach fixed point, but in most
   * othello positions with few empties this ought to work, and besides,
   * this is justifiable since the definition of "hole" in othello
   * is somewhat arbitrary anyway.
   *
   * For ffo-40 max_iters must be 2.
   */
  const int max_iters = 4;

  for (z = max_iters; z > 0; z--) {
    for (i = 80; i >= 10; i--) {
      if (board[i] == IFES_EMPTY) {
        k = hole_id_map[i];
        if (board[i +10] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i +10]);
        if (board[i + 9] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i + 9]);
        if (board[i + 8] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i + 8]);
        if (board[i + 1] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i + 1]);
      }
    }
    for (i = 10; i <= 80; i++) {
      if (board[i] == IFES_EMPTY) {
        k = hole_id_map[i];
        if (board[i - 10] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i - 10]);
        if (board[i -  9] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i -  9]);
        if (board[i -  8] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i -  8]);
        if (board[i -  1] == IFES_EMPTY) hole_id_map[i] = minu(k, hole_id_map[i -  1]);
      }
    }
  }
  /* find parity of holes: */
  region_parity = 0;
  for (i = 10; i <= 80; i++) {
    region_parity ^= hole_id_map[i];
  }
  /* create list of empty squares: */
  k = 0;
  pt = &em_head;
  pt->pred = NULL;
  for (i = 60-1; i >= 0; i--) {
    sqnum = worst_to_best[i];
    if (board[sqnum] == IFES_EMPTY) {
      pt->succ = &(ems[k]);
      ems[k].pred = pt;
      k++;
      pt = pt->succ;
      pt->square = sqnum;
      pt->hole_id = hole_id_map[sqnum];
    }
  }
  pt->succ = NULL;
}

/**
 * @brief Searches whithout any move sorting up to the leafs.
 *
 * The last two discs are placed without recursion.
 *
 * @param [in, out] solution the solution object
 * @param [in]      board    a pointer to the game board
 * @param [in]      alpha    the alpha value
 * @param [in]      beta     the beta value
 * @param [in]      color    the color of the player having to move
 * @param [in]      empties  the count of empty squares
 * @param [in]      discdiff the disc difference between player and opponennt
 * @param [in]      prevmove the previous move or zero if it was a pass
 * @return                   the best node (move/value pairs) available
 */
static Node
no_parity_end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
                     int color, int empties, int discdiff, int prevmove)
{
  uint8 move_square;
  int flip_count;
  EmList *previous_move;
  EmList *current_move;
  Node evaluated_n;

  const int oppcol = opponent_color(color);

  solution->node_count++;

  Node selected_n = init_node(); /* Best node, selected, and then returned. */

  for (previous_move = &em_head, current_move = previous_move->succ;
       current_move != NULL;
       previous_move = current_move, current_move = current_move->succ) {
    /* Goes thru list of possible move-squares. */
    move_square = current_move->square;
    flip_count = do_flips(board, move_square, color, oppcol);
    if (flip_count) { /* Legal move. */
      /* Places the player disc. */
      *(board + move_square) = color;
      /* Deletes square from empties list. */
      previous_move->succ = current_move->succ;
      if (empties == 2) { /* One empty square is there. */
        solution->leaf_count++;
        solution->node_count++;
        int last_move_flip_count;
        last_move_flip_count = count_flips(board, em_head.succ->square, oppcol, color);
        if (last_move_flip_count) { /* Oppenent does the last move. */
          evaluated_n.value = discdiff + 2 * (flip_count - last_move_flip_count);
        }
        else { /* Opponent has to pass. */
          solution->node_count++;
          last_move_flip_count = count_flips(board, em_head.succ->square, color, oppcol);
          evaluated_n.value = discdiff + 2 * flip_count;
          if (last_move_flip_count) { /* Player put the last disc. */
            evaluated_n.value += 2 * (last_move_flip_count + 1);
          }
          else { /* Nobody can place the last disc. */
            if (evaluated_n.value >= 0)
              evaluated_n.value += 2;
          }
        }
      } else {
        evaluated_n = node_negate(no_parity_end_solve(solution,
                                                      board,
                                                      -beta,
                                                      -alpha,
                                                      oppcol,
                                                      empties - 1,
                                                      -discdiff - 2 * flip_count - 1,
                                                      move_square));
      }
      undo_flips(flip_count, oppcol);
      /* Un-places player disc. */
      *(board + move_square) = IFES_EMPTY;
      /* Restores deleted empty square. */
      previous_move->succ = current_move;

      if (evaluated_n.value > selected_n.value) { /* Better move. */
        selected_n.value = evaluated_n.value;
        selected_n.square = move_square;
        if (evaluated_n.value > alpha) {
          alpha = evaluated_n.value;
          if (evaluated_n.value >= beta) { /* Cutoff. */
            goto end;
          }
        }
      }
    }
  }
  if (selected_n.value == -infinity) {  /* No legal move found. */
    if (prevmove == 0) { /* Game over. */
      solution->leaf_count++;
      if (discdiff > 0) {
        selected_n.value = discdiff + empties;
      } else if (discdiff < 0) {
        selected_n.value = discdiff - empties;
      } else {
        selected_n.value = 0;
      }
    }
    else { /* Pass. */
      selected_n = node_negate(no_parity_end_solve(solution,
                                                   board,
                                                   -beta,
                                                   -alpha,
                                                   oppcol,
                                                   empties,
                                                   -discdiff,
                                                   0));
    }
  }
 end:
  return selected_n;
}

/**
 * @brief Searches by sorting the available moves using the parity heuristic.
 *
 * @param [in, out] solution the solution object
 * @param [in]      board    a pointer to the game board
 * @param [in]      alpha    the alpha value
 * @param [in]      beta     the beta value
 * @param [in]      color    the color of the player having to move
 * @param [in]      empties  the count of empty squares
 * @param [in]      discdiff the disc difference between player and opponennt
 * @param [in]      prevmove the previous move or zero if it was a pass
 * @return                   the best node (move/value pairs) available
 */
static Node
parity_end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
                  int color, int empties, int discdiff, int prevmove)
{
  uint8 move_square;
  int flip_count;
  EmList *previous_move;
  EmList *current_move;
  uint64 parity_mask;
  int par, holepar;
  Node evaluated_n;

  const int oppcol = opponent_color(color);

  solution->node_count++;

  Node selected_n = init_node(); /* Best node, selected, and then returned. */

  for (par = 1, parity_mask = region_parity; par >= 0;
       par--, parity_mask = ~parity_mask) {
    for (previous_move = &em_head, current_move = previous_move->succ;
         current_move != NULL;
         previous_move = current_move, current_move = current_move->succ) {
      /* Go thru list of possible move-squares. */
      holepar = current_move->hole_id;
      if (holepar & parity_mask) {
        move_square = current_move->square;
        flip_count = do_flips(board, move_square, color, oppcol);
        if (flip_count) { /* legal move */
          /* Place your disc. */
          *(board + move_square) = color;
          /* Update parity. */
          region_parity ^= holepar;
          /* Delete square from empties list. */
	  previous_move->succ = current_move->succ;
          evaluated_n = node_negate(end_solve(solution,
                                              board,
                                              -beta,
                                              -alpha, 
                                              oppcol,
                                              empties - 1,
                                              -discdiff - 2 * flip_count - 1,
                                              move_square));
          undo_flips(flip_count, oppcol);
          /* Restore parity of hole. */
          region_parity ^= holepar;
          /* Un-place your disc. */
          *(board + move_square) = IFES_EMPTY;
          /* Restore deleted empty square. */
	  previous_move->succ = current_move;

          if (evaluated_n.value > selected_n.value) { /* Better move. */
            selected_n.value = evaluated_n.value;
            selected_n.square = move_square;
            if (evaluated_n.value > alpha) {
              alpha = evaluated_n.value;
              if (evaluated_n.value >= beta) { /* Cutoff. */ 
                goto end;
              }
	    }
          }
        }
      }
    }
  }
  if (selected_n.value == -infinity) {  /* No legal move found. */
    if (prevmove == 0) { /* Game over. */
      solution->leaf_count++;
      if (discdiff > 0) {
        selected_n.value = discdiff + empties;
      } else if (discdiff < 0) {
        selected_n.value = discdiff - empties;
      } else {
        selected_n.value = 0;
      }
    }
    else { /* Pass. */
      selected_n = node_negate(parity_end_solve(solution,
                                                board,
                                                -beta,
                                                -alpha,
                                                oppcol,
                                                empties,
                                                -discdiff,
                                                0));
    }
  }
 end:
  return selected_n;
}

/**
 * @brief Searches by sorting the legal moves minimizing the opponent's mobility.
 *
 * @param [in, out] solution the solution object
 * @param [in]      board    a pointer to the game board
 * @param [in]      alpha    the alpha value
 * @param [in]      beta     the beta value
 * @param [in]      color    the color of the player having to move
 * @param [in]      empties  the count of empty squares
 * @param [in]      discdiff the disc difference between player and opponennt
 * @param [in]      prevmove the previous move or zero if it was a pass
 * @return                   the best node (move/value pairs) available
 */
static Node
fastest_first_end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
                         int color, int empties, int discdiff, int prevmove)
{
  uint8 move_square;
  int flip_count;
  EmList *previous_move;
  EmList *current_move;
  int moves, mobility;
  int best_value, best_index;
  EmList *move_ptr[64];
  int holepar;
  int goodness[64];
  Node evaluated_n;

#ifdef GAME_TREE_DEBUG
  call_count++;
  GamePosition *gp = ifes_game_position_translation(board, color);
  uint64 hash = game_position_hash(gp);
  gchar *gp_to_s = game_position_to_string(gp);
  fprintf(game_tree_debug_file, "%8lld;%016llx;%s;%2d\n", call_count, hash, gp_to_s, empties);
  gp = game_position_free(gp);
  g_free(gp_to_s);
#endif


  const int oppcol = opponent_color(color);

  solution->node_count++;

  Node selected_n = init_node(); /* Best node, selected, and then returned. */

  moves = 0;
  for (previous_move = &em_head, current_move = previous_move->succ;
       current_move != NULL;
       previous_move = current_move, current_move = current_move->succ ) {
    move_square = current_move->square;
    flip_count = do_flips(board, move_square, color, oppcol);
    if (flip_count) {
      board[move_square] = color;
      previous_move->succ = current_move->succ;
      mobility = count_mobility(board, oppcol);
      previous_move->succ = current_move;
      undo_flips(flip_count, oppcol);
      board[move_square] = IFES_EMPTY;
      move_ptr[moves] = current_move;
      goodness[moves] = -mobility;
      moves++;
    }
  }

  if (moves != 0) {
    for (int i = 0; i < moves; i++) {
      best_value = goodness[i];
      best_index = i;
      for (int j = i + 1; j < moves; j++)
	if (goodness[j] > best_value) {
	  best_value = goodness[j];
	  best_index = j;
	}
      current_move = move_ptr[best_index];
      move_ptr[best_index] = move_ptr[i];
      goodness[best_index] = goodness[i];

      /* p: player, e: empties, bm: best move. */
      /*
      printf("p=%c, e=%46s, bm=%s, a-b=[%+3d %+3d];\n",
             (color == IFES_BLACK) ? 'B' : 'W',
             square_list_print(&em_head),
             ifes_square_to_string(current_move->square),
             alpha, beta);
      */

      move_square = current_move->square;
      holepar = current_move->hole_id;
      flip_count = do_flips(board, move_square, color, oppcol);
      board[move_square] = color;
      region_parity ^= holepar;
      current_move->pred->succ = current_move->succ;
      if (current_move->succ != NULL)
	current_move->succ->pred = current_move->pred;
      evaluated_n = node_negate(fastest_first_end_solve(solution, //MODIFIED must be end_solve(....
                                                        board,
                                                        -beta,
                                                        -alpha,
                                                        oppcol,
                                                        empties - 1,
                                                        -discdiff - 2 * flip_count - 1,
                                                        move_square));
      undo_flips(flip_count, oppcol);
      region_parity ^= holepar;
      board[move_square] = IFES_EMPTY;
      current_move->pred->succ = current_move;
      if (current_move->succ != NULL)
	current_move->succ->pred = current_move;

      if (evaluated_n.value > selected_n.value) { /* Better move. */
	selected_n.value = evaluated_n.value;
        selected_n.square = move_square;
	if (evaluated_n.value > alpha) {
	  alpha = evaluated_n.value;
	  if (evaluated_n.value >= beta) { /* Cutoff. */
            //printf("-cut-");
            goto end;
          }
	}
      }
    }
  } else {
    if (prevmove == 0) { /* Game over. */
      solution->leaf_count++;
      if (discdiff > 0) {
        selected_n.value = discdiff + empties;
      } else if (discdiff < 0) {
        selected_n.value = discdiff - empties;
      } else {
        selected_n.value = 0;
      }
      ;
      /*
      printf("p=%c, leaf_value=%+02d;\n",
             (color == IFES_BLACK) ? 'B' : 'W',
             selected_n.value);
      */
    } else { /* Pass. */

      /* p: player, e: empties, bm: best move. */
      /*
      printf("p=%c, e=%46s, bm=%s, a-b=[%+3d %+3d];\n",
             (color == IFES_BLACK) ? 'B' : 'W',
             square_list_print(&em_head),
             "--",
             alpha, beta);
      */

      selected_n = node_negate(fastest_first_end_solve(solution,
                                                       board,
                                                       -beta,
                                                       -alpha,
                                                       oppcol,
                                                       empties,
                                                       -discdiff,
                                                       0));
    }
  }
  // printf("     ");
 end:
  ;
  /*
  gchar* move_to_s = ifes_square_to_string(selected_n.square);
  printf("return node: n.move=%3s n.value=%+3d [%016llx]\n", move_to_s, selected_n.value, hash);
  g_free(move_to_s);
  */
  return selected_n;
}

/**
 * @brief The search itself.
 *
 * It is plain alphabeta, no transposition table.
 * It can be used for WLD solve by setting `alpha=-1` and `beta=1` or
 * for full solve with `alpha=-64`, `beta=64`.
 * It uses a fixed preference ordering of squares.
 * This is not such a bad thing to do when <=10 empties,
 * where the demand for speed is paramount. 
 *
 * Assumes relevant data structures have been set up with prepare_to_solve().
 *
 * @param [in,out] solution
 * @param [in]     board
 * @param [in]     alpha
 * @param [in]     beta
 * @param [in]     color    the color on move
 * @param [in]     empties  the number of empty squares
 * @param [in]     discdiff color disc count less opposite_color disc count
 * @param [in]     prevmove the previous move, or zero if previous move was a pass
 * @return                  the node having the best value among the legal moves
 */
inline static Node
end_solve (ExactSolution *solution, uint8 *board, int alpha, int beta, 
           int color, int empties, int discdiff, int prevmove)
{
  if (empties > fastest_first)
    return fastest_first_end_solve(solution, board, alpha, beta, color, empties, discdiff, prevmove);
  else {
    if (empties <= (2 > use_parity ? 2 : use_parity))
      return no_parity_end_solve(solution, board, alpha, beta, color, empties, discdiff, prevmove);
    else
      return parity_end_solve(solution, board, alpha, beta, color, empties, discdiff, prevmove);
  }
}

/**
 * @brief Initialize a new node.
 *
 * @return a new initialized node.
 */
inline static Node
init_node (void)
{
  Node n;
  n.value = -infinity;
  n.square = 0;
  return n;
}

/**
 * @brief Changes the sign of the node's value.
 *
 * @param n the given node
 * @return  the updated node
 */
inline static Node
node_negate (Node n)
{
  n.value = -n.value;
  return n;
}

/**
 * @brief Returns the opponenent's color.
 *
 * @param [in] color the player's color
 * @return           the opponent's color
 */
inline static int
opponent_color (int color)
{
  return 2 - color;
}

static char *
square_list_print (const EmList * const sl)
{
  EmList *current_move;
  gchar *ml_to_s;
  gchar space[] = {' ', '\0'};

  static const size_t size_of_ml_to_s = (3 * 64 + 1) * sizeof(gchar);
  ml_to_s = (gchar*) malloc(size_of_ml_to_s);

  *ml_to_s = '\0';
  gchar *cursor = ml_to_s;
  for (current_move = sl->succ;
       current_move != NULL;
       current_move = current_move->succ ) {
    gchar *move_to_s = square_to_string(ifes_square_to_square (current_move->square));
    cursor = g_stpcpy(cursor, move_to_s);
    g_free(move_to_s);
    cursor = g_stpcpy(cursor, &space[0]);
  }
  if (ml_to_s != cursor) {
    *--cursor = '\0';
  }
  return ml_to_s;
}

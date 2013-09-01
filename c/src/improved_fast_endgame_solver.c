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

/**
 * @enum IFES_SquareState
 * @brief The `IFES_SquareState` identifies the state, or as a synonym the "color",
 * of each board square.
 */
typedef enum {
  IFES_WHITE,   /**< A white piece. */
  IFES_EMPTY,   /**< An empty square. */
  IFES_BLACK,   /**< A black piece. */
  IFES_DUMMY    /**< A piece out of board. */
} IFES_SquareState;

/**
 * @typedef uchar
 * @brief Unsigned one byte integer.
 */
typedef unsigned char uchar;

/**
 * @typedef schar
 * @brief Signed one byte integer.
 */
typedef signed char schar;

/**
 * @typedef uint
 * @brief Unigned four byte integer.
 */
typedef signed char uint;

/**
 * @brief An empty list collects a set of ordered squares.
 *
 * Details to be documented.
 */
typedef struct em_list {
  int square;           /**< @brief To be documented. */
  int hole_id;
  struct em_list *pred;
  struct em_list *succ;
} EmList;

/**
 * @brief A node in the search tree.
 *
 * Details to be documented.
 */
typedef struct {
  int square;
  int value;
} Node;


/*
 * Prototypes for internal functions.
 */

inline static void
directional_flips (uchar *sq, int inc, int color, int oppcol);

static int
do_flips (uchar *board, int sqnum, int color, int oppcol);

inline static int
ct_directional_flips (uchar *sq, int inc, int color, int oppcol);

static int
count_flips (uchar *board, int sqnum, int color, int oppcol);

inline static int
any_directional_flips (uchar *sq, int inc, int color, int oppcol);

static int
any_flips (uchar *board, int sqnum, int color, int oppcol);

inline static void
undo_flips (int flip_count, int oppcol);

inline static uint
minu (uint a, uint b);

static int
count_mobility (uchar *board, int color);

static void
prepare_to_solve (uchar *board);

static Node
no_parity_end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
                     int color, int empties, int discdiff, int prevmove);

static Node
parity_end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
                  int color, int empties, int discdiff, int prevmove);

static Node
fastest_first_end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
                         int color, int empties, int discdiff, int prevmove);

static Node
end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
           int color, int empties, int discdiff, int prevmove);

static void
game_position_to_ifes_board (const GamePosition * const gp, int *p_emp, int *p_wc, int *p_bc);

static IFES_SquareState
game_position_get_ifes_player(const GamePosition * const gp);

static char *
ifes_square_to_string (const int sq);

static Square
ifes_square_to_square (const int sq);



/*
 * Internal constants.
 */

/*
 * This code will work for any number of empties up to the number
 * of bits in a uint (should be 32).
 *
 * The function prepare_to_solve aborts if empties are more than max_empties.
 */
static const int max_empties = 32;

/*
 * This is the best/worst case board value.
 * Any value greather than 64 can be adopted.
 * The value thirty thousands has an hystorical heritage. 
 */
static const int infinity = 30000;

/*
 * The selection of the variant of end end_solver() function follows these rules.
 *
 * Let's take as an example the two values: use_parity = 4, and fastest_first = 7.
g *
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
 */
static const int use_parity = 4;

/*
 * The parameter fastest_first. See above for explanation.
 */
static const int fastest_first = 7;

/*
 * The 8 legal directions, plus no direction an ninth value.
 */
static const schar dirinc[] = {1, -1, 8, -8, 9, -9, 10, -10, 0};

/*
 * Fixed square ordering.
 */
static const int worst_to_best[64] =
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

/*
 * The bit mask for direction i is 1<<i
 *
 * Bit masks for the directions squares can flip in,
 * for example dirmask[10]=81=64+16+1=(1<<6)+(1<<4)+(1<<0)
 * hence square 10 (A1) can flip in directions dirinc[0]=1,
 * dirinc[4]=9, and dirinc[6]=10:
 */
static const uchar dirmask[91] = {
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

/*
 * Inside this fast endgame solver, the board is represented by
 * a 1D array of 91 uchars board[0..90]:
 * ddddddddd
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx
 * dxxxxxxxx       where A1 is board[10], H8 is board[80].
 * dxxxxxxxx       square(a,b) = board[10+a+b*9] for 0<= a,b <=7.
 * dddddddddd   
 * where d (dummy) squares contain DUMMY, x are EMPTY, BLACK, or WHITE:
 */
static uchar board[91];

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
static uint region_parity;

/* Must be documented. */
static uchar  *global_flip_stack[2048];

/* Must be documented. */
static uchar **flip_stack = &(global_flip_stack[0]);



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

/**
 * @brief Documentation to be prepared.
 */
ExactSolution *
game_position_ifes_solve (const GamePosition * const root)
{
  ExactSolution *result;
  int            emp;
  int            wc, bc;
  int            discdiff;
  Node           n;

  result = exact_solution_new();
  result->solved_game_position = game_position_clone(root);

  game_position_to_ifes_board(root, &emp, &wc, &bc);

  IFES_SquareState player = game_position_get_ifes_player(root);

  discdiff = player == IFES_BLACK ? bc - wc : wc - bc;

  prepare_to_solve(board);

  /** Debug info **/
  if (TRUE) {
    printf("\nEmpty Square Doubly linked List debug info:\n");
    printf("em_head: address=%p [square=%2d (%s), hole_id=%d] pred=%p succ=%p\n",
           (void*) &em_head, em_head.square, ifes_square_to_string(em_head.square), em_head.hole_id,
           (void*) em_head.pred, (void*) em_head.succ);
    for (int k = 0; k < 64; k++) {
      if (ems[k].square != 0)
        printf("ems[%2d]: address=%p [square=%2d (%s), hole_id=%d] pred=%p succ=%p\n",
               k, (void*) &ems[k], ems[k].square, ifes_square_to_string(ems[k].square), ems[k].hole_id,
               (void*) ems[k].pred, (void*) ems[k].succ);
    }
    printf("region_parity=%d\n", region_parity);
    printf("\n");
  }
  /** **/

  n = end_solve(result, board, -64, 64, player, emp, discdiff, 1);

  result->outcome = n.value;
  result->principal_variation[0] = ifes_square_to_square(n.square);

  printf("use_parity=%d. fastest_first=%d.\n",
         use_parity, fastest_first);

  return result;
}



/*
 * Internal functions.
 */

/**
 * @brief Documentation to be prepared.
 *
 */
static void
game_position_to_ifes_board (const GamePosition * const gp, int *p_emp, int *p_wc, int *p_bc)
{
  int emp, wc, bc, j, k, x, y;
  for (j = 0; j <= 90; j++) board[j] = IFES_DUMMY;
  wc = bc = emp = 0;
  for (j = 0; j < 64; j++) {
    x = j&7; y = (j>>3)&7; k = x+10+9*y;
    if      ((gp->board->whites & (1ULL << j)) != 0ULL) { board[k] = IFES_WHITE; wc++; }
    else if ((gp->board->blacks & (1ULL << j)) != 0ULL) { board[k] = IFES_BLACK; bc++; }
    else                                                { board[k] = IFES_EMPTY; emp++; }
  }
  *p_emp = emp;
  *p_wc = wc;
  *p_bc = bc;
}

/**
 * @brief Documentation to be prepared.
 *
 */
static IFES_SquareState
game_position_get_ifes_player(const GamePosition * const gp)
{
  IFES_SquareState ifes_player;
  ifes_player = gp->player == BLACK_PLAYER ? IFES_BLACK : IFES_WHITE;
  return ifes_player;
}

/**
 * @brief Documentation to be prepared.
 *
 */
static Square
ifes_square_to_square (const int sq)
{
  const uchar col = (sq % 9) - 1;
  const uchar row = (sq / 9) - 1;
  return row * 8 + col;
}

/**
 * @brief Documentation to be prepared.
 *
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
    const uchar col = (sq % 9) - 1;
    const uchar row = (sq / 9) - 1;
    *symbol = 'A' + col;
    *(symbol + 1) = '1' + row;
    *(symbol + 2) = '\0';
  }

  return symbol;
}

/**
 * @brief Documentation to be prepared.
 *
 * sq is a pointer to the square the move is to.
 * inc is the increment to go in some direction.
 * color is the color of the mover.
 * oppcol = 2-color is the opposite color.
 * flip_stack records locations of flipped men so can unflip later.
 * This routine flips in direction inc and returns count of flips it made:
 */
inline static void
directional_flips (uchar *sq, int inc, int color, int oppcol)
{
  uchar *pt = sq + inc;
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
 * @brief Do all flips involved in making a move to square sqnum of board,
 * and return their count.
 */
static int
do_flips (uchar *board, int sqnum,
          int color, int oppcol)
{
  int j = dirmask[sqnum];
  uchar **old_flip_stack = flip_stack;
  uchar *sq;
  sq = sqnum + board;
  if (j & 128)
    directional_flips(sq, dirinc[7], color, oppcol);
  if (j & 64)
    directional_flips(sq, dirinc[6], color, oppcol);
  if (j & 32)
    directional_flips(sq, dirinc[5], color, oppcol);
  if (j & 16)
    directional_flips(sq, dirinc[4], color, oppcol);
  if (j & 8)
    directional_flips(sq, dirinc[3], color, oppcol);
  if (j & 4)
    directional_flips(sq, dirinc[2], color, oppcol);
  if (j & 2)
    directional_flips(sq, dirinc[1], color, oppcol);
  if (j & 1)
    directional_flips(sq, dirinc[0], color, oppcol);

  return flip_stack - old_flip_stack;
}

/**
 * @brief For the last move, we compute the score without updating the board:
 */
inline static int
ct_directional_flips (uchar *sq, int inc, int color, int oppcol)
{
  uchar *pt = sq + inc;
  if (*pt == oppcol) {
    int count = 1;
    pt += inc;
    if (*pt == oppcol) {
      count++;                /* 2 */
      pt += inc;
      if (*pt == oppcol) {
        count++;              /* 3 */
        pt += inc;
        if (*pt == oppcol) {
          count++;            /* 4 */
          pt += inc;
          if (*pt == oppcol) {
            count++;          /* 5 */
            pt += inc;
            if (*pt == oppcol) {
              count++;        /* 6 */
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
 * @brief Documentation to be prepared.
 *
 */
static int
count_flips (uchar *board, int sqnum, int color, int oppcol)
{
  int ct = 0;
  int j = dirmask[sqnum];
  uchar *sq;
  sq = sqnum + board;
  if (j & 128)
    ct += ct_directional_flips(sq, dirinc[7], color, oppcol);
  if (j & 64)
    ct += ct_directional_flips(sq, dirinc[6], color, oppcol);
  if (j & 32)
    ct += ct_directional_flips(sq, dirinc[5], color, oppcol);
  if (j & 16)
    ct += ct_directional_flips(sq, dirinc[4], color, oppcol);
  if (j & 8)
    ct += ct_directional_flips(sq, dirinc[3], color, oppcol);
  if (j & 4)
    ct += ct_directional_flips(sq, dirinc[2], color, oppcol);
  if (j & 2)
    ct += ct_directional_flips(sq, dirinc[1], color, oppcol);
  if (j & 1)
    ct += ct_directional_flips(sq, dirinc[0], color, oppcol);
  return(ct);
}

/**
 * @brief Sometimes we only want to know if a move is legal, not how
 * many discs it flips.
 */
inline static int
any_directional_flips (uchar *sq, int inc, int color, int oppcol)
{
  uchar *pt = sq + inc;
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
 * @brief To be documented
 */
static int
any_flips (uchar *board, int sqnum, int color, int oppcol)
{
  int any = 0;
  int j = dirmask[sqnum];
  uchar *sq;
  sq = sqnum + board;
  if (j & 128)
    any += any_directional_flips(sq, dirinc[7], color, oppcol);
  if (j & 64)
    any += any_directional_flips(sq, dirinc[6], color, oppcol);
  if (j & 32)
    any += any_directional_flips(sq, dirinc[5], color, oppcol);
  if (j & 16)
    any += any_directional_flips(sq, dirinc[4], color, oppcol);
  if (j & 8)
    any += any_directional_flips(sq, dirinc[3], color, oppcol);
  if (j & 4)
    any += any_directional_flips(sq, dirinc[2], color, oppcol);
  if (j & 2)
    any += any_directional_flips(sq, dirinc[1], color, oppcol);
  if (j & 1)
    any += any_directional_flips(sq, dirinc[0], color, oppcol);
  return any;
}

/**
 * @brief Call this function right after `flip_count = do_flips()` to undo those flips!
 */
inline static void
undo_flips (int flip_count, int oppcol)
{
  /*
   * This is functionally equivalent to the simpler but slower code line:
   * while (flip_count) { flip_count--;  *(*(--flip_stack)) = oppcol; }
   */
  if (flip_count & 1) {
    flip_count--;
    * (*(--flip_stack)) = oppcol;
  }
  while (flip_count) {
    flip_count -= 2;
    * (*(--flip_stack)) = oppcol;
    * (*(--flip_stack)) = oppcol;
  }
}

/**
 * @brief To be documented
 */
inline static uint
minu (uint a, uint b)
{
  if (a < b) return a;
  return b;
}

/**
 * @brief To be documented
 */
static int
count_mobility (uchar *board, int color)
{
  int     mobility;
  int     square;
  EmList *em;

  const int oppcol = 2 - color;

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
 * which will be used by solver. Since this routine consumes
 * about 0.0002 of the time needed for a 12-empty solve,
 * I haven't worried about speeding it up.
 */
static void
prepare_to_solve (uchar *board)
{
  uint hole_id_map[91];
  int i, sqnum;
  uint k;
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
  if (k > max_empties) abort(); /* better not have too many empties... */
}

/**
 * @brief To be documented
 */
static Node
no_parity_end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
                     int color, int empties, int discdiff, int prevmove)
{
  int sqnum, j;
  EmList *em, *old_em;
  Node evaluated_n;

  const int oppcol = 2 - color;

  solution->node_count++;

  Node selected_n; /* Best node, selected, and then returned. */
  selected_n.value = -infinity;
  selected_n.square = 0;

  for (old_em = &em_head, em = old_em->succ; em != NULL;
       old_em = em, em = em->succ) {
    /* go thru list of possible move-squares */
    sqnum = em->square;
    j = do_flips(board, sqnum, color, oppcol);
    if (j) { /* legal move */
      /* place your disc: */
      *(board+sqnum) = color;
      /* delete square from empties list: */
      old_em->succ = em->succ;
      if (empties == 2) { /* So, now filled but for 1 empty: */
        solution->leaf_count++; /* Could be more than one leaf_node, it cold be one or two!. */
        int j1;
        j1 = count_flips(board, em_head.succ->square, oppcol, color);
        if (j1) { /* I move then he moves */
          evaluated_n.value = discdiff + 2*(j-j1);
        }
        else { /* he will have to pass */
          j1 = count_flips(board, em_head.succ->square, color, oppcol);
          evaluated_n.value = discdiff + 2*j;
          if (j1) { /* I pass then he passes then I move */
            evaluated_n.value += 2 * (j1 + 1);
          }
          else { /* I move then both must pass, so game over */
            if (evaluated_n.value >= 0)
              evaluated_n.value += 2;
          }
        }
      }
      else {
        evaluated_n = no_parity_end_solve(solution, board, -beta, -alpha, 
                                          oppcol, empties-1, -discdiff-2*j-1, sqnum);
        evaluated_n.value = -evaluated_n.value;
      }
      undo_flips(j, oppcol);
      /* un-place your disc: */
      *(board+sqnum) = IFES_EMPTY;
      /* restore deleted empty square: */
      old_em->succ = em;

      if (evaluated_n.value > selected_n.value) { /* better move: */
        selected_n.value = evaluated_n.value;
        selected_n.square = sqnum;
        if (evaluated_n.value > alpha) {
          alpha = evaluated_n.value;
          if (evaluated_n.value >= beta) { /* cutoff */
            goto end;
          }
        }
      }
    }
  }
  if (selected_n.value == -infinity) {  /* No legal move */
    if (prevmove == 0) { /* game over: */
      solution->leaf_count++;
      if (discdiff > 0) {
        selected_n.value = discdiff + empties;
      } else if (discdiff < 0) {
        selected_n.value = discdiff - empties;
      } else {
        selected_n.value = 0;
      }
    }
    else { /* I pass: */
      selected_n = no_parity_end_solve(solution, board, -beta, -alpha, oppcol, empties, -discdiff, 0);
      selected_n.value = -selected_n.value;
    }
  }
 end:
  return selected_n;
}

/**
 * @brief To be documented
 */
static Node
parity_end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
                  int color, int empties, int discdiff, int prevmove)
{
  int sqnum, j;
  EmList *em, *old_em;
  uint parity_mask;
  int par, holepar;
  Node evaluated_n;

  const int oppcol = 2 - color;

  solution->node_count++;

  Node selected_n;
  selected_n.value = -infinity;
  selected_n.square = 0;

  for (par = 1, parity_mask = region_parity; par >= 0;
       par--, parity_mask = ~parity_mask) {
    for (old_em = &em_head, em = old_em->succ; em != NULL;
         old_em = em, em = em->succ) {
      /* go thru list of possible move-squares */
      holepar = em->hole_id;
      if (holepar & parity_mask) {
        sqnum = em->square;
        j = do_flips(board, sqnum, color, oppcol);
        if (j) { /* legal move */
          /* place your disc: */
          *(board+sqnum) = color;
          /* update parity: */
          region_parity ^= holepar;
          /* delete square from empties list: */
	  old_em->succ = em->succ;
          if (empties <= 1 + use_parity)
            evaluated_n = no_parity_end_solve(solution, board, -beta, -alpha, 
                                              oppcol, empties-1, -discdiff-2*j-1, sqnum);
          else
            evaluated_n = parity_end_solve(solution, board, -beta, -alpha, 
                                           oppcol, empties-1, -discdiff-2*j-1, sqnum);
          evaluated_n.value = -evaluated_n.value;
          undo_flips(j, oppcol);
          /* restore parity of hole */
          region_parity ^= holepar;
          /* un-place your disc: */
          *(board+sqnum) = IFES_EMPTY;
          /* restore deleted empty square: */
	  old_em->succ = em;

          if (evaluated_n.value > selected_n.value) { /* better move: */
            selected_n.value = evaluated_n.value;
            selected_n.square = sqnum;
            if (evaluated_n.value > alpha) {
              alpha = evaluated_n.value;
              if (evaluated_n.value >= beta) { 
                goto end;
              }
	    }
          }
        }
      }
    }
  }
  if (selected_n.value == -infinity) {  /* No legal move found */
    if (prevmove == 0) { /* game over: */
      solution->leaf_count++;
      if (discdiff > 0) {
        selected_n.value = discdiff + empties;
      } else if (discdiff < 0) {
        selected_n.value = discdiff - empties;
      } else {
        selected_n.value = 0;
      }
    }
    else { /* I pass: */
      selected_n = parity_end_solve(solution, board, -beta, -alpha, oppcol, empties, -discdiff, 0);
      selected_n.value = -selected_n.value;
    }
  }
 end:
  return selected_n;
}

/**
 * @brief To be documented
 */
static Node
fastest_first_end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
                         int color, int empties, int discdiff, int prevmove)
{
  int i, j;
  int sqnum;
  int flipped;
  int moves, mobility;
  int best_value, best_index;
  EmList *em, *old_em;
  EmList *move_ptr[64];
  int holepar;
  int goodness[64];
  Node evaluated_n;

  const int oppcol = 2 - color;

  solution->node_count++;

  Node selected_n;
  selected_n.value = -infinity;
  selected_n.square = 0;

  moves = 0;
  for (old_em = &em_head, em = old_em->succ; em != NULL;
       old_em = em, em = em->succ ) {
    sqnum = em->square;
    flipped = do_flips(board, sqnum, color, oppcol);
    if (flipped) {
      board[sqnum] = color;
      old_em->succ = em->succ;
      mobility = count_mobility(board, oppcol);
      old_em->succ = em;
      undo_flips(flipped, oppcol);
      board[sqnum] = IFES_EMPTY;
      move_ptr[moves] = em;
      goodness[moves] = -mobility;
      moves++;
    }
  }

  if (moves != 0) {
    for (i = 0; i < moves; i++) {
      best_value = goodness[i];
      best_index = i;
      for (j = i + 1; j < moves; j++)
	if (goodness[j] > best_value) {
	  best_value = goodness[j];
	  best_index = j;
	}
      em = move_ptr[best_index];
      move_ptr[best_index] = move_ptr[i];
      goodness[best_index] = goodness[i];

      sqnum = em->square;
      holepar = em->hole_id;
      j = do_flips(board, sqnum, color, oppcol);
      board[sqnum] = color;
      region_parity ^= holepar;
      em->pred->succ = em->succ;
      if (em->succ != NULL)
	em->succ->pred = em->pred;
      if (empties <= fastest_first + 1)
	evaluated_n = parity_end_solve(solution, board, -beta, -alpha, oppcol, empties - 1,
                                       -discdiff - 2 * j - 1, sqnum);
      else
	evaluated_n = fastest_first_end_solve(solution, board, -beta, -alpha, oppcol,
                                              empties - 1, -discdiff - 2 * j - 1,
                                              sqnum);
      evaluated_n.value = -evaluated_n.value;
      undo_flips(j, oppcol);
      region_parity ^= holepar;
      board[sqnum] = IFES_EMPTY;
      em->pred->succ = em;
      if (em->succ != NULL)
	em->succ->pred = em;

      if (evaluated_n.value > selected_n.value) { /* better move: */
	selected_n.value = evaluated_n.value;
        selected_n.square = sqnum;
	if (evaluated_n.value > alpha) {
	  alpha = evaluated_n.value;
	  if (evaluated_n.value >= beta) {
            goto end;
          }
	}
      }
    }
  } else {
    if (prevmove == 0) { // game-over
      solution->leaf_count++;
      if (discdiff > 0) {
        selected_n.value = discdiff + empties;
      } else if (discdiff < 0) {
        selected_n.value = discdiff - empties;
      } else {
        selected_n.value = 0;
      }
    } else { /* I pass: */
      selected_n = fastest_first_end_solve(solution, board, -beta, -alpha, oppcol,
                                           empties, -discdiff, 0);
      selected_n.value = -selected_n.value;
    }
  }
 end:
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
 * @param solution
 * @param board
 * @param alpha
 * @param beta
 * @param color    the color on move
 * @param empties  the number of empty squares
 * @param discdiff color disc count less opposite_color disc count
 * @param prevmove the previous move, or zero if previous move was a pass
 * @return         the node having the best value among the legal moves
 */
static Node
end_solve (ExactSolution *solution, uchar *board, int alpha, int beta, 
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

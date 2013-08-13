/**
 * @file
 *
 * @brief Improved fast endgame solver.
 * @details Solver derived from the Gunnar Anderrsson work.
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

/**
 * This code is designed to solve positions with <=10 empties (although
 * in principle it will work for any number of empties up to the number
 * of bits in a uint (32 for me).
 */
#define MAXEMPTIES 32

/**
 * It is plain alphabeta, no transposition table.  It can be used for
 * WLD solve by setting alpha=-1 and beta=1 (in which case
 * WINNER_GETS_EMPTIES could be turned off to get a tiny speedup...) or
 * for full solve with alpha=-64, beta=64.  It uses a fixed preference
 * ordering of squares. This is not such a bad thing to do when <=10
 * empties, where the demand for speed is paramount. When USE_PARITY=X is
 * turned on (X>0), it also uses parity to help with move ordering,
 * specifically it will consider all moves into odd regions before
 * considering any moves into even regions, in situations with more than
 * X empties.
 */
#define USE_PARITY 4

/**
 * If WINNER_GETS_EMPTIES is turned on it will do scoring using the
 * winner-gets-empties convention - which never changes who won, but can
 * change the final score.
 */
#define WINNER_GETS_EMPTIES 1

/**
 * In positions with <= FASTEST_FIRST empty, fastest first is disabled.
 */
#define FASTEST_FIRST 7

#define INFINITY 30000

#include <stdio.h>
#include <stdlib.h>

/**
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
#define WHITE 0
#define EMPTY 1
#define BLACK 2
#define DUMMY 3
#define uchar unsigned char
#define schar signed char
#define uint unsigned int
uchar board[91];

/**
 * Also there is a doubly linked list of the empty squares.
 * EmHead points to the first empty square in the list (or NULL if none).
 * The list in maintained in a fixed best-to-worst order.
 */
struct EmList
{
    int square;
    int hole_id;
    struct EmList *pred;
    struct EmList *succ;
} EmHead, Ems[64];

/**
 * Also, and finally, each empty square knows the region it is in
 * and knows the directions you can flip in via some bit masks.
 * There are up to 32 regions. The parities of the regions are in
 * the RegionParity bit vector:
 */
uint HoleId[91];
uint RegionParity;

/**
 * The 8 legal directions:
 */
const schar dirinc[] = {1, -1, 8, -8, 9, -9, 10, -10, 0};

/**
 * The bit mask for direction i is 1<<i
 *
 * Bit masks for the directions squares can flip in,
 * for example dirmask[10]=81=64+16+1=(1<<6)+(1<<4)+(1<<0)
 * hence square 10 (A1) can flip in directions dirinc[0]=1,
 * dirinc[4]=9, and dirinc[6]=10:
 */
uchar dirmask[91] = {
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

/**
 * Fixed square ordering:
 * jcw's order, which is the best of 4 tried:
 */
int worst2best[64] =
{
  /*B2*/      20 , 25 , 65 , 70 ,
  /*B1*/      11 , 16 , 19 , 26 , 64 , 71 , 74 , 79 ,
  /*C2*/      21 , 24 , 29 , 34 , 56 , 61 , 66 , 69 ,
  /*D2*/      22 , 23 , 38 , 43 , 47 , 52 , 67 , 68 ,
  /*D3*/      31 , 32 , 39 , 42 , 48 , 51 , 58 , 59 ,
  /*D1*/      13 , 14 , 37 , 44 , 46 , 53 , 76 , 77 ,
  /*C3*/      30 , 33 , 57 , 60 ,
  /*C1*/      12 , 15 , 28 , 35 , 55 , 62 , 75 , 78 ,
  /*A1*/      10 , 17 , 73 , 80 , 
  /*D4*/      40 , 41 , 49 , 50
};

uchar * GlobalFlipStack[2048];
uchar **FlipStack = &(GlobalFlipStack[0]);

/**
 * Internal variables and constants.
 */

typedef unsigned long long int uint64;

static uint64 leaf_count = 0;
static uint64 node_count = 0;

/**
 * sq is a pointer to the square the move is to.
 * inc is the increment to go in some direction.
 * color is the color of the mover.
 * oppcol = 2-color is the opposite color.
 * FlipStack records locations of flipped men so can unflip later.
 * This routine flips in direction inc and returns count of flips it made:
 */
inline
void
DrctnlFlips (uchar *sq, int inc, int color, int oppcol)
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
        *(FlipStack++) = pt;
        pt -= inc;
      } while (pt != sq);
    }
  }
}

/*
 * Do all flips involved in making a move to square sqnum of board,
 * and return their count.
 */
int
DoFlips (uchar *board, int sqnum,
         int color, int oppcol)
{
  int j = dirmask[sqnum];
  uchar **OldFlipStack = FlipStack;
  uchar *sq;
  sq = sqnum + board;
  if (j & 128)
    DrctnlFlips(sq, dirinc[7], color, oppcol);
  if (j & 64)
    DrctnlFlips(sq, dirinc[6], color, oppcol);
  if (j & 32)
    DrctnlFlips(sq, dirinc[5], color, oppcol);
  if (j & 16)
    DrctnlFlips(sq, dirinc[4], color, oppcol);
  if (j & 8)
    DrctnlFlips(sq, dirinc[3], color, oppcol);
  if (j & 4)
    DrctnlFlips(sq, dirinc[2], color, oppcol);
  if (j & 2)
    DrctnlFlips(sq, dirinc[1], color, oppcol);
  if (j & 1)
    DrctnlFlips(sq, dirinc[0], color, oppcol);

  return FlipStack - OldFlipStack;
}

/**
 * For the last move, we compute the score without updating the board:
 */
inline
int
CtDrctnlFlips (uchar *sq, int inc, int color, int oppcol)
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

int
CountFlips (uchar *board, int sqnum,
            int color, int oppcol)
{
  int ct = 0;
  int j = dirmask[sqnum];
  uchar *sq;
  sq = sqnum + board;
  if (j & 128)
    ct += CtDrctnlFlips(sq, dirinc[7], color, oppcol);
  if (j & 64)
    ct += CtDrctnlFlips(sq, dirinc[6], color, oppcol);
  if (j & 32)
    ct += CtDrctnlFlips(sq, dirinc[5], color, oppcol);
  if (j & 16)
    ct += CtDrctnlFlips(sq, dirinc[4], color, oppcol);
  if (j & 8)
    ct += CtDrctnlFlips(sq, dirinc[3], color, oppcol);
  if (j & 4)
    ct += CtDrctnlFlips(sq, dirinc[2], color, oppcol);
  if (j & 2)
    ct += CtDrctnlFlips(sq, dirinc[1], color, oppcol);
  if (j & 1)
    ct += CtDrctnlFlips(sq, dirinc[0], color, oppcol);
  return(ct);
}

/**
 * Sometimes we only want to know if a move is legal, not how
 * many discs it flips.
 */
inline
int
AnyDrctnlFlips (uchar *sq, int inc, int color, int oppcol)
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
    if(*pt == color) return 1;
  }

  return 0;
}

int
AnyFlips (uchar *board, int sqnum,
	  int color, int oppcol)
{
  int any = 0;
  int j = dirmask[sqnum];
  uchar *sq;
  sq = sqnum + board;
  if (j & 128)
    any += AnyDrctnlFlips(sq, dirinc[7], color, oppcol);
  if (j & 64)
    any += AnyDrctnlFlips(sq, dirinc[6], color, oppcol);
  if (j & 32)
    any += AnyDrctnlFlips(sq, dirinc[5], color, oppcol);
  if (j & 16)
    any += AnyDrctnlFlips(sq, dirinc[4], color, oppcol);
  if (j & 8)
    any += AnyDrctnlFlips(sq, dirinc[3], color, oppcol);
  if (j & 4)
    any += AnyDrctnlFlips(sq, dirinc[2], color, oppcol);
  if (j & 2)
    any += AnyDrctnlFlips(sq, dirinc[1], color, oppcol);
  if (j & 1)
    any += AnyDrctnlFlips(sq, dirinc[0], color, oppcol);
  return(any);
}

/**
 * Call this right after FlipCount=DoFlips() to Undo those flips!
 */
inline
void
UndoFlips (int FlipCount, int oppcol)
{
  /**************************************************************************
   ** This is functionally equivalent to the simpler but slower code line:  *
   **   while(FlipCount){ FlipCount--;  *(*(--FlipStack)) = oppcol; }       *
   **************************************************************************/
  if (FlipCount & 1) {
    FlipCount--;
    * (*(--FlipStack)) = oppcol;
  }
  while (FlipCount) {
    FlipCount -= 2;
    * (*(--FlipStack)) = oppcol;
    * (*(--FlipStack)) = oppcol;
  }
}

inline
uint
minu (uint a, uint b)
{
  if(a<b) return a;
  return b;
}

/**
 * Set up the data structures, other than board array,
 * which will be used by solver. Since this routine consumes
 * about 0.0002 of the time needed for a 12-empty solve,
 * I haven't worried about speeding it up. */
void
PrepareToSolve (uchar *board)
{
  int i, sqnum;
  uint k;
  struct EmList *pt;
  int z;
  /* find hole IDs: */
  k = 1;
  for (i = 10; i <= 80; i++) {
    if (board[i] == EMPTY) {
      if (board[i-10] == EMPTY) HoleId[i] = HoleId[i-10];
      else if (board[i - 9] == EMPTY) HoleId[i] = HoleId[i - 9];
      else if (board[i - 8] == EMPTY) HoleId[i] = HoleId[i - 8];
      else if (board[i - 1] == EMPTY) HoleId[i] = HoleId[i - 1];
      else { HoleId[i] = k; k<<=1; }
    }
    else HoleId[i] = 0;
  }
#define MAXITERS 1
  /* In some sense this is wrong, since you
   * ought to keep doing iters until reach fixed point, but in most
   * othello positions with few empties this ought to work, and besides,
   * this is justifiable since the definition of "hole" in othello
   * is somewhat arbitrary anyway. */
  for (z = MAXITERS; z > 0; z--) {
    for (i = 80; i >= 10; i--) {
      if (board[i] == EMPTY) {
        k = HoleId[i];
        if (board[i +10] == EMPTY) HoleId[i] = minu(k,HoleId[i +10]);
        if (board[i + 9] == EMPTY) HoleId[i] = minu(k,HoleId[i + 9]);
        if (board[i + 8] == EMPTY) HoleId[i] = minu(k,HoleId[i + 8]);
        if (board[i + 1] == EMPTY) HoleId[i] = minu(k,HoleId[i + 1]);
      }
    }
    for (i = 10; i <= 80; i++) {
      if (board[i] == EMPTY) {
        k = HoleId[i];
        if (board[i - 10] == EMPTY) HoleId[i] = minu(k,HoleId[i - 10]);
        if (board[i -  9] == EMPTY) HoleId[i] = minu(k,HoleId[i -  9]);
        if (board[i -  8] == EMPTY) HoleId[i] = minu(k,HoleId[i -  8]);
        if (board[i -  1] == EMPTY) HoleId[i] = minu(k,HoleId[i -  1]);
      }
    }
  }
  /* find parity of holes: */
  RegionParity = 0;
  for (i = 10; i <= 80; i++){
    RegionParity ^= HoleId[i];
  }
  /* create list of empty squares: */
  k = 0;
  pt = &EmHead;
  for (i = 60-1; i >= 0; i--){
    sqnum = worst2best[i];
    if (board[sqnum] == EMPTY) {
      pt->succ = &(Ems[k]);
      Ems[k].pred = pt;
      k++;
      pt = pt->succ;
      pt->square = sqnum;
      pt->hole_id = HoleId[sqnum];
    }
  }
  pt->succ = NULL;
  if (k > MAXEMPTIES) abort(); /* better not have too many empties... */
}

int
NoParEndSolve (uchar *board, double alpha, double beta, 
               int color, int empties, int discdiff, int prevmove )
{
  node_count++;

  int score = -INFINITY;
  int oppcol = 2-color;
  int sqnum,j,ev;
  struct EmList *em, *old_em;
  for (old_em = &EmHead, em = old_em->succ; em != NULL;
      old_em = em, em = em->succ){
    /* go thru list of possible move-squares */
    sqnum = em->square;
    j = DoFlips( board, sqnum, color, oppcol );
    if (j) { /* legal move */
      /* place your disc: */
      *(board+sqnum) = color;
      /* delete square from empties list: */
      old_em->succ = em->succ;
      if (empties == 2){ /* So, now filled but for 1 empty: */
        int j1;
        j1 = CountFlips( board, EmHead.succ->square, oppcol, color);
        if (j1) { /* I move then he moves */
          ev = discdiff + 2*(j-j1);
        }
        else { /* he will have to pass */
          j1 = CountFlips(board, EmHead.succ->square, color, oppcol);
          ev = discdiff + 2*j;
          if (j1) { /* I pass then he passes then I move */
            ev += 2 * (j1 + 1);
          }
          else { /* I move then both must pass, so game over */
#if WINNER_GETS_EMPTIES
            if (ev >= 0)
              ev += 2;
#else
            ev++;
#endif
          }
        }
      }
      else {
        ev = -NoParEndSolve(board, -beta, -alpha, 
                            oppcol, empties-1, -discdiff-2*j-1, sqnum);
      }
      UndoFlips(j, oppcol);
      /* un-place your disc: */
      *(board+sqnum) = EMPTY;
      /* restore deleted empty square: */
      old_em->succ = em;

      if (ev > score) { /* better move: */
        score = ev;
        if (ev > alpha) {
          alpha = ev;
          if (ev >= beta) { /* cutoff */
            return score;
          }
        }
      }
    }
  }
  if (score == -INFINITY) {  /* No legal move */
    if (prevmove == 0) { /* game over: */
      leaf_count++;
#if WINNER_GETS_EMPTIES
      if (discdiff > 0) return discdiff+empties;
      if (discdiff < 0) return discdiff-empties;
      return 0;
#else
      return discdiff;
#endif
    }
    else /* I pass: */
      return -NoParEndSolve( board, -beta, -alpha, oppcol, empties, -discdiff, 0);
  }
  return score;
}

int
ParEndSolve (uchar *board, double alpha, double beta, 
             int color, int empties, int discdiff, int prevmove )
{
  node_count++;

  int score = -INFINITY;
  int oppcol = 2-color;
  int sqnum,j,ev;
  struct EmList *em, *old_em;
  uint parity_mask;
  int par, holepar;

  for (par = 1, parity_mask = RegionParity; par >= 0;
       par--, parity_mask = ~parity_mask) {

    for (old_em = &EmHead, em = old_em->succ; em != NULL;
         old_em = em, em = em->succ){
      /* go thru list of possible move-squares */
      holepar = em->hole_id;
      if (holepar & parity_mask) {
        sqnum = em->square;
        j = DoFlips(board, sqnum, color, oppcol);
        if (j) { /* legal move */
          /* place your disc: */
          *(board+sqnum) = color;
          /* update parity: */
          RegionParity ^= holepar;
          /* delete square from empties list: */
	  old_em->succ = em->succ;
          if (empties <= 1 + USE_PARITY)
            ev = -NoParEndSolve(board, -beta, -alpha, 
                                oppcol, empties-1, -discdiff-2*j-1, sqnum);
          else
            ev = -ParEndSolve(board, -beta, -alpha, 
                              oppcol, empties-1, -discdiff-2*j-1, sqnum);
          UndoFlips(j, oppcol);
          /* restore parity of hole */
          RegionParity ^= holepar;
          /* un-place your disc: */
          *(board+sqnum) = EMPTY;
          /* restore deleted empty square: */
	  old_em->succ = em;

          if(ev > score){ /* better move: */
            score = ev;
            if(ev > alpha){
              alpha = ev;
              if(ev >= beta){ 
                return score;
              }
	    }
          }
        }
      }
    }
  }

  if (score == -INFINITY) {  /* No legal move found */
    if (prevmove == 0) { /* game over: */
      leaf_count++;
#if WINNER_GETS_EMPTIES
      if (discdiff > 0) return discdiff+empties;
      if (discdiff < 0) return discdiff-empties;
      return 0;
#else
      return discdiff;
#endif
    }
    else /* I pass: */
      return -ParEndSolve(board, -beta, -alpha, oppcol, empties, -discdiff, 0);
  }
  return score;
}

int
count_mobility (uchar *board, int color) {
  int oppcol = 2 - color;
  int mobility;
  int square;
  struct EmList *em;

  mobility = 0;
  for (em = EmHead.succ; em != NULL; em = em->succ) {
    square = em->square;
    if (AnyFlips(board, square, color, oppcol))
      mobility++;
  }

  return mobility;
}

int
FastestFirstEndSolve (uchar *board, double alpha, double beta, 
		      int color, int empties, int discdiff,
		      int prevmove) {
  int i, j;
  int score = -INFINITY;
  int oppcol = 2 - color;
  int sqnum, ev;
  int flipped;
  int moves, mobility;
  int best_value, best_index;
  struct EmList *em, *old_em;
  struct EmList *move_ptr[64];
  int holepar;
  int goodness[64];

  node_count++;

  moves = 0;
  for (old_em = &EmHead, em = old_em->succ; em != NULL;
       old_em = em, em = em->succ ) {
    sqnum = em->square;
    flipped = DoFlips( board, sqnum, color, oppcol );
    if (flipped) {
      board[sqnum] = color;
      old_em->succ = em->succ;
      mobility = count_mobility(board, oppcol);
      old_em->succ = em;
      UndoFlips(flipped, oppcol);
      board[sqnum] = EMPTY;
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
      j = DoFlips( board, sqnum, color, oppcol );
      board[sqnum] = color;
      RegionParity ^= holepar;
      em->pred->succ = em->succ;
      if (em->succ != NULL)
	em->succ->pred = em->pred;
      if (empties <= FASTEST_FIRST + 1)
	ev = -ParEndSolve( board, -beta, -alpha, oppcol, empties - 1,
			   -discdiff - 2 * j - 1, sqnum);
      else
	ev = -FastestFirstEndSolve(board, -beta, -alpha, oppcol,
                                   empties - 1, -discdiff - 2 * j - 1,
                                   sqnum);
      UndoFlips(j, oppcol);
      RegionParity ^= holepar;
      board[sqnum] = EMPTY;
      em->pred->succ = em;
      if (em->succ != NULL)
	em->succ->pred = em;

      if (ev > score) { /* better move: */
	score = ev;
	if (ev > alpha) {
	  alpha = ev;
	  if (ev >= beta) {
	    return score;
          }
	}
      }
    }
  }
  else {
    if (prevmove == 0) { // game-over
      leaf_count++;
      if (discdiff > 0)
	return discdiff + empties;
      if (discdiff < 0)
	return discdiff - empties;
      return 0;
    }
    else { /* I pass: */
      score = -FastestFirstEndSolve(board, -beta, -alpha, oppcol,
                                    empties, -discdiff, 0);
    }
  }

  return score;
}

/** 
 * The search itself. Assumes relevant data structures have been set up with
 * PrepareToSolve.
 * color is the color on move. Discdiff is color disc count - opposite
 * color disc count. The value of this at the end of the game is returned.
 * prevmove==0 if previous move was a pass, otherwise non0.
 * empties>0 is number of empty squares.
 */
int
EndSolve (uchar *board, double alpha, double beta, 
          int color, int empties, int discdiff, int prevmove)
{
  if (empties > FASTEST_FIRST)
    return FastestFirstEndSolve(board,alpha,beta,color,empties,discdiff,prevmove);
  else {
    if (empties <= (2>USE_PARITY ? 2 : USE_PARITY))
      return NoParEndSolve(board,alpha,beta,color,empties,discdiff,prevmove);
    else
      return ParEndSolve(board,alpha,beta,color,empties,discdiff,prevmove);
  }
}

/**
 * 2 for full, make 1 if just WLD solve, 0 if WD solve.
 */
#define FULLSOLVE 2

// FFO-40 w..wwwwb.wwwwwwbwwbbwwwbwwbwwwbbwwwwwwbb...wwwwb....w..b........
// FFO-41 .wwwww....wwwwb..wwwwww.bbbbbww..bbwwb..wwbwbb....wbbw...www..w.
// FFO-42 ..www.......bb.wwwwwwbww.wwwwbwwb.wwwbbw...wwbww...wwwbw..wwww..

int
main (void) {
  int val, emp, wc, bc, j, k, x, y;
  char bds[65] = {
    "w..wwwwb.wwwwwwbwwbbwwwbwwbwwwbbwwwwwwbb...wwwwb....w..b........"
  };
  for (j = 0; j <= 90; j++) board[j] = DUMMY;
  wc=bc=emp=0;
  for (j = 0; j < 64; j++) {
    x = j&7; y = (j>>3)&7; k = x+10+9*y;
    if (bds[j]=='w'){ board[k] = WHITE; wc++; }
    else if (bds[j]=='b'){ board[k] = BLACK; bc++; }
    else if (bds[j]=='.'){ board[k] = EMPTY; emp++; }
  }
  PrepareToSolve(board);

#if FULLSOLVE==2
  val = EndSolve(board, -64, 64, BLACK, emp, -wc+bc, 1);
#else
#if FULLSOLVE==1
  val = EndSolve(board,  -1,  1, BLACK, emp, -wc+bc, 1);
#else
  val = EndSolve(board,   0,  1, BLACK, emp, -wc+bc, 1);
#endif
#endif

  printf("%3d (emp=%2d wc=%2d bc=%2d) %s\n", val, emp, wc, bc, bds);

  printf("USE_PARITY=%d. FULLSOLVE=%d. WINNER_GETS_EMPTIES=%d. FASTEST_FIRST=%d.\n",
         USE_PARITY, FULLSOLVE, WINNER_GETS_EMPTIES, FASTEST_FIRST);

  printf("[node_count=%llu, leaf_count=%llu]\n", node_count, leaf_count);

  return EXIT_SUCCESS;
}

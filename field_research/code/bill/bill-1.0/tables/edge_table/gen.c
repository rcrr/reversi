/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* Edge table - includes both X-squares.

   Kai-Fu Lee
   11/4/85
 */

#include <stdio.h>
#define Opposite(x) (!(x))
#define MAX_PIECES 10
#define BLACK_INF 99999999/* Best Black can do */
#define WHITE_INF -99999999/* Best White can do */
#define FALSE 0
#define TRUE 1
#define BLACK 0
#define WHITE 1
#define EMPTY 2
#define UNSTABLE 0  /* Indices into the static */
#define ALONE 1      /* evaluation array */
#define NOT_UNSTABLE 2 
#define UNANCHORED_STABLE 3
#define PSEUDO_STABLE 4
#define ALMOST_STABLE 5
#define STABLE 6
#define PROB_FILE "/usr/kfl/othello/edge.prob"
#define Abs(x) (((x) > 0) ? (x) : -(x))
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) > (y)) ? (y) : (x))
#define TABLE_SIZE 59049

/* Probabilistic moves (moves to an edge that do not flip edge dics) */

typedef struct imove
{
  double  score;
  double  prob;
} imove;


/* Table entry = value of edge position (may not be correct)
   and whether the entry has converged (then value is correct)
 */

struct table
{
  double	  bscore, wscore;
  double	  stat_eval;
  int		  converged;
} Edge_Table[3][3][3][3][3][3][3][3][3][3];

double  eval_move (), find_prob (), prob (), prob2 ();

/* The array value contains the static value of (stability, square-index) */
double  Value[7][8] =
{
  800, -50, 20, 15, 15, 20, -50, 800,
  800, -75, -25, -50, -50, -25, -75, 800,
  800, -150, 100, 100, 100, 100, -150, 800,   /* 200 for c-sq if has seq6 */
  800, 100, 300, 200, 200, 300, 100, 800,
  800, 800, 800, 800, 800, 800, 800, 800,
  800, 1000, 1000, 1000, 1000, 1000, 1000, 800,
  800, 1200, 1000, 1000, 1000, 1000, 1200, 800
};

/* double  Prob[19683][8]; */
int   Converged;


main ()
{
  int j0, j1, j2, j3, j4, j5, j6, j7;
  register int j8, j9;
  int edge[MAX_PIECES];

/*  read_prob (PROB_FILE); */
  Converged = 0;

  for (j0 = 0; j0 <= 2; j0++)
  {
    edge[0] = j0;
    for (j1 = 0; j1 <= 2; j1++)
    {
      edge[1] = j1;
      for (j2 = 0; j2 <= 2; j2++)
      {
	edge[2] = j2;
	for (j3 = 0; j3 <= 2; j3++)
	{
	  edge[3] = j3;
	  for (j4 = 0; j4 <= 2; j4++)
	  {
	    edge[4] = j4;
	    for (j5 = 0; j5 <= 2; j5++)
	    {
	      edge[5] = j5;
	      for (j6 = 0; j6 <= 2; j6++)
	      {
		edge[6] = j6;
		for (j7 = 0; j7 <= 2; j7++)
		{
		  edge[7] = j7;
		  for (j8 = 0; j8 <= 2; j8++)
		  { 
		    edge[8] = j8;
		    for (j9 = 0; j9 <= 2; j9++)
		    {
		      edge[9] = j9;
		      initialize_edge (edge, &Edge_Table[j0][j1][j2][j3][j4][j5][j6][j7][j8][j9]);
		    }
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }

  for (j0 = 0; j0 < 10; j0++)
    edge[j0] = EMPTY;
  compute_edge (edge, &Edge_Table[EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY]);
  
  average_equiv ();
  subtract_empty ();
  print_result (Edge_Table);
  return 0;
}



/* Average symmetric positions together */

average_equiv ()
{
  int j0, j1, j2, j3, j4, j5, j6, j7;
  int r0, r1, r2, r3, r4, r5, r6, r7;
  register int j8, j9;
  register int r8, r9;
  static int rev[3] = { WHITE, BLACK, EMPTY };

  for (j0 = 0; j0 <= 2; j0++)
    {
      r0 = rev[j0];
      for (j1 = 0; j1 <= 2; j1++)
	{
	  r1 = rev[j1];
	  for (j2 = 0; j2 <= 2; j2++)
	    {
	      r2 = rev[j2];
	      for (j3 = 0; j3 <= 2; j3++)
		{
		  r3 = rev[j3];
		  for (j4 = 0; j4 <= 2; j4++)
		    {
		      r4 = rev[j4];
		      for (j5 = 0; j5 <= 2; j5++)
			{
			  r5 = rev[j5];
			  for (j6 = 0; j6 <= 2; j6++)
			    {
			      r6 = rev[j6];
			      for (j7 = 0; j7 <= 2; j7++)
				{
				  r7 = rev[j7];
				  for (j8 = 0; j8 <= 2; j8++)
				    {
				      r8 = rev[j8];
				      for (j9 = 0; j9 <= 2; j9++)
					{
					  register struct table *e1, *e2;

					  r9 = rev[j9];
					  e1 = &Edge_Table[j0][j1][j2][j3][j4][j5][j6][j7][j8][j9];
					  e2 = &Edge_Table[j7][j6][j5][j4][j3][j2][j1][j0][j9][j8];
					  e1->bscore =
					    e2->bscore = (e1->bscore + e2->bscore -
							  Edge_Table[r0][r1][r2][r3][r4][r5][r6][r7][r8][r9].wscore -
							  Edge_Table[r7][r6][r5][r4][r3][r2][r1][r0][r9][r8].wscore) *
							  0.25;
					}
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
}


subtract_empty ()
{
  register int score, i;

  score = Edge_Table[EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY][EMPTY].bscore;
#if PRINT
  fprintf (stderr, "Subtracting %d for all guys\n", score);
#endif
  if (score == 0)
    return;
  for (i = 0; i < TABLE_SIZE; i++)
    ((struct table *)Edge_Table)[i].bscore -= score;
}


/*****************************************************************
                       IO Routines 
 *****************************************************************/

/* print out a .c source file for use by Bill.
   This version prints all blacks, then all whites */

print_result (tab)
     struct table tab[3][3][3][3][3][3][3][3][3][3];
{
  register int i = 0, j8, j9;
  int j0, j1, j2, j3, j4, j5, j6, j7;
  double max_edge = 0.0, factor;

  for (i = 0; i < TABLE_SIZE; i++)
    if (((struct table *)Edge_Table)[i].bscore > max_edge)
      max_edge = ((struct table *)Edge_Table)[i].bscore;
  factor = 1000.0 / max_edge;

  printf ("short Edge_Table[3][3][3][3][3][3][3][3][3][3] =\n{\n");
  i = 0;
  for (j0 = 0; j0 <= 2; j0++)
    for (j1 = 0; j1 <= 2; j1++)
      for (j2 = 0; j2 <= 2; j2++)
	for (j3 = 0; j3 <= 2; j3++)
	  for (j4 = 0; j4 <= 2; j4++)
	    for (j5 = 0; j5 <= 2; j5++)
	      for (j6 = 0; j6 <= 2; j6++)
		for (j7 = 0; j7 <= 2; j7++)
		{
		  printf ("   ");
		  for (j8 = 0; j8 <= 2; j8++)
		    for (j9 = 0; j9 <= 2; j9++)
			printf (" %5d,", (int)(tab[j9][j8][j7][j6][j5][j4][j3][j2][j1][j0].bscore * factor));
		  printf("\n");
		}
  printf ("};\n");
}



/***************************************************************
                 Static Evaluation Procedures
 ***************************************************************/

/* Compute the static edge value by using the Value array on 
   stable/NOT_UNSTABLE/unstable pieces.  These static values
   are to be used for the MINIMAX search.
 */

initialize_edge (edge, entry)
     register int edge[MAX_PIECES];
     register struct table *entry;
{
  register int i;
  int stability[MAX_PIECES];

  for (i = 0; i <= 7; i++)
    stability[i] = UNSTABLE;

  find_stable (edge, stability);
  find_semi (edge, stability);

  entry->converged = TRUE;
  entry->stat_eval = 0;
  for (i = 0; i <= 7; i++)
    switch (edge[i])
      {
      case BLACK:
	entry->stat_eval += Value[stability[i]][i];
	break;

      case WHITE:
	entry->stat_eval -= Value[stability[i]][i];
	break;

      default:
	entry->converged = FALSE;
      }

  /* Figure out value of x squares */
  entry->stat_eval += 
	x_square_stat (edge[8], edge[0], edge[1], edge[2]) +
        x_square_stat (edge[9], edge[7], edge[6], edge[5]);

  /* Adjust value -- c square is worth something if have 6 in a row */
  if (edge[0] == EMPTY && edge[1] == BLACK && edge[2] == BLACK &&
	edge[3] == BLACK && edge[4] == BLACK && edge[5] == BLACK &&
	edge[6] == BLACK && edge[7] == EMPTY && edge[8] != BLACK && 
	edge[9] != BLACK)
      entry->stat_eval += 700;
  else if (edge[0] == EMPTY && edge[1] == WHITE && edge[2] == WHITE &&
	edge[3] == WHITE && edge[4] == WHITE && edge[5] == WHITE &&
	edge[6] == WHITE && edge[7] == EMPTY && edge[8] != WHITE && 
	edge[9] != WHITE)
      entry->stat_eval -= 700;
  /* some bonus for balanced 4 */
  else if (edge[0] == EMPTY && edge[1] == EMPTY && edge[2] == BLACK &&
	edge[3] == BLACK && edge[4] == BLACK && edge[5] == BLACK &&
	edge[6] == EMPTY && edge[7] == EMPTY && edge[8] != BLACK && 
	edge[9] != BLACK)
      entry->stat_eval += 250;
  else if (edge[0] == EMPTY && edge[1] == EMPTY && edge[2] == WHITE &&
	edge[3] == WHITE && edge[4] == WHITE && edge[5] == WHITE &&
	edge[6] == EMPTY && edge[7] == EMPTY && edge[8] != WHITE && 
	edge[9] != WHITE)
      entry->stat_eval -= 250;
  /* unbalanced edge */
  else if (edge[0] == EMPTY && edge[1] == BLACK && edge[2] == BLACK &&
	edge[3] == BLACK && edge[4] == BLACK && edge[5] == BLACK &&
	edge[6] == EMPTY && edge[7] == EMPTY)
    {
      if (edge[9] == WHITE)
	entry->stat_eval -= 1650;
      else if (edge[9] == EMPTY)
	entry->stat_eval -= 1150;
    }
  else if (edge[0] == EMPTY && edge[1] == WHITE && edge[2] == WHITE &&
	   edge[3] == WHITE && edge[4] == WHITE && edge[5] == WHITE &&
	   edge[6] == EMPTY && edge[7] == EMPTY)
    {
      if (edge[9] == BLACK)
	entry->stat_eval += 1650;
      else if (edge[9] == EMPTY)
        entry->stat_eval += 1150;
    }
  else if (edge[0] == EMPTY && edge[1] == EMPTY && edge[2] == BLACK &&
	edge[3] == BLACK && edge[4] == BLACK && edge[5] == BLACK &&
	edge[6] == BLACK && edge[7] == EMPTY )
    {
      if (edge[8] == WHITE)
	entry->stat_eval -= 1650;
      else if (edge[8] == EMPTY)
        entry->stat_eval -= 1150;
    }
  else if (edge[0] == EMPTY && edge[1] == EMPTY && edge[2] == WHITE &&
	edge[3] == WHITE && edge[4] == WHITE && edge[5] == WHITE &&
	edge[6] == WHITE && edge[7] == EMPTY)
    {
      if (edge[8] == BLACK)
	entry->stat_eval += 1650;
      else if (edge[8] == EMPTY)
        entry->stat_eval += 1150;
    }

  entry->bscore = entry->wscore = entry->stat_eval;
  if (edge[8] == EMPTY || edge[9] == EMPTY) 
    entry->converged = FALSE;
  else if (entry->converged)
    Converged++;
}



x_square_stat (x, cor, c, b)
int x, cor, c, b;
{ 
  /* These numbers represent : given that x is occupied,
     cor, c, b are (same, diff, EMPTY)
  */
  static int x_square_table[3][3][3] =
  { 100,  75,  75,  50,  50,  50,  75,  50,   0,
      0,   0, -25,   0,   0, -25, -25, -25, -50,
   -125,   0,-100,  50,-100, -75, -75, -75, -50
  };

  if (x == EMPTY) return (0);
  else
    if (x == WHITE)
      return (-x_square_table[x_conv (x, cor)][x_conv (x, c)][x_conv (x, b)]);
  else
      return (x_square_table[x_conv (x, cor)][x_conv (x, c)][x_conv (x, b)]);
}

x_conv (x, other)
int x, other;
{
  if (x == other) return (0);
   else if (x == EMPTY) return (2);
   else return (1);
}


/* Finds all the stable pieces by:
     start from each of the two corners
     find last non-EMPTY disc (last)
     find the first disc in same-color sequence leading up to last (first)
     if there is no change of color, everything up to last is stable.
     if there is a change of color (first != real first disc), everything
       from the corner to the disc before first is stable.
   STABLE = Can never be flipped.
   ALMOST_STABLE = Can be unstable only after 2 (MAX) "stupid" forced moves.
   PSEUDO_STABLE = Can be unstable only after 1 "stupid" forced move.
 */

find_stable (edge, stability)
     register int edge[MAX_PIECES];
     int stability[MAX_PIECES];
{
  register int i, last, first;
  int   num_empty,
        first1;

  if (edge[0] != EMPTY)
  {
    for (i = 1; i <= 7; i++)
      if (edge[i] == EMPTY)
	break;

  /* Last is the last non-EMPTY disc */
    last = i - 1;
    if (last == 7)
    {	 /* Everything is stable */
      for (i = 0; i <= 7; i++)
	stability[i] = STABLE;
      return;
    }
    else
    {
      for (i = last + 1; i <= 7; i++)
	if (edge[i] != EMPTY)
	  break;
      num_empty = i - last - 1;

      for (i = last; i >= 0; i--)
	if (edge[i] != edge[last])
	  break;

    /* first is the first disc in same-color sequence leading up to last 
    */
      first = i + 1;
      if (first == 0)
      {	 /* Everything up to last is stable */
	for (i = 0; i <= last; i++)
	  stability[i] = STABLE;
      }
      else
      {	 /* Everything up to, but not including, first is stable */
	for (i = 0; i < first; i++)
	  stability[i] = STABLE;
	if (num_empty >= 2)
	{
	  for (i = first - 1; i >= 0; i--)
	    if (edge[i] != edge[first - 1])
	      break;
	  first1 = i + 1;
	  if (first1 != 0)
	  {
	    for (i = first1; i < first; i++)
	      stability[i] = PSEUDO_STABLE;
	    if (num_empty >= 3)
	    {
	      for (i = first1 - 1; i >= 0; i--)
		if (edge[i] != edge[first1 - 1])
		  break;
	      first = i + 1;
	      if (first != 0)
		for (i = first; i < first1; i++)
		  stability[i] = ALMOST_STABLE;
	    }
	  }
	}
      }
    }
  }

  if (edge[7] != EMPTY)
  {
    for (i = 7; i >= 0; i--)
      if (edge[i] == EMPTY)
	break;

  /* Last is the last non-EMPTY disc */
    last = i + 1;

    for (i = last - 1; i >= 0; i--)
      if (edge[i] != EMPTY)
	break;
    num_empty = last - i - 1;

    for (i = last; i <= 7; i++)
      if (edge[i] != edge[last])
	break;

  /* first is the first disc in same-color sequence leading up to last */
    first = i - 1;
    if (first == 7)
    {	 /* Everything up to last is stable */
      for (i = 7; i >= last; i--)
	stability[i] = STABLE;
    }
    else
    {	 /* Everything up to, but not including, first is stable */
      for (i = 7; i > first; i--)
	stability[i] = STABLE;
      if (num_empty >= 2)
      {
	for (i = first + 1; i <= 7; i++)
	  if (edge[i] != edge[first + 1])
	    break;
	first1 = i - 1;
	if (first1 == 7)
	  return;
	for (i = first1; i > first; i--)
	  stability[i] = PSEUDO_STABLE;
	if (num_empty >= 3)
	{
	  for (i = first1 + 1; i <= 7; i++)
	    if (edge[i] != edge[first1 + 1])
	      break;
	  first = i - 1;
	  if (first == 7)
	    return;
	  for (i = first; i > first1; i--)
	    stability[i] = ALMOST_STABLE;
	}
      }
    }
  }

  return;
}


/* Finds semi-stable pieces
   This is done by :
     finding all unstable pieces by :
       making all possible moves, and see if a piece has changed in color.
       if so, it is unstable.
     everything that is not unstable and not stable is semi-stable.
*/

find_semi (edge, stability)
     register int edge[MAX_PIECES];
     int stability[MAX_PIECES];
{
  register int i, j;
  int unstable[MAX_PIECES];

  for (i = 0; i <= 7; i++)
    unstable[i] = FALSE;

  for (i = 0; i <= 7; i++)
    if (edge[i] == EMPTY)
      make_move (unstable, edge, i);

  for (i = 0; i <= 7; i++)
    {
      if (unstable[i])
	continue;

      switch (stability[i])
	{
	case STABLE:
	case PSEUDO_STABLE:
	case ALMOST_STABLE:
	  continue;
	}

      if (i > 0 && i < 7 && edge[i-1] == EMPTY && edge[i+1] == EMPTY)
	{
	  stability[i] = ALONE;
	  continue;
	}

      stability[i] = NOT_UNSTABLE;
      for (j = i+2; j <= 7; j++)
        if (edge[j] == EMPTY && edge[j-1] == Opposite (edge[i]))
	  goto ok0;
      continue;

    ok0:
      for (j = i-2; j >= 0; j--)
        if (edge[j] == EMPTY && edge[j+1] == Opposite (edge[i]))
	  goto ok1;
      continue;

    ok1:
      stability[i] = UNANCHORED_STABLE;
    }
}


/* Tries the (only) two possible moves in the two directions (+1, -1),
   finds pieces that are unstable and mark them in the unstable array
   as such.  Don't need to worry which color is to move since only
   one color can move for each direction.
 */

make_move (unstable, edge, move)
register   unstable[MAX_PIECES],
      edge[MAX_PIECES],
      move;
{
  register int i, j;

  if (move != 7)
  {
    if (edge[move + 1] != EMPTY)
      for (i = move + 2; i <= 7; i++)
      {
	if (edge[i] == EMPTY)
	  break;
	else
	  if (edge[i] == Opposite (edge[move + 1]))
	  {
	    for (j = move + 1; j <= i - 1; j++)
	      unstable[j] = TRUE;
	    break;
	  }
      }
  }

  if (move != 0)
  {
    if (edge[move - 1] != EMPTY)
      for (i = move - 2; i >= 0; i--)
	if (edge[i] != EMPTY && edge[i] == Opposite(edge[move - 1]))
	  {
	    for (j = move - 1; j >= i + 1; j--)
	      unstable[j] = TRUE;
	    break;
	  }
  }
}



/* Two comparison routines used by qsort called in compute_edge */

incr_cmp (x, y)
register imove *x,
  *y;
{
  if (x -> score > y -> score)
    return (1);
  else if (x -> score < y -> score)
    return (-1);
  else
    return (0);
}

decr_cmp (x, y)
register imove *x,
  *y;
{
  if (x -> score > y -> score)
    return (-1);
  else if (x -> score < y -> score)
    return (1);
  else
    return (0);
}


/***************************************************************
                   MINiMAX Search Routines
 ***************************************************************/

/* Compute the edge value by finding all possible moves and picking
   the best.  It is recursivly applied (MINiMAX) until all values
   can be computed directly (all leaf nodes have converged values).
   Recursion actually takes place in eval_move.

 */

compute_edge (edge, entry)
     register int edge[MAX_PIECES];
     register struct table *entry;
{
  int   black_num_illegals, white_num_illegals;
  double  black_best_legal, white_best_legal,
          btemp, wtemp,
          bm_score, wm_score,
          bnm_score, wnm_score;
  struct imove  black_illegals[20], white_illegals[20];

  compute_color_edge (edge, BLACK, &black_best_legal, &black_num_illegals, black_illegals);
  compute_color_edge (edge, WHITE, &white_best_legal, &white_num_illegals, white_illegals);

  bm_score = prob (black_best_legal, black_num_illegals, black_illegals, BLACK);
  wm_score = prob (white_best_legal, white_num_illegals, white_illegals, WHITE);

  if (entry->stat_eval < wm_score)
    wtemp = prob (entry->stat_eval, white_num_illegals, white_illegals, WHITE);
  else
    wtemp = wm_score;
  if (entry->stat_eval > bm_score)
    btemp = prob (entry->stat_eval, black_num_illegals, black_illegals, BLACK);
  else
    btemp = bm_score;
  wnm_score = prob (btemp, white_num_illegals, white_illegals, WHITE);
  bnm_score = prob (wtemp, black_num_illegals, black_illegals, BLACK);

/*
  if (edge[0] == EMPTY && edge[1] == BLACK && edge[2] == EMPTY && edge[3] == WHITE && edge[4] == BLACK && edge[5] == WHITE && edge[6] == EMPTY && edge[7] == EMPTY && edge[8] == WHITE && edge[9] == EMPTY)
    {
      printf ("bmscore = %f; bnm_score = %f\n", bm_score, bnm_score);
      printf ("best legal = %f\n", black_best_legal);
      for (i = 0; i < black_num_illegals; i++)
        printf ("illegal[%d] = %f = %f\n", i, 
	  black_illegals[i].prob, black_illegals[i].score);
      fflush (stdout);
    }
*/

  entry->bscore = MAX(bm_score, bnm_score);
  entry->wscore = MIN(wm_score, wnm_score);
  entry->converged = TRUE;
  Converged++;
}

/* Return the likely score with a number of illegals (each with probability)
   and a legal.
 */

double prob (best_legal, num_illegals, illegals, turn)
     int num_illegals;
     int turn;
     double best_legal;
     struct imove illegals[20];
{
  double left_prob, total;
  register int i;

  if (best_legal == BLACK_INF || best_legal == WHITE_INF)
    return (best_legal);
  left_prob = 1.0;
  total = 0;

  for (i = 0; i < num_illegals; i++)
  {
    if ((turn == WHITE && illegals[i].score > best_legal) ||
	(turn == BLACK && illegals[i].score < best_legal))
      break;
    total += (illegals[i].score * illegals[i].prob * left_prob);
    left_prob *= (1 - illegals[i].prob);
  }
  return (total + best_legal * left_prob);
}


compute_color_edge (edge, turn, best_legal, num_illegals, illegals)
int   edge[MAX_PIECES], turn, *num_illegals;
double *best_legal;
register struct imove  illegals[20];
{
  register  i;
  double  worth,
          best,
          temp_prob;
  if (turn == WHITE)
    best = BLACK_INF;
  else
    best = WHITE_INF;

  *num_illegals = 0;
 /* Go through each possible move */
  for (i = 0; i < 10; i++)
    {
      if (edge[i] != EMPTY)
	continue;

    /* minimax here */
      worth = eval_move (edge, i, turn, &temp_prob);
      if (temp_prob >= 1.0)
	{
	  if ((turn == BLACK && worth > best) ||
	      (turn == WHITE && worth < best))
	    best = worth;
	}
      else
	{
	  illegals[*num_illegals].score = worth;
	  illegals[(*num_illegals)++].prob = temp_prob;
	}
    }

  *best_legal = best;
  if (turn == BLACK)
    qsort ((char *) illegals, *num_illegals, sizeof (struct imove), decr_cmp);
  else
    qsort ((char *) illegals, *num_illegals, sizeof (struct imove), incr_cmp);
}


/* Eval move tries out an empty square for a move for turn,
   it returns a value for the move if the value has converged.
   Otherwise, it calls compute_move recursively to get the
   value 
 */
double  eval_move (edge, move, turn, prob)
     register int edge[MAX_PIECES];
     int move, turn;
     double *prob;
{
  register int i, j;
  int   new[MAX_PIECES];
  register struct table *entry;
  int move_ok;
  double p1, p2;

  for (i = 0; i < MAX_PIECES; i++)
    new[i] = edge[i];

  move_ok = FALSE;

  if (move < 7)
  {
    if (new[move + 1] == Opposite (turn))
      for (i = move + 2; i <= 7; i++)
      {
	if (new[i] == EMPTY)
	  break;

	if (new[i] == turn)
	  {
	    for (j = move; j <= i - 1; j++)
	      new[j] = turn;
	    move_ok = 1;
	    break;
	  }
      }
  }

  if (move != 0 && move != 8 && move != 9)
  {
    if (new[move - 1] == Opposite (turn))
      for (i = move - 2; i >= 0; i--)
      {
	if (new[i] == EMPTY)
	  break;

	if (new[i] == turn)
	  {
	    for (j = move; j >= i + 1; j--)
	      new[j] = turn;
	    move_ok = 1;
	    break;
	  }
      }
  }

  new[move] = turn;

  if (move_ok)
    *prob = 1;
  else
    *prob = find_prob (edge, turn, move);

  /* x-square not flipped */
  entry = &Edge_Table[new[0]][new[1]][new[2]][new[3]][new[4]][new[5]][new[6]][new[7]][new[8]][new[9]];
  if (turn == BLACK && entry->converged)
    p1 = entry->wscore;
  else if (turn == WHITE && entry->converged)
    p1 = entry->bscore;
  else
    {
      compute_edge (new, entry);
      p1 = turn == BLACK ? entry->wscore : entry->bscore;
    }

  /* x-squares got flipped ... */
  if ((move <= 2 && move >= 0 && new[8] == Opposite (turn)) ||
      (move >= 5 && move <= 7 && new[9] == Opposite (turn)))
  {  
    if (move <= 2) new[8] = turn;
    else if (move >= 5) new[9] = turn;
    
    entry = &Edge_Table[new[0]][new[1]][new[2]][new[3]][new[4]][new[5]][new[6]][new[7]][new[8]][new[9]];
    if (turn == BLACK && entry->converged)
      p2 = entry->wscore;
    else if (turn == WHITE && entry->converged)
      p2 = entry->bscore;
    else
      {
        compute_edge (new, entry);
        if (turn == WHITE)
	  p2 = entry->bscore;
        else  
	  p2 = entry->wscore;  
      }

    switch (move)
      {
      case 0:
      case 7:
	if (move_ok)
	  return (p2 * 0.75 + p1 * 0.25);
	else
	  return (p2 * 0.95 + p1 * 0.05);

      case 1:
      case 6:
	if (move_ok)
	  return (p2 * 0.6 + p1 * 0.4);
	else
	  return (p2 * 0.8 + p1 * 0.2);

      case 2:
      case 5:
	if (move_ok)
	  return (p2 * 0.175 + p1 * 0.825);
	else
	  return (p2 * 0.3 + p1 * 0.7);
      }
  }
  return p1;
}


/***************************************************************
                   Miscellaneous
 ***************************************************************/

double find_prob (edge, turn, move)
     register int edge[MAX_PIECES];
     register int turn;
     int move;
{
  register int opp_turn = Opposite(turn);
  int i;
  double coef, opp;

  for (opp = 0, coef = 0, i = 0; i < 8; i++)
  {
    if (edge[i] == opp_turn) opp++;
    if (edge[i] != EMPTY) coef++;
  }

  /* x-square moves */
  if (move == 8 || move == 9)
    return (0.5 + coef * 0.03);

  /* corner moves */
  if (move == 0 && edge[1] == opp_turn &&
      !(edge[2] == opp_turn && edge[3] == opp_turn &&
	edge[4] == opp_turn && edge[5] == opp_turn &&
	edge[6] == opp_turn))
    coef += 2;
  else if (move == 0 && edge[1] == turn)
    coef--;
  else if (move == 7 && edge[6] == opp_turn &&
      !(edge[1] == opp_turn && edge[2] == opp_turn &&
	edge[3] == opp_turn && edge[4] == opp_turn &&
	edge[5] == opp_turn))
    coef += 2;
  else if (move == 7 && edge[6] == turn)
    coef--;
  if (coef < 0) coef = 0;
  else if (coef > 10) coef = 10;

  if (move == 0)
    {
      if (edge[8] == EMPTY)
	return coef * coef * 0.0011;
      else if (edge[8] == turn)
	return coef * coef * 0.0003;
      else
	return coef * 0.01 + 0.89;
    }
  else if (move == 7)
    {
      if (edge[9] == EMPTY)
	return coef * coef * 0.0011;
      else if (edge[9] == turn)
	return coef * coef * 0.0003;
      else return coef * 0.01 + 0.89;
    }

  /* Non-corner && non-x-square moves */
  coef = coef + coef + opp;


  {
    register int next = edge[move + 1], prev = edge[move - 1];

    if (next == EMPTY)
      {
	if (prev == EMPTY)
	  coef += 10;
	else if (prev == turn)
	  ;
	else /* prev == opp_turn */
	  coef += 15;
      }
    else if (next == turn)
      {
	if (prev == EMPTY)
	  ;
	else if (prev == turn)
	  coef -= 10;
	else /* prev == opp_turn */
	  coef += 10;
      }
    else /* next == opp_turn */
      {
	if (prev == EMPTY)
	  coef += 15;
	else if (prev == turn)
	  coef += 10;
	else /* prev == opp_turn */
	  coef += 20;
      }
  }

  if (move >= 2)
    {
      if (edge[move-2] == turn)
	coef--;
      else if (edge[move-2] == opp_turn)
	coef += 3;
    }
  if (move <= 5)
    {
      if (edge[move+2] == turn)
	coef--;
      else if (edge[move+2] == opp_turn)
	coef += 3;
    }

  if (coef < 0)
    coef = 0;
  else if (coef > 50)
    coef = 50;

  switch (move)
    {
    case 1:
      if (edge[8] == opp_turn)
	return coef * 0.002 + 0.89;
      else
	return coef * 0.009 + 0.03;

    case 6:
      if (edge[9] == opp_turn)
	return coef * 0.002 + 0.89;
      else
	return coef * 0.009 + 0.03;

    case 2:
      if (((edge[6] == opp_turn) && (edge[5] == EMPTY) &&
	   (edge[7] == EMPTY)) ||
	  ((edge[1] == opp_turn) && (edge[0] == EMPTY) &&
	   (edge[2] == EMPTY))  ||
	  ((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	   (edge[4] == EMPTY) && (edge[7] == EMPTY))  ||
	  ((edge[1] == opp_turn) && (edge[2] == opp_turn) &&
	   (edge[0] == EMPTY) && (edge[3] == EMPTY)))
	return coef * 0.015 + 0.2;
      else if (edge[8] == opp_turn) 
	return coef * 0.016 + 0.07;
      else if (edge[8] == turn)
	return (coef * 0.010 + 0.04);
      else
	return (coef * 0.015 + 0.05);

    case 5:
      if (((edge[6] == opp_turn) && (edge[5] == EMPTY) &&
	   (edge[7] == EMPTY)) ||
	  ((edge[1] == opp_turn) && (edge[0] == EMPTY) &&
	   (edge[2] == EMPTY))  ||
	  ((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	   (edge[4] == EMPTY) && (edge[7] == EMPTY))  ||
	  ((edge[1] == opp_turn) && (edge[2] == opp_turn) &&
	   (edge[0] == EMPTY) && (edge[3] == EMPTY)))
	return coef * 0.015 + 0.2;
    else if (edge[9] == opp_turn)
      return coef * 0.016 + 0.07;
    else if (edge[9] == turn)
      return coef * 0.010 + 0.04;
    else
      return coef * 0.015 + 0.05;

    case 3:
      if (((edge[6] == opp_turn) && (edge[5] == EMPTY) &&
	 (edge[7] == EMPTY)) ||
        ((edge[1] == opp_turn) && (edge[0] == EMPTY) &&
	 (edge[2] == EMPTY)) ||
        ((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	 (edge[4] == EMPTY) && (edge[7] == EMPTY))  ||
        ((edge[1] == opp_turn) && (edge[2] == opp_turn) &&
	 (edge[0] == EMPTY) && (edge[3] == EMPTY)))
	return coef * 0.015 + 0.2;
      else 
	return coef * 0.0165 + 0.08;

    case 4:
      if (((edge[6] == opp_turn) && (edge[5] == EMPTY) &&
	 (edge[7] == EMPTY)) ||
        ((edge[1] == opp_turn) && (edge[0] == EMPTY) &&
	 (edge[2] == EMPTY)) ||
        ((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	 (edge[4] == EMPTY) && (edge[7] == EMPTY))  ||
        ((edge[1] == opp_turn) && (edge[2] == opp_turn) &&
	 (edge[0] == EMPTY) && (edge[3] == EMPTY)))
	return coef * 0.015 + 0.2;
      else
	return coef * 0.0165 + 0.08;

    default:
      fputs ("compute_prob: bad move\n", stderr);
      return 0;
    }
}

quit(code, s)
     int code;
     char *s;
{
  fprintf (stderr, "%s", s);
  exit(code);
}

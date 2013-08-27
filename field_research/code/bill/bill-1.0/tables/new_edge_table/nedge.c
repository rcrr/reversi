/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* Edge table - incls both X-squares.  (kfl - 11/4/85)

   Rewrote so doesn't use NO_MOVE hack (ssm - 7/87)
 */

#include <stdio.h>
#include <math.h>

#define Opposite(x) (!(x))
#define MAX_PIECES 10
#define BLACK_INF 99999999		/* Best Black can do */
#define WHITE_INF -99999999		/* Best White can do */
#define INFINITY BLACK_INF
#define FALSE 0
#define TRUE 1
#define BLACK 0
#define WHITE 1
#define EMPTY 2
#define Empty(x) ((x)==2)
#define Abs(x) (((x) > 0) ? (x) : -(x))
#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) > (y)) ? (y) : (x))
#define TABLE_SIZE 59049


/* #define DEBUG */
#define NO_SURPRISE

#define is_better(a,b,color) (color==BLACK ? (a > b) : (a < b))
#define better(a,b,color) (color==BLACK ? MAX(a,b) : MIN(a,b))
#define SURPRISE(edge) (Surprise[(edge)[0]][(edge)[8]] + Surprise[(edge)[7]][(edge)[9]])

/* Probabilistic moves (moves to an edge that do not flip edge dics) */

typedef struct
{
  int 	          move;
  float          score;
  float          prob;
}               imove;

/* Table entry = value of edge position (may not be correct) and whether the
 * entry has converged (then value is correct) */

typedef struct
{
  float score[2]
#ifndef NO_SURPRISE
    , adjust
#endif
          ;
  int   converged;
} table;

table Edge_Table[3][3][3][3][3][3][3][3][3][3];
table Save_Table[3][3][3][3][3][3][3][3][3][3];

/* Surprise[Corner][X-Square] */
#ifndef NO_SURPRISE
float Surprise[3][3] = {{ 4, 4, 4}, /* black in corner */
			 {-4,-4,-4}, /* white in corner */
			 {-4, 4, 0}};/* no one in corner */
#endif

/* Value[] contains the value of each square for Black, in a terminal
 * position.  X-squares are high: Value[8,9]
 * Right now, X-squares are worthless */

#define CORNER 	 5.07		/* bonus for having corner.  Takes into account other edge */
#define REAL_CORNER 0.50

#define X_SQ  	-4.50		/* bonus for being in x-square when corner is empty */
#define REAL_X_SQ 0.00		/* x-squares are really worthless */

#define BOTH	 5.50		/* bonus for having x-square and corner */
#define REAL_BOTH 1.00		/* not counting other edge */

#define OPP    	 5.11		/* bonus for having corner, other guy having x-square */
#define REAL_OPP 0.00		/* what it should be */

float          Value[MAX_PIECES] = {5.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.5, 0.5, 5.0};
int Total_Iters,	/* total number of iterations through the loop in compute_edge */
    Converged,		/* how many positions do we have values for */
    Max_Num_Times;	/* maximum number of trips through loop */
#ifdef DEBUG
FILE *Debug_Fp;
#endif

/* double  Prob[19683][8]; */

main ()
{
  int        edge[MAX_PIECES],
             i,
  	     e = EMPTY;

#ifdef DEBUG
  if (!(Debug_Fp = fopen ("debug.out", "w")))
    quit(-1,"Couldn't open debug.out for writing\n");
#endif
  for (i = 0; i < 10; i++)
    edge[i] = EMPTY;

  compute_edge (edge, &Edge_Table[e][e][e][e][e][e][e][e][e][e]);
  process_all ();
  fprintf (stderr, "Average iterations = %f,  Maximum = %d\n",
	   (double) Total_Iters/Converged, Max_Num_Times);
  print_result (Edge_Table);
  return 0;
}

process_all ()
{
  int    score,
         i,
  	 e = EMPTY;

  /* first subtract value of empty position.  Should be zero, though */
  score = Edge_Table[e][e][e][e][e][e][e][e][e][e].score[BLACK];
  fprintf (stderr, "Subtracting %d for all guys\n", score);
  for (i = 0; i < TABLE_SIZE; i++)
    ((table *) Edge_Table)[i].score[BLACK] -= score;

  /* now process guy for printing.  That is, reduce the penalties for being
     in the x-square and losing the corner, because the adjacent edge evaluation
     will take care of it.
   */

  for (i=0;i<TABLE_SIZE;i++)
    {
      table *entry = &((table *)Edge_Table)[i];

      /* adjust each value */
#ifndef NO_SURPRISE
      entry->score[BLACK] -= entry->adjust;
      entry->score[WHITE] -= entry->adjust;
#endif
    }  
}

/*****************************************************************
                       IO Routines 
 *****************************************************************/

/* print out a .c source file for use by Bill. */

print_result (tab)
  table    tab[3][3][3][3][3][3][3][3][3][3];
{
  int    i = 0,
                  j8,
                  j9;
  int             j0,
                  j1,
                  j2,
                  j3,
                  j4,
                  j5,
                  j6,
                  j7;
  FILE  *fp;
  double          max_edge = 0.0,
                  factor;

  for (i = 0; i < TABLE_SIZE; i++)
    if (((table *) Edge_Table)[i].score[BLACK] > max_edge)
      max_edge = ((table *) Edge_Table)[i].score[BLACK];
  factor = 1000.0 / max_edge;
  fp = fopen ("new_edge_table/new_edge_table.c", "w");
  if (!fp)
    quit (-1, "Couldn't open et.c for output\n");
  fputs ("short New_Edge_Table[3][3][3][3][3][3][3][3][3][3] = {\n", fp);
  for (j0 = 0; j0 <= 2; j0++)
    for (j1 = 0; j1 <= 2; j1++)
      for (j2 = 0; j2 <= 2; j2++)
	for (j3 = 0; j3 <= 2; j3++)
	  for (j4 = 0; j4 <= 2; j4++)
	    for (j5 = 0; j5 <= 2; j5++)
	      for (j6 = 0; j6 <= 2; j6++)
		for (j7 = 0; j7 <= 2; j7++)
		  for (j8 = 0; j8 <= 2; j8++)
		    for (j9 = 0; j9 <= 2; j9++)
		    {
		      int edge[MAX_PIECES];

		      edge[0] = j0;
		      edge[1] = j1;
		      edge[2] = j2;
		      edge[3] = j3;
		      edge[4] = j4;
		      edge[5] = j5;
		      edge[6] = j6;
		      edge[7] = j7;
		      edge[8] = j8;
		      edge[9] = j9;
#ifndef DEBUG
		      fprintf (fp, "%6d", (int) (tab[j9][j8][j7][j6][j5][j4][j3][j2][j1][j0].score[BLACK] * factor));
		      putc (',', fp);
		      if (++i % 10 == 0)
			putc ('\n', fp);
#else
		      /* now print human usable form */
		      print_stuff(edge,
				  tab[j0][j1][j2][j3][j4][j5][j6][j7][j8][j9]);
#endif
		    }
#ifndef DEBUG
  fprintf (fp, "\n};\n");
#endif
  fclose (fp);
}

/* Two comparison routines used by qsort called in compute_edge */

incr_cmp (x, y)
imove *x,
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
imove *x,
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

typedef struct
{
  float           worth,
                  remain;
}               combine_t;

/* compute the static value of an filled edge.  The value is always for Black.
   It is computed by adding up the square values that Black has and subtracting
   what White has */

static_eval (edge, entry)
     int edge[MAX_PIECES];
     table *entry;
{
  float value = 0;
  int i;

  for (i=0;i<MAX_PIECES;i++)
    {
      if (edge[i] == BLACK)
	value += Value[i];
      else
	value -= Value[i];
    }
  /* now adjust for effect on the other edge. */
#ifndef NO_SURPRISE
  value += (entry->adjust = SURPRISE(edge));
#endif
  entry->score[WHITE] = entry->score[BLACK] = value;
  entry->converged = TRUE;
}         

/* Compute the edge value by finding all possible moves and picking the best.
 * It is recursivly applied until all values are converged.  Recursion actually
 * takes place in eval_move. */

compute_edge (edge, entry)
     int edge[MAX_PIECES];
     table   	     *entry;
{
  combine_t       combine (), ret[2];
  int   	  num_times = 0,
                  previous_first[2];
  int              num_illegals[2],
  		  i,
                  first_cutoff[2],
                  color;
  float          best_legal[2];
  imove illegals[2][20];

  for (i = 0; i < MAX_PIECES; i++)
    if (edge[i] == EMPTY)
      break;
  if (i == MAX_PIECES)			/* no empty discs */
  {
    static_eval (edge, entry);		/* count discs, mostly */
    entry->converged = TRUE;
    return;
  }
  /* get value of the illegals moves, with probabilities, and also the value
   * of the best legal move (not counting no-move). Only have to do this once */
  for (color = BLACK; color <= WHITE; color++)
    {
      double fabs ();

      compute_color_edge (edge, color, &best_legal[color], &num_illegals[color], illegals[color]);
      /* if there is a legal move, make it be the last illegal move, with prob 1.0 */
      if (fabs(best_legal[color]) < INFINITY-1)
	{
	  illegals[color][num_illegals[color]].score = best_legal[color];
	  illegals[color][num_illegals[color]++].prob = 1.0;
	}
    }
  previous_first[BLACK] = previous_first[WHITE] = -1000; /* junk value */

  /* now compute first_cutoff[].  This is the index into illegals of the
   * first move illegal move that isn't considered because NO_MOVE or some
   * other legal is better than it.  If some legal move is better than
   * NO_MOVE, then first_cutoff = num_legals+1 */
  while (TRUE)
  {
    if (num_times++ == 150)
      quit(-1,"Loop\n");
    for (color = BLACK; color <= WHITE; color++)
    {
#define THRESH 5e-06
	for (i = 0; i < num_illegals[color]; i++)
	  if (fabs(entry->score[Opposite(color)]-
		   illegals[color][i].score) > THRESH &&
	      is_better (entry->score[Opposite(color)],
			 illegals[color][i].score, color))
	    break;
	first_cutoff[color] = i;
    }
    /* check if one guy got all his moves cutoff but the other didn't.
       This is an impossible situation, caused by the random initial value
       So, just fix it by forcing each guy to make at least one move.  */
    if (num_times == 1 &&	/* only do this hack once */
	first_cutoff[BLACK] * first_cutoff[WHITE] == 0 &&
	first_cutoff[BLACK] != first_cutoff[WHITE])
      {
/*	quit (-1, "Doing hack\n"); */
	if (first_cutoff[BLACK] == 0)
	  first_cutoff[BLACK] = 1;
	else
	  first_cutoff[WHITE] = 1;
      }
    /* check if converged */
    else if (first_cutoff[BLACK] == previous_first[BLACK] &&
	     first_cutoff[WHITE] == previous_first[WHITE])
      break;
    /* compute stuff do fixed-point calculation */
    for (color = BLACK; color <= WHITE; color++)
      ret[color] = combine (illegals[color], first_cutoff[color]);
    /* see if both players would rather not move */
    if (ret[BLACK].remain == 1.0 && ret[WHITE].remain == 1.0)
      {
	for (color = BLACK;color<=WHITE;color++)
	  {
	    /* force the guy to choose a move: */
	    ret[color] = combine(illegals[color], num_illegals[color]);
	    /* compute the value if he did choose a move, by redistributing
	       the left-over probability.  */
	    entry->score[color] = ret[color].worth/(1-ret[color].remain);
	  }
	/* real value is the average of the two values */
	entry->score[BLACK] = entry->score[WHITE] = (entry->score[BLACK]+entry->score[WHITE])/2.0;
	break;			/* we are done.  Value is converged by definition */
      }
    /* compute new entry->score's, using the new cutoff information */
    for (color = BLACK; color <= WHITE; color++)
    {
      int             opp = Opposite (color);

      /* first save where we cutoff, so we can see if we are done next time */
      previous_first[color] = first_cutoff[color];
      entry->score[color] = (ret[opp].worth * ret[color].remain + ret[color].worth) /
			    (1.0 - ret[opp].remain * ret[color].remain);
    }
  }
  entry->converged = TRUE;	/* mark it for later reference */
#ifndef NO_SURPRISE
  entry->adjust = SURPRISE(edge);
#endif
  Total_Iters += num_times;
  Converged++;
  if (num_times > Max_Num_Times)
    Max_Num_Times = num_times;
}

#ifdef DEBUG
print_stuff (edge, entry)
     int edge[MAX_PIECES];
     table *entry;
{
  int i;
  static char name[] = "BW.";
  
  for (i=0;i<8;i++)
    putc (name[edge[i]], Debug_Fp);
  putc ('/', Debug_Fp);
  putc (name[edge[8]], Debug_Fp);
  putc (name[edge[9]], Debug_Fp);
#ifdef NO_SURPRISE  
  fprintf (Debug_Fp, "\t%6.2f\t%6.2f\n",
	   entry->score[BLACK], entry->score[WHITE]);
#else
  fprintf (Debug_Fp, "\t%6.2f\t%6.2f\t%6.2f\n",
	   entry->score[BLACK], entry->score[WHITE], entry->adjust);
#endif  
}
#endif

/* returns a structure containing the combined worth of the illegal moves,
 * and the remaining probability */

combine_t       combine (illegals, num_illegals)
     imove  illegals[20];
     int        num_illegals;
{
  combine_t       ret;
  int       i;

  ret.worth = 0.0;
  ret.remain = 1.0;
  for (i = 0; i < num_illegals; i++)
  {
    ret.worth += illegals[i].score * illegals[i].prob * ret.remain;
    ret.remain *= (1.0 - illegals[i].prob);
  }
  return ret;
}

compute_color_edge (edge, turn, best_legal, num_illegals, illegals)
  int edge[MAX_PIECES];
  int             turn,
                 *num_illegals;
  float         *best_legal;
  imove  illegals[20];
{
  int        i;
  float          worth,
                  best,
                  temp_prob,
  		  eval_move ();
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
    } else
    {
      illegals[*num_illegals].move = i;
      illegals[*num_illegals].score = worth;
      illegals[(*num_illegals)++].prob = temp_prob;
    }
  }

  *best_legal = best;
  if (turn == BLACK)
    qsort ((char *) illegals, *num_illegals, sizeof (imove), decr_cmp);
  else
    qsort ((char *) illegals, *num_illegals, sizeof (imove), incr_cmp);
  /* cutoff useless illegal moves */
  if (best != BLACK_INF && best != WHITE_INF)
    {
      if (turn == BLACK)
	{
	  for (i=0;i<*num_illegals;i++)
	    if (best > illegals[i].score-THRESH)
	      break;
	  *num_illegals = i;
	}
      else
	{
	  for (i=0;i<*num_illegals;i++)
	    if (best < illegals[i].score+THRESH)
	      break;
	  *num_illegals = i;
	}
    }	  
}

/* Eval move tries out an empty square for a move for turn, it returns a
 * value for the move if the value has converged. Otherwise, it calls
 * compute_move recursively to get the value */

float          eval_move (edge, move, turn, prob)
  int     edge[MAX_PIECES];
  int             move,
		     turn;
     float         *prob;
{
  int    i,
                  j;
  int        new[MAX_PIECES];
  table *entry;
  int             move_ok;
  float          p1,
                  p2,
  		  find_prob ();

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
    p1 = entry->score[WHITE];
  else
  if (turn == WHITE && entry->converged)
    p1 = entry->score[BLACK];
  else
  {
    compute_edge (new, entry);
    p1 = turn == BLACK ? entry->score[WHITE] : entry->score[BLACK];
  }

  /* x-squares got flipped ... */
  if ((move <= 2 && move >= 0 && new[8] == Opposite (turn)) ||
      (move >= 5 && move <= 7 && new[9] == Opposite (turn)))
  {
    if (move <= 2)
      new[8] = turn;
    else
    if (move >= 5)
      new[9] = turn;

    entry = &Edge_Table[new[0]][new[1]][new[2]][new[3]][new[4]][new[5]][new[6]][new[7]][new[8]][new[9]];
    if (turn == BLACK && entry->converged)
      p2 = entry->score[WHITE];
    else
    if (turn == WHITE && entry->converged)
      p2 = entry->score[BLACK];
    else
    {
      compute_edge (new, entry);
      if (turn == WHITE)
	p2 = entry->score[BLACK];
      else
	p2 = entry->score[WHITE];
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

float          find_prob (edge, turn, move)
     int  edge[MAX_PIECES];
     int  turn,
             move;
{
  int    opp_turn = Opposite (turn);
  int             i;
  float          coef,
                  opp;

  for (opp = 0, coef = 0, i = 0; i < 8; i++)
  {
    if (edge[i] == opp_turn)
      opp++;
    if (edge[i] != EMPTY)
      coef++;
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
  else
  if (move == 0 && edge[1] == turn)
    coef--;
  else
    if (move == 7 && edge[6] == opp_turn &&
	!(edge[1] == opp_turn && edge[2] == opp_turn &&
	  edge[3] == opp_turn && edge[4] == opp_turn &&
	  edge[5] == opp_turn))
    coef += 2;
  else
  if (move == 7 && edge[6] == turn)
    coef--;
  if (coef < 0)
    coef = 0;
  else
  if (coef > 10)
    coef = 10;

  if (move == 0)
  {
    if (edge[8] == EMPTY)
      return coef * coef * 0.0011;
    else
    if (edge[8] == turn)
      return coef * coef * 0.0003;
    else
      return coef * 0.01 + 0.89;
  } else
  if (move == 7)
  {
    if (edge[9] == EMPTY)
      return coef * coef * 0.0011;
    else
    if (edge[9] == turn)
      return coef * coef * 0.0003;
    else
      return coef * 0.01 + 0.89;
  }
  /* Non-corner && non-x-square moves */
  coef = coef + coef + opp;


  {
    int    next = edge[move + 1],
                    prev = edge[move - 1];

    if (next == EMPTY)
    {
      if (prev == EMPTY)
	coef += 10;
      else
      if (prev == turn)
	;
      else				/* prev == opp_turn */
	coef += 15;
    } else
    if (next == turn)
    {
      if (prev == EMPTY)
	;
      else
      if (prev == turn)
	coef -= 10;
      else				/* prev == opp_turn */
	coef += 10;
    } else				/* next == opp_turn */
    {
      if (prev == EMPTY)
	coef += 15;
      else
      if (prev == turn)
	coef += 10;
      else				/* prev == opp_turn */
	coef += 20;
    }
  }

  if (move >= 2)
  {
    if (edge[move - 2] == turn)
      coef--;
    else
    if (edge[move - 2] == opp_turn)
      coef += 3;
  }
  if (move <= 5)
  {
    if (edge[move + 2] == turn)
      coef--;
    else
    if (edge[move + 2] == opp_turn)
      coef += 3;
  }
  if (coef < 0)
    coef = 0;
  else
  if (coef > 50)
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
	 (edge[2] == EMPTY)) ||
	((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	 (edge[4] == EMPTY) && (edge[7] == EMPTY)) ||
	((edge[1] == opp_turn) && (edge[2] == opp_turn) &&
	 (edge[0] == EMPTY) && (edge[3] == EMPTY)))
      return coef * 0.015 + 0.2;
    else
    if (edge[8] == opp_turn)
      return coef * 0.016 + 0.07;
    else
    if (edge[8] == turn)
      return (coef * 0.010 + 0.04);
    else
      return (coef * 0.015 + 0.05);

  case 5:
    if (((edge[6] == opp_turn) && (edge[5] == EMPTY) &&
	 (edge[7] == EMPTY)) ||
	((edge[1] == opp_turn) && (edge[0] == EMPTY) &&
	 (edge[2] == EMPTY)) ||
	((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	 (edge[4] == EMPTY) && (edge[7] == EMPTY)) ||
	((edge[1] == opp_turn) && (edge[2] == opp_turn) &&
	 (edge[0] == EMPTY) && (edge[3] == EMPTY)))
      return coef * 0.015 + 0.2;
    else
    if (edge[9] == opp_turn)
      return coef * 0.016 + 0.07;
    else
    if (edge[9] == turn)
      return coef * 0.010 + 0.04;
    else
      return coef * 0.015 + 0.05;

  case 3:
    if (((edge[6] == opp_turn) && (edge[5] == EMPTY) &&
	 (edge[7] == EMPTY)) ||
	((edge[1] == opp_turn) && (edge[0] == EMPTY) &&
	 (edge[2] == EMPTY)) ||
	((edge[6] == opp_turn) && (edge[5] == opp_turn) &&
	 (edge[4] == EMPTY) && (edge[7] == EMPTY)) ||
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
	 (edge[4] == EMPTY) && (edge[7] == EMPTY)) ||
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

quit (int status, char *msg)
{
  fprintf (stderr, "%s", msg);
  exit (status);
}

/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#include <ctype.h>

#include <node.h>
#include <search.h>
#include <stat.h>
#include <ab_info.h>
#include <math.h>

void fix_killer ();
extern void backtrack_in_move_file (int num_moves);

extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern ab_info_t AB_Info[2];
extern general_info_t General_Info[2];
extern stat_info_t Stat_Info[2][MAX_MOVES];
extern Node    Game_Record[MAX_MOVES];
extern int      Turn_Num,
                Discs,
                Type1_Time[MAX_PIECES],
                Type2_Time[MAX_PIECES];


#define DEBUG

/* copy_rec copies an entire game record. */

void copy_rec (from, to)
    Node *from, *to;
{
  bcopy ((char *)from, (char *)to, sizeof (Node));
}


/* cvt converts moves from numeric (0-63) to alphabet-digit (A1-H8). */

move_t cvt (str)
    char           *str;
{
  return str[0] < 'A' ? NO_MOVE : (str[0] | 0x20) - 'a' + (str[1] - '1') * 8;
}

/* uncvt converts moves from alphabet-digit (A1-H8) to numeric (0-63). */

char *uncvt (move)
     move_t move;
{
  static char uncvt_array[MAX_PIECES + 2][3] =
    {
      "--", "--",
      "A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1",
      "A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2",
      "A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3",
      "A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4",
      "A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5",
      "A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6",
      "A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7",
      "A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8",
    };

  return &*uncvt_array[move + 2];
}


/* take_back takes back num_moves previous half-moves.  Usually it is 2.
   Known bug: if game was read in via read_pos (-p), takeback can cause trouble because some moves are unknown. */

void take_back (color, num_moves)
     color_t color;
     int num_moves;
{
  int cnt;
  char buff[1000];

  sprintf (buff, "Taking back last %d moves.", num_moves);
  curse (buff);
  backtrack_in_move_file (num_moves);
#if 0
  opening_takeback (Opposite (color));	/* we should unhardwire the constant 2 in this procedure */
#endif
  cnt = num_moves;
  while (--cnt >= 0)
    {
      Turn_Num--;
      fix_killer (Game_Record[Turn_Num].move, BLACK);
      fix_killer (Game_Record[Turn_Num].move, WHITE);
    }
  Discs = num_discs (&Game_Record[Turn_Num]);
  SS_clear_cache ();			/* blow away SS's cached trees */
}


/* fix_killer is used only after take_back, because we must put back the now available moves. */

void fix_killer (new_move, color)
     move_t	     new_move;
     color_t	     color;
{
  register int i;

  if (!LEGAL(new_move))
    return;
  for (i = 0; i < MAX_PIECES; i++)
    {
      register struct double_link *node, *current_head;

      if (i == new_move)
	continue;

      switch (i)
	{
	case 033:
	case 034:
	case 043:
	case 044:
	  continue;
	}

      node = (struct double_link *) malloc (sizeof (struct double_link));
      if (node == NULL)
	  abort ();
      node->next = current_head = Killer_Table[color][i].order;
      node->prev = NULL;
      node->response = new_move;
      Killer_Table[color][i].resp_addr[new_move] = Killer_Table[color][i].order = current_head->prev = node;
    }
}


/* compute_nps computes approximate nodes searched per second,
   and normalizes Type1_Time and Type2_Time accordingly. */

void compute_nps (color)
     color_t color;
{
  int             i, total_time = 0, total_nodes = 0;
  double          ratio;
  int from, to;

  from = 1;				/* ??? */
  to = Turn_Num;			/* ??? */
  for (i = from; i < to; i += 2)
    {
      int nodes = Stat_Info[color][i].nodes_searched;

      if (nodes > 0)
	{
	  total_nodes += nodes;
	  total_time += Stat_Info[color][i].time_used;
	}
    }
  if (total_time <= 0)
    return;

  ratio = DEFAULT_NPS / ((double) (total_nodes * 1000) / total_time);
  General_Info[color].done_nps = TRUE;
  printf ("%d (nodes) / %d (time) = %f\n",
	  total_nodes, (int) (total_time/1000.0), ratio);

  for (i = 0; i < MAX_PIECES; i++)
    {
      if (Type1_Time[i] >= INFINITY)
	break;

      Type1_Time[i] *= ratio;
      Type2_Time[i] *= ratio;
    }
}


/* set_steal_depth estimates depth values used in steal
   based on how good the opponent is and how much time we have.
 */

void set_steal_depth (int rating, float time_left)
{
  register int rating_index = rating <= 1000 ? 0 : rating >= 1900 ? 9
    : rating / 100 - 10,
    time_index = time_left <= 0 ? 0 : time_left >= 5400000 ? 9 : time_left / 600000;

  static char small[10][10] =
    {
      { 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, },
      { 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, },
      { 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, },
      { 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, },
      { 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, },
      { 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, },
      { 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, },
      { 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, },
      { 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, },
      { 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, },
    },
  steal[10][10] =
    {
      { 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, },
      { 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, },
      { 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, },
      { 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, },
      { 7, 7, 7, 8, 8, 8, 9, 9, 9, 9, },
      { 7, 8, 8, 8, 9, 9, 9, 9, 10, 10, },
      { 8, 8, 9, 9, 9, 9, 10, 10, 10, 11, },
      { 9, 9, 9, 9, 10, 10, 10, 11, 11, 11, },
      { 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, },
      { 10, 10, 10, 11, 11, 11, 12, 12, 12, 13, },
    };

  AB_Info[BLACK].first_steal_depth = AB_Info[WHITE].first_steal_depth = steal[rating_index][time_index];
  AB_Info[BLACK].small_search_depth = AB_Info[WHITE].small_search_depth = small[rating_index][time_index];
}


void compute_discs (node)
     register Node *node;
{
  register int square;
  register int bl_pieces = 0, wh_pieces = 0;

  for (square = 0; square < MAX_PIECES; square++)
    switch (node->board[square])
      {
      case BLACK:
	bl_pieces++;
	break;

      case WHITE:
	wh_pieces++;
	break;
      }

  node->bl_pieces = bl_pieces;
  node->wh_pieces = wh_pieces;
}

/* gauss returns the probability of winning corresponding to an evaluation. */

double gauss (eval)
     eval_t eval;
{
  return eval <= -INFINITY ? 0.0 : eval >= INFINITY ? 1.0 : 1.0 / (exp (eval * (-1 / 200000.0)) + 1.0);
}

/* return the (adjusted) win probability for the player to move (the values
   in NODE have been filled in by deepen, or some search procedure).  If
   it's an endgame search result, return the disc count, or the estimated
   disc count.

   Adjusted means that we try to patch the odd-even level problem, by
   using the magic value of SS_Offset to adjust the evaluation result */

float node_value (Node *node)
{
  int depth;
  int eval = AB_Info[node->color].last_score;

  if (AB_Info[node->color].bottom_searched) /* endgame search result? */
    return eval;
  /* otherwise, have to adjust for odd-even problem
     add 0.97 to round depth up, since partial level probably means
     we have a value from the next ply, though it isn't reliable */

  /*  printf (" %f ", AB_Info[node->color].depth); */
  
  depth = (int) (AB_Info[node->color].depth+0.99);
  if (depth % 2 == 0)		/* even depth */
    eval += 64000;
  else
    eval -= 64000;
  return gauss(eval);
}

#if 0
/* ungauss returns the inverse sigmoid function, after some scaling. */

eval_t ungauss (eval)
     double eval;
{
  return eval <= 0.0 ? -INFINITY : eval >= 1.0 ? INFINITY : log (1.0 / eval - 1.0) * 200000.0;
}
#endif

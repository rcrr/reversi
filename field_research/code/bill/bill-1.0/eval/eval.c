/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Gaussian Classifier routines for NewBill
 *
 * Kai-Fu Lee 7/4/86 Created.
 *
 * Sanjoy Mahajan 8/10/86 rewritten for Bill 3.0
 *
 * Joe Keane 1/16/87 moved out compute ()
 * Joe Keane 4/11/87 modified some
 */

#include <stdio.h>
#include <node.h>

void compute4 (), combine4 ();

#define INF ((eval_t)(1 << 28))

#undef DEBUG_EVAL			/* debugging legal moves, etc. */

extern int      Leaf_Nodes,
		Sq_Map[MAX_PIECES];

#ifdef DEBUG_EVAL

/* data for real_gen.c */

#define NUM_TABLES 32

extern struct one
{
  char           *name;			/* the human+array name for this table */
  int             map[8],		/* the squares that go with in bits
					 * 0-7 */
  size;		/* total ternary digits */
}               data[NUM_TABLES];
#endif

eval_t win_prob4();

/*
 * The probabilistic non-linear evaluation function for Bill. Returns ln
 * (p_win / p_loss) * 200000.
 *
 * Needs to be modifed to use the data from the previous record (with other color
 * to move).  Also will put the stuff just evaluated into the game_rec, so it
 * can be used next time.
 */


/*
 * eval1 is now the same as evaluate (uses same gauss_data)
 */

eval_t eval1 (node, old_node)
     Node *node, *old_node;
{
  extern double	  Mean_Win4[MAX_PIECES + 1][4],
		  Mean_Loss4[MAX_PIECES + 1][4],
		  ICov_Win4[MAX_PIECES + 1][4][4],
		  ICov_Loss4[MAX_PIECES + 1][4][4],
		  LnDiff4[MAX_PIECES + 1];
 feature_t combined_features[4];
  int discs;

  Leaf_Nodes++;
  if (node->bl_pieces <= 0)
    return node->color == BLACK ? -INF : INF;
  if (node->wh_pieces <= 0)
    return node->color == BLACK ? INF : -INF;
  discs = num_discs(node);
  compute4 (node->board, node->color, node->features);
  combine4 (node->features, old_node->features, combined_features);
  return win_prob4 (combined_features, Mean_Win4[discs], Mean_Loss4[discs],
		    ICov_Win4[discs], ICov_Loss4[discs], LnDiff4[discs]);
}

static feature_t Save_Combined_Features[4];

/*
 * THE evaluation function (also the default)
 */

eval_t evaluate (node, old_node)
     Node *node, *old_node;
{
  extern double	  Mean_Win2[MAX_PIECES + 1][4],
		  Mean_Loss2[MAX_PIECES + 1][4],
		  ICov_Win2[MAX_PIECES + 1][4][4],
		  ICov_Loss2[MAX_PIECES + 1][4][4],
		  LnDiff2[MAX_PIECES + 1];
  feature_t combined_features[4];
  extern int Flag_Y;
  register eval_t eval;
  int discs;

  Leaf_Nodes++;
  if (node->bl_pieces <= 0)
    return node->color == BLACK ? -INF : INF;
  if (node->wh_pieces <= 0)
    return node->color == BLACK ? INF : -INF;
  compute4 (node->board, node->color, node->features);
  combine4 (node->features, old_node->features, combined_features);
  discs = num_discs(node);
  if (Flag_Y)
    bcopy ((char *) combined_features, (char *) Save_Combined_Features, sizeof combined_features);
  eval = win_prob4 (combined_features, Mean_Win2[discs], Mean_Loss2[discs],
		    ICov_Win2[discs], ICov_Loss2[discs], LnDiff2[discs]);
#define BIG	(1 << 24)
  if (eval < -BIG || eval > BIG)
    internal_error ("ridiculous evaluation");
  return eval;
}

/*
 * 4-feature linear hand-tuned evaluation
 */

eval_t eval4 (node, old_node)
     Node *node, *old_node;
{
  extern int      Mob_Coef[MAX_PIECES], Seq_Coef[MAX_PIECES];
  feature_t comb[4];
  int discs;

  Leaf_Nodes++;
  if (node->bl_pieces <= 0)
    return node->color == BLACK ? -INF : INF;
  if (node->wh_pieces <= 0)
    return node->color == BLACK ? INF : -INF;
  discs = num_discs(node);
  compute4 (node->board, node->color, node->features);
  combine4 (node->features, old_node->features, comb);
  return comb[0] * 150 + comb[1] * Mob_Coef[discs] + comb[2] * Seq_Coef[discs] / 5 + comb[3] * Mob_Coef[discs] / 3;
}

/* evaluation that uses the classifier-generated linearity, 4 features */

eval_t eval10 (node, old_node)
     Node *node, *old_node;
{
  feature_t combined_features[4];

  Leaf_Nodes++;
  if (node->bl_pieces <= 0)
    return node->color == BLACK ? -INF : INF;
  if (node->wh_pieces <= 0)
    return node->color == BLACK ? INF : -INF;
  compute4 (node->board, node->color, node->features);
  combine4 (node->features, old_node->features, combined_features);
  {
    extern int Coef4[65][5];
    register feature_t *comb = combined_features;
    register int *coef = &Coef4[num_discs(node)][0];

    return *comb++ * *coef++ + *comb++ * *coef++ + *comb++ * *coef++ + *comb++ * *coef++ + *coef++;
  }
}

eval_t eval_out (node, old_node)
     Node *node, *old_node;
{
  register int i;
  register eval_t eval;
  feature_t *comb = Save_Combined_Features;

  for (i = 0; i < MAX_PIECES; i++)
    {
      print_char (node->board[i]["*O.,"]);
      if (i % SIZE == SIZE - 1)
	print_newline ();
    }

  eval = evaluate (node, old_node);
  {
    feature_t *new = node->features, *old = old_node->features;

    printf ("%c[%4d%4d%4d%4d][%4d%4d%4d%4d]=[%4d%4d%4d%4d]=%8d\n",
	    "BW"[node->color], old[0], old[1], old[2], old[3],
	    new[0], new[1], new[2], new[3],
	    comb[0], comb[1], comb[2], comb[3], eval);
  }
  return eval;
}


#ifdef DEBUG_EVAL
print_bytes (bytes)
     register unsigned char *bytes;
{
  register int i, j;
  unsigned char   byte;

  for (i = 0; i < 32; i++)		/* print one */
    {
      byte = bytes[i];
      printf ("Doing table: %s\tbyte = %d\n", data[i].name, (int) byte);
      if (bytes <= 0)			/* no legal moves? */
	printf ("No legals here.\n");
      else
	{
	  char * uncvt();

	  for (j = 0; j < data[i].size; j++) /* check each square */
	    if ((byte >> j) & 1)	/* if a legal move */
	      printf ("%d = %s   ", j, uncvt (data[i].map[j]));
	  printf ("\n");
	}
    }					/* for () */
}

debug_stuff (board, color, moves)
     board_t         board;
     int             color;
     bit_string      moves;
{
  register        i;
  char           *uncvt ();
  extern char     Color_Name[3][10];

  board_out(board);
  printf ("Legals (%s):", Color_Name[color]);
  for (i = 0; i < 32; i++)		/* print low half of legals */
    if ((moves.int_way.long1 >> i) & 1)
      printf (" %s", uncvt (Sq_Map[i]));
  for (i = 0; i < 32; i++)		/* print high half of legals */
    if ((moves.int_way.long2 >> i) & 1)	/* if a legal move at
					 * bit pos i */
      printf (" %s", uncvt (Sq_Map[i]));
  printf ("\n\n");
}

pboard (board)
     new_board_t board;
{
  int i,j;
  static char print_name[] = "*0..";

  printf ("  a b c d e f g h\n");
  for (i = 0; i < SIZE; i++)
    {
      printf ("%c ", i+'1');
      for (j = 0; j < SIZE; j++)
	printf ("%c ",print_name[board[i][j]]);
      printf ("\n");
    }
}
#endif

/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <general.h>
#include <board.h>
#include <color.h>
#include <move.h>
#include <bill-features.h>
#include <eval.h>
#include <killer.h>

#define MAX_MOVES	80	/* maximum possible move number */
#define PARENT 1

/* a plain vanilla node */

typedef struct
{
  board_t	  board;
  color_t	  color;	/* whose move */
  char            bl_pieces,
  	          wh_pieces;	/* all node types must have above four fields */
  move_t	  move;		/* move made from here */
  feature_t	  features[NUM_FEATURES];
  legal_list      legals;	/* the legals */
} Node;

/* a selective-search node */

typedef struct SNode
{
  board_t	  board;
  color_t	  color;
  char		  bl_pieces,
		  wh_pieces;
  move_t	  to_here;
  feature_t	  features[NUM_FEATURES];
  eval_t	  upper, lower;
  int		  nodes;
#if PARENT
  struct SNode	  *parent;
#endif
  struct SNode    *child;
  struct SNode	  *next;
} SNode;

/* a singular-move-search node */

typedef struct
{
  board_t board;
  color_t color;
  char    bl_pieces, wh_pieces;
  move_t  to_here;
  feature_t features[NUM_FEATURES];
  int key;
} SmsNode;

/* an alpha-beta node */

typedef struct
{
  board_t	  board;
  color_t	  color;
  char		  bl_pieces, wh_pieces;
  move_t	  to_here;
  feature_t	  features[NUM_FEATURES];
  int		  key;		/* hash key */
  eval_t          alpha;	/* alpha value in search */
  int             exact;	/* whether the alpha value is exact */
  struct double_link *next_legal_try;
  move_t          best,		/* best move from here */
		  move,		/* move made from here */
                  next_move;	/* Next move to be considered */
  int             already_tried, /* Hash table that we tried already */
		  more,		/* how many more levels to go.  Used to selectively deepen the search. */
		  book_position; /* number of bytes from beginning of book */
} ANode, GNode;

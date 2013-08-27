/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#ifndef GENERAL_H

#include <stdio.h>
#include <move.h>
#include <eval.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#define SIZE	8
#define MAX_PIECES	(SIZE * SIZE)
#define MAX_BLANKS (MAX_PIECES - 4)

/* some standard macros */

#define Min(x,y) (((x) > (y)) ? (y) : (x))
#define Max(x,y) (((x) > (y)) ? (x) : (y))
#define Abs(x) (((x) >= 0) ? (x) : -(x))
#define num_discs(node) ((node)->bl_pieces+(node)->wh_pieces)

typedef enum { NONE, MOVES, BOARD } read_t; /* how reading a game in */
typedef enum { S_SELECT, S_ALPHA } search_t;
typedef move_t (*move_fn_t)();
typedef move_t (*search_fn_t ) ();

typedef struct
{
  move_fn_t	  play;	/* function to get a move from this color */
  search_t search;		/* what search type: AB, Select, Singular (n.y.i) */
  search_fn_t search_fn;		/* the function to call to do the search */
  PFI fix_steal_data;		/* for fixing steal data */
  eval_rec  eval_stuff;		/* data about the evaluation function to use
				   and related stuff */
  float    time_left;		/* time left in msec */
  int	   has_opening,
  	   done_nps,	/* have computed nodes/sec this color */
  	   nps,			/* nodes per second */
           done_time;		/* have computed time allocation for this color */
  signed char *book_ptr,	/* location in the book array */
              *book;		/* start of book, for freeing, starting over */
} general_info_t;

typedef struct { search_fn_t search; PFI fix_steal; } search_record;
/* some standard function definitions */

char *uncvt ();
extern char Color_Name[3][10], Print_Name[4];
extern FILE *fwantwrite (), *fwantread ();

#define GENERAL_H
#endif

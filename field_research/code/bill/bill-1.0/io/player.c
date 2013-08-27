/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

extern int Ios;

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>

#include <stat.h>
#include <general.h>
#include <search.h>
#include <node.h>
#include <tourn.h>
#include <ab_info.h>

void print ();

extern ab_info_t AB_Info[2];
extern stat_info_t Stat_Info[2][MAX_MOVES];
extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern int    Turn_Num;
extern general_info_t General_Info[2];
extern Node Game_Record[MAX_MOVES];

static ab_info_t AB_Info_Save;

/* player thinks on the opponent's time, if no_steal is FALSE,
   but enters a read loop once the user types something.
 */

RETSIGTYPE wake_up (), quit_bill();

move_t player (node)
     Node *node;
{
  int num_legals;
  move_t getcmd(), cgetcmd(), move;
  color_t color = node->color;
  PFI fn;
  extern int Steal;
  move_t guess;

  num_legals = node->legals.max;
  /* in case we guess wrong */
  AB_Info_Save = AB_Info[Opposite(Turn_Num&1)];
  if (Steal && (!Ios || num_legals >= 1))
    {
      write (0, "steal> ", 7);
      /* think-ahead */
      guess = (*General_Info[Opposite(color)].search_fn) (node, TRUE);
    }

  signal (SIGINT, quit_bill);
  move = getcmd (node);
  /* if need to fix up some data because of stealing */
  if (Steal && (!Ios || num_legals >= 1)
      && (fn = General_Info[color].fix_steal_data))
    (*fn) (move, guess);		/* fixes up if guessed wrong */
  return move;
}

void ab_fix_steal (actual, guess)
     move_t actual, guess;
{
  static stat_info_t zero;

  if (actual != guess)			/* guessed wrong */
  {
    Stat_Info[Opposite(Turn_Num&1)][Turn_Num] = zero; /* nothing done */
    AB_Info[Opposite(Turn_Num&1)] = AB_Info_Save;
  }
}

/* getcmd gets a move from the player.  It won't exit until told to quit Bill
   or given a legal move. */

move_t getcmd (node)
     Node *node;
{
  int num_legals;

  printf ("%s> ", Color_Name[node->color]);
  if (Ios)
    fflush (stdout);
  num_legals = node->legals.max;

  for (;;)
    {
      char cmd[80];
      move_t mov;

      fgets (cmd, 79, stdin);
      if (isupper(cmd[0]))
	cmd[0] = tolower (cmd[0]);
      switch (cmd[0])
	{
	case '\0':		/* empty line entered */
	  print ("Enter the move you want to make (-- for pass).\n");
	  break;
	case '?':
	  print_help ();
	  break;

	case 'q':
	  quit_bill ();
	  break;

	case 't':
	  take_back (node->color, 2);
	  node = &Game_Record[Turn_Num]; /* should be right */
	  break;

	case 'r':
	  response_out (node->color);
	  break;

	case 'm':
	  {
	    int time;

	    if (sscanf (cmd, "%*s %d\n", &time) < 0)
	      do
	        {
		  printf ("Set time (seconds) for %s: ",
			  Color_Name[Opposite (node->color)]);
		  fflush (stdout);
		}
	      while (scanf ("%d", &time) != 1);
	    General_Info[Opposite(node->color)].time_left = time * 1000;
	  }
	  break;

	case 'a': case 'b': case 'c': case 'd':
	case 'e': case 'f': case 'g': case 'h':
	  if (cmd[1] < '0' && cmd[1] > '8')
	      goto bad_command;

	  {
	    register move_t mov = cvt (cmd);
	    register int i;

	    for (i = 0; i < num_legals; i++)
	      if (node->legals.data[i] == mov)
	      {
		printf ("You move to %s.\n", uncvt (mov));
		return mov;
	      }
	  }
	  /* got an illegal move */
	  printf ("Illegal move `%s'.\n", cmd);
	  if (Ios) fflush (stdout);
	  break;
	case '-':		/* pass, a.k.a. NO_MOVE */
	  if (num_legals > 0) /* pass isn't legal */
	    printf ("You may not pass with legal moves available.\n");
	  else
	    return NO_MOVE;
	  break;
	case 'p':
	  board_out (node->board);
	  legal_out (&node->legals);
	  break;

	case 'v':
	  (*General_Info[node->color].eval_stuff.out) (node, node-1);
	  break;

	default:
	bad_command:
	  printf ("Bad command `%s'.\n", cmd);
	  if (Ios) fflush (stdout);
	  break;
	}
      printf ("%s> ", Color_Name[node->color]);
      if (Ios) fflush (stdout);
    }
}

void quit_bill ()
{
  fputs ("Really quit? ", stderr);
  if (getchar () == 'y')
    quit ();
  while (getchar () != '\n')
    ;
}

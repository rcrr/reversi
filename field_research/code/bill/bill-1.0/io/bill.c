/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*****************************************************************
 * Bill - A World-Championship Level Othello Program
 *
 * Kai-Fu Lee and Sanjoy Mahajan
 * 9/3/85 Created
 *
 *****************************************************************/

/*
 * New switches: -f do one move and quit (for Hewlett stuff) -v switches: -iI
 * printing of status files at end -nN	printing of leaf nodes at iteration
 */

/*
 * SS switches: -ABWN use Bob on which colors, -P set Parm parameter,
 * -I set info nodes (number of nodes between printing search info),
 * -D file to dump tree to, -S selectivity for dump, -C to do compaction,
 * -E endgame alpha-beta depth (good), -H non-endgame alpha-beta depth (bad).
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <signal.h>

#include <ctype.h>
#include <stdio.h>

#include <general.h>
#include <node.h>
#include <tourn.h>
#include <search.h>
#include <files.h>
#include <ab_info.h>
#include <ss_info.h>

#define MINUTE 60000

double atof(char *);
void user_error ();
void set_steal_depth (int, float);
void setup_opening ();

extern general_info_t General_Info[2];
extern int      Opening_Book_Size[3],
                Accept_Interrupt,	/* trap interrupt (C-c)? */
  		Steal;
extern FILE *Save_Fp;

extern struct eval_rec All_Evals[MAX_EVALS];

extern Node	Game_Record[MAX_MOVES];	/* contains what Bill saves, mostly for take back */

extern RETSIGTYPE wake_up (), interrupt (), signal_error ();
extern void quit ();
extern int      player (),
                computer ();
double          Depth_Searched[2][MAX_MOVES / 2]; /* how deep Bill searched */
int		Discs = 4,		/* number of discs on board now */
                Turn_Num = 0,		/* current turn number */
                Color_Turn_Num[2],	/* need Turn_Num for a color? */
                Steal_Time = 0,
  		Quit_Done = FALSE,
		Time_Coef[2] = { 0, 0 },
  		Flag_X = FALSE,
  		Flag_Y = FALSE,
  		Flag_Z = FALSE;
int Ios = FALSE;

static int	Single_Move;		/* do one move and quit? */


int main (argc, argv)
     int argc;
     char **argv;
{
  char		  play_color = 0;
  int		  use_book = FALSE;	/* use opening book? (default no) */
  int		  rating;		/* opponent's rating */
  int		human_black = FALSE, human_white = FALSE; /* whether humans are playing each color */
  color_t ab_color;
  char		 *save_file = "Bill.moves",
  		 *board_file = NULL,
		 *moves_file = NULL;
  static int eval_num[2] = {0, 0};
  move_t this_move;
  register Node *record;

  extern int Dump_Selectivity, Bell;
  extern ss_info_t SS_Info[2];
  extern ab_info_t AB_Info[2];
  extern int Print_Eval, Print_Stat, Print_Legals, Print_Nodes, Print_Steal, Print_Response, Print_Search, Print_Time, Print_Depth, Print_Best;

  SS_Info[BLACK].nps = SS_Info[WHITE].nps = 1000; /* a reasonable guess */
  SS_Info[BLACK].parm = SS_Info[WHITE].parm = 100000;
  SS_Info[BLACK].interval = SS_Info[WHITE].interval = 5000;
  SS_Info[BLACK].endgame_depth = SS_Info[WHITE].endgame_depth = 6;
  SS_Info[BLACK].offset = SS_Info[WHITE].offset = 128000;
  AB_Info[BLACK].max_levels = AB_Info[WHITE].max_levels = 32;
  General_Info[BLACK].time_left = General_Info[WHITE].time_left = 5 * MINUTE;

#ifdef TOURNAMENT
  /* set tournament defaults */
  General_Info[BLACK].search = General_Info[WHITE].search = S_ALPHA;
  use_book = TRUE;
  rating = 2000;		/* playing against good programs */
  Print_Steal = FALSE;
  Bell = TRUE;
#endif
  
  while (*++argv != NULL)
  {
    if (**argv == '-')
      (*argv)++;
    else
      user_error ("Must use dashes with switches (e.g. Bill -s -t 5)");

    switch (**argv)
    {
    case '-':		/* GNU-style long option, e.g. "--ios" */
      if (strcmp ((*argv)+1, "ios") == 0) /* --ios -> play on IOS */
      {
	Ios = TRUE;
      }
      else
      {
	char msg[1000];
	    
	sprintf (msg, "Unrecognized option: %s", *argv);
	user_error (msg);
      }
      break;
    case 'q':		/* quit after done with optimal search to end */
      {
	Quit_Done = TRUE;
	break;
      }
#ifdef TOURNAMENT
    case 'Q':		/* quiet: turn off bell */
      {
	Bell = FALSE;
	break;
      }
#endif

#ifndef TOURNAMENT
    case 'C':			/* how to compact dump file */
      Dump_Compaction = atoi ((*argv)++);
      break;

    case 'D':			/* dump tree? */
      Dump_File = *++argv;
      break;
#endif

    case 'E':
      SS_Info[BLACK].endgame_depth = SS_Info[WHITE].endgame_depth = atoi (*++argv);
      break;

    case 'H':
      SS_Info[BLACK].hybrid_depth = SS_Info[WHITE].hybrid_depth = atoi (*++argv);
      break;

    case 'S':
      Dump_Selectivity = atoi (*++argv);
      break;

    case 'X':
      Flag_X = TRUE;
      break;

    case 'Y':
      Flag_Y = TRUE;
      break;

    case 'Z':
      Flag_Z = TRUE;
      break;

    case 'd':			/* no stealing */
      Steal = FALSE;
      break;

    case 'f':			/* do one move and quit? */
      Single_Move = TRUE;
      break;

#if 0
    case 'h':			/* help */
      system ("cat /../sp/usr/kfl/bill/Bill.help");
      return 0;
#endif

    case 'i':			/* specify moves file */
      moves_file = *++argv;
      break;

    case 'm':			/* specify save file */
      save_file = *++argv;
      break;

    case 'p':			/* specify board file */
      board_file = *++argv;
      break;

    case 'r':			/* specify opponent's rating */
      rating = atoi (*++argv);
      printf ("Opponent's rating is %d.\n", rating);
      break;

    case 'u':
      use_book = TRUE;		/* use the opening book */
      break;

    case 'U':			/* don't use opening book */
      use_book = FALSE;
      break;

    case 'I':			/* set node intervals */
      SS_Info[BLACK].interval = atoi (*++argv);
      SS_Info[WHITE].interval = argv[1] != NULL && isdigit (*argv[1]) ? atoi (*++argv) : SS_Info[BLACK].interval;
      break;

    case 'O':			/* set selective search offsets */
      SS_Info[BLACK].offset = atoi (*++argv) * 1000;
      SS_Info[WHITE].offset = argv[1] != NULL && isdigit (*argv[1]) ? atoi (*++argv) * 1000 : SS_Info[BLACK].offset; /* parameter for each player here? */
      break;

    case 'P':			/* set selective search parameters */
      SS_Info[BLACK].parm = atoi (*++argv) * 1000;
      SS_Info[WHITE].parm = argv[1] != NULL && isdigit (*argv[1]) ? atoi (*++argv) * 1000 : SS_Info[BLACK].parm; /* parameter for each player here? */
      break;

    case 'T':			/* set selective time allocation coefficients */
      SS_Info[BLACK].time_coef = atoi (*++argv) * 1000;
      SS_Info[WHITE].time_coef = argv[1] != NULL && isdigit (*argv[1]) ? atoi (*++argv) * 1000 : SS_Info[BLACK].time_coef; /* coefficient for each player here? */
      break;

    case 'e':			/* list evaluation functions */
      {
	register int j;

	for (j = 0; j < MAX_EVALS && All_Evals[j].comment != NULL; j++)
	  printf ("%d: %s\n", j, All_Evals[j].comment);
      }
      return 0;

    case 'l':			/* assign levels for searching */
      AB_Info[BLACK].max_levels = atoi (*++argv) - 1;
      AB_Info[WHITE].max_levels = argv[1] != NULL && isdigit (*argv[1]) ? atoi (*++argv) - 1 : AB_Info[BLACK].max_levels;
      break;

    case 't':			/* specify time for game */
      {
	float t = atof (*++argv) * MINUTE;

	if (argv[1] == NULL || !isdigit (*argv[1]))
	  General_Info[BLACK].time_left =
	    General_Info[WHITE].time_left = t / 2;
	else
	{
	  General_Info[BLACK].time_left = t;
	  General_Info[WHITE].time_left = atof (*++argv) * MINUTE;
	}
      }
      break;

    case 'A':
      General_Info[BLACK].search = General_Info[WHITE].search = S_SELECT;
      break;
    case 'N':
      General_Info[ab_color = BLACK].search = General_Info[WHITE].search = S_ALPHA;
      break;
    case 'B':
      General_Info[BLACK].search = S_SELECT;
      General_Info[ab_color = WHITE].search = S_ALPHA;
      break;
    case 'W':
      General_Info[ab_color = BLACK].search = S_ALPHA;
      General_Info[WHITE].search = S_SELECT;
      break;

    case 'F':			/* search function pair: a = alpha-beta, s = select */
      {
	register char b, w;

	++argv;
	b = (*argv)[0];
	if ((w = (*argv)[1]) == '\0') /* no second guy */
	  w = b;

	switch (b | 0x20)
	{
	case 'a':
	  General_Info[ab_color = BLACK].search = S_ALPHA;
	  break;
	case 's':
	  General_Info[BLACK].search = S_SELECT;
	  break;
	}

	switch (w | 0x20)
	{
	case 'a':
	  General_Info[ab_color = WHITE].search = S_ALPHA;
	  break;
	case 's':
	  General_Info[WHITE].search = S_SELECT;
	  break;
	}
      }
      break;

    case 'a':
    case 'n':
    case 'b':			/* specifying who plays what */
    case 'w':
      play_color = argv[0][0];	/* now get chars specifying evaluation functions */
      if (isdigit (argv[0][1])) /* is an eval num? */
      {
	eval_num[BLACK] = argv[0][1] - '0';
	eval_num[WHITE] =
	  isdigit(argv[0][2]) ?
	  argv[0][2] - '0' : eval_num[BLACK];
      }
      break;

    case 'v':			/* verbosity information */
      {
	register char *ptr;

	for (ptr = *++argv; *ptr != '\0'; ptr++)
	{
	  int c = *ptr, upper = !(c & 0x20);

	  switch (c | 0x20)
	  {
	  case 'b':
	    Print_Best = upper;
	    break;
	  case 'd':
	    Print_Depth = upper;
	    break;
	  case 'e':
	    Print_Eval = upper;
	    break;
	  case 'i':
	    Print_Stat = upper;
	    break;
	  case 'l':
	    Print_Legals = upper;
	    break;
	  case 'n':		/* printing of # of leaf nodes */
	    Print_Nodes = upper;
	    break;
	  case 'o':
	    Print_Steal = upper;
	    break;
	  case 'r':
	    Print_Response = upper;
	    break;
	  case 's':
	    Print_Search = upper;
	    break;
	  case 't':
	    Print_Time = upper;
	    break;

	  default:
	    user_error ("bad verbosity switch on command line");
	  }
	}
      }
      break;

    default:
      user_error ("bad switch on command line");
    }
  }

  if ((Save_Fp = fopen (save_file, "w")) == NULL)
    user_error ("can't open save file");
  fprintf (stderr, "Saving moves in `%s'.\n", save_file);

  switch (play_color)
    {
    case 'n':
      break;
    case 'b':
      human_black = TRUE;
      break;
    case 'w':
      human_white = TRUE;
      break;
    case 'a':
      human_black = human_white = TRUE;
      break;
    default:			/* no color specified = -n */
      break;
    }

  {
    register int color;

    for (color = BLACK; color <= WHITE; color++)
      {
	extern search_record Search_Table[];

	/* assign evaluation functions */
	General_Info[color].eval_stuff = All_Evals[eval_num[color]];
	/* assign searching function */
	General_Info[color].search_fn =
	  Search_Table[(int) General_Info[color].search].search;
	General_Info[color].fix_steal_data =
	  Search_Table[(int) General_Info[color].search].fix_steal;
      }
  }

  if (General_Info[BLACK].time_left != General_Info[WHITE].time_left)
    {
      printf ("Starting time: Black %.0f, White %.0f\n",
	      General_Info[BLACK].time_left / MINUTE,
	      General_Info[WHITE].time_left / MINUTE);
    }
  else if (General_Info[BLACK].time_left <= 0)
    print ("No time for the game.\n");
  else
      printf ("Time for the game: %.0f minutes.\n",
	      General_Info[BLACK].time_left * 2 / MINUTE);

  printf ("BLACK: %s vs.\nWHITE: %s\n",
	  human_black ? "you" : General_Info[BLACK].eval_stuff.comment,
	  human_white ? "you" : General_Info[WHITE].eval_stuff.comment);

  /* signal stuff */
  signal (SIGTTOU, SIG_IGN);
  signal (SIGALRM, wake_up);		/* timing alarm */
  signal (SIGTERM, signal_error);	/* cleanup for terminate */

  /* make C-c quit Bill if playing itself, unless only doing one move */
  if (Single_Move)
  {
    if (signal (SIGINT, SIG_IGN) != SIG_IGN)
      signal (SIGINT, wake_up);
  }
  else if (!human_black && !human_white && signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, quit);	/* do graceful cleanup */

  /* setup book and steal stuff */
  if (human_black && human_white)
    Steal = FALSE;
  if (!human_black)
    {
      if (Steal)
	set_steal_depth (rating, General_Info[BLACK].time_left);
      if (use_book)
	setup_opening (BLACK);
    }
  if (!human_white)
    {
      if (Steal)
	set_steal_depth (rating, General_Info[WHITE].time_left);
      if (use_book)
	setup_opening (WHITE);
    }

  General_Info[BLACK].play = human_black ? player : computer;
  General_Info[WHITE].play = human_white ? player : computer;

  setup_killer ();		/* setup killer table.  need this before read_moves() */
  init_cov ();			/* for 4-component non-linear eval fn. */

  this_move = NO_MOVE;

  if (!board_file)
    /* initial position */
    {
      extern board_t init_board;

      record = Game_Record;
      copy_board (init_board, record->board);	/* initial board */
      record->color = BLACK;			/* Black moves first */
      record->bl_pieces = record->wh_pieces = 2;
      record->legals.max = 4;
      record->legals.data[0] = 023;
      record->legals.data[1] = 032;
      record->legals.data[2] = 045;
      record->legals.data[3] = 054;
    }
  else
    {
      FILE *board_fp = fopen (board_file, "r");

      if (!board_fp)
	user_error ("can't open board file");
      read_board (board_fp);
    }

  if (moves_file)
    {
      FILE *moves_fp = fopen (moves_file, "r");

      if (!moves_fp)
	user_error ("can't open moves file for input");
      read_moves (moves_fp);
      if (Turn_Num > 0)
	this_move = Game_Record[Turn_Num - 1].move;
      if (!LEGAL(this_move) && Turn_Num >= 2 &&
	  !LEGAL(Game_Record[Turn_Num - 2].move))
	user_error ("illegal move in move file");
    }

  elapsed_time (TRUE);		/* start timing */

  /* main loop */
  for (;;)
    {
      record = &Game_Record[Turn_Num];
      print_info (record);
      this_move = (*General_Info[record->color].play) (record);
      General_Info[record->color].time_left -= elapsed_time (TRUE);
      make_move (record, record + 1, this_move);
      record++;
      if (Quit_Done && Discs > 53) /* cheap hack */
	break;
      if (Single_Move || (!LEGAL(this_move) && record->legals.max == 0))
	break;
    }

  /* print summary lines */
  printf ("Time left: Black %7.2f, White %7.2f.\nFinal score: Black %d, White %d.\n",
	   General_Info[BLACK].time_left * 0.001, General_Info[WHITE].time_left * 0.001,
	   Game_Record[Turn_Num].bl_pieces, Game_Record[Turn_Num].wh_pieces);

  /* and statistics */
  if (Print_Stat)
    {
      if (!human_black)
	print_stat (BLACK);
      if (!human_white)
	print_stat (WHITE);
    }
  return 0;
}

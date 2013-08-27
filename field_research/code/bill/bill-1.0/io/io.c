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

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <sys/types.h>

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>		/* it's worth trying */
#endif

#if HAVE_STRING_H
#include <string.h>
#else
#if HAVE_STRINGS_H
#include <strings.h>
#endif
#endif

#include <debug.h>
#include <general.h>
#include <hash.h>
#include <files.h>
#include <node.h>
#include <search.h>
#include <tourn.h>
#include <exact.h>
#include <ab_info.h>

void print ();
void print_newline ();
void legal_out ();
void board_out ();
void user_error ();

FILE *Save_Fp;

extern struct hash_entry *Hash_Table[HASH_SIZE];
extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern int Print_Gauss, Print_Stat, Print_Nodes, Print_Search, Print_Response, Print_Time,
                Illegal_Hash,
                Hash_Count,
                Read_Game,
                Discs,
                Turn_Num;
extern Node	Game_Record[MAX_MOVES];
extern general_info_t General_Info[2];
double gauss ();


/* Top level print-info --> calls appropriate printing routines */

void print_info (node)
     Node *node;
{
  extern int Print_Board, Print_Legals;
  color_t color = node->color;

  if (Print_Board)
    board_out (node->board);
  if (Print_Legals)
    legal_out (&node->legals);
  if (Print_Time)
      printf ("Time left for %s: %7.2f seconds.\n", Color_Name[color], General_Info[color].time_left * 0.001);
  print_newline ();
}


/* print legal moves */

void legal_out (legals)
     legal_t legals;
{
  int i;
  char buff[1000];

  strcpy (buff, "Legal moves:");
  for (i = 0; i < legals->max; i++)
    {
      strcat (buff, " ");
      strcat (buff, uncvt (legals->data[i]));
    }
  strcat (buff, ".");
  puts (buff);
}

/* board_out prints out the board to the appropriate places. */

void board_out (board)
    register board_t board;
{
  register int row, col;
  register int bl_pieces = 0, wh_pieces = 0;

  print ("  a b c d e f g h\n");
  for (row = 0; row < SIZE; row++)
    {
      print_char (row + '1');
      for (col = 0; col < SIZE; col++)
	{
	  switch (board[row * SIZE + col])
	    {
	    case BLACK:
	      print (" #");
	      bl_pieces++;
	      break;

	    case WHITE:
	      print (" O");
	      wh_pieces++;
	      break;

	    default:
	      print (" .");
	      break;
	    }
	}
      print_newline ();
    }
  printf ("Discs: Black %d, White %d.\n", bl_pieces, wh_pieces);
}


/* print search information */

void print_all (type, depth, best, reply,
		evaluation, used, lower, upper, color)
     char *type, *depth, *evaluation;
     move_t best, reply;
     int used, lower, upper;
     color_t color;
{
  printf ("%s (%s) = %s %s %s, time %8.2f", 
	  type, depth, uncvt (best), uncvt (reply), 
	  evaluation, used * 0.001);
  if (lower >= 0)
  {
      printf (" (%8.2f", lower * 0.001);
      if (upper > 0)
	  printf ("/%8.2f", upper * 0.001);
      printf (")");
  }
  putchar ('\n');
  fflush (stdout);
}

char *print_type (type)
     who_t type;
{
  static char type_name[5][7] = { "Deepen", "Select", "Type 1", "Type 2", "Steal" };

  return &*type_name[(int)type];
}

char *print_depth (depth)
     int depth;
{
  static char buff[9];

  sprintf (buff, "%-5d", depth);
  return buff;
}

char *print_finished_depth (finished_depth)
     double finished_depth;
{
  static char buff[9];

  /* finished complete level at the end? */
  if (abs(((int)finished_depth) - finished_depth) < 0.001)
    sprintf (buff, "%-5d", (int)finished_depth);
  else
    sprintf (buff, "%-5.2f", finished_depth);
  return buff;
}

char *print_discs (discs)
     eval_t discs;
{
  static char buff[9];

  sprintf (buff, "%8d", discs);
  return buff;
}

char *print_eval (eval)
     eval_t eval;
{
  static char buff[9];

  sprintf (buff, "%8.6f", gauss (eval));
  return buff;
}


/* print_interval returns the printable representation of an interval. */

char *print_interval (upper, lower)
     register eval_t upper, lower;
{
  int upper_diff;
  int lower_diff;
  static char buff[20];

  upper_diff = UNSHIFT_DIFF(upper);
  lower_diff = UNSHIFT_DIFF(lower);
  if (INEXACT_UPPER(upper))
  {
    if (INEXACT_LOWER(lower))
      sprintf (buff, "%8.6f /%3d", gauss ((UNSHIFT_UPPER(upper) + UNSHIFT_LOWER(lower)) / 2), (int) rint ((UNSHIFT_LOWER (lower) - UNSHIFT_UPPER (upper)) * (100.0 / 400000.0)));
    else
      sprintf (buff, "  >= %+3d /%3d", lower_diff, (int) rint (log ((double) UNSHIFT_AUX(lower) / CONS_VALUE) * 100.0));
  }
  else if (INEXACT_LOWER(lower))
    sprintf (buff, "  <= %+3d /%3d", upper_diff, (int) rint (log ((double) -UNSHIFT_AUX(upper) / CONS_VALUE) * 100.0));
  else if (upper_diff == lower_diff)
    sprintf (buff, "     %+3d     ", upper_diff);
  else if (upper == MAX_SCORE && lower == MIN_SCORE)
    return "      --     ";
  else
    sprintf (buff, " %+3d,%+3d /%3d", lower_diff, upper_diff, (int) rint (log ((double) UNSHIFT_AUX(lower) * -UNSHIFT_AUX(upper) / (CONS_VALUE * CONS_VALUE)) * 50.0));
  return buff;
}


/* print_help gives the user a short help message. */

void print_help ()
{
  /* Yes, i know this looks gruesome. */
  printf ("\
Top-level commands are (case-insensitive):\n\
\n\
\ta-h 1-8\tMake move in square specified.\n\
\tq\tQuit the game.\n\
\t?\tType this message.\n\
\tt\tTake back last two moves.\n\
\tm\tModify Bill's time.\n\
\tv\tPrint evaluation of this board.\n\
\t%s.\n\
\t^C\tStop Bill's searching.\n\
\t\tAgain, quit the game.\n\
\n\
For more information, type Bill -h to shell.\n\n",
	  "p\tPrint board.\n\
\tr\tPrint expected responses");
}


/* read_moves reads in a partial or full game record (list of moves) from a file. */

void read_moves (read_fp)
     register FILE  *read_fp;
{
  register move_t move = NO_MOVE, last_move = NO_MOVE;
  char buff[1000];

  /* don't use book if doing read_game */
  General_Info[BLACK].has_opening = General_Info[WHITE].has_opening = FALSE;
  while (fscanf (read_fp, "%s", buff) != EOF)
    {
      switch (*buff)
	{
	case '!':			/* skip over comment in move file */
	case '#':
	case ';':
	  while (getc (read_fp) != '\n')
	    ;
	  continue;

	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
	case '-':
	  break;

	default:
	  user_error ("illegal character in move file");
	}

      if ((move = cvt (buff)) < 0 && last_move < 0)
	user_error ("two passes in move file");
      make_move (Game_Record + Turn_Num, Game_Record + Turn_Num + 1, last_move = move);
    }

  fclose (read_fp);
}


/* read_board reads in a board and color to move from a file. */

color_t read_board (read_fp)
     register FILE  *read_fp;
{
  color_t color;
  register int square;
  register int discs = 0;
  board_t board;
  char buff[1000];

  General_Info[BLACK].has_opening = General_Info[WHITE].has_opening = FALSE;
  fputs ("?? ", Save_Fp);		/* previous moves are unknown */
  fscanf (read_fp, "%s", buff);
  switch (*buff)
    {
    case 'B':
    case 'b':
      color = BLACK;
      break;

    case 'W':
    case 'w':
      color = WHITE;
      break;

    default:
      user_error ("can't find color to move in board file");
    }

  for (square = 0; fscanf (read_fp, "%s", buff) != EOF; square++)
    switch (*buff)
      {
      case '#':
      case '*':
      case '0':
      case 'B':
      case 'b':
      case 'X':
      case 'x':
	discs++;
	board[square] = BLACK;
	break;

      case '1':
      case 'O':
      case 'W':
      case 'o':
      case 'w':
	discs++;
	board[square] = WHITE;
	break;

      case ' ':
      case '-':
      case '.':
      case '2':
      case '3':
	board[square] = EMPTY;
	break;

      default:
	user_error ("illegal piece in board file");
      }

  if (square != MAX_PIECES)
    user_error ("board file doesn't contain exactly 64 squares");
  fclose (read_fp);

  for (square = 0; square < MAX_PIECES; square++)
    if (Empty(board[square]))
      {
	extern int  Next_Arr[NEXT_MAX],
	            Next_St[MAX_PIECES + 1];
	register int *ptr, *stop;

	stop = &Next_Arr[Next_St[square + 1]]; /* where to stop */
	for (ptr = &Next_Arr[Next_St[square]]; ptr < stop; ptr++)
	  if (Occupied(board[*ptr]))
	    {
	      board[square] = ESSQ;
	      break;
	    }
      }

  /* Assume nobody had a NO_MOVE up to this point */
  if ((Turn_Num = (Discs = discs) - 4) > 0)
    Game_Record[Turn_Num - 1].move = NO_MOVE;
  copy_rec (Game_Record, Game_Record + Turn_Num);
  copy_board (board, Game_Record[Turn_Num].board);
  Game_Record[Turn_Num].color = color;
  find_legals (Game_Record + Turn_Num, NO_MOVE, &Game_Record[Turn_Num].legals);
  compute_discs (Game_Record + Turn_Num);
  return color;
}

/* is input waiting on fd 0? */

int input_ready ()
{
  fd_set rfds;
  struct timeval tv;
  int retval;

  /* Watch stdin (fd 0) to see when it has input. */
  FD_ZERO(&rfds);
  FD_SET(0, &rfds);
  /* set timeout to 0, so the check won't block */
  tv.tv_sec = 0;
  tv.tv_usec = 0;

  retval = select(1, &rfds, NULL, NULL, &tv);
  if (retval == -1)
    perror ("Bill: select");
  return (retval == 1);
}

void backtrack_in_move_file (int num_moves)
{
  fseek (Save_Fp, num_moves * -3L, 1);	/* backup over the previous moves */
  fflush (Save_Fp);
}

void save_move (move_t move)
{
  fprintf (Save_Fp, "%s ", uncvt (move));
  fflush (Save_Fp);
}

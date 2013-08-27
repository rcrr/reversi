/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* print_stat is called at the end of the game to print some statistical information. */

#include <stdio.h>
#include <math.h>

#include <node.h>
#include <general.h>
#include <stat.h>
#include <hash.h>
#include <files.h>

extern Node Game_Record[MAX_MOVES]; 
stat_info_t Stat_Info[2][MAX_MOVES];	/* info for each color and turn */
struct expansion_stats Expansion_Stats[2][MAX_PIECES][2];
extern general_info_t General_Info[2];

void print_stat (color)
     color_t             color;
{
  FILE *fp;
  char buff[1000];
  int turn;

  fp = fopen (color == BLACK ? BLACK_STAT_FILE : WHITE_STAT_FILE, "w");
  if (fp == NULL)
    {
      sprintf (buff, "Can't open %s stat file", Color_Name[color]);
      user_error (buff);
    }

  fputs (" Turn   Depth   Nodes    Leaf    Time   Value\n\
---------------------------------------------\n", fp);
  for (turn=0;turn<MAX_MOVES;turn++)
    {
      stat_info_t *guy = &Stat_Info[color][turn];
      double gauss ();

      /* check if nobody put anything here */
      if (guy->depth_searched == 0 &&
	  guy->nodes_searched == 0 &&
	  guy->leaf_searched == 0 &&
	  guy->time_used == 0)
	continue;

      fprintf (fp, " %4d %7.2f %7d %7d %7.1f ",
	       turn,
	       guy->depth_searched,
	       guy->nodes_searched,
	       guy->leaf_searched,
	       guy->time_used/1000.0);

      switch (guy->who)
	{
	  char *print_interval ();

	case SELECT:
	  fputs (print_interval(-guy->value, -guy->other_value), fp);
	  break;

	case AB:
	  fprintf (fp, "%7.4f", gauss (guy->value));
	  break;

	case AB2_1:
	  fputs (guy->value < 0 ? "LOSS" : guy->value > 0 ? "WIN" : "TIE", fp);
	  break;

	case AB2_2:
	  fprintf (fp, "%3d", guy->value);
	  break;
	}

      fputs (guy->exact == INEXACT ? " ?\n" : guy->exact == INCOMPLETE ? " ??\n" : "\n", fp); /* need some question marks */
    }

  if (General_Info[color].search == S_SELECT)
  {
    fputs("expansion stats:\n", fp);
    fprintf(fp, "PC M  nodes  A  mean/ s.dev.  m.abs.  U  mean/ s.dev.  L  mean/ s.dev.\n");
    fprintf(fp, "-- - ------  -------/------- -------  -------/-------  -------/-------\n");
    
    {
      int pieces;
      color_t move_color;
      int master_count;
      double master_upper_sum;
      double master_lower_sum;
      double master_total_sum_square;
      double master_total_sum_abs;
      double master_upper_sum_square;
      double master_lower_sum_square;
      double master_total_sum;
      
      master_count = 0;
      master_upper_sum = 0;
      master_lower_sum = 0;
      master_total_sum_square = 0;
      master_total_sum_abs = 0;
      master_upper_sum_square = 0;
      master_lower_sum_square = 0;
      
      for (pieces = 4; pieces < MAX_PIECES; pieces++)
	for (move_color = BLACK; move_color <= WHITE; move_color++)
	{
	  struct expansion_stats *stats;
	  int count;
	  double upper_sum;
	  double lower_sum;
	  double total_sum;
	  
	  stats = &Expansion_Stats[color][pieces][move_color];
	  count = stats->count;
	  upper_sum = stats->upper_sum;
	  lower_sum = stats->lower_sum;
	  master_count += count;
	  master_upper_sum += upper_sum;
	  master_lower_sum += lower_sum;
	  master_total_sum_square += stats->total_sum_square;
	  master_total_sum_abs += stats->total_sum_abs;
	  master_upper_sum_square += stats->upper_sum_square;
	  master_lower_sum_square += stats->lower_sum_square;
	  if (count < 16)
	    continue;
	  total_sum = upper_sum + lower_sum;
	  fprintf(fp, "%2d %c %6d  %7d/%7d %7d  %7d/%7d  %7d/%7d\n",
		  pieces, move_color == BLACK ? 'B' : 'W', count,
		  (int)rint(total_sum / (2 * count)),
		  (int)rint(sqrt((stats->total_sum_square - total_sum *
				  total_sum / count) / (4 * count))),
		  (int)rint(stats->total_sum_abs / (2 * count)),
		  (int)rint(upper_sum / count),
		  (int)rint(sqrt((stats->upper_sum_square - upper_sum *
				  upper_sum / count) / count)),
		  (int)rint(lower_sum / count),	
		  (int)rint(sqrt((stats->lower_sum_square - lower_sum *
				  lower_sum / count) / count)));
	}
      
      master_total_sum = master_upper_sum + master_lower_sum;
      fprintf(fp, "tot. %6d  %7d/%7d %7d  %7d/%7d  %7d/%7d\n",
	      master_count,
	      (int)rint(master_total_sum / (2 * master_count)),
	      (int)rint(sqrt((master_total_sum_square - master_total_sum *
			      master_total_sum / master_count) /
			     (4 * master_count))),
	      (int)rint(master_total_sum_abs / (2 * master_count)),
	      (int)rint(master_upper_sum / master_count),
	      (int)rint(sqrt((master_upper_sum_square - master_upper_sum *
			      master_upper_sum / master_count) / master_count)),
	      (int)rint(master_lower_sum / master_count),	
	      (int)rint(sqrt((master_lower_sum_square - master_lower_sum *
			      master_lower_sum / master_count) / master_count)));
    }
  }

  fclose (fp);
}


int count_hash (entry)
     struct hash_entry *entry;
{
  register struct hash_entry *ptr;
  register int i;

  for (ptr = entry, i = 0; ptr != NULL; ptr = ptr->next, i++)
    ;
  return i;
}

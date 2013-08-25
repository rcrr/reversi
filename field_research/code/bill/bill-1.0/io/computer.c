/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#endif

#if HAVE_GETRUSAGE
float ru_to_time (struct timeval before, struct timeval after)
{
  return after.tv_sec - before.tv_sec +
    (after.tv_usec - before.tv_usec)/1000000.0;
}
#endif

#include <stdio.h>

#include <node.h>
#include <search.h>
#include <check.h>
#include <stat.h>

extern general_info_t General_Info[2];
extern stat_info_t Stat_Info[2][MAX_MOVES];
extern total_leaf_nodes;

float node_value (Node *node);

void response_out ();

move_t computer (node)
     Node *node;
{
  extern int Turn_Num, Print_Response;
  register color_t color;
  register move_t mov;

  color = node->color;

  if (General_Info[color].has_opening)	/* book move? */
    {
      mov = opening_search (color);
      if (LEGAL(mov))
	{
	  Stat_Info[color][Turn_Num].exact = EXACT;
	  goto check_legality;
	}
    }

  /* run search */
    {
      extern int Discs;

      if (!General_Info[color].done_time)
	allocate_time (Turn_Num / 2, color);
      if (Discs > 40 && !General_Info[color].done_nps)
	compute_nps (color);		/* reestimate Type1,Type2 times */
      if (node->legals.max > 1)		/* more than one choice? */
      {
#if HAVE_GETRUSAGE
	int before_nodes;
	int err;
	float nodes;
	struct rusage before, after;
	float user_time;
	float system_time;

	before_nodes = total_leaf_nodes;
	err = getrusage (RUSAGE_SELF, &before);
	if (err == -1)
	  perror ("Bill: getrusage");
#endif
	mov = (*General_Info[color].search_fn) (node, FALSE);
#if HAVE_GETRUSAGE
	err = getrusage (RUSAGE_SELF, &after);
	if (err == -1)
	  perror ("Bill: getrusage");

	nodes = total_leaf_nodes - before_nodes;
	user_time = ru_to_time (before.ru_utime, after.ru_utime);
	system_time = ru_to_time (before.ru_stime, after.ru_stime);

	if (user_time > 0)
	  printf ("%7.1f %7.1f nodes/user, user+sys sec;  %d page faults\n",
		  nodes / user_time, nodes / (user_time+system_time),
		  after.ru_majflt - before.ru_majflt);
#endif
      }
      else if (node->legals.max >= 1)
	mov = node->legals.data[0];
      else
	mov = NO_MOVE;
    }
  if (!LEGAL(mov))
  {
    char buff[1000];

    sprintf (buff, "Bill has no moves.  Value: %f", node_value (node));
    curse2 (color, buff);
  }
  else					/* check legality of move */
  check_legality:
    {
      register int i;
      char buff[1000];

      for (i = 0; i < node->legals.max; i++)
	if (node->legals.data[i] == mov)
	  goto legal;
      fprintf (stderr, "attempted illegal move %s", uncvt (mov));
      internal_error ("");

    legal:
      sprintf (buff, "Bill moves to %s.  Value: %f",
	       uncvt (mov), node_value(node));
      curse2 (color, buff);
#ifdef TOURNAMENT
      {
	extern int Bell;
	
	if (Bell)
	  curse2 (color, "");
      }
#endif
    }

  if (Print_Response)
    response_out (color);
  return mov;
}

void response_out (color)
     color_t color;
{
}

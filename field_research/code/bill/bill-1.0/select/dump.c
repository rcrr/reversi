/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#include <node.h>

static FILE *dump;
static dump_node (SNode *node);

int Dump_Selectivity = 1;
color_t		Dump_Compaction = EMPTY; /* color to compact dump for; not BLACK or WHITE -> no compaction */
char	       *Dump_File = NULL;


#if NEW_DUMP_TREE
dump_tree (node)
     SNode *node;
{
  static int depth = -1;
  register int i;
  feature_t *f = node->features;

  depth++;
  for (i = 0; i < depth; i++)
    putchar (' ');
  printf ("%2s %8d %5d %5d %5d %5d\n", uncvt(node->to_here), node->upper+node->lower,
	  f[0],f[1],f[2],f[3]);
  for (node = node->child; node; node=node->next)
    dump_tree(node);
  depth--;
}
#else
dump_tree (root)
     register SNode *root;
{
#if CANT_HAPPEN
  if (Dump_File == NULL)
    {
      printf ("Dump file: ");
      gets (Buff);
      Dump_File = Buff;
    }
#endif
  if ((dump = fopen (Dump_File, "w")) == NULL)
    user_error ("can't open dump file");

  dump_node (root);
  putc ('\n', dump);
  fputs ("Tree dumped.\n", stderr);
  exit (0);
}
#endif

static dump_node (SNode *node)
{
  SNode *best_child ();
  static int depth = 0;

  fputs (uncvt (node->to_here), dump);
  if (node->color == Dump_Compaction)	/* compacting here? */
    if (node->child == NULL)
      putc ('?', dump);			/* no info here */
    else
      {
	putc ('-', dump);
	depth++;
	dump_node (best_child (node));
	depth--;
      }
  else if (node->nodes > Dump_Selectivity)
    {
      register SNode *best, *child;
      register int i;

      best = best_child (node);
      depth++;
      for (child = node->child; ; )
	{
	  putc (child == best ? '-' : ' ', dump);
	  dump_node (child);
	  if ((child = child->next) == NULL)
	    break;
	  putc ('\n', dump);
	  for (i = depth * 3; i > 8; i -= 8)
	    putc ('\t', dump);		/* tabs save a lot of space */
	  while (--i > 0)
	    putc (' ', dump);
	}
      depth--;
    }
  else if (node->nodes == 1)	/* leaf node */
    {
#if 0
      feature_t *f = node->features;

      printf (" %d %d %d %d %d ", f[0],f[1],f[2],f[3], node->lower+node->upper);
#endif
      printf (" %d ", node->lower+node->upper);
    }
  putc ('$', dump);
}

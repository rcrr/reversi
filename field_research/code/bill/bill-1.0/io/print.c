/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>

#include <general.h>
#include <color.h>

void print (string)
     register char *string;
{
  fputs (string, stdout);
}

void print_newline ()
{
  putchar ('\n');
}

void print_char (c)
     register char c;
{
  putchar (c);
}

/* Print to curse w/o newline, if using it.  Otherwise, print to stdout with newline.  Always prints to log file. */

void curse (string)
    char *string;
{
  puts (string);
}

/* same except it's an action by a color */

void curse2 (color, string)
     color_t color;
     char *string;
{
  printf ("%s> %s\n", Color_Name[color], string);
  fflush (stdout);
}

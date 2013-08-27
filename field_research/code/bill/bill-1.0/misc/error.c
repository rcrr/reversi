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

#include <signal.h>
#include <stdio.h>

#include <tourn.h>

static void do_exit (char *message)
{
  fputs (message, stderr);
  _exit (1);
}

void quit ()
{
  do_exit ("Quit.\n");
}

RETSIGTYPE
signal_error (sig)
     int sig;
{
  char buff[100];

  sprintf (buff, "Caught signal %d, exiting...\n", sig);
  do_exit (buff);
}

void user_error (message)
     char *message;
{
  char buff[1000];

  sprintf (buff, "Error: %s.\n", message);
  do_exit (buff);
}

void internal_error (message)
     char *message;
{
  char buff[1000];

  sprintf (buff, "Internal error (%s).\n", message);
  fputs (buff, stderr);
  if (!Ios)
  {
    fputs ("Abort (and dump core)? ", stderr);
    switch (getchar ())
    {
    case 'y':
      kill (getpid (), SIGQUIT);
      
    default:
      while (getchar () != '\n');
    case '\n':;
    }

    fputs ("Sorry.\n", stderr);
    _exit (2);
  }
}

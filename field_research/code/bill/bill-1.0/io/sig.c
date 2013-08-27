/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Signals used by Bill - Most of them use the same flag (Wake_Up)
 * which is checked by ab and ab2 (alpha-beta search)
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <signal.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

int Wake_Up;			/* either input is ready or alarm rang */
int Accept_Interrupt = TRUE;

/* Wake up when the alarm clock goes off - used by zero.c for making sure 
   the upper time bound is not exceeded. */

RETSIGTYPE
wake_up (sig)
     int sig;
{
  Wake_Up = TRUE;
  /* restore handler, in case the system doesn't (BSD/SunOS do, Linux
     doesn't unless you include bsd/signal.h, but not worth checking for */
  signal (sig, wake_up);
}

/* Catch interrupt and make bill think time is up, or input is ready. */

RETSIGTYPE
interrupt (sig)
     int sig;
{
  if (Accept_Interrupt)
    Wake_Up = TRUE;
  /* restore handler in case system doesn't.  See comment in wake_up () */
  signal (sig, wake_up);
}

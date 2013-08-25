/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* general board copying routine, calls memcpy */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <board.h>

copy_board(from, to)
    square_t *from;
    square_t *to;
{
#if HAVE_MEMCPY
    memcpy((char*) to, (char*) from, sizeof (board_t));
#else  /* should have bcopy() instead then */
    bcopy ((char *) from, (char *) to, sizeof(board_t));
#endif
}

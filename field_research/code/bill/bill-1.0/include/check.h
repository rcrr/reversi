/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <tourn.h>

#ifdef TOURNAMENT
#define CHECK(test, message) 0
#else
#define CHECK(test, message) ((test) ? 0 : internal_error(message))
#endif				/* #ifdef TOURNAMENT */

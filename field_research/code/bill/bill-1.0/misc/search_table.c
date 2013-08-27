/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* searching functions */

#include <general.h>

extern move_t SS (), deepen_front_end ();
extern ab_fix_steal (), SS_fix_steal ();

search_record Search_Table[] =
{
  { SS, SS_fix_steal},		/* selective search */
  { deepen_front_end, ab_fix_steal }, /* normal alpha beta */
};

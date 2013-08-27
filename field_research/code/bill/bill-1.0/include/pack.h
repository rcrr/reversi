/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* This header file contains definitions needed by the evaluation function and the table generators so that
   they can put stuff into an int consistently.
*/

#ifndef PACK_H

#define P_BITS 9			/* including overflow */
#define W_BITS 10
#define M_BITS 13

#define P_ACTUAL 5
#define W_ACTUAL 6
#define M_ACTUAL 9

typedef union
{
  unsigned int	  all;
  unsigned short  word[2];
  unsigned char	  byte[4];
  struct
    {
      unsigned int    pscore:P_BITS;
      unsigned int    wscore:W_BITS;
      unsigned int    mscore:M_BITS;
    }               scores;
    struct
    {
      unsigned int    bit0:1,		/* on a little-endian machine */
		      fill1:4,
		      bit5:1,
		      fill6:10,
		      bit16:1,
		      fill17:4,
		      bit21:1;
    }               bits;
} packed;

#define PACK_H
#endif

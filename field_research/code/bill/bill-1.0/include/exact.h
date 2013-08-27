/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* macros for selective search number system */

#define PROB_OFFSET	0x40000000	/* a big number */
#define PROB_CUTOFF	0x20000000	/* a not-as-big number */
#define MAX_CUTOFF	0x60000000
#define MAX_SCORE	1879048192
#define MIN_SCORE      -1879048192
#define DIFF_SHIFT	23
#define DIFF_VALUE	0x00800000
#define AUX_CUTOFF	0x00400000
#define CONS_SHIFT	12
#define CONS_VALUE	0x00001000
#define MAX_PIECE_DIFF	16
#define PIECE_SHIFT	1

#define DONE(node) ((node)->upper <= (node)->lower)
#define SHIFT_UPPER(upper) ((upper) + PROB_OFFSET)
#define SHIFT_LOWER(lower) ((lower) - PROB_OFFSET)
#define SHIFT_DIFF(diff) ((diff) == MAX_PIECES ? MAX_SCORE : (diff) == -MAX_PIECES ? MIN_SCORE : (diff) << DIFF_SHIFT)
#define UNSHIFT_UPPER(upper) ((upper) - PROB_OFFSET)
#define UNSHIFT_LOWER(lower) ((lower) + PROB_OFFSET)
#define UNSHIFT_DIFF(value) ((value) >= MAX_CUTOFF ? MAX_PIECES : (value) <= -MAX_CUTOFF ? -MAX_PIECES: ((value) + DIFF_VALUE / 2) >> DIFF_SHIFT)
#define UNSHIFT_AUX(value) (value - EXACT_BASE(value))
#define EXACT_BASE(value) (((value) + DIFF_VALUE / 2) >> DIFF_SHIFT << DIFF_SHIFT)
#define INEXACT_UPPER(upper) ((upper) >= PROB_CUTOFF && (upper) < MAX_CUTOFF)
#define INEXACT_LOWER(lower) ((lower) < -PROB_CUTOFF && (lower) >= -MAX_CUTOFF)
#define WIN_VALUE(value) ((value) < 0 ? -1 : (value) == 0 ? 0 : 1)

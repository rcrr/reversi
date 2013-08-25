/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* contains definitions for accessing the features used in the evaluation function */

#include <bill-features.h>

#undef ESCORE			/* remove stuff from features.h */
#undef NUM_FEATURES

#define ESCORE		0	/* edge advantage.  Not really useful */
#define THIS_PSCORE	1	/* pot mob */
#define PREV_PSCORE	2
#define THIS_WSCORE	3
#define PREV_WSCORE	4
#define THIS_MFINAL	5
#define PREV_MFINAL	6
#define PIECE_DIFFERENCE 7

#if 0
#define EMOVES		4
#define EPENALTY	5	/* penalty for this */
#define RMOVES		6	/* available moves on the rest of the board */
#define RPENALTY	7	/* penalty for this */
#endif

#define NUM_FEATURES	8
#define TERMS 		NUM_FEATURES

/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* Winner of coefficient tournament - a.k.a. c9, d2 */

#include <general.h>

int   Move_Coef[MAX_PIECES] =
{
 350, 348, 346, 344, 342, 340, 338, 336, 334, 332,
 330, 328, 326, 324, 322, 320, 318, 316, 314, 312,
 310, 308, 306, 304, 302, 300, 298, 296, 294, 292,
 290, 288, 286, 284, 282, 280, 278, 276, 274, 272,
 270, 268, 266, 264, 262, 260, 258, 256, 254, 252,
 250, 248, 246, 244, 242, 240, 238, 236, 234, 232,
 230, 228, 226, 224
};

int   Pot_Coef[MAX_PIECES] =
{
 500, 498, 496, 494, 492, 490, 488, 486, 484, 482,
 480, 476, 472, 468, 464, 460, 456, 452, 448, 444,
 440, 428, 416, 404, 392, 380, 368, 356, 344, 332,
 320, 308, 296, 284, 272, 260, 248, 236, 224, 212,
 200, 185, 170, 155, 140, 125, 110,  95,  80,  65,
 50,  40,  30,  20,  10, 0, 0, 0, 0, 0, 0, 0, 0, 0 
};

int   W_Coef[MAX_PIECES] =
{
 500, 498, 496, 494, 492, 490, 488, 486, 484, 482,
 480, 476, 472, 468, 464, 460, 456, 452, 448, 444,
 440, 428, 416, 404, 392, 380, 368, 356, 344, 332,
 320, 308, 296, 284, 272, 260, 248, 236, 224, 212,
 200, 185, 170, 155, 140, 125, 110,  95,  80,  65,
 50,  40,  30,  20,  10, 0, 0, 0, 0, 0, 0, 0, 0, 0 
};


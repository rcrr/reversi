/**
 * @file
 *
 * @brief Reversi C - Board implementation
 */

/**
 * @cond
 *
 * board.c
 *
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 *
 * Copyright (c) 2013 Roberto Corradini. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 *
 * @endcond
 */

#include <stdio.h>
#include <stdlib.h>

#include "board.h"

SquareState color(const Player p)
{
    switch (p) {
    case BLACK_PLAYER: return BLACK_SQUARE;
    case WHITE_PLAYER: return WHITE_SQUARE;
    default:
      fprintf(stderr, "Argument Player p = %d is invalid.\n", p);
      abort();
    }
}

Player opponent(const Player p)
{
  return (p == BLACK_PLAYER) ? WHITE_PLAYER : BLACK_PLAYER;
}

char *description(const Player p)
{
  return "TO BE IMPLEMENTED";
}


SquareState get_square(const Board *b, const Square sq)
{
  unsigned long long int bitsquare = 1ULL << sq;
  if (bitsquare & b->blacks)
    return BLACK_SQUARE;
  else if (bitsquare & b->whites)
    return WHITE_SQUARE;
  else
    return EMPTY_SQUARE;
}


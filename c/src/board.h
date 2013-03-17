/**
 * @file
 *
 * @brief Reversi C - Board header file
 */

/**
 * @cond
 *
 * board.h
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

#ifndef BOARD_H
#define BOARD_H

typedef enum {
  BLACK_PLAYER,
  WHITE_PLAYER
} Player;

typedef enum {
  EMPTY_SQUARE,
  BLACK_SQUARE,
  WHITE_SQUARE
} SquareState;

typedef enum {
  A1, B1, C1, D1, E1, F1, G1, H1,
  A2, B2, C2, D2, E2, F2, G2, H2,
  A3, B3, C3, D3, E3, F3, G3, H3,
  A4, B4, C4, D4, E4, F4, G4, H4,
  A5, B5, C5, D5, E5, F5, G5, H5,
  A6, B6, C6, D6, E6, F6, G6, H6,
  A7, B7, C7, D7, E7, F7, G7, H7,
  A8, B8, C8, D8, E8, F8, G8, H8
} Square;

typedef unsigned long long int SquareSet;

typedef struct Board {
  unsigned long long int blacks;
  unsigned long long int whites;
} Board;

typedef struct GamePosition {
  Board board;
  Player player;
} GamePosition;

extern SquareState player_color(const Player p);

extern Player player_opponent(const Player p);

extern char *player_description(const Player p);

extern Board *new_board(const SquareSet b, const SquareSet w);

extern Board *delete_board(Board *b);

extern SquareState board_get_square(const Board *b, const Square sq);

#endif /* BOARD_H */

/**
 * @file
 *
 * @brief Endgame Solver.
 * @details This executable analyzes an end game position and
 * computes the exact outcome.
 *
 * @par endgame_solver.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#include <stdio.h>
#include "board.h"

/**
 * Main entry to the Reversi C implementation.
 * 
 * Has to be completly developed.
 */
int main(void)
{

  Player p = WHITE_PLAYER;

  printf("Hello, reversi player!\n");

  SquareSet squares;
  printf("sizeof SquareSet = %zu\n", sizeof(squares));

  Board b;
  printf("sizeof Board = %zu\n", sizeof(b));

  GamePosition gp;
  printf("sizeof GamePosition = %zu\n", sizeof(gp));

  printf("player_opponent(p) = %d\n", player_opponent(p));

  return 0;

}


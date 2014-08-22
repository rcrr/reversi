/**
 * @file
 *
 * @brief Dump bitrow changes for player array.
 * @details This executable create a file that contains the
 * values computed by the function bitrow_changes_for_player_array.
 *
 * @par dump_bitrow_changes.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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



/* Static constants. */



/* Static variables. */



/**
 * @brief Main entry for the dump utility.
 * 
 * @todo It dumps the contetnt of the bitrow changes for player array.
 * Call it as "build/bin/dump_bitrow_changes > out/bitrow_changes_for_player.csv".
 */
int
main (int argc, char *argv[])
{
  int player_row;
  int opponent_row;
  int move_position;

  board_module_init();

  printf("ARRAY_INDEX;PLAYER_ROW;OPPONENT_ROW;MOVE_POSITION;PLAYER_CHANGES\n");

  for (player_row = 0; player_row < 256; player_row++) {
    for (opponent_row = 0; opponent_row < 256; opponent_row++) {
      for (move_position = 0; move_position < 8; move_position++) {
        const int array_index = player_row | (opponent_row << 8) | (move_position << 16);
        const uint8_t player_changes = board_bitrow_changes_for_player(player_row, opponent_row, move_position);
        printf("%d;%d;%d;%d;%d\n", array_index, player_row, opponent_row, move_position, player_changes);
      }
    }
  }

  return 0;
}

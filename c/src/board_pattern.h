/**
 * @file
 *
 * @brief Defines pattern and indexes for reversi boards.
 *
 * @details To be completed.
 *
 * The `EDGE` pattern has four instances ranging from `[0..3]`:
 *
 * @code
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 1     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 2  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 3  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 4  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 5  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 6  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 7  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 8  . . . . . . . .     . . . . . . . 1     1 1 1 1 1 1 1 1     1 . . . . . . .
 *
 * @endcode
 *
 *
 * @par board_pattern.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018 Roberto Corradini. All rights reserved.
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

#ifndef BOARD_PATTERN_H
#define BOARD_PATTERN_H

#include "board.h"
#include "board_trans.h"


/*
 * board_t definitions.
 */

typedef struct board_s board_t;

struct board_s { SquareSet square_sets[2]; };

extern SquareSet
board_get_mover_square_set (const board_t *const b);

extern SquareSet
board_get_opponent_square_set (const board_t *const b);

extern void
board_set_mover_square_set (board_t *b,
                            SquareSet s);

extern void
board_set_opponent_square_set (board_t *b,
                               SquareSet s);

extern void
board_set_square_sets (board_t *b,
                       SquareSet m,
                       SquareSet o);

/*
 * End of board_t definitions
 */



#define EDGE_PRINCIPAL_PATTERN 0x00000000000000ff



typedef uint16_t board_pattern_index_t;

typedef struct board_pattern_s board_pattern_t;

extern SquareSet
board_pattern_pack_edge (SquareSet s);

struct board_pattern_s {
  unsigned int id;
  char name[5];
  unsigned int n_instances;
  unsigned int n_squares;
  SquareSet principal_pattern;
  board_trans_f trans_to_principal_f[8]; // array of transformation functions
  SquareSet (*pattern_pack_f) (SquareSet);
};

static const board_pattern_t
board_pattern_edge = { 0,
                       "EDGE",
                       4,
                       8,
                       EDGE_PRINCIPAL_PATTERN,
                       { board_trans_identity,
                         board_trans_rotate_90a,
                         board_trans_rotate_180,
                         board_trans_rotate_90c,
                         NULL, NULL, NULL, NULL },
                       board_pattern_pack_edge };



/**
 * @brief TBD.
 *
 * @details TBD.
 *
 * @code
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 1     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 2  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 3  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 4  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 5  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 6  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 7  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 8  . . . . . . . .     . . . . . . . 1     1 1 1 1 1 1 1 1     1 . . . . . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       tbd
 */
extern void
board_pattern_compute_indexes (board_pattern_index_t *indexes,
                               const board_pattern_t *const p,
                               const board_t *const b);



#endif

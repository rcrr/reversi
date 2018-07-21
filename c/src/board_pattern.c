/**
 * @file
 *
 * @brief Board pattern module implementation.
 *
 * @par board_pattern.c
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "board_trans.h"
#include "board_pattern.h"



/*
 * board_t implementation should be moved into the board module.
 */

SquareSet
board_get_mover_square_set (const board_t *const b)
{
  return b->square_sets[0];
}

SquareSet
board_get_opponent_square_set (const board_t *const b)
{
  return b->square_sets[1];
}

void
board_set_mover_square_set (board_t *b,
                            SquareSet s)
{
  b->square_sets[0] = s;
}

void
board_set_opponent_square_set (board_t *b,
                               SquareSet s)
{
  b->square_sets[1] = s;
}

void
board_set_square_sets (board_t *b,
                       SquareSet m,
                       SquareSet o)
{
  b->square_sets[0] = m;
  b->square_sets[1] = o;
}

/*
 * End of board_t implementation
 */

SquareSet
board_pattern_pack_edge (SquareSet s)
{
  return s & 0x00000000000000ff;
}

SquareSet
board_pattern_pack_corner (SquareSet s)
{
  SquareSet s1, s2, s3;
  s1 = s & 0x0000000000000007;
  s2 = s & 0x0000000000000700;
  s3 = s & 0x0000000000070000;
  return s1 | (s2 >> 5) | (s3 >> 10);
}

SquareSet
board_pattern_pack_xedge (SquareSet s)
{
  SquareSet s1, s2, s3;
  s1 = s & 0x00000000000000ff;
  s2 = s & 0x0000000000000200;
  s3 = s & 0x0000000000004000;
  return s1 | (s2 >> 1) | (s3 >> 5);
}

SquareSet
board_pattern_pack_r2 (SquareSet s)
{
  return (s >> 8) & 0x00000000000000ff;
}

SquareSet
board_pattern_pack_r3 (SquareSet s)
{
  return (s >> 16) & 0x00000000000000ff;
}

SquareSet
board_pattern_pack_r4 (SquareSet s)
{
  return (s >> 24) & 0x00000000000000ff;
}

SquareSet
board_pattern_pack_diag4 (SquareSet s)
{
  const SquareSet diag4 = 0x0000000001020408;
  const SquareSet mask  = 0x000000000000000f;
  s &= diag4;
  s |= s >> 16;
  s |= s >>  8;
  return s & mask;
}

SquareSet
board_pattern_pack_diag5 (SquareSet s)
{
  const SquareSet diag5 = 0x0000000102040810;
  const SquareSet mask  = 0x000000000000001f;
  s &= diag5;
  s |= s >> 32;
  s |= s >> 16;
  s |= s >>  8;
  return s & mask;
}

SquareSet
board_pattern_pack_diag6 (SquareSet s)
{
  const SquareSet diag6 = 0x0000010204081020;
  const SquareSet mask  = 0x000000000000003f;
  s &= diag6;
  s |= s >> 32;
  s |= s >> 16;
  s |= s >>  8;
  return s & mask;
}

SquareSet
board_pattern_pack_diag7 (SquareSet s)
{
  const SquareSet diag7 = 0x0001020408102040;
  const SquareSet mask  = 0x000000000000007f;
  s &= diag7;
  s |= s >> 32;
  s |= s >> 16;
  s |= s >>  8;
  return s & mask;
}

SquareSet
board_pattern_pack_diag8 (SquareSet s)
{
  const SquareSet diag8 = 0x0102040810204080;
  const SquareSet mask  = 0x00000000000000ff;
  s &= diag8;
  s |= s >> 32;
  s |= s >> 16;
  s |= s >>  8;
  return s & mask;
}

SquareSet
board_pattern_pack_2x5cor (SquareSet s)
{
  const SquareSet mask0  = 0x000000000000001f;
  const SquareSet mask1  = 0x0000000000001f00;
  return (s & mask0) | ((s & mask1) >> 3);
}

bool
board_pattern_get_id_by_name (board_pattern_id_t *idp,
                              char *name)
{
  for (int i = 0; i < BOARD_PATTERN_INVALID; i++) {
    if (strcmp(name, board_patterns[i].name) == 0) {
      *idp = i;
      return true;
    }
  }
  return false;
}



void
board_pattern_compute_indexes (board_pattern_index_t *indexes,
                               const board_pattern_t *const p,
                               const board_t *const b)
{
  board_t tb[8];
  SquareSet m, o, c;
  board_trans_f tf;
  SquareSet mover_pattern_packed[8];
  SquareSet opponent_pattern_packed[8];

  const uint64_t cim[] = { 1, 3,  9, 27,  81, 243,  729, 2187,  6561, 19683,  59049, 177147 };
  const uint64_t cio[] = { 2, 6, 18, 54, 162, 486, 1458, 4374, 13122, 39366, 118098, 354294 };

  /*
   * This part should go out of the specific pattern, and done once for all patterns.
   * For now we focus on EDGE pattern alone.
   */

  m = board_get_mover_square_set(b);
  o = board_get_opponent_square_set(b);

  for (int i = 0; i < p->n_instances; i++) {
    tf = p->trans_to_principal_f[i];
    board_set_square_sets(tb + i, tf(m), tf(o));
  }

  for (int i = 0; i < p->n_instances; i++) {
    mover_pattern_packed[i] = p->pattern_pack_f(board_get_mover_square_set(tb + i));
    opponent_pattern_packed[i] = p->pattern_pack_f(board_get_opponent_square_set(tb + i));
  }

  for (int i = 0; i < p->n_instances; i++) {
    indexes[i] = 0;
    for (int j = 0; j < p->n_squares; j++) {
      c = 1ULL << j;
      indexes[i] += (c & mover_pattern_packed[i]   ) ? cim[j] : 0;
      indexes[i] += (c & opponent_pattern_packed[i]) ? cio[j] : 0;
    }
  }

  return;
}

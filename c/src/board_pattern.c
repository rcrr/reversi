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
 * @copyright 2018, 2019, 2020 Roberto Corradini. All rights reserved.
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
#include <assert.h>

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

void
board_feature_values_intercept (board_t *board,
                                double *values)
{
  values[0] = 1.0;
}

static double
board_feature_values_mobility0 (board_t *board)
{
  GamePositionX gpx;
  SquareSet legal_moves, empties;
  unsigned int legal_move_count, empty_count, imobility;
  double mobility;

  gpx.blacks = board_get_mover_square_set(board);
  gpx.whites = board_get_opponent_square_set(board);
  gpx.player = BLACK_PLAYER;

  legal_moves = game_position_x_legal_moves(&gpx);
  legal_move_count = bitw_bit_count_64(legal_moves);

  if (legal_move_count > 0) {
    imobility = legal_move_count;
  } else {
    gpx.blacks = board_get_opponent_square_set(board);
    gpx.whites = board_get_mover_square_set(board);
    legal_moves = game_position_x_legal_moves(&gpx);
    legal_move_count = bitw_bit_count_64(legal_moves);
    imobility = legal_move_count ? 1 : 0;
  }

  empties = game_position_x_empties(&gpx);
  empty_count = bitw_bit_count_64(empties);

  mobility = imobility / (double) empty_count;

  return mobility;
}

void
board_feature_values_mobility (board_t *board,
                               double *values)
{
  values[0] = board_feature_values_mobility0(board);
}

void
board_feature_values_mobility2 (board_t *board,
                                double *values)
{
  double mobility = board_feature_values_mobility0(board);
  values[0] = mobility;
  values[1] = mobility * mobility;
}

void
board_feature_values_mobility3 (board_t *board,
                                double *values)
{
  double mobility = board_feature_values_mobility0(board);
  values[0] = mobility;
  values[1] = mobility * mobility;
  values[2] = mobility * mobility * mobility;
}

bool
board_feature_get_id_by_name (board_feature_id_t *idp,
                              char *name)
{
  for (int i = 0; i < BOARD_FEATURE_INVALID; i++) {
    if (strcmp(name, board_features[i].name) == 0) {
      if (idp) *idp = i;
      return true;
    }
  }
  return false;
}

SquareSet
board_pattern_mask (SquareSet s,
                    board_pattern_index_t p,
                    unsigned int instance)
{
  assert(p < BOARD_PATTERN_INVALID);
  assert (instance < 8);
  return s & board_patterns[p].masks[instance];
}

SquareSet
board_pattern_pack_edge (SquareSet s)
{
  return s & 0x00000000000000ff;
}

SquareSet
board_pattern_unpack_edge (SquareSet s)
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
board_pattern_unpack_corner (SquareSet s)
{
  SquareSet s1, s2, s3;
  s1 = s & 0x0000000000000007;
  s2 = s & 0x0000000000000038;
  s3 = s & 0x00000000000001c0;
  return s1 | (s2 << 5) | (s3 << 10);
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
board_pattern_unpack_xedge (SquareSet s)
{
  SquareSet s1, s2, s3;
  s1 = s & 0x00000000000000ff;
  s2 = s & 0x0000000000000100;
  s3 = s & 0x0000000000000200;
  return s1 | (s2 << 1) | (s3 << 5);
}

SquareSet
board_pattern_pack_r2 (SquareSet s)
{
  return (s >> 8) & 0x00000000000000ff;
}

SquareSet
board_pattern_unpack_r2 (SquareSet s)
{
  return (s & 0x00000000000000ff) << 8;
}

SquareSet
board_pattern_pack_r3 (SquareSet s)
{
  return (s >> 16) & 0x00000000000000ff;
}

SquareSet
board_pattern_unpack_r3 (SquareSet s)
{
  return (s & 0x00000000000000ff) << 16;
}

SquareSet
board_pattern_pack_r4 (SquareSet s)
{
  return (s >> 24) & 0x00000000000000ff;
}

SquareSet
board_pattern_unpack_r4 (SquareSet s)
{
  return (s & 0x00000000000000ff) << 24;
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
board_pattern_unpack_diag4 (SquareSet s)
{
  const SquareSet diag4 = 0x0000000001020408;
  const SquareSet mask  = 0x000000000000000f;
  s &= mask;
  s |= (s <<  8);
  s |= (s << 16);
  return s & diag4;
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
board_pattern_unpack_diag5 (SquareSet s)
{
  const SquareSet diag5 = 0x0000000102040810;
  const SquareSet mask  = 0x000000000000001f;
  s &= mask;
  s |= (s <<  8);
  s |= (s << 16);
  s |= (s << 32);
  return s & diag5;
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
board_pattern_unpack_diag6 (SquareSet s)
{
  const SquareSet diag6 = 0x0000010204081020;
  const SquareSet mask  = 0x000000000000003f;
  s &= mask;
  s |= (s <<  8);
  s |= (s << 16);
  s |= (s << 32);
  return s & diag6;
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
board_pattern_unpack_diag7 (SquareSet s)
{
  const SquareSet diag7 = 0x0001020408102040;
  const SquareSet mask  = 0x000000000000007f;
  s &= mask;
  s |= (s <<  8);
  s |= (s << 16);
  s |= (s << 32);
  return s & diag7;
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
board_pattern_unpack_diag8 (SquareSet s)
{
  const SquareSet diag8 = 0x0102040810204080;
  const SquareSet mask  = 0x00000000000000ff;
  s &= mask;
  s |= (s <<  8);
  s |= (s << 16);
  s |= (s << 32);
  return s & diag8;
}

SquareSet
board_pattern_pack_2x5cor (SquareSet s)
{
  const SquareSet mask0  = 0x000000000000001f;
  const SquareSet mask1  = 0x0000000000001f00;
  return (s & mask0) | ((s & mask1) >> 3);
}

SquareSet
board_pattern_unpack_2x5cor (SquareSet s)
{
  const SquareSet mask0  = 0x000000000000001f;
  const SquareSet mask1  = 0x00000000000003e0;
  return (s & mask0) | ((s & mask1) << 3);
}

SquareSet
board_pattern_pack_diag3 (SquareSet s)
{
  const SquareSet diag3 = 0x0000000000010204;
  const SquareSet mask  = 0x0000000000000007;
  s &= diag3;
  s |= s >> 16;
  s |= s >>  8;
  return s & mask;
}

SquareSet
board_pattern_unpack_diag3 (SquareSet s)
{
  const SquareSet diag3 = 0x0000000000010204;
  const SquareSet mask  = 0x0000000000000007;
  s &= mask;
  s |= (s <<  8);
  s |= (s << 16);
  return s & diag3;
}

bool
board_pattern_get_id_by_name (board_pattern_id_t *idp,
                              char *name)
{
  for (int i = 0; i < BOARD_PATTERN_INVALID; i++) {
    if (strcmp(name, board_patterns[i].name) == 0) {
      if (idp) *idp = i;
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

void
board_pattern_compute_principal_indexes (board_pattern_index_t *principals,
                                         const board_pattern_index_t *indexes,
                                         const board_pattern_t *const p,
                                         const bool one_value)
{
  static bool initialized = false;
  static board_pattern_index_t a[BOARD_PATTERN_INDEX_TABLE_SIZE];
  static board_pattern_index_t *ap[BOARD_PATTERN_COUNT];

  size_t n_values;

  if (!initialized) {
    board_pattern_index_t *p = a;
    size_t s = 0;
    for (board_pattern_id_t bp = 0; bp < BOARD_PATTERN_INVALID; bp++) {
      const unsigned long int n = board_patterns[bp].n_configurations;
      ap[bp] = p;
      for (board_pattern_index_t idx = 0; idx < n; idx++) {
        board_pattern_index_t principal_index_value, mirror_index_value;
        board_t board;
        SquareSet m, o;
        board_pattern_index_to_packed(&board, idx);
        m = board_patterns[bp].pattern_unpack_f(board_get_mover_square_set(&board));
        o = board_patterns[bp].pattern_unpack_f(board_get_opponent_square_set(&board));
        m = board_patterns[bp].pattern_mirror_f(m);
        o = board_patterns[bp].pattern_mirror_f(o);
        m = board_patterns[bp].pattern_pack_f(m);
        o = board_patterns[bp].pattern_pack_f(o);
        board_set_square_sets(&board, m, o);
        mirror_index_value = board_pattern_packed_to_index(&board, board_patterns[bp].n_squares);
        principal_index_value = mirror_index_value < idx ? mirror_index_value : idx;
        *p++ = principal_index_value;
      }
      s += n;
    }
    if (s != BOARD_PATTERN_INDEX_TABLE_SIZE) {
      fprintf(stderr, "Macro BOARD_PATTERN_INDEX_TABLE_SIZE has a definition not compatible with board_patterns definition. Aborting ... \n");
      abort();
    }
    initialized = true;
  }

  n_values = one_value ? 1 : p->n_instances;
  for (unsigned int i = 0; i < n_values; i++) {
    principals[i] = *(ap[p->id] + indexes[i]);
  }
}

board_pattern_index_t
board_pattern_packed_to_index (board_t *packed,
                               unsigned int n_squares)
{
  board_pattern_index_t idx;
  int k;
  SquareSet m, o, x, mask;

  m = board_get_mover_square_set(packed);
  o = board_get_opponent_square_set(packed);
  mask = 0x0000000000000001;

  k = 1;
  idx = 0;
  for (int i = 0; i < n_squares; i++) {
    x = m & mask;
    idx += k * x;
    x = o & mask;
    idx += k * x * 2;
    m >>= 1;
    o >>= 1;
    k *= 3;
  }

  return idx;
}

void
board_pattern_index_to_packed (board_t *packed,
                               board_pattern_index_t index)
{
  SquareSet m, o, c;

  m = 0x0000000000000000;
  o = 0x0000000000000000;
  c = 0x0000000000000001;

  while (index) {
    switch (index % 3) {
    case 2:
      o |= c;
      break;
    case 1:
      m |= c;
      break;
    case 0:
      ;
    }
    index /= 3;
    c <<= 1;
  }

  board_set_mover_square_set(packed, m);
  board_set_opponent_square_set(packed, o);
}

void
board_pattern_compute_rotated (board_t *board,
                               board_pattern_rotated_t *rotated)
{
  assert(board);
  assert(rotated);

  SquareSet *s;

  const SquareSet m = board->square_sets[0];
  const SquareSet o = board->square_sets[1];

  s = rotated->named_boards.identity.square_sets;
  s[0] = m;
  s[1] = o;

  s = rotated->named_boards.rot_90a.square_sets;
  s[0] = board_trans_rotate_90a(m);
  s[1] = board_trans_rotate_90a(o);

  s = rotated->named_boards.rot_180.square_sets;
  s[0] = board_trans_rotate_180(m);
  s[1] = board_trans_rotate_180(o);

  s = rotated->named_boards.rot_90c.square_sets;
  s[0] = board_trans_rotate_90c(m);
  s[1] = board_trans_rotate_90c(o);

  s = rotated->named_boards.flip_ve.square_sets;
  s[0] = board_trans_flip_vertical(m);
  s[1] = board_trans_flip_vertical(o);

  s = rotated->named_boards.flip_dh.square_sets;
  s[0] = board_trans_flip_diag_h1a8(m);
  s[1] = board_trans_flip_diag_h1a8(o);

  s = rotated->named_boards.flip_ho.square_sets;
  s[0] = board_trans_flip_horizontal(m);
  s[1] = board_trans_flip_horizontal(o);

  s = rotated->named_boards.flip_da.square_sets;
  s[0] = board_trans_flip_diag_a1h8(m);
  s[1] = board_trans_flip_diag_a1h8(o);
}

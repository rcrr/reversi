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
 * @copyright 2018, 2019, 2020, 2021 Roberto Corradini. All rights reserved.
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

/*
 * Comment this line to enable assertion in the module.
 * The line must be inserted before the inclusion of <assert.h>
 */
#define NDEBUG

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <immintrin.h>

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

void
board_from_gpx (board_t *const b,
                const GamePositionX *const gpx)
{
  if (gpx->player == BLACK_PLAYER) {
    b->square_sets[0] = gpx->blacks;
    b->square_sets[1] = gpx->whites;
  } else {
    b->square_sets[0] = gpx->whites;
    b->square_sets[1] = gpx->blacks;
  }
}

/*
 * End of board_t implementation
 */

void
board_feature_values_intercept (const board_t *board,
                                double *values)
{
  values[0] = 1.0;
}

static double
board_feature_values_mobility0 (const board_t *board)
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
board_feature_values_mobility (const board_t *board,
                               double *values)
{
  values[0] = board_feature_values_mobility0(board);
}

void
board_feature_values_mobility2 (const board_t *board,
                                double *values)
{
  double mobility = board_feature_values_mobility0(board);
  values[0] = mobility;
  values[1] = mobility * mobility;
}

void
board_feature_values_mobility3 (const board_t *board,
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
  for (int i = 0; i < BOARD_FEATURE_COUNT; i++) {
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
  assert(p < BOARD_PATTERN_COUNT);
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
  for (int i = 0; i < BOARD_PATTERN_COUNT; i++) {
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
    for (board_pattern_id_t bp = 0; bp < BOARD_PATTERN_COUNT; bp++) {
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

  board_pattern_index_t idxv;
  const uint64_t kv[16] = {1,3,9,27,81,243,729,2187,6561,19683,59049,177147,531441,1594323,4782969,14348907};
  const uint64_t mv[16] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768};

  SquareSet *bv = (SquareSet *) packed;

  const SquareSet m = bv[0];
  const SquareSet o = bv[1] << 1;

  idxv = 0;
  for (int i = 0; i < 10; i++) {
    idxv += kv[i] * (((m & mv[i]) + (o & mv[i+1])) >> i);
    if (i == n_squares -1) break;
  }

  return idxv;
}

void
fprintf_256i_epi8 (FILE *f,
                   __m256i var)
{
  uint8_t val[32];
  memcpy(val, &var, sizeof(val));
  fprintf(f, "%u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u",
          val[0], val[1], val[2],  val[3],  val[4],  val[5],  val[6],  val[7],
          val[8], val[9], val[10], val[11], val[12], val[13], val[14], val[15],
          val[16], val[17], val[18], val[19], val[20], val[21], val[22], val[23],
          val[24], val[25], val[26], val[27], val[28], val[29], val[30], val[31]);
}

void
fprintf_256i_epi16 (FILE *f,
                    __m256i var)
{
  uint16_t val[16];
  memcpy(val, &var, sizeof(val));
  fprintf(f, "%u %u %u %u %u %u %u %u %u %u %u %u %u %u %u %u",
          val[0], val[1], val[2],  val[3],  val[4],  val[5],  val[6],  val[7],
          val[8], val[9], val[10], val[11], val[12], val[13], val[14], val[15]);
}

void
fprintf_256i_epi32 (FILE *f,
                    __m256i var)
{
  uint32_t val[8];
  memcpy(val, &var, sizeof(val));
  fprintf(f, "%u %u %u %u %u %u %u %u",
          val[0], val[1], val[2], val[3], val[4], val[5], val[6], val[7]);
}

void
fprintf_hex_256i_epi32 (FILE *f,
                        __m256i var)
{
  uint32_t val[8];
  memcpy(val, &var, sizeof(val));
  fprintf(f, "%08x %08x %08x %08x %08x %08x %08x %08x",
          val[0], val[1], val[2], val[3], val[4], val[5], val[6], val[7]);
}

static board_pattern_index_t
board_pattern_packed_to_index8_vec (board_t *packed)
{
  const __m256i vmask32_lo = _mm256_set_epi32((uint32_t) 0x0080, (uint32_t) 0x0040, (uint32_t) 0x0020, (uint32_t) 0x0010,
                                              (uint32_t) 0x0008, (uint32_t) 0x0004, (uint32_t) 0x0002, (uint32_t) 0x0001);

  const __m256i vkm32_lo = _mm256_setr_epi32( 1, 3,  9, 27,  81, 243,  729, 2187 );
  const __m256i vko32_lo = _mm256_setr_epi32( 2, 6, 18, 54, 162, 486, 1458, 4374 );

  const __m256i zero = _mm256_set1_epi32(0x00);

  SquareSet *bv = (SquareSet *) packed;
  const SquareSet m = bv[0]; // m: mover
  const SquareSet o = bv[1]; // o: opponent

  const uint32_t m_lo8 = (uint32_t) m & 0x00000000000000ff;
  const uint32_t o_lo8 = (uint32_t) o & 0x00000000000000ff;

  const __m256i mv = _mm256_set1_epi32(m_lo8);
  const __m256i ov = _mm256_set1_epi32(o_lo8);

  const __m256i mvm_lo = _mm256_and_si256(mv, vmask32_lo);
  const __m256i ovm_lo = _mm256_and_si256(ov, vmask32_lo);

  const __m256i mvmc_lo = _mm256_cmpgt_epi32(mvm_lo, zero); // mover vectorized masked compared lower
  const __m256i ovmc_lo = _mm256_cmpgt_epi32(ovm_lo, zero);

  const __m256i vkm32m_lo = _mm256_and_si256(mvmc_lo, vkm32_lo); // masked
  const __m256i vko32m_lo = _mm256_and_si256(ovmc_lo, vko32_lo);

  __m256i z = _mm256_hadd_epi32(vkm32m_lo, vko32m_lo);
  z = _mm256_hadd_epi32(z, z);
  z = _mm256_hadd_epi32(z, z);

  const uint32_t z0 = _mm256_extract_epi32(z, 0);
  const uint32_t z4 = _mm256_extract_epi32(z, 4);
  const uint32_t index = z0 + z4;

  return index;
}

/*
 * This function is appropriate for patterns having up to 16 squares.
 * When squeres are up to 8 included, there is a more optimized version: board_pattern_packed_to_index8_vec
 */
static board_pattern_index_t
board_pattern_packed_to_index16_vec (board_t *packed)
{
  const __m256i vmask32_hi = _mm256_set_epi32((uint32_t) 0x8000, (uint32_t) 0x4000, (uint32_t) 0x2000, (uint32_t) 0x1000,
                                              (uint32_t) 0x0800, (uint32_t) 0x0400, (uint32_t) 0x0200, (uint32_t) 0x0100);
  const __m256i vmask32_lo = _mm256_set_epi32((uint32_t) 0x0080, (uint32_t) 0x0040, (uint32_t) 0x0020, (uint32_t) 0x0010,
                                              (uint32_t) 0x0008, (uint32_t) 0x0004, (uint32_t) 0x0002, (uint32_t) 0x0001);

  const __m256i vkm32_lo = _mm256_setr_epi32(     1,     3,      9,     27,      81,     243,     729,     2187 );
  const __m256i vkm32_hi = _mm256_setr_epi32(  6561, 19683,  59049, 177147,  531441, 1594323, 4782969, 14348907 );
  const __m256i vko32_lo = _mm256_setr_epi32(     2,     6,     18,     54,     162,     486,    1458,     4374 );
  const __m256i vko32_hi = _mm256_setr_epi32( 13122, 39366, 118098, 354294, 1062882, 3188646, 9565938, 28697814 );

  const __m256i zero = _mm256_set1_epi32(0x00);

  SquareSet *bv = (SquareSet *) packed;
  const SquareSet m = bv[0]; // m: mover
  const SquareSet o = bv[1]; // o: opponent

  const uint32_t m_lo16 = (uint32_t) m & 0x000000000000ffff;
  const uint32_t o_lo16 = (uint32_t) o & 0x000000000000ffff;

  const __m256i mv = _mm256_set1_epi32(m_lo16);
  const __m256i ov = _mm256_set1_epi32(o_lo16);

  const __m256i mvm_lo = _mm256_and_si256(mv, vmask32_lo);
  const __m256i mvm_hi = _mm256_and_si256(mv, vmask32_hi);

  const __m256i ovm_lo = _mm256_and_si256(ov, vmask32_lo);
  const __m256i ovm_hi = _mm256_and_si256(ov, vmask32_hi);

  const __m256i mvmc_lo = _mm256_cmpgt_epi32(mvm_lo, zero); // mover vectorized masked compared lower
  const __m256i mvmc_hi = _mm256_cmpgt_epi32(mvm_hi, zero);
  const __m256i ovmc_lo = _mm256_cmpgt_epi32(ovm_lo, zero);
  const __m256i ovmc_hi = _mm256_cmpgt_epi32(ovm_hi, zero);

  const __m256i vkm32m_lo = _mm256_and_si256(mvmc_lo, vkm32_lo); // masked
  const __m256i vkm32m_hi = _mm256_and_si256(mvmc_hi, vkm32_hi);
  const __m256i vko32m_lo = _mm256_and_si256(ovmc_lo, vko32_lo);
  const __m256i vko32m_hi = _mm256_and_si256(ovmc_hi, vko32_hi);

  const __m256i vkm = _mm256_add_epi32(vkm32m_lo, vkm32m_hi);
  const __m256i vko = _mm256_add_epi32(vko32m_lo, vko32m_hi);

  __m256i z = _mm256_hadd_epi32(vkm, vko);
  z = _mm256_hadd_epi32(z, z);
  z = _mm256_hadd_epi32(z, z);

  const uint32_t z0 = _mm256_extract_epi32(z, 0);
  const uint32_t z4 = _mm256_extract_epi32(z, 4);
  const uint32_t index = z0 + z4;

  return index;
}

/*
 * It has the same result of the function board_pattern_packed_to_index, but it
 * requires AVX2 instructions.
 * It is on average 3x faster.
 */
board_pattern_index_t
board_pattern_packed_to_index_vec (board_t *packed,
                                   unsigned int n_squares)
{
  if (n_squares > 8) return board_pattern_packed_to_index16_vec(packed);
  return board_pattern_packed_to_index8_vec(packed);
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
board_pattern_compute_rotated (const board_t *board,
                               board_pattern_rotated_t *rotated)
{
  assert(board);
  assert(rotated);

#define BOARD_PATTERN_MOVER    0
#define BOARD_PATTERN_OPPONENT 1

  const SquareSet m = board->square_sets[BOARD_PATTERN_MOVER];
  const SquareSet o = board->square_sets[BOARD_PATTERN_OPPONENT];

  const SquareSet xm = board_trans_flip_diag_a1h8(m);
  const SquareSet xo = board_trans_flip_diag_a1h8(o);

  const SquareSet ym = board_trans_flip_vertical(m);
  const SquareSet yo = board_trans_flip_vertical(o);

  const SquareSet um = board_trans_flip_diag_h1a8(m);
  const SquareSet uo = board_trans_flip_diag_h1a8(o);

  const SquareSet zm = board_trans_flip_horizontal(m);
  const SquareSet zo = board_trans_flip_horizontal(o);

  rotated->named_boards.identity.square_sets[BOARD_PATTERN_MOVER]    = m;
  rotated->named_boards.identity.square_sets[BOARD_PATTERN_OPPONENT] = o;

  /* rotate_90a = flip_diag_h1a8(flip_horizontal) */
  rotated->named_boards.rot_90a.square_sets[BOARD_PATTERN_MOVER]    = board_trans_flip_diag_h1a8(zm);
  rotated->named_boards.rot_90a.square_sets[BOARD_PATTERN_OPPONENT] = board_trans_flip_diag_h1a8(zo);

  /* rotate_180 = flip_vertical(flip_horizontal */
  rotated->named_boards.rot_180.square_sets[BOARD_PATTERN_MOVER]    = board_trans_flip_vertical(zm);
  rotated->named_boards.rot_180.square_sets[BOARD_PATTERN_OPPONENT] = board_trans_flip_vertical(zo);

  /* rotate_90c = flip_horizontal(flip_diag_h1a8 */
  rotated->named_boards.rot_90c.square_sets[BOARD_PATTERN_MOVER]    = board_trans_flip_horizontal(um);
  rotated->named_boards.rot_90c.square_sets[BOARD_PATTERN_OPPONENT] = board_trans_flip_horizontal(uo);

  rotated->named_boards.flip_ve.square_sets[BOARD_PATTERN_MOVER]    = ym;
  rotated->named_boards.flip_ve.square_sets[BOARD_PATTERN_OPPONENT] = yo;

  rotated->named_boards.flip_dh.square_sets[BOARD_PATTERN_MOVER]    = um;
  rotated->named_boards.flip_dh.square_sets[BOARD_PATTERN_OPPONENT] = uo;

  rotated->named_boards.flip_ho.square_sets[BOARD_PATTERN_MOVER]    = zm;
  rotated->named_boards.flip_ho.square_sets[BOARD_PATTERN_OPPONENT] = zo;

  rotated->named_boards.flip_da.square_sets[BOARD_PATTERN_MOVER]    = xm;
  rotated->named_boards.flip_da.square_sets[BOARD_PATTERN_OPPONENT] = xo;

#undef BOARD_PATTERN_MOVER
#undef BOARD_PATTERN_OPPONENT
}

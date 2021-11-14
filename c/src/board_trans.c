/**
 * @file
 *
 * @brief Board transformation utilities.
 *
 * @par board_trans.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018, 2021 Roberto Corradini. All rights reserved.
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
#if !defined NDEBUG
#define NDEBUG
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <immintrin.h>

#include "board_trans.h"

void
fprintf_hex_128i_epi64 (FILE *f,
                        __m128i var)
{
  uint64_t val[2];
  memcpy(val, &var, sizeof(val));
  fprintf(f, "0x%016lx 0x%016lx", val[0], val[1]);
}

SquareSet
board_trans_flip_horizontal (SquareSet s)
{
  return
    ((s << 56)                     ) |
    ((s << 40) & 0x00ff000000000000) |
    ((s << 24) & 0x0000ff0000000000) |
    ((s <<  8) & 0x000000ff00000000) |
    ((s >>  8) & 0x00000000ff000000) |
    ((s >> 24) & 0x0000000000ff0000) |
    ((s >> 40) & 0x000000000000ff00) |
    ((s >> 56) );
}

void
board_trans_flip_horizontal_vec (SquareSet *b,
                                 SquareSet *r)
{
  __m128i x = _mm_loadu_si128((__m128i *) b);

  x =
    _mm_slli_epi64(x, 56) |
    _mm_and_si128(_mm_slli_epi64(x, 40), _mm_set1_epi64x(0x00ff000000000000)) |
    _mm_and_si128(_mm_slli_epi64(x, 24), _mm_set1_epi64x(0x0000ff0000000000)) |
    _mm_and_si128(_mm_slli_epi64(x,  8), _mm_set1_epi64x(0x000000ff00000000)) |
    _mm_and_si128(_mm_srli_epi64(x,  8), _mm_set1_epi64x(0x00000000ff000000)) |
    _mm_and_si128(_mm_srli_epi64(x, 24), _mm_set1_epi64x(0x0000000000ff0000)) |
    _mm_and_si128(_mm_srli_epi64(x, 40), _mm_set1_epi64x(0x000000000000ff00)) |
    _mm_srli_epi64(x, 56);

  _mm_store_si128((__m128i *) r, x);
}

SquareSet
board_trans_flip_vertical (SquareSet s)
{
  const SquareSet k1 = 0x5555555555555555;
  const SquareSet k2 = 0x3333333333333333;
  const SquareSet k4 = 0x0f0f0f0f0f0f0f0f;
  s = ((s >> 1) & k1) | ((s & k1) << 1);
  s = ((s >> 2) & k2) | ((s & k2) << 2);
  s = ((s >> 4) & k4) | ((s & k4) << 4);
  return s;
}

void
board_trans_flip_vertical_vec (SquareSet *b,
                               SquareSet *r)
{
  __m128i x = _mm_loadu_si128((__m128i *) b);

  const __m128i k1 = _mm_set1_epi64x(0x5555555555555555);
  const __m128i k2 = _mm_set1_epi64x(0x3333333333333333);
  const __m128i k4 = _mm_set1_epi64x(0x0f0f0f0f0f0f0f0f);

  x = (_mm_srli_epi64(x, 1) & k1) | _mm_slli_epi64((x & k1), 1);
  x = (_mm_srli_epi64(x, 2) & k2) | _mm_slli_epi64((x & k2), 2);
  x = (_mm_srli_epi64(x, 4) & k4) | _mm_slli_epi64((x & k4), 4);

  _mm_store_si128((__m128i *) r, x);
}

SquareSet
board_trans_flip_diag_a1h8 (SquareSet s)
{
  SquareSet t;
  const SquareSet k1 = 0x5500550055005500;
  const SquareSet k2 = 0x3333000033330000;
  const SquareSet k4 = 0x0f0f0f0f00000000;
  t  = k4 & (s ^ (s << 28));
  s ^=       t ^ (t >> 28) ;
  t  = k2 & (s ^ (s << 14));
  s ^=       t ^ (t >> 14) ;
  t  = k1 & (s ^ (s <<  7));
  s ^=       t ^ (t >>  7) ;
  return s;
}

void
board_trans_flip_diag_a1h8_vec (SquareSet *b,
                                SquareSet *r)
{
  __m128i x = _mm_loadu_si128((__m128i *) b);
  __m128i t;

  const __m128i k1 = _mm_set1_epi64x(0x5500550055005500);
  const __m128i k2 = _mm_set1_epi64x(0x3333000033330000);
  const __m128i k4 = _mm_set1_epi64x(0x0f0f0f0f00000000);

  t  = k4 & (x ^ _mm_slli_epi64(x, 28));
  x ^=       t ^ _mm_srli_epi64(t, 28) ;
  t  = k2 & (x ^ _mm_slli_epi64(x, 14));
  x ^=       t ^ _mm_srli_epi64(t, 14) ;
  t  = k1 & (x ^ _mm_slli_epi64(x,  7));
  x ^=       t ^ _mm_srli_epi64(t,  7) ;

  _mm_store_si128((__m128i *) r, x);
}

SquareSet
board_trans_flip_diag_h1a8 (SquareSet s)
{
  SquareSet t;
  const SquareSet k1 = 0xaa00aa00aa00aa00;
  const SquareSet k2 = 0xcccc0000cccc0000;
  const SquareSet k4 = 0xf0f0f0f00f0f0f0f;
  t  =       s ^ (s << 36) ;
  s ^= k4 & (t ^ (s >> 36));
  t  = k2 & (s ^ (s << 18));
  s ^=       t ^ (t >> 18) ;
  t  = k1 & (s ^ (s <<  9));
  s ^=       t ^ (t >>  9) ;
  return s;
}

void
board_trans_flip_diag_h1a8_vec (SquareSet *b,
                                SquareSet *r)
{
  __m128i x = _mm_loadu_si128((__m128i *) b);
  __m128i t;

  const __m128i k1 = _mm_set1_epi64x(0xaa00aa00aa00aa00);
  const __m128i k2 = _mm_set1_epi64x(0xcccc0000cccc0000);
  const __m128i k4 = _mm_set1_epi64x(0xf0f0f0f00f0f0f0f);

  t  =       x ^ _mm_slli_epi64(x, 36) ;
  x ^= k4 & (t ^ _mm_srli_epi64(x, 36));
  t  = k2 & (x ^ _mm_slli_epi64(x, 18));
  x ^=       t ^ _mm_srli_epi64(t, 18) ;
  t  = k1 & (x ^ _mm_slli_epi64(x,  9));
  x ^=       t ^ _mm_srli_epi64(t,  9) ;

  _mm_store_si128((__m128i *) r, x);
}

SquareSet
board_trans_rotate_180 (SquareSet s)
{
  return board_trans_flip_vertical(board_trans_flip_horizontal(s));
}

SquareSet
board_trans_rotate_90c (SquareSet s)
{
  return board_trans_flip_horizontal(board_trans_flip_diag_h1a8(s));
}

SquareSet
board_trans_rotate_90a (SquareSet s)
{
  return board_trans_flip_diag_h1a8(board_trans_flip_horizontal(s));
}

SquareSet
board_trans_identity (SquareSet s)
{
  return s;
}

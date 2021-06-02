/**
 * @file
 *
 * @brief Transposition Table module definitions.
 * @details This module defines ... .
 *
 * @par transposition_table.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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

#ifndef TRANSPOSITION_TABLE_H
#define TRANSPOSITION_TABLE_H

#include <inttypes.h>

#include "board.h"

/*
 *
 */
typedef struct tratab_data_s {
  uint8_t  depth;                /**< @brief aa. */
  int8_t   lower_bound;          /**< @brief aa. */
  int8_t   upper_bound;          /**< @brief aa. */
  int8_t   best_move;            /**< @brief aa. */
} tratab_data_t;

/*
 *
 */
typedef struct tratab_item_s {
  uint64_t      hash;            /**< @brief Zobrist hash key. */
  GamePositionX gpx;             /**< @brief aaa. */
  tratab_data_t data;            /**< @brief aaa. */
} tratab_item_t;

/*
 *
 */
typedef struct tratab_table_s {
  size_t         size;            /**< @brief aaa. */
  size_t         item_size;       /**< @brief aaa. */
  size_t         max_n_item;      /**< @brief aaa. */
  size_t         n_item;          /**< @brief aaa. */
  size_t         allocated_size;  /**< @brief aaa. */
  uint8_t        mask_size;       /**< @brief aaa. */
  uint64_t       mask;            /**< @brief aaa. */
  void          *memory;          /**< @brief aaa. */
  size_t         n_slot_touched;  /**< @brief aaa. */
  size_t         n_update_bound;  /**< @brief aaa. */
  size_t         n_update_depth;  /**< @brief aaa. */
  size_t         n_override;      /**< @brief aaa. */
  size_t         n_conflict;      /**< @brief aaa. */
  size_t         n_retrieve;      /**< @brief aaa. */
} tratab_table_t;

extern tratab_table_t *
tratab_table_create (size_t size);

extern void
tratab_table_destroy (tratab_table_t *table);

extern void
tratab_table_init (tratab_table_t *table);

extern void
tratab_insert_item (tratab_table_t *table,
                    uint64_t hash,
                    GamePositionX *gpx,
                    int depth,
                    int lower_bound,
                    int upper_bound,
                    Square best_move);

extern void
tratab_table_header_to_stream (tratab_table_t *table,
                               FILE *file);

extern tratab_item_t *
tratab_item_retrieve (tratab_table_t *table,
                      uint64_t hash,
                      GamePositionX *gpx,
                      int depth);

/* ### ### ### ### */


#define T ttab_t
#define I ttab_item_t

typedef struct ttab_s *T;
typedef struct ttab_item_s *I;

extern T
ttab_new (int log_size);

extern void
ttab_free (T *tp);

extern void
ttab_init (T t);

extern void
ttab_insert (T t,
             I i);

extern void
ttab_retrieve (T t,
               I i);

extern void
ttab_header_to_stream (T t,
                       FILE *file);


#endif /* TRANSPOSITION_TABLE_H */

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

#define T ttab_t
#define I ttab_item_t

#define TTAB_RECORDED_BEST_MOVE_COUNT 16

typedef struct ttab_s *ttab_t;
typedef struct ttab_item_s *ttab_item_t;

struct ttab_item_s {
  uint64_t hash;                 /**< @brief Item's hash. */
  uint8_t  depth;                /**< @brief Number of ply used for the analysis counting from this node ut to the leafs. */
  int8_t   lower_bound;          /**< @brief Minimum value in the windows of possible ones. */
  int8_t   upper_bound;          /**< @brief Maximum value in the windows of possible ones. */
  int8_t   best_move;            /**< @brief Best move found. */
  int      pq_index;             /**< @brief the index used to retrieve the priority queue item. */
  /* --- --- */
  uint64_t legal_move_set;
  int8_t legal_move_count;
  int8_t best_moves[TTAB_RECORDED_BEST_MOVE_COUNT];
  int8_t move_values[TTAB_RECORDED_BEST_MOVE_COUNT];
};

extern void
ttab_item_clone_data (I from,
                      I to);

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
               I *i);

extern void
ttab_summary_to_stream (T t,
                        FILE *file);

extern void
ttab_table_to_stream (T t,
                      FILE *file);

extern void
ttab_bucket_filling_stats (T t,
                           size_t *stats,
                           size_t stats_size);

extern void
ttab_stats_to_stream (T t,
                      FILE *file);

#undef T
#undef I

#endif /* TRANSPOSITION_TABLE_H */

/**
 * @file
 *
 * @brief Transposition table module.
 * @details A transposition table is a database that stores results of previously performed searches.
 *
 * @par transposition_table.c
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

#include <stdio.h>
#include <stdlib.h>
//#include <stdbool.h>
#include <assert.h>
//#include <math.h>

#include "transposition_table.h"

/*
 * Usefull links .....
 *
 * https://people.csail.mit.edu/plaat/mtdf.html
 *
 * https://www.chessprogramming.org/Transposition_Table
 *
 * https://en.wikipedia.org/wiki/Transposition_table
 *
 * http://mediocrechess.blogspot.com/2007/01/guide-transposition-tables.html
 *
 * https://github.com/abulmo/edax-reversi/blob/master/src/hash.h
 * https://github.com/abulmo/edax-reversi/blob/master/src/hash.c
 */

tratab_table_t *
tratab_table_create (size_t size)
{
  tratab_table_t *table = NULL;

  const size_t item_size = sizeof(tratab_item_t);
  const size_t max_n_item = size / item_size;
  const size_t n_item = bitw_highest_set_bit_64(max_n_item);
  const size_t allocated_size = n_item * item_size;
  const uint8_t mask_size = 64 - bitw_lzcnt_64(n_item);
  const uint64_t mask = n_item - 1;

  table = (tratab_table_t *) malloc(sizeof(tratab_table_t));
  if (!table) return NULL;

  table->memory = malloc(allocated_size);
  if (!table->memory) {
    free(table);
    return NULL;
  }

  table->size = size;
  table->item_size = item_size;
  table->max_n_item = max_n_item;
  table->n_item = n_item;
  table->allocated_size = allocated_size;
  table->mask_size = mask_size;
  table->mask = mask;

  return table;
}

void
tratab_table_destroy (tratab_table_t *table)
{
  if (!table) return;
  if (table->memory) free(table->memory);
  free(table);
}

void
tratab_table_init (tratab_table_t *table)
{
  if (!table) return;
  assert(table->memory);

  tratab_item_t *items = (tratab_item_t *) table->memory;

  for (size_t i = 0; i < table->n_item; i++) {
    tratab_item_t *item = &items[i];
    item->hash = 0;
    item->gpx.blacks = 0;
    item->gpx.whites = 0;
    item->gpx.player = BLACK_PLAYER;
    item->data.depth = -1;
    item->data.lower_bound = -66;
    item->data.upper_bound = +66;
    item->data.best_move = invalid_move;
  }

  table->n_slot_touched = 0;
  table->n_update_bound = 0;
  table->n_update_depth = 0;
  table->n_override = 0;
  table->n_conflict = 0;
  table->n_retrieve = 0;
}

void
tratab_insert_item (tratab_table_t *table,
                    uint64_t hash,
                    GamePositionX *gpx,
                    int depth,
                    int lower_bound,
                    int upper_bound,
                    Square best_move)
{
  assert(table);
  assert(table->memory);
  const uint64_t hash_check = game_position_x_hash(gpx);
  if (hash != hash_check) {
    fprintf(stderr, "Error: hash mismatch\n");
    abort();
  }

  tratab_item_t *const items = (tratab_item_t *) table->memory;

  const size_t index = hash & table->mask;

  tratab_item_t *const item = &items[index];

  if (item->hash == 0) {
    table->n_slot_touched++;
  } else if (item->hash == hash) {
    const int comp = game_position_x_compare(gpx, &item->gpx);
    if (comp != 0 ) {
      table->n_conflict++;
    } else {
      if (item->data.depth == depth) {
        table->n_update_bound++;
      } else if (item->data.depth < depth) {
        table->n_update_depth++;
      } else {
        return;
      }
    }
  } else {
    table->n_override++;
  }

  /* Updates item. */
  item->hash = hash;
  item->gpx.blacks = gpx->blacks;
  item->gpx.whites = gpx->whites;
  item->gpx.player = gpx->player;
  item->data.depth = depth;
  item->data.lower_bound = lower_bound;
  item->data.upper_bound = upper_bound;
  item->data.best_move = best_move;
}

void
tratab_table_header_to_stream (tratab_table_t *table,
                               FILE *file)
{
  assert(table);

  fprintf(file, "size           = %zu\n", table->size);
  fprintf(file, "item_size      = %zu\n", table->item_size);
  fprintf(file, "max_n_item     = %zu\n", table->max_n_item);
  fprintf(file, "n_item         = %zu\n", table->n_item);
  fprintf(file, "allocated_size = %zu\n", table->allocated_size);
  fprintf(file, "mask_size      = %u\n",  table->mask_size);
  fprintf(file, "mask           = %zu\n", table->mask);
  fprintf(file, "n_slot_touched = %zu\n", table->n_slot_touched);
  fprintf(file, "n_update_bound = %zu\n", table->n_update_bound);
  fprintf(file, "n_update_depth = %zu\n", table->n_update_depth);
  fprintf(file, "n_override     = %zu\n", table->n_override);
  fprintf(file, "n_conflict     = %zu\n", table->n_conflict);
  fprintf(file, "n_retrieve     = %zu\n", table->n_retrieve);
}

tratab_item_t *
tratab_item_retrieve (tratab_table_t *table,
                      uint64_t hash,
                      GamePositionX *gpx,
                      int depth)
{
  assert(table);
  assert(table->memory);

  tratab_item_t *item = NULL;

  tratab_item_t *const items = (tratab_item_t *) table->memory;
  const size_t index = hash & table->mask;
  tratab_item_t *const item_with_matching_index = &items[index];

  if (item_with_matching_index->hash == hash && item_with_matching_index->data.depth >= depth) {
    item = item_with_matching_index;
    table->n_retrieve++;
  }

  return item;
}

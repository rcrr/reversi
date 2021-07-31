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
#include <string.h>
#include <assert.h>

#include "hashtable.h"
#include "binary_heap.h"
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


/* ### ### ### ### */

#define T ttab_t
#define I ttab_item_t

#define TTAB_HT_SIZE_ITEM_RATIO 4
#define TTAB_HT_MIN_SIZE 4

struct ttab_s {
  size_t max_n_item;             /**< @brief Number of items that could be stored in the table. */
  I items;                       /**< @brief Array of items. */
  I next_item;                   /**< @brief Next item to be used when inserting. */
  size_t n_item;                 /**< @brief The number of item currently held in the table. */
  htab_t ht;                     /**< @brief Hashtable containing <Key:item , Item:item>. */
  bihp_pq_t pq;                  /**< @brief Priority queue. */
};

/* bihp_item_call_back_on_swap_f */
static void
icbf (item_t i,
      int index)
{
  ((I) i)->pq_index = index;
}

static void
reset_item (I i)
{
  i->hash = 0ULL;
  i->depth = -1;
  i->lower_bound = -66;
  i->upper_bound = +66;
  i->best_move = invalid_move;
  i->pq_index = -1;
}

static I
new_item (T t)
{
  I i, e;
  //I o = bihp_pq_peek(t->pq);
  //if (o) printf("o->pq_index = %d\n", o->pq_index);
  if (t->n_item == t->max_n_item) {
    i = bihp_pq_pull(t->pq);
    if (false ) printf("new_item: substitution, removed item is i->hash = %zu, i->depth = %u, i->pq_index = %d\n", i->hash, i->depth, i->pq_index);
    e = htab_remove(t->ht, i);
    assert(i == e);
    // reset item, it is not required but is clean.
    reset_item(i);
  } else {
    if (false) printf("new_item: insertion\n");
    i = t->next_item--;
    t->n_item++;
  }
  return i;
}

static void
copy (const void *origin,
      const void *dest)
{
  I o = (I) origin;
  I d = (I) dest;
  *d = *o;
}

static int
cmp (const void *x,
     const void *y)
{
  I ix = (I) x;
  I iy = (I) y;
  return ix->hash == iy->hash ? 0 : 1;
}

static unsigned
hash (const void *key)
{
  I item = (I) key;
  return item->hash;
}

/*
 * Add the age of the record in the priority.
 */
static int
priority (item_t a)
{
  I item = (I) a;
  return item->depth;
}

static void
print_item (item_t a,
            FILE *f)
{
  /* Eventually to be completed. */
  return;
}



T
ttab_new (int log_size)
{
  T t;
  int ht_hint;

  assert(log_size < 8 * sizeof(size_t));

  /* Allocating the table header. */
  t = (T) malloc(sizeof(*t));
  if (!t) return NULL;

  /* Computing the table size (max number of items). */
  if (log_size < 0)
    t->max_n_item = 0;
  else {
    t->max_n_item = 1ULL << log_size;
  }

  /* Allocating the items array. */
  t->items = (I) malloc(t->max_n_item * sizeof(struct ttab_item_s));
  if (!t->items) {
    free(t);
    return NULL;
  }

  /* Create the hew hashtable. */
  ht_hint = t->max_n_item / TTAB_HT_SIZE_ITEM_RATIO;
  if (ht_hint < TTAB_HT_MIN_SIZE ) ht_hint = TTAB_HT_MIN_SIZE;
  t->ht = htab_new(ht_hint, cmp, hash);
  if (!t->ht) {
    free(t->items);
    free(t);
    return NULL;
  }
  //printf("ht_hint = %d\n", ht_hint);
  //printf("t->max_n_item = %zu\n", t->max_n_item);

  /* Crete the min priority queue. */
  t->pq = bihp_pq_create(BIHP_PQ_TYPE_MIN, t->max_n_item, priority, print_item, icbf);
  if (!t->pq) {
    htab_free(&t->ht);
    free(t->items);
    free(t);
    return NULL;
  }

  ttab_init(t);

  return t;
}

void
ttab_free (T *tp)
{
  assert(tp && *tp);

  T t = *tp;

  if (t->max_n_item > 0) {
    assert(t->pq);
    bihp_pq_destroy(t->pq);
    assert(t->ht);
    htab_free(&t->ht);
    assert(t->items);
    free(t->items);
  }

  free(t);
  *tp = NULL;
}

void
ttab_init (T t)
{
  assert(t);
  assert(t->items);
  assert(t->ht);
  assert(t->pq);

  /* Initialize next_item to be the on top of the array. */
  t->next_item = t->items + t->max_n_item - 1;

  /* Initialize n_item to be zero. */
  t->n_item = 0;

  /* Zeroes the items array. */
  memset(t->items, 0, t->max_n_item * sizeof(struct ttab_item_s));
}

void
ttab_insert (T t,
             I i)
{
  assert(t);
  if (!i) return;

  I e = htab_get(t->ht, i);
  if (e) {
    printf("updating ... \n");
    bihp_pq_update_priority(t->pq, i->pq_index);
    copy(i, e);
  } else {
    e = new_item(t);
    copy(i, e);
    htab_put(t->ht, e, e);
    bihp_pq_insert(t->pq, e);
  }
}

void
ttab_retrieve (T t,
               I *i)
{
  assert(t);
  assert(i);
  if (!*i) return;

  I e = htab_get(t->ht, *i);
  if (e) {
    copy(e, *i);
  } else
    *i = NULL;
}

void
ttab_summary_to_stream (T t,
                        FILE *file)
{
  if (!file) return;
  if (!t) fprintf(file, "Transposition Table pointer is null.\n");

  fprintf(file, "Transposition Table pointer: %p\n", (void *) t);
  fprintf(file, "  max_n_item: %20zu  -  Number of items that could be stored in the table\n", t->max_n_item);
  fprintf(file, "  items:      %20p  -  Array of items\n", (void *) t->items);
  fprintf(file, "  next_item:  %20p  -  Next item to be used when inserting\n", (void *) t->next_item);
  fprintf(file, "  n_item:     %20zu  -  The number of item currently held in the table\n", t->n_item);
  fprintf(file, "  ht:         %20p  -  Hashtable containing <Key:item , Item:item>\n", (void *) t->ht);
  fprintf(file, "  pq:         %20p  -  Priority queue\n", (void *) t->ht);
}

static void
htab_item_to_stream (const void *key,
                     void **value,
                     void *cl)
{
  assert(key);
  assert(value);
  assert(*value);
  assert(cl);

  I i = (I) *value;
  FILE *f = (FILE *) cl;

  fprintf(f, "%20zu;  %4u;        %4d;        %4d;      %4d;%12d\n",
          i->hash, i->depth, i->lower_bound, i->upper_bound, i->best_move, i->pq_index);
}


void
ttab_table_to_stream (T t,
                      FILE *file)
{
  assert(t);

  if (!file) return;

  fprintf(file, "                HASH; DEPTH; LOWER_BOUND; UPPER_BOUND; BEST_MOVE;    PQ_INDEX\n");
  htab_map(t->ht, htab_item_to_stream, file);
}

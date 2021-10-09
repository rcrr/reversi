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
  if (t->n_item == t->max_n_item) {
    i = bihp_pq_pull(t->pq);
    e = htab_remove(t->ht, i);
    (void) e; assert(i == e);
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

void
ttab_item_clone_data (I from,
                      I to)
{
  assert(from);
  assert(to);

  to->hash = from->hash;
  to->depth = from->depth;
  to->lower_bound = from->lower_bound;
  to->upper_bound =from->upper_bound;
  to->best_move = from->best_move;
  to->pq_index = from->pq_index;
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

  /*
   * Zeroes the items array. It is not required, but it is an help in case of debugging.
   * It is turned off being a sensible time consumer.
   */
  if (false) memset(t->items, 0, t->max_n_item * sizeof(struct ttab_item_s));
}

void
ttab_insert (T t,
             I i)
{
  assert(t);
  if (!i) return;

  I e = htab_get(t->ht, i);
  if (e) {
    if (false) printf("updating ... i->pq_index = %d, e->pq_index = %d\n", i->pq_index, e->pq_index);
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
  fprintf(file, "  ht.size:    %20zu  -  Hashtable size\n", htab_size(t->ht));
  fprintf(file, "  ht.length:  %20zu  -  Hashtable length\n", htab_length(t->ht));
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

void
ttab_bucket_filling_stats (T t,
                           size_t *stats,
                           size_t stats_size)
{
  assert(t);

  htab_bucket_filling_stats(t->ht, stats, stats_size);
}

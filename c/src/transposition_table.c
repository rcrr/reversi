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
 * @copyright 2021, 2022 Roberto Corradini. All rights reserved.
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
#include <limits.h>
#include <assert.h>

#include "binary_heap.h"
#include "transposition_table.h"



/* Hashtable inteface: BEGIN. */

typedef void * item_t;

#define HT htab_t

typedef struct htab_s *HT;

static HT
htab_new (int hint,
          int cmp (const void *x, const void *y),
          unsigned hash (const void *key),
          size_t max_n_item);

static void
htab_reinit (HT *t);

static void
htab_free (HT *t);

static size_t
htab_length (HT t);

static size_t
htab_size (HT t);

static void *
htab_put (HT t,
          const void *key,
          void *value);

static void *
htab_get (HT t,
          const void *key);

static void *
htab_remove (HT t,
             const void *key);

static void
htab_map (HT t,
          void apply(const void *key, void **value, void *cl),
          void *cl);

static void
htab_bucket_filling_stats (HT t,
                           size_t *stats,
                           size_t stats_size);

#undef HT

/* Hashtable inteface: END. */

/* Hashtable implementation: BEGIN. */

#define HT htab_t

typedef struct htab_s *HT;

/*
 * The table has a fixed size.
 * There is no malloc/free happening during put/remove actions.
 *
 * TODO:
 * Move all int to size_t ...
 * Remove timestamp
 * Remove KEY and hash function
 * The binding structure and the ttab_item are a duplication ...
 * Do we need compare ?
 * void * value could be moved to ttab_item_t ...
 */
struct htab_s {
  int size;                                   /* The number of buckets. */
  int (*cmp)(const void *x, const void *y);   /* Compare function. */
  unsigned (*hash)(const void *key);          /* Hash function. */
  int length;                                 /* The actual number of item held in the table. */
  unsigned timestamp;                         /* Used to avoid changes during the map function execution. */
  struct binding {
    struct binding *link;
    const void *key;
    void *value;
  } **buckets;                                /* Hashtable array of pointers to the binding structure. */
  size_t max_n_item;                          /* Maximum number of items. */
  struct binding *bindings;                   /* Array of binding objects. */
  struct binding **binding_inventory;         /* Array of pointers to binding objects. */
};

HT
htab_new (int hint,
          int cmp (const void *x, const void *y),
          unsigned hash (const void *key),
          size_t max_n_item)
{
  HT table;
  int i;

  static const int primes[] = { 1, 1, 3, 7, 13, 31, 61, 113, 251, 509, 1021, 2039, 4093,
                                8191, 16381, 32749, 65521, 131071, 262139, 524287, 1048573,
                                2097143, 4194301, 8388593, 16777213, 33554393, 67108859,
                                134217689, 268435399, 536870909, 1073741789, 2147483647,
                                INT_MAX };
  /*
   * INT_MAX = 2,147,483,647
   *
   * Prog. |  Power of 2 | Prime just less | diff.
   *_______________________________________________
   *
   * 00               1                 1        0
   * 01               2                 1        1
   * 02               4                 3        1
   * 03               8                 7        1
   * 04              16                13        3
   * 05              32                31        1
   * 06              64                61        3
   * 07             128               113       15
   * 08             256               251        5
   * 09             512               509        3
   * 10           1,024             1,021        3
   * 11           2,048             2,039        9
   * 12           4,096             4,093        3
   * 13           8,192             8,191        1
   * 14          16,384            16,381        3
   * 15          32,768            32,749       19
   * 16          65,536            65,521       15
   * 17         131,072           131,071        1
   * 18         262,144           262,139        5
   * 19         524,288           524,287        1
   * 20       1,048,576         1,048,573        3
   * 21       2,097,152         2,097,143        9
   * 22       4,194,304         4,194,301        3
   * 23       8,388,608         8,388,593       15
   * 24      16,777,216        16,777,213        3
   * 25      33,554,432        33,554,393       39
   * 26      67,108,864        67,108,859        5
   * 27     134,217,728       134,217,689       39
   * 28     268,435,456       268,435,399       57
   * 29     536,870,912       536,870,909        3
   * 30   1,073,741,824     1,073,741,789       35
   * 31   2,147,483,648     2,147,483,647        1
   * 32   4,294,967,296     4,294,967,291        5
   * 33   8,589,934,592     8,589,934,583        9
   * 34  17,179,869,184    17,179,869,143       41
   *
   */


  assert(hint >= 0);
  assert(cmp);
  assert(hash);

  for (i = 1; primes[i] < hint; i++)
    ;

  table = malloc(sizeof(*table));
  if (!table) return NULL;
  table->size = primes[i - 1];
  table->buckets = malloc(primes[i - 1] * sizeof(table->buckets[0]));
  if (!table->buckets) {
    free(table);
    return NULL;
  }

  table->max_n_item = max_n_item;
  table->bindings = (struct binding *) malloc(max_n_item * sizeof(struct binding));
  if (!table->bindings) {
    free(table->buckets);
    free(table);
    return NULL;
  }
  table->binding_inventory = (struct binding **) malloc(max_n_item * sizeof(struct binding *));
  if (!table->binding_inventory) {
    free(table->bindings);
    free(table->buckets);
    free(table);
    return NULL;
  }
  for (size_t i = 0; i < max_n_item; i++) {
    table->binding_inventory[i] = &table->bindings[i];
  }

  /*
    This code has been substitueded by the call to memset.
    The change reduced the LLd writes detected by valgrind/cachegrind.

    for (i = 0; i < table->size; i++)
      table->buckets[i] = NULL;
  */
  memset(table->buckets, 0, table->size * sizeof(table->buckets[0]));
  table->cmp = cmp;
  table->hash = hash;
  table->length = 0;
  table->timestamp = 0;

  return table;
}

void
htab_reinit (HT *t)
{
  assert(t && *t);

  for (size_t i = 0; i < (*t)->max_n_item; i++) {
    (*t)->binding_inventory[i] = &(*t)->bindings[i];
  }

  memset((*t)->buckets, 0, (*t)->size * sizeof((*t)->buckets[0]));
  (*t)->length = 0;
  (*t)->timestamp = 0;
}

void
htab_free (HT *t)
{
  assert(t && *t);

  free((*t)->bindings);
  free((*t)->binding_inventory);
  free((*t)->buckets);
  free(*t);
  *t = NULL;
}

size_t
htab_length (HT t)
{
  assert(t);
  return t->length;
}

size_t
htab_size (HT t)
{
  assert(t);
  return t->size;
}

void *
htab_put (HT t,
          const void *key,
          void *value)
{
  int i;
  struct binding *p;
  void *prev;

  assert(t);
  assert(key);

  /* Search table for key. */
  i = (*t->hash)(key) % t->size;
  if (false) printf("hashtable index = %d\n", i);
  for (p = t->buckets[i]; p; p = p->link)
    if ((*t->cmp)(key, p->key) == 0)
      break;

  if (p == NULL) {
    p = t->binding_inventory[t->length];
    p->key = key;
    p->link = t->buckets[i];
    t->buckets[i] = p;
    t->length++;
    prev = NULL;
  } else {
    prev = p->value;
  }
  p->value = value;
  t->timestamp++;

  return prev;
}

void *
htab_get (HT t,
          const void *key)
{
  int i;
  struct binding *p;

  assert(t);
  assert(key);

  /* Search table for key. */
  i = (*t->hash)(key) % t->size;
  for (p = t->buckets[i]; p; p = p->link) {
    if ((*t->cmp)(key, p->key) == 0)
      break;
  }

  return p ? p->value : NULL;
}

void *
htab_remove (HT t,
             const void *key)
{
  int i;
  struct binding **pp;

  assert(t);
  assert(key);
  t->timestamp++;

  i = (*t->hash) (key) % t->size;
  for (pp = &t->buckets[i]; *pp; pp = &(*pp)->link) {
    if ((*t->cmp)(key, (*pp)->key) == 0) {
      struct binding *p = *pp;
      void *value = p->value;
      *pp = p->link;
      t->length--;
      t->binding_inventory[t->length] = p;
      return value;
    }
  }
  return NULL;
}

void
htab_map (HT t,
          void apply (const void *key, void **value, void *cl),
          void *cl)
{
  int i;
  unsigned stamp;
  struct binding *p;

  assert(t);
  assert(apply);

  stamp = t->timestamp;
  (void) stamp;
  for (i = 0; i < t->size; i++)
    for (p = t->buckets[i]; p; p = p->link) {
      apply(p->key, &p->value, cl);
      assert(t->timestamp == stamp);
    }
}

void
htab_bucket_filling_stats (HT t,
                           size_t *stats,
                           size_t stats_size)
{
  assert(t);
  if (stats_size > 0) assert(stats);

  for (size_t i = 0; i < stats_size; i++) stats[i] = 0;

  for (size_t i = 0; i < t->size; i++) {
    struct binding *b = t->buckets[i];
    size_t bc = 0;
    while (b) {
      bc++;
      b = b->link;
    }
    if (bc > stats_size - 1) bc = stats_size - 1;
    stats[bc]++;
  }
}

#undef HT

/* Hashtable implementation: END. */



/*
 * Transposition Table implementation.
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
  if (log_size < 0) {
    free(t);
    return NULL;
  } else {
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
  t->ht = htab_new(ht_hint, cmp, hash, t->max_n_item);
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

  /* Initialize next_item to be the on top of the array. */
  t->next_item = t->items + t->max_n_item - 1;

  /* Initialize n_item to be zero. */
  t->n_item = 0;

  /*
   * Zeroes the items array. It is not required, but it is an help in case of debugging.
   * It is turned off being a sensible time consumer.
   */
  if (false) memset(t->items, 0, t->max_n_item * sizeof(struct ttab_item_s));

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
ttab_reinit (T t)
{
  assert(t);
  assert(t->items);
  assert(t->ht);
  assert(t->pq);

  /* Re-init the priority-queue. */
  bihp_pq_reinit(t->pq);

  /* Re-init the hashtable. */
  htab_reinit(&t->ht);

  /* Re-initialize next_item to be the on top of the array. */
  t->next_item = t->items + t->max_n_item - 1;

  /* Re-initialize n_item to be zero. */
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
  fprintf(file, "  pq:         %20p  -  Priority queue\n", (void *) t->pq);
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

void
ttab_stats_to_stream (T t,
                      FILE *file)
{
    const size_t stats_max_size = 64;
    size_t stats_size;
    size_t stats[stats_max_size];
    ttab_summary_to_stream(t, file);
    ttab_bucket_filling_stats(t, stats, stats_max_size);
    for (stats_size = stats_max_size -1; stats_size > 0; stats_size--)
      if (stats[stats_size] > 0) break;
    fprintf(file, "TT hashtable stats:\n");
    fprintf(file, " ELEMENTS_X_BUCKET; ELEMENT_CNT\n");
    for (size_t i = 0; i < stats_size; i++) {
      fprintf(file, "%18zu;%12zu\n", i, stats[i]);
    }
}

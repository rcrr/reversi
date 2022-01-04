/**
 * @file
 *
 * @brief Hashtable module implementation.
 *
 * @par hashtable.c
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

/*
 * Comment this line to enable assertion in the module.
 * The line must be inserted before the inclusion of <assert.h>
 */
#if !defined NDEBUG
#define NDEBUG
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <string.h>

#include "hashtable.h"

#define T htab_t

typedef struct htab_s *T;

struct htab_s {
  int size;
  int (*cmp)(const void *x, const void *y);
  unsigned (*hash)(const void *key);
  int length;
  unsigned timestamp;
  struct binding {
    struct binding *link;
    const void *key;
    void *value;
  } **buckets;
};

T
htab_new (int hint,
          int cmp (const void *x, const void *y),
          unsigned hash (const void *key))
{
  T table;
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
htab_reinit (T *t)
{
  assert(t && *t);

  memset((*t)->buckets, 0, (*t)->size * sizeof((*t)->buckets[0]));
  (*t)->length = 0;
  (*t)->timestamp = 0;
}

void
htab_free (T *t)
{
  assert(t && *t);

  if ((*t)->length > 0) {
    int i;
    struct binding *p, *q;
    for (i = 0; i < (*t)->size; i++) {
      for (p = (*t)->buckets[i]; p; p = q) {
        q = p->link;
        free(p);
      }
    }
  }
  free((*t)->buckets);
  free(*t);
  *t = NULL;
}

size_t
htab_length (T t)
{
  assert(t);
  return t->length;
}

size_t
htab_size (T t)
{
  assert(t);
  return t->size;
}

void *
htab_put (T t,
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
    p = (struct binding *) malloc(sizeof(*p));
    if (!p) return NULL;
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
htab_get (T t,
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
htab_remove (T t,
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
      free(p);
      t->length--;
      return value;
    }
  }
  return NULL;
}

void
htab_map (T t,
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

void **
htab_to_array (T t,
               void *end)
{
  int i, j = 0;
  void **array;
  struct binding *p;

  assert(t);
  array = (void **) malloc((2 * t->length + 1) * sizeof (*array));
  if (!array) return NULL;
  for (i = 0; i < t->size; i++)
    for (p = t->buckets[i]; p; p = p->link) {
      array[j++] = (void *)p->key;
      array[j++] = p->value;
    }
  array[j] = end;
  return array;
}

void
htab_bucket_filling_stats (T t,
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

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
#include <assert.h>
#include <limits.h>

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
  static const int primes[] = { 3, 3, 7, 13, 31, 61, 251, 509, 1021, 2053, 4093,
                                8191, 16381, 32771, 65521, INT_MAX };

  assert(hint >= 0);
  assert(cmp);
  assert(hash);

  for (i = 1; primes[i] < hint; i++)
    ;

  table = malloc(sizeof(*table) +
                 primes[i - 1] * sizeof(table->buckets[0]));
  if (!table) return NULL;
  table->size = primes[i - 1];
  table->buckets = (struct binding **)(table + 1);
  for (i = 0; i < table->size; i++)
    table->buckets[i] = NULL;
  table->cmp = cmp;
  table->hash = hash;
  table->length = 0;
  table->timestamp = 0;

  return table;
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
  free(*t);
  *t = NULL;
}

size_t
htab_length (T t)
{
  assert(t);
  return t->length;
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
  for (p = t->buckets[i]; p; p = p->link)
    if ((*t->cmp)(key, p->key) == 0)
      break;

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

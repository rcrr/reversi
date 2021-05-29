/**
 * @file
 *
 * @brief Hashtable module unit test suite.
 * @details Collects tests and helper methods for the hashtable module.
 *
 * @par ut_hashtable.c
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "unit_test.h"
#include "hashtable.h"


typedef struct aux_item_s {
  int value;
} * aux_item_t;

typedef struct aux_item_s const * const_aux_item_t;

/*
 * Auxiliary functions.
 */

int
aux_cmp (const void *x,
         const void *y) {
  assert(x);
  assert(y);
  const int *xi = (const int *) x;
  const int *yi = (const int *) y;
  return (*xi - *yi);
}

unsigned
aux_hash (const void *key)
{
  assert(key);
  const int *ikey = (int *) key;
  unsigned a = (unsigned) *ikey;
  a = (a + 0x7ed55d16) + (a << 12);
  a = (a ^ 0xc761c23c) ^ (a >> 19);
  a = (a + 0x165667b1) + (a <<  5);
  a = (a + 0xd3a2646c) ^ (a <<  9);
  a = (a + 0xfd7046c5) + (a <<  3);
  a = (a ^ 0xb55a4f09) ^ (a >> 16);
  return a;
}

void
aux_apply (const void *key,
           void **value,
           void *cl)
{
  assert(key);
  assert(value);
  assert(*value);

  struct v_s {
    char *value;
  };

  struct char_count_s {
    char c;
    int count;
  };

  struct v_s *v = (struct v_s *) *value;
  struct char_count_s *cc = (struct char_count_s *) cl;

  char *c = v->value;
  while (*c) {
    if (*c == cc->c) cc->count++;
    c++;
  }

}

/*
 * Test functions.
 */

static void
htab_dummy_test_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
aux_hash_t (ut_test_t *const t)
{
  int key;
  unsigned hash, expected;

  struct t_s {
    int key;
    unsigned expected;
  } v[] =
      {
       { 0, 0x6b4ed927},
       { 1, 0xb48681b6},
       { 2, 0xe267b84c},
       { 3, 0x4f6e0e9c},
       { 4, 0x3868201d},
       { 5, 0x01f266ee},
       { 6, 0xf2b32b34},
       { 7, 0xf3ced760},
       { 8, 0x6d25f2f4},
       { 9, 0xb5b9ef41},
       {-1, 0xfe64c182},
      };

  const size_t v_size = sizeof(v) / sizeof(v[0]);

  for (size_t i = 0; i < v_size; i++) {
    key = v[i].key;
    expected = v[i].expected;
    hash = aux_hash(&key);
    if (expected != hash) {
      printf("\ni = %zu, key = %d, expected = 0x%08x, hash = 0x%08x\n", i, key, expected, hash);
      ut_assert(t, false);
    }
  }
}

static void
htab_new_free_t (ut_test_t *const t)
{
  htab_t table = NULL;

  table = htab_new(4, aux_cmp, aux_hash);
  ut_assert(t, table);

  htab_free(&table);
  ut_assert(t, table == NULL);
}

static void
htab_put_get_t (ut_test_t *const t)
{
  htab_t table = NULL;

  struct v_k {
    int key;
  } keys[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
       { 4 },
       { 5 },
       { 6 },
       { 7 },
       { 8 },
       { 9 },
       {-1 },
       {-2 },
      };

  struct v_s {
    int value;
  } values[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
       { 4 },
       { 5 },
       { 6 },
       { 7 },
       { 8 },
       { 9 },
       {-1 },
       {-2 },
      };

  const size_t values_size = sizeof(values) / sizeof(values[0]);
  const size_t keys_size = sizeof(keys) / sizeof(keys[0]);
  ut_assert(t, keys_size == values_size);

  table = htab_new(4, aux_cmp, aux_hash);
  ut_assert(t, table);
  ut_assert(t, 0 == htab_length(table));

  for (size_t i = 0; i < keys_size; i++) {
    htab_put(table, &keys[i], &values[i]);
    ut_assert(t, i + 1 == htab_length(table));
  }

  for (size_t i = 0; i < keys_size; i++) {
    struct v_s *v = htab_get(table, &keys[i]);
    if (v->value != values[i].value) {
      printf("i = %zu, values[i].value = %d, v->value = %d\n", i, values[i].value, v->value);
      ut_assert(t, false);
    }
  }

  htab_free(&table);
  ut_assert(t, table == NULL);
}

static void
htab_update_t (ut_test_t *const t)
{
  htab_t table = NULL;
  struct v_s *v = NULL;

  struct v_k {
    int key;
  } keys[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
       { 4 },
       { 5 },
       { 0 },
       { 1 },
       { 2 },
       { 3 },
       { 4 },
       { 5 },
      };

  struct v_s {
    int value;
  } values[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
       { 4 },
       { 5 },
       { 6 },
       { 7 },
       { 8 },
       { 9 },
       {10 },
       {11 },
      };

  const size_t values_size = sizeof(values) / sizeof(values[0]);
  const size_t keys_size = sizeof(keys) / sizeof(keys[0]);
  ut_assert(t, keys_size == values_size);

  table = htab_new(64, aux_cmp, aux_hash);
  ut_assert(t, table);

  for (size_t i = 0; i < keys_size; i++) {
    htab_put(table, &keys[i], &values[i]);
  }

  v = htab_get(table, &keys[0]);
  ut_assert(t, 6 == v->value);

  v = htab_get(table, &keys[1]);
  ut_assert(t, 7 == v->value);

  v = htab_get(table, &keys[2]);
  ut_assert(t, 8 == v->value);

  v = htab_get(table, &keys[3]);
  ut_assert(t, 9 == v->value);

  v = htab_get(table, &keys[4]);
  ut_assert(t, 10 == v->value);

  v = htab_get(table, &keys[5]);
  ut_assert(t, 11 == v->value);

  v = htab_get(table, &keys[6]);
  ut_assert(t, 6 == v->value);

  htab_free(&table);
  ut_assert(t, table == NULL);
}

static void
htab_not_found_t (ut_test_t *const t)
{
  htab_t table = NULL;

  struct v_k {
    int key;
  } keys[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
      };

  struct v_s {
    int value;
  } values[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
      };

  struct v_k ks;
  struct v_s *v;

  const size_t values_size = sizeof(values) / sizeof(values[0]);
  const size_t keys_size = sizeof(keys) / sizeof(keys[0]);
  ut_assert(t, keys_size == values_size);

  table = htab_new(256, aux_cmp, aux_hash);
  ut_assert(t, table);

  ks.key = 7;
  v = htab_get(table, &ks);
  ut_assert(t, NULL == v);

  for (size_t i = 0; i < keys_size; i++) {
    htab_put(table, &keys[i], &values[i]);
  }

  v = htab_get(table, &keys[0]);
  ut_assert(t, 0 == v->value);

  v = htab_get(table, &keys[3]);
  ut_assert(t, 3 == v->value);

  v = htab_get(table, &ks);
  ut_assert(t, NULL == v);

  htab_free(&table);
  ut_assert(t, table == NULL);
}

static void
htab_remove_t (ut_test_t *const t)
{
  htab_t table = NULL;

  struct v_k {
    int key;
  } keys[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
      };

  struct v_s {
    int value;
  } values[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
      };

  struct v_k ks;
  struct v_s *v;

  const size_t values_size = sizeof(values) / sizeof(values[0]);
  const size_t keys_size = sizeof(keys) / sizeof(keys[0]);
  ut_assert(t, keys_size == values_size);

  table = htab_new(256, aux_cmp, aux_hash);
  ut_assert(t, table);

  ks.key = 7;
  v = htab_remove(table, &ks);
  ut_assert(t, NULL == v);

  for (size_t i = 0; i < keys_size; i++) {
    htab_put(table, &keys[i], &values[i]);
    ut_assert(t, i + 1 == htab_length(table));
  }
  ut_assert(t, keys_size == htab_length(table));

  ks.key = 7;
  v = htab_remove(table, &ks);
  ut_assert(t, NULL == v);
  ut_assert(t, keys_size == htab_length(table));

  ks.key = 1;
  v = htab_remove(table, &ks);
  ut_assert(t, v);
  ut_assert(t, v->value == 1);
  ut_assert(t, keys_size - 1 == htab_length(table));

  v = htab_get(table, &keys[0]);
  ut_assert(t, 0 == v->value);

  v = htab_get(table, &keys[1]);
  ut_assert(t, NULL == v);

  v = htab_get(table, &keys[2]);
  ut_assert(t, 2 == v->value);

  v = htab_get(table, &keys[3]);
  ut_assert(t, 3 == v->value);

  v = htab_get(table, &ks);
  ut_assert(t, NULL == v);

  ks.key = 1;
  v = htab_remove(table, &ks);
  ut_assert(t, NULL == v);

  ks.key = 0;
  v = htab_remove(table, &ks);
  ut_assert(t, v);
  ut_assert(t, v->value == 0);

  ks.key = 2;
  v = htab_remove(table, &ks);
  ut_assert(t, v);
  ut_assert(t, v->value == 2);

  ks.key = 3;
  v = htab_remove(table, &ks);
  ut_assert(t, v);
  ut_assert(t, v->value == 3);
  ut_assert(t, 0 == htab_length(table));

  htab_free(&table);
  ut_assert(t, table == NULL);
}

static void
htab_map_t (ut_test_t *const t)
{
  htab_t table = NULL;
  struct v_s *v;

  struct char_count_s {
    char c;
    int count;
  };

  struct char_count_s ccs;

  struct v_k {
    int key;
  } keys[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
      };

  struct v_s {
    char *value;
  } values[] =
      {
       { "zero"  },
       { "one"   },
       { "two"   },
       { "three" },
      };

  const size_t values_size = sizeof(values) / sizeof(values[0]);
  const size_t keys_size = sizeof(keys) / sizeof(keys[0]);
  ut_assert(t, keys_size == values_size);

  table = htab_new(8, aux_cmp, aux_hash);
  ut_assert(t, table);

  for (size_t i = 0; i < keys_size; i++) {
    htab_put(table, &keys[i], &values[i]);
    ut_assert(t, i + 1 == htab_length(table));
  }
  ut_assert(t, keys_size == htab_length(table));

  v = htab_get(table, &keys[0]);
  ut_assert(t, strcmp(v->value, "zero") == 0);

  v = htab_get(table, &keys[1]);
  ut_assert(t, strcmp(v->value, "one") == 0);

  v = htab_get(table, &keys[2]);
  ut_assert(t, strcmp(v->value, "two") == 0);

  v = htab_get(table, &keys[3]);
  ut_assert(t, strcmp(v->value, "three") == 0);

  ccs.c = 'o';
  ccs.count = 0;
  htab_map(table, aux_apply, &ccs);
  ut_assert(t, 3 == ccs.count);

  ccs.c = 'k';
  ccs.count = 0;
  htab_map(table, aux_apply, &ccs);
  ut_assert(t, 0 == ccs.count);

  ccs.c = 'e';
  ccs.count = 0;
  htab_map(table, aux_apply, &ccs);
  ut_assert(t, 4 == ccs.count);

  htab_free(&table);
  ut_assert(t, table == NULL);
}

static void
htab_to_array_t (ut_test_t *const t)
{
  htab_t table = NULL;

  struct v_k {
    int key;
  } keys[] =
      {
       { 0 },
       { 1 },
       { 2 },
       { 3 },
      };

  struct v_s {
    char *value;
  } values[] =
      {
       { "zero"  },
       { "one"   },
       { "two"   },
       { "three" },
      };

  const size_t values_size = sizeof(values) / sizeof(values[0]);
  const size_t keys_size = sizeof(keys) / sizeof(keys[0]);
  ut_assert(t, keys_size == values_size);

  table = htab_new(8, aux_cmp, aux_hash);
  ut_assert(t, table);

  for (size_t i = 0; i < keys_size; i++) {
    htab_put(table, &keys[i], &values[i]);
    ut_assert(t, i + 1 == htab_length(table));
  }
  ut_assert(t, keys_size == htab_length(table));

  char end_object;

  void **a = htab_to_array(table, &end_object);

  for (int i = 0; i < keys_size; i++) {
    struct v_k *k = (struct v_k *) a[2 * i];
    struct v_s *v = (struct v_s *) a[2 * i + 1];
    int key = k->key;
    char *string = v->value;
    if (key == 0)
      ut_assert(t, strcmp("zero", string) == 0);
    if (key == 1)
      ut_assert(t, strcmp("one", string) == 0);
    if (key == 2)
      ut_assert(t, strcmp("two", string) == 0);
    if (key == 3)
      ut_assert(t, strcmp("three", string) == 0);
  }
  ut_assert(t, a[2 * keys_size] == &end_object);

  free(a);

  htab_free(&table);
  ut_assert(t, table == NULL);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "hashtable");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "aux_hash", aux_hash_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_dummy_test", htab_dummy_test_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_new_free", htab_new_free_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_put_get", htab_put_get_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_update", htab_update_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_not_found", htab_not_found_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_remove", htab_remove_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_map", htab_map_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "htab_to_array", htab_to_array_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}

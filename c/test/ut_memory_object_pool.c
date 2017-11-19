/**
 * @file
 *
 * @brief Memory object pool unit test suite.
 * @details Collects tests and helper methods for the memory object pool module.
 *
 * @par ut_memory_object_pool.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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
#include <errno.h>
#include <stdint.h>

#include "unit_test.h"
#include "memory_object_pool.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
mopool_dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
mopool_create_destroy_t (ut_test_t *const t)
{
  mopool_t *mop;

  mop = mopool_create(1, MOPOOL_EXT_POLICY_FIXED, 4, 0, 0);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));
  mopool_destroy(mop);
  mop = NULL;

  mop = mopool_create(1, MOPOOL_EXT_POLICY_LIMITED, 4, 4, 16);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));
  mopool_destroy(mop);
  mop = NULL;

  mop = mopool_create(1, MOPOOL_EXT_POLICY_UNLIMITED, 4, 4, 0);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));
  mopool_destroy(mop);
  mop = NULL;
}

static void
mopool_malloc_free_fixed_a_t (ut_test_t *const t)
{
  char *c0, *c1;

  const mopool_ext_policy_t policy = MOPOOL_EXT_POLICY_FIXED;
  const size_t nobjs_initial = 4;
  const size_t nobjs_extension = 0;
  const size_t nobjs_limit = 0;
  const size_t sizeof_void_ptr = sizeof(void *);

  errno = 0;

  mopool_t *mop = mopool_create(sizeof(char), policy, nobjs_initial, nobjs_extension, nobjs_limit);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));

  c0 = mopool_malloc(mop);
  ut_assert(t, mopool_check_consistency(mop));
  for (int i = 1; i < nobjs_initial; i++) {
    c1 = mopool_malloc(mop);
    ut_assert(t, sizeof_void_ptr == (char *) c1 - (char *) c0);
    ut_assert(t, errno == 0);
    ut_assert(t, mopool_check_consistency(mop));
    c0 = c1;
  }

  c1 = mopool_malloc(mop);
  ut_assert(t, !c1);
  ut_assert(t, errno == ENOMEM);
  ut_assert(t, mopool_check_consistency(mop));

  errno = 0;

  mopool_free(mop, c0);
  c1 = mopool_malloc(mop);
  ut_assert(t, c1);
  ut_assert(t, mopool_check_consistency(mop));

  mopool_destroy(mop);
  mop = NULL;
  ut_assert(t, errno == 0);
}

static void
mopool_malloc_free_fixed_b_t (ut_test_t *const t)
{
  static const size_t nobjs = 5;

  typedef struct data_s {
    uint64_t x0;
    uint64_t x1;
    uint64_t x2;
    uint64_t x3;
    uint64_t x4;
  } data_t;

  static const size_t expected_obj_size = 40;
  static const size_t expected_slot_size = 40;

  ut_assert(t, sizeof(data_t) == expected_obj_size);

  data_t *data_refs[nobjs];

  const mopool_ext_policy_t policy = MOPOOL_EXT_POLICY_FIXED;
  const size_t nobjs_initial = nobjs;
  const size_t nobjs_extension = 0;
  const size_t nobjs_limit = 0;

  errno = 0;

  mopool_t *mop = mopool_create(sizeof(data_t), policy, nobjs_initial, nobjs_extension, nobjs_limit);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));

  ut_assert(t, nobjs == mopool_get_nobjs(mop));
  ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
  ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
  ut_assert(t, nobjs == mopool_get_nfree(mop));
  ut_assert(t, policy == mopool_get_policy(mop));
  ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
  ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
  ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));

  for (size_t i = 0; i < nobjs; i++) {
    data_refs[i] = mopool_malloc(mop);
    ut_assert(t, data_refs[i]);
    ut_assert(t, mopool_check_consistency(mop));
    data_refs[i]->x0 = 0;
    data_refs[i]->x1 = i;
    data_refs[i]->x2 = i * i;
    data_refs[i]->x3 = i * i * i;
    data_refs[i]->x4 = i * i * i * i;

    ut_assert(t, nobjs == mopool_get_nobjs(mop));
    ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
    ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
    ut_assert(t, nobjs - i - 1 == mopool_get_nfree(mop));
    ut_assert(t, policy == mopool_get_policy(mop));
    ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
    ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
    ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));
  }

  for (size_t i = 0; i < nobjs; i++) {
    ut_assert(t, 0 == data_refs[i]->x0);
    ut_assert(t, i == data_refs[i]->x1);
    ut_assert(t, i * i == data_refs[i]->x2);
    ut_assert(t, i * i * i == data_refs[i]->x3);
    ut_assert(t, i * i * i * i == data_refs[i]->x4);
    mopool_free(mop, data_refs[i]);
    data_refs[i] = NULL;
    ut_assert(t, mopool_check_consistency(mop));

    ut_assert(t, nobjs == mopool_get_nobjs(mop));
    ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
    ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
    ut_assert(t, 0 + i + 1 == mopool_get_nfree(mop));
    ut_assert(t, policy == mopool_get_policy(mop));
    ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
    ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
    ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));
  }

  mopool_destroy(mop);
  mop = NULL;
  ut_assert(t, errno == 0);
}

static void
mopool_malloc_free_fixed_c_t (ut_test_t *const t)
{
  static const size_t nobjs = 3;

  typedef struct data_s {
    uint64_t x;
    char     c;
  } data_t;

  static const size_t expected_obj_size = 16;
  static const size_t expected_slot_size = 16;

  ut_assert(t, sizeof(data_t) == expected_obj_size);

  data_t *data_refs[nobjs];

  const mopool_ext_policy_t policy = MOPOOL_EXT_POLICY_FIXED;
  const size_t nobjs_initial = nobjs;
  const size_t nobjs_extension = 0;
  const size_t nobjs_limit = 0;

  errno = 0;

  mopool_t *mop = mopool_create(sizeof(data_t), policy, nobjs_initial, nobjs_extension, nobjs_limit);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));

  ut_assert(t, nobjs == mopool_get_nobjs(mop));
  ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
  ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
  ut_assert(t, nobjs == mopool_get_nfree(mop));
  ut_assert(t, policy == mopool_get_policy(mop));
  ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
  ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
  ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));

  for (size_t i = 0; i < nobjs; i++) {
    data_refs[i] = mopool_malloc(mop);
    ut_assert(t, data_refs[i]);
    ut_assert(t, mopool_check_consistency(mop));
    data_refs[i]->x = i;
    data_refs[i]->c = i;

    ut_assert(t, nobjs - i - 1 == mopool_get_nfree(mop));
  }

  for (size_t i = 0; i < nobjs; i++) {
    ut_assert(t, i == data_refs[i]->x);
    ut_assert(t, i == data_refs[i]->c);
    mopool_free(mop, data_refs[i]);
    data_refs[i] = NULL;
    ut_assert(t, mopool_check_consistency(mop));

    ut_assert(t, 0 + i + 1 == mopool_get_nfree(mop));
  }

  mopool_destroy(mop);
  mop = NULL;
  ut_assert(t, errno == 0);
}

static void
mopool_malloc_free_fixed_d_t (ut_test_t *const t)
{
  static const size_t nobjs = 13;

  typedef struct data_s { char x[9]; } data_t;

  static const size_t expected_obj_size = 9;
  static const size_t expected_slot_size = 16;

  ut_assert(t, sizeof(data_t) == expected_obj_size);

  data_t *data_refs[nobjs];

  const mopool_ext_policy_t policy = MOPOOL_EXT_POLICY_FIXED;
  const size_t nobjs_initial = nobjs;
  const size_t nobjs_extension = 0;
  const size_t nobjs_limit = 0;

  errno = 0;

  mopool_t *mop = mopool_create(sizeof(data_t), policy, nobjs_initial, nobjs_extension, nobjs_limit);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));

  ut_assert(t, nobjs == mopool_get_nobjs(mop));
  ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
  ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
  ut_assert(t, nobjs == mopool_get_nfree(mop));
  ut_assert(t, policy == mopool_get_policy(mop));
  ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
  ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
  ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));

  for (size_t i = 0; i < nobjs; i++) {
    data_refs[i] = mopool_malloc(mop);
    ut_assert(t, data_refs[i]);
    ut_assert(t, mopool_check_consistency(mop));
    data_refs[i]->x[0] = i;

    ut_assert(t, nobjs - i - 1 == mopool_get_nfree(mop));
  }

  for (size_t i = 0; i < nobjs; i++) {
    ut_assert(t, i == data_refs[i]->x[0]);
    mopool_free(mop, data_refs[i]);
    data_refs[i] = NULL;
    ut_assert(t, mopool_check_consistency(mop));

    ut_assert(t, 0 + i + 1 == mopool_get_nfree(mop));
  }

  mopool_destroy(mop);
  mop = NULL;
  ut_assert(t, errno == 0);
}

static void
mopool_malloc_free_limited_a_t (ut_test_t *const t)
{
  static const size_t nobjs = 8;

  typedef struct data_s {
    uint64_t       value;
    struct data_s *next;
  } data_t;

  static const size_t expected_obj_size = 16;
  static const size_t expected_slot_size = 16;

  ut_assert(t, sizeof(data_t) == expected_obj_size);

  data_t *data_refs[nobjs];

  const mopool_ext_policy_t policy = MOPOOL_EXT_POLICY_LIMITED;
  const size_t nobjs_initial = 4;
  const size_t nobjs_extension = 2;
  const size_t nobjs_limit = nobjs;

  errno = 0;

  mopool_t *mop = mopool_create(sizeof(data_t), policy, nobjs_initial, nobjs_extension, nobjs_limit);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));

  ut_assert(t, nobjs_initial == mopool_get_nobjs(mop));
  ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
  ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
  ut_assert(t, nobjs_initial == mopool_get_nfree(mop));
  ut_assert(t, policy == mopool_get_policy(mop));
  ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
  ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
  ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));

  size_t count = 0;
  data_t **p = NULL;

  for (size_t i = 0; i < nobjs_initial; i++) {
    data_refs[i] = mopool_malloc(mop);
    ut_assert(t, data_refs[i]);
    ut_assert(t, mopool_check_consistency(mop));
    if (p) *p = data_refs[i];
    data_refs[i]->value = count++;
    data_refs[i]->next = NULL;
    p = &(data_refs[i]->next);

    ut_assert(t, nobjs_initial - i - 1 == mopool_get_nfree(mop));
  }

  ut_assert(t, 0 == mopool_get_nfree(mop));
  ut_assert(t, errno == 0);

  for (size_t i = nobjs_initial; i < nobjs; i++) {
    data_refs[i] = mopool_malloc(mop);
    ut_assert(t, data_refs[i]);
    ut_assert(t, mopool_check_consistency(mop));
    if (p) *p = data_refs[i];
    data_refs[i]->value = count++;
    data_refs[i]->next = NULL;
    p = &(data_refs[i]->next);
  }

  for (size_t i = 0; i < nobjs; i++) {
    ut_assert(t, i == data_refs[i]->value);
  }

  count = 0;
  data_t *obj = data_refs[0];
  while (obj) {
    ut_assert(t, count++ == obj->value);
    obj = obj->next;
  }

  /*
  printf("\n");
  for (int i = 0; i < mopool_get_nobjs(mop); i++) {
    printf("[%d]: data_refs[%d]=%p, data_refs[%d]->value=%zu, data_refs[%d]->next=%p\n", i, i, (void *) data_refs[i], i, data_refs[i]->value, i, (void *) data_refs[i]->next);
  }
  */

  ut_assert(t, 0 == mopool_get_nfree(mop));
  ut_assert(t, errno == 0);

  obj = mopool_malloc(mop);
  ut_assert(t, obj == NULL);
  ut_assert(t, errno == ENOMEM);

  errno = 0;
  mopool_destroy(mop);
  mop = NULL;
  ut_assert(t, errno == 0);
}

static void
mopool_malloc_free_unlimited_a_t (ut_test_t *const t)
{
  static const size_t nobjs = 258;

  typedef struct data_s {
    uint64_t       value;
    struct data_s *prev;
    struct data_s *next;
  } data_t;

  static const size_t expected_obj_size = 24;
  static const size_t expected_slot_size = 24;

  ut_assert(t, sizeof(data_t) == expected_obj_size);

  data_t *obj, *new, *prev;
  uint64_t count = 0;

  const mopool_ext_policy_t policy = MOPOOL_EXT_POLICY_UNLIMITED;
  const size_t nobjs_initial = 32;
  const size_t nobjs_extension = 16;
  const size_t nobjs_limit = 0;

  errno = 0;

  mopool_t *mop = mopool_create(sizeof(data_t), policy, nobjs_initial, nobjs_extension, nobjs_limit);
  ut_assert(t, mop);
  ut_assert(t, mopool_check_consistency(mop));

  ut_assert(t, nobjs_initial == mopool_get_nobjs(mop));
  ut_assert(t, expected_obj_size == mopool_get_obj_size(mop));
  ut_assert(t, expected_slot_size == mopool_get_slot_size(mop));
  ut_assert(t, nobjs_initial == mopool_get_nfree(mop));
  ut_assert(t, policy == mopool_get_policy(mop));
  ut_assert(t, nobjs_initial == mopool_get_nobjs_initial(mop));
  ut_assert(t, nobjs_extension == mopool_get_nobjs_extension(mop));
  ut_assert(t, nobjs_limit == mopool_get_nobjs_limit(mop));

  obj = mopool_malloc(mop);
  ut_assert(t, obj);
  ut_assert(t, mopool_check_consistency(mop));
  obj->value = count++;
  obj->prev = obj;
  obj->next = obj;

  for (size_t i = 1; i < nobjs; i++) {
    new = mopool_malloc(mop);
    ut_assert(t, new);
    ut_assert(t, mopool_check_consistency(mop));
    new->value = count++;
    prev = obj->prev;
    prev->next = new;
    obj->prev = new;
    new->prev = prev;
    new->next = obj;
    obj = new;
  }

  count = 0;
  prev = obj->prev;
  while (prev != obj) {
    ut_assert(t, prev->value == count++);
    prev = prev->prev;
  }

  ut_assert(t, mopool_check_consistency(mop));

  mopool_destroy(mop);
  mop = NULL;
  ut_assert(t, errno == 0);
}



/**
 * @brief Runs the test suite.
 */
int
main (argc, argv)
     int argc;
     char **argv;
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "mopool");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", mopool_dummy_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "create-destroy", mopool_create_destroy_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "malloc-free-fixed-a", mopool_malloc_free_fixed_a_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "malloc-free-fixed-b", mopool_malloc_free_fixed_b_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "malloc-free-fixed-c", mopool_malloc_free_fixed_c_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "malloc-free-fixed-d", mopool_malloc_free_fixed_d_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "malloc-free-limited-a", mopool_malloc_free_limited_a_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "malloc-free-unlimited-a", mopool_malloc_free_unlimited_a_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

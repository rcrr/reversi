/**
 * @file
 *
 * @brief Sort utils unit test suite.
 * @details Collects tests and helper methods for the sort utils module.
 *
 * @par ut_sort_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014, 2015, 2017 Roberto Corradini. All rights reserved.
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
#include <string.h>
#include <assert.h>

#include "unit_test.h"

#include "sort_utils.h"
#include "prng.h"



#define MIN_UINT64   0
#define MAX_UINT64   0xffffffffffffffff
#define LARGE_UINT64 0xfffffffffffffffe

#define MIN_INT64   (-9223372036854775807LL-1LL)
#define SMALL_INT64 -9223372036854775807LL
#define MAX_INT64   +9223372036854775807LL
#define LARGE_INT64 +9223372036854775806LL



/*
 * Auxiliary functions.
 */

static void
aux_dummy (void)
{
  return;
}



/*
 * Test functions.
 */

static void
dummy_t (ut_test_t *const t)
{
  aux_dummy();
  ut_assert(t, true);
}



/*************************************/
/* Unit tests for compare functions. */
/************************************/

static void
sort_utils_double_compare_t (ut_test_t *const t)
{
  double a;
  double b;

  a = 2.;
  b = 3.;
  ut_assert(t, sort_utils_double_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_double_icmp(&a, &b) == +1);

  a = 3.;
  b = 3.;
  ut_assert(t, sort_utils_double_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_double_icmp(&a, &b) ==  0);

  a = 3.;
  b = 2.;
  ut_assert(t, sort_utils_double_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_double_icmp(&a, &b) == -1);
}

static void
sort_utils_int_compare_t (ut_test_t *const t)
{
  int a;
  int b;

  a = 2;
  b = 3;
  ut_assert(t, sort_utils_int_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_int_icmp(&a, &b) == +1);

  a = 3;
  b = 3;
  ut_assert(t, sort_utils_int_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_int_icmp(&a, &b) ==  0);

  a = 3;
  b = 2;
  ut_assert(t, sort_utils_int_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_int_icmp(&a, &b) == -1);
}

static void
sort_utils_uint64_t_compare_t (ut_test_t *const t)
{
  uint64_t a;
  uint64_t b;

  a = 2;
  b = 3;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_uint64_t_icmp(&a, &b) == +1);

  a = 3;
  b = 3;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_uint64_t_icmp(&a, &b) ==  0);

  a = 3;
  b = 2;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_uint64_t_icmp(&a, &b) == -1);

  a = MAX_UINT64;
  b = LARGE_UINT64;
  ut_assert(t, sort_utils_uint64_t_cmp(&a, &b) > 0);
}

static void
sort_utils_int64_t_compare_t (ut_test_t *const t)
{
  int64_t a;
  int64_t b;

  a = 2;
  b = 3;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) == -1);
  ut_assert(t, sort_utils_int64_t_icmp(&a, &b) == +1);

  a = 3;
  b = 3;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) ==  0);
  ut_assert(t, sort_utils_int64_t_icmp(&a, &b) ==  0);

  a = 3;
  b = 2;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) == +1);
  ut_assert(t, sort_utils_int64_t_icmp(&a, &b) == -1);

  a = MAX_INT64;
  b = LARGE_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) > 0);

  a = 0;
  b = LARGE_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) < 0);

  a = 0;
  b = SMALL_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) > 0);

  a = MIN_INT64;
  b = SMALL_INT64;
  ut_assert(t, sort_utils_int64_t_cmp(&a, &b) < 0);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("sort_utils");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", dummy_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_double_compare", sort_utils_double_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_int_compare", sort_utils_int_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_uint64_t_compare", sort_utils_uint64_t_compare_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sort_utils_int64_t_compare", sort_utils_int64_t_compare_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

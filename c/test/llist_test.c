/**
 * @file
 *
 * @brief Linked list utils unit test suite.
 * @details Collects tests and helper methods for the linked list module.
 *
 * @par llist_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015 Roberto Corradini. All rights reserved.
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

#include "unit_test.h"
#include "linked_list.h"



/*
 * Auxiliary functions.
 */

void
aux_print_elm (d, aux)
     void *const d;
     void *const aux;
{
  int *ip = (int *) d;
  printf("%d\n", *ip);
}



/*
 * Test functions.
 */

static void
llist_new_free_test (ut_test_t *t)
{
  llist_t *l = llist_new();
  ut_assert(t, l != NULL);
  llist_free(l);
}

static void
llist_add_test (ut_test_t *t)
{
  llist_t *l = llist_new();
  ut_assert(t, l != NULL);

  int data0 = 7;
  void *e0 = &data0;
  llist_add(l, e0);
  llist_foreach(l, aux_print_elm, NULL);
  printf("\n");

  int data1 = 3;
  void *e1 = &data1;
  llist_add(l, e1);
  llist_foreach(l, aux_print_elm, NULL);
  printf("\n");

  int data2 = 9;
  void *e2 = &data2;
  llist_add(l, e2);
  llist_foreach(l, aux_print_elm, NULL);
  printf("\n");

  llist_free(l);
}

static void
dummy_ok_test (ut_test_t *t)
{
  ut_assert(t, TRUE);
}

static void
dummy_ko_test (ut_test_t *t)
{
  ut_assert(t, FALSE);
}


/**
 * @brief Runs the tests.
 */

int
main (int argc,
      char **argv)
{
  ut_suite_t *const s = ut_suite_new();

  ut_suite_add_simple_test(s, "dummy 0", dummy_ok_test);
  ut_suite_add_simple_test(s, "dummy 1", dummy_ok_test);
  ut_suite_add_simple_test(s, "dummy 2", dummy_ok_test);
  ut_suite_add_simple_test(s, "dummy 3", dummy_ko_test);
  ut_suite_add_simple_test(s, "dummy 4", dummy_ok_test);

  ut_suite_add_simple_test(s, "llist_new_free", llist_new_free_test);
  ut_suite_add_simple_test(s, "llist_add", llist_add_test);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

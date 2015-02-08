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
#include <assert.h>

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

void
aux_add_elements (d, aux)
     void *const d;
     void *const aux;
{
  assert(d);
  assert(aux);
  int *value = (int *) d;
  int *sum = (int *) aux;
  *sum += *value;
  //printf("value=%d, sum=%d\n", *value, *sum);
}



/*
 * Test functions.
 */

static void
llist_new_free_test (ut_test_t *const t)
{
  llist_t *l = llist_new();
  ut_assert(t, l != NULL);
  llist_free(l);
}

static void
llist_add_remove_foreach_test (ut_test_t *const t)
{
  int data[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  {
    llist_t *l = llist_new();
    ut_assert(t, l != NULL);
    int sum = 0;
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 0);
    llist_free(l);
  }

  {
    llist_t *l = llist_new();
    ut_assert(t, l != NULL);
    int sum = 0;
    llist_add(l, &data[7]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 7);
    llist_free(l);
  }

  {
    int sum;
    llist_t *l = llist_new();
    ut_assert(t, l != NULL);

    sum = 0;
    llist_add(l, &data[0]);
    llist_add(l, &data[1]);
    llist_add(l, &data[2]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 3);

    sum = 0;
    llist_remove(l, NULL);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 3);

    sum = 0;
    llist_remove(l, &data[0]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 3);

    sum = 0;
    llist_remove(l, &data[2]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 1);

    sum = 0;
    llist_add(l, &data[7]);
    llist_add(l, &data[8]);
    llist_add(l, &data[9]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 25);

    sum = 0;
    llist_remove(l, &data[8]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 17);

    llist_free(l);
  }
}



/**
 * @brief Runs the test suite.
 */

int
main (int argc,
      char **argv)
{
  ut_suite_t *const s = ut_suite_new();

  ut_suite_add_simple_test(s, "llist_new_free", llist_new_free_test);
  ut_suite_add_simple_test(s, "llist_add_remove_foreach", llist_add_remove_foreach_test);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

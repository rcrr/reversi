/**
 * @file
 *
 * @brief Sort Utils unit test suite.
 * @details Collects tests and helper methods for the sort utils module.
 *
 * @par sort_utils_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "sort_utils.h"



/* Test function prototypes. */

static void dummy_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/sort_utils/dummy", dummy_test);

  return g_test_run();
}



/*
 * Test functions.
 */

static void
dummy_test (void)
{
  g_assert(TRUE);

  double vals_to_sort[] = {
    1.4, 50.2, 5.11, -1.55, 301.521, 0.3301, 40.17,
    -18.0, 88.1, 30.44, -37.2, 3012.0, 49.2};

  const int values_size = sizeof(vals_to_sort) / sizeof(vals_to_sort[0]);
  void *pointers_to_sort[values_size];

  for (int i = 0; i < values_size; i++) {
    pointers_to_sort[i] = vals_to_sort + i;
  }

  void *tmp;
  tmp = pointers_to_sort[5];
  pointers_to_sort[5] = pointers_to_sort[3];
  pointers_to_sort[3] = tmp;

  printf("index;value;address;point_to\n");
  for (int i = 0; i < values_size; i++) {
    printf("%2d;%10.3f;%p;%p\n", i, vals_to_sort[i], (void *) (vals_to_sort + i), pointers_to_sort[i]);
  }

  printf("pointers_to_sort[0] < pointers_to_sort[1] = %d\n", pointers_to_sort[0] < pointers_to_sort[1]);
  printf("pointers_to_sort[1] < pointers_to_sort[0] = %d\n", pointers_to_sort[1] < pointers_to_sort[0]);

  sort_utils_heapsort_d(vals_to_sort, values_size);

  sort_utils_heapsort_p(pointers_to_sort, values_size);

  printf("{");
  for (int i = 0; i < values_size; i++) printf(" %.3f ", vals_to_sort[i]);
  printf("}\n");

  printf("index;value;address;point_to\n");
  for (int i = 0; i < values_size; i++) {
    printf("%2d;%10.3f;%p;%p\n", i, vals_to_sort[i], (void *) (vals_to_sort + i), pointers_to_sort[i]);
  }

}

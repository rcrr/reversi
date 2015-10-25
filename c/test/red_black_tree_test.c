/**
 * @file
 *
 * @brief Red black tree unit test suite.
 * @details Collects tests and helper methods for the red-black tree module.
 *
 * @par red_black_tree_test.c
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

#include <glib.h>

#include "red_black_tree.h"



/* Test function prototypes. */

static void dummy_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/red_black_tree/dummy", dummy_test);

  return g_test_run();
}

static int
compare_int (const void *item_a,
             const void *item_b,
             void *param)
{
  assert(item_a);
  assert(item_b);
  assert(param == NULL);
  int *a = (int *) item_a;
  int *b = (int *) item_b;
  if (*a > *b) return +1;
  else if (*a == *b) return 0;
  else return -1;
}

/*
 * Test functions.
 */

static void
dummy_test (void)
{
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  rbt_destroy(table, NULL);
  g_assert(TRUE);
}

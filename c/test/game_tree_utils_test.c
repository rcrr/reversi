/**
 * @file
 *
 * @brief Game tree utils unit test suite.
 * @details Collects tests and helper methods for the game tree utils module.
 *
 * @par game_tree_utils_test.c
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

#include "game_tree_utils.h"



/* Test function prototypes. */

static void dummy_test (void);
static void pve_create_test (void);
static void pve_internals_to_stream_test (void);
static void pve_verify_consistency_test (void);


int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/game_tree_utils/dummy", dummy_test);
  g_test_add_func("/game_tree_utils/pve_create_test", pve_create_test);
  g_test_add_func("/game_tree_utils/pve_internals_to_stream_test", pve_internals_to_stream_test);
  g_test_add_func("/game_tree_utils/pve_verify_consistency_test", pve_verify_consistency_test);

  return g_test_run();
}



/*
 * Test functions.
 */

static void
dummy_test (void)
{
  g_assert(TRUE);
}

static void
pve_verify_consistency_test (void)
{
  PVEnv *pve;
  gboolean is_consistent = TRUE;
  int error_code = 0;
  gchar *error_message = NULL;

  pve = pve_new();
  pve->cells_stack_head = pve->cells_stack - 1;
  is_consistent = pve_verify_consistency(pve, &error_code, &error_message);
  g_assert(!is_consistent && (error_code == 1));
  pve_free(pve);

  pve = pve_new();
  pve->cells_stack_head = pve->cells_stack + pve->cells_size;
  is_consistent = pve_verify_consistency(pve, &error_code, &error_message);
  g_assert(!is_consistent && (error_code == 2));
  pve_free(pve);
}

static void
pve_create_test (void)
{
  PVEnv *pve;

  pve = pve_new();
  pve_free(pve);

  pve = pve_new();
  pve_free(pve);

  g_assert(TRUE);
}

static void
pve_internals_to_stream_test (void)
{
   FILE * fp;
   fp = fopen ("build/test/pve_internals_to_stream_test.txt", "w+");

  PVEnv *pve = pve_new();
  pve_internals_to_stream(pve, fp);
  pve_free(pve);

  fclose(fp);

  g_assert(TRUE);
}

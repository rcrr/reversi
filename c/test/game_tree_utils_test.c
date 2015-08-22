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
 * @copyright 2015, 2014 Roberto Corradini. All rights reserved.
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
static void pve_is_invariant_satisfied_test (void);


int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/game_tree_utils/dummy", dummy_test);
  g_test_add_func("/game_tree_utils/pve_create_test", pve_create_test);
  g_test_add_func("/game_tree_utils/pve_internals_to_stream_test", pve_internals_to_stream_test);
  g_test_add_func("/game_tree_utils/pve_is_invariant_satisfied_test", pve_is_invariant_satisfied_test);

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
pve_is_invariant_satisfied_test (void)
{
  PVEnv *pve;
  pve_error_code_t error_code;
  PVCell ***lines_segments_tmp;
  PVCell ***lines_segments_head_tmp;

  gboolean is_consistent = TRUE;
  switches_t check_mask = 0xFFFFFFFF;

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  pve->lines_segments_size = 0; // 0 is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_SIZE_IS_INCORRECT));
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  pve->lines_first_size = 0; // 0 is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == PVE_ERROR_CODE_LINES_FIRST_SIZE_IS_INCORRECT));
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  lines_segments_head_tmp = pve->lines_segments_head;
  pve->lines_segments_head = NULL; // NULL is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_HEAD_IS_NULL));
  pve->lines_segments_head = lines_segments_head_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  lines_segments_tmp = pve->lines_segments;
  pve->lines_segments = NULL; // NULL is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_IS_NULL));
  pve->lines_segments = lines_segments_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  lines_segments_tmp = pve->lines_segments;
  lines_segments_head_tmp = pve->lines_segments_head;
  pve->lines_segments = lines_segments_head_tmp;
  pve->lines_segments_head = lines_segments_tmp;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_HEADS_PRECEDES_ARRAY_INDEX_0));
  pve->lines_segments = lines_segments_tmp;
  pve->lines_segments_head = lines_segments_head_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  lines_segments_head_tmp = pve->lines_segments_head;
  pve->lines_segments_head = pve->lines_segments + pve->lines_segments_size + 1;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == PVE_ERROR_CODE_ACTIVE_LINES_SEGMENTS_COUNT_EXCEEDS_BOUND));
  pve->lines_segments_head = lines_segments_head_tmp;
  pve_free(pve);




  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  pve->lines_stack_head = pve->lines_stack - 1;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == 1004));
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new();
  pve->lines_stack_head = pve->lines_stack + pve->lines_size;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  g_assert(!is_consistent && (error_code == 1009));
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
  pve_internals_to_stream(pve, fp, 0xFF);
  pve_free(pve);

  fclose(fp);

  g_assert(TRUE);
}

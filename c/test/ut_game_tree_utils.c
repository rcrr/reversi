/**
 * @file
 *
 * @brief Utest unit test suite.
 * @details Collects tests and helper methods for the utest program.
 *
 * @par ut_utest.c
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

#include "game_tree_utils.h"
#include "unit_test.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
pve_create_t (ut_test_t *const t)
{
  PVEnv *pve;

  GamePositionX *dummy_gpx = game_position_x_new(empty_square_set,
                                                 empty_square_set,
                                                 BLACK_PLAYER);

  pve = pve_new(dummy_gpx);
  pve_free(pve);

  pve = pve_new(dummy_gpx);
  pve_free(pve);

  ut_assert(t, true);
}

static void
pve_internals_to_stream_t (ut_test_t *const t)
{
  FILE * fp;
  fp = fopen ("build/test/pve_internals_to_stream_test.txt", "w+");

  GamePositionX *dummy_gpx = game_position_x_new(empty_square_set,
                                                 empty_square_set,
                                                 BLACK_PLAYER);

  PVEnv *pve = pve_new(dummy_gpx);
  pve_internals_to_stream(pve, fp, 0xFF);
  pve_free(pve);

  fclose(fp);

  ut_assert(t, true);
}

static void
pve_is_invariant_satisfied_t (ut_test_t *const t)
{
  PVEnv *pve;
  pve_error_code_t error_code;
  PVCell ***lines_segments_tmp;
  PVCell ***lines_segments_head_tmp;
  size_t lines_size_tmp;
  PVCell **lines_segment_tmp;
  ptrdiff_t active_lines_segments_count;

  bool is_consistent = false;
  switches_t check_mask = 0xFFFFFFFF;

  GamePositionX *dummy_gpx = game_position_x_new(empty_square_set,
                                                 empty_square_set,
                                                 BLACK_PLAYER);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  pve->lines_segments_size = 0; // 0 is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_SIZE_IS_INCORRECT));
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  pve->lines_first_size = 0; // 0 is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_FIRST_SIZE_IS_INCORRECT));
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  lines_segments_head_tmp = pve->lines_segments_head;
  pve->lines_segments_head = NULL; // NULL is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_HEAD_IS_NULL));
  pve->lines_segments_head = lines_segments_head_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  lines_segments_tmp = pve->lines_segments;
  pve->lines_segments = NULL; // NULL is a wrong value.
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_IS_NULL));
  pve->lines_segments = lines_segments_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  lines_segments_tmp = pve->lines_segments;
  lines_segments_head_tmp = pve->lines_segments_head;
  pve->lines_segments = lines_segments_head_tmp;
  pve->lines_segments_head = lines_segments_tmp;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_HEADS_PRECEDES_ARRAY_INDEX_0));
  pve->lines_segments = lines_segments_tmp;
  pve->lines_segments_head = lines_segments_head_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  lines_segments_head_tmp = pve->lines_segments_head;
  pve->lines_segments_head = pve->lines_segments + pve->lines_segments_size + 1;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_ACTIVE_LINES_SEGMENTS_COUNT_EXCEEDS_BOUND));
  pve->lines_segments_head = lines_segments_head_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  lines_size_tmp = pve->lines_size;
  pve->lines_size++;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SIZE_MISMATCH));
  pve->lines_size = lines_size_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  lines_segment_tmp = *pve->lines_segments;
  *pve->lines_segments = NULL;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_HAS_AN_INVALID_NULL_VALUE));
  *pve->lines_segments = lines_segment_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  PVCell *line_fake_value;
  pve = pve_new(dummy_gpx);
  active_lines_segments_count = pve->lines_segments_head - pve->lines_segments;
  lines_segment_tmp = *(pve->lines_segments + active_lines_segments_count); // The first unused lines segment is saved, and then corrupted.
  *(pve->lines_segments + active_lines_segments_count) = &line_fake_value;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENTS_HAS_AN_INVALID_NOT_NULL_VALUE));
  *(pve->lines_segments + active_lines_segments_count) = lines_segment_tmp;
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  size_t size_tmp = *pve->lines_segments_sorted_sizes;
  *pve->lines_segments_sorted_sizes = PVE_LINES_FIRST_SIZE * 4;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == PVE_ERROR_CODE_LINES_SEGMENT_COMPUTED_INDEX_OUT_OF_RANGE));
  *pve->lines_segments_sorted_sizes = size_tmp;
  pve_free(pve);

  /*
   * These seven error codes are not tested. They require more than one active segment,
   * and the api to increase the lines segments are not exported.
   *
   * PVE_ERROR_CODE_LINES_SEGMENTS_POS_0_AND_1_ANOMALY
   * PVE_ERROR_CODE_LINES_SEGMENTS_SORTED_AND_UNSORTED_DO_NOT_MATCH
   * PVE_ERROR_CODE_LINES_SEGMENTS_ARE_NOT_PROPERLY_SORTED
   * PVE_ERROR_CODE_LINES_SIZE_DOESNT_MATCH_WITH_CUMULATED
   * PVE_ERROR_CODE_LINES_SEGMENT_POSITION_0_NOT_FOUND
   * PVE_ERROR_CODE_LINES_SEGMENT_POSITION_0_OR_1_NOT_FOUND
   * PVE_ERROR_CODE_LINES_SEGMENTS_UNUSED_SEGMENT_HAS_SIZE
   */





  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  pve->lines_stack_head = pve->lines_stack - 1;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == 1004));
  pve_free(pve);

  error_code = PVE_ERROR_CODE_OK;
  pve = pve_new(dummy_gpx);
  pve->lines_stack_head = pve->lines_stack + pve->lines_size;
  is_consistent = pve_is_invariant_satisfied(pve, &error_code, check_mask);
  ut_assert(t, !is_consistent && (error_code == 1009));
  pve_free(pve);
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

  ut_suite_t *const s = ut_suite_new(&config, "game_tree_utils");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "pve_create", pve_create_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "pve_internals_to_stream", pve_internals_to_stream_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "pve_is_invariant_satisfied", pve_is_invariant_satisfied_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

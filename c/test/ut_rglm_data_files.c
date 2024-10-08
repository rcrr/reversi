/**
 * @file
 *
 * @brief RGLM Data Files test suite.
 * @details Collects tests and helper methods for the RGLM Data Files utilities.
 *
 * @par ut_rglm_data_files.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2020, 2022, 2023 Roberto Corradini. All rights reserved.
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
#include "file_utils.h"
#include "sort_utils.h"
#include "rglm_data_files.h"



/*
 * Test functions.
 */

static void
rglmdf_dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
rglmdf_set_batch_ids_t (ut_test_t *const t)
{
  rglmdf_general_data_t gds;
  rglmdf_general_data_t *gd;

  size_t res;

  gd = &gds;

  rglmdf_general_data_init(gd);

  uint64_t batch_ids[] = { 1, 3, 5 };
  size_t batch_id_cnt = sizeof(batch_ids) / sizeof(uint64_t);

  res = rglmdf_set_batch_ids(gd, batch_ids, batch_id_cnt);
  ut_assert(t, res == batch_id_cnt);

  ut_assert(t, 1 == gd->batch_ids[0]);
  ut_assert(t, 3 == gd->batch_ids[1]);
  ut_assert(t, 5 == gd->batch_ids[2]);

  uint64_t *bids = rglmdf_get_batch_ids(gd);

  for (size_t i = 0; i < batch_id_cnt; i++) {
    ut_assert(t, bids[i] == batch_ids[i]);
  }

  rglmdf_general_data_release(gd);
}

static void
rglmdf_set_position_statuses_t (ut_test_t *const t)
{
  rglmdf_general_data_t gds;
  rglmdf_general_data_t *gd;

  size_t res, cnt;

  gd = &gds;

  rglmdf_general_data_init(gd);

  char* position_statuses[] = { "CMR", "CMS", "CMP" };
  size_t position_status_cnt = sizeof(position_statuses) / sizeof(char*);

  res = rglmdf_set_position_statuses(gd, position_statuses, position_status_cnt);
  ut_assert(t, res == position_status_cnt);

  cnt = rglmdf_get_position_status_cnt(gd);
  ut_assert(t, cnt == position_status_cnt);

  sort_utils_insertionsort_asc_string(position_statuses, position_status_cnt);

  char **pss = rglmdf_get_position_statuses(gd);
  for (size_t i = 0; i < cnt; i++) {
    char *a = position_statuses[i];
    char *b = pss[i];
    if (0 != strcmp(a, b)) {
      printf("\n");
      printf("Strings a: %s\n", a);
      printf("        b: %s\n", b);
      printf("  differs, they should be the same.\n");
      ut_assert(t, false);
    }
  }

  rglmdf_general_data_release(gd);
}

static void
rglmdf_set_features_t (ut_test_t *const t)
{
  rglmdf_general_data_t gds;
  rglmdf_general_data_t *gd;

  size_t res, cnt;

  gd = &gds;

  rglmdf_general_data_init(gd);
  gd->format = RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL;

  board_feature_id_t features[] = { 1, 0 };
  size_t feature_cnt = sizeof(features) / sizeof(board_feature_id_t);

  res = rglmdf_set_features(gd, features, feature_cnt);
  ut_assert(t, res == feature_cnt);

  cnt = rglmdf_get_feature_cnt(gd);
  ut_assert(t, cnt == feature_cnt);

  sort_utils_insertionsort(features, feature_cnt, sizeof(board_feature_id_t), sort_utils_int_cmp);

  board_feature_id_t *fs = rglmdf_get_features(gd);
  for (size_t i = 0; i < cnt; i++) {
      ut_assert(t, fs[i] == features[i]);
  }

  rglmdf_general_data_release(gd);
}

static void
rglmdf_set_patterns_t (ut_test_t *const t)
{
  rglmdf_general_data_t gds;
  rglmdf_general_data_t *gd;

  size_t res, cnt;

  gd = &gds;

  rglmdf_general_data_init(gd);
  gd->format = RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL;

  board_pattern_id_t patterns[] = { BOARD_PATTERN_CORNER, BOARD_PATTERN_EDGE, BOARD_PATTERN_DIAG8 };
  size_t pattern_cnt = sizeof(patterns) / sizeof(board_pattern_id_t);

  res = rglmdf_set_patterns(gd, patterns, pattern_cnt);
  ut_assert(t, res == pattern_cnt);

  cnt = rglmdf_get_pattern_cnt(gd);
  ut_assert(t, cnt == pattern_cnt);

  sort_utils_insertionsort(patterns, pattern_cnt, sizeof(board_pattern_id_t), sort_utils_int_cmp);

  board_pattern_id_t *fs = rglmdf_get_patterns(gd);
  for (size_t i = 0; i < cnt; i++) {
      ut_assert(t, fs[i] == patterns[i]);
  }

  rglmdf_general_data_release(gd);
}

static void
rglmdf_get_endianness_t (ut_test_t *const t)
{
  int ret;
  ret = rglmdf_get_endianness();
  /* The REVERSI program has been developed on a LITTLE Endian architecture.
   * Most likely on BIG Endian processor it would not work as expected. */
  ut_assert(t, ret == 0);
}

static void
rglmdf_verify_type_sizes_t (ut_test_t *const t)
{
  rglmdf_verify_type_sizes();
  ut_assert(t, rglmdf_verify_type_sizes());
}

static void
rglmdf_general_data_init_t (ut_test_t *const t)
{
  rglmdf_general_data_t gds;
  rglmdf_general_data_t *gd;

  gd = &gds;

  rglmdf_general_data_init(gd);

  rglmdf_general_data_release(gd);

  ut_assert(t, true);
}

static void
rglmdf_check_sha3_file_digest_t (ut_test_t *const t)
{
  const char *dirname = "test/data/ut_rglm_data_files";
  const char *modelname = "b7_y20_ucms_fintercept-mobility_pdiag3";
  const char *extension = "dat";
  const char *hashext = "SHA3-256";

  char filename[1024];
  char hashname[1024];

  int ccount, ret;

  ccount = snprintf(filename, sizeof filename, "%s/%s.%s", dirname, modelname, extension);
  ut_assert(t, ccount < sizeof filename);
  ut_assert(t, fut_file_exists(filename));

  ccount = snprintf(hashname, sizeof hashname, "%s.%s", filename, hashext);
  ut_assert(t, ccount < sizeof hashname);
  ut_assert(t, fut_file_exists(hashname));

  ret = rglmdf_check_sha3_file_digest(filename, NULL);
  ut_assert(t, ret == 0);
}

static void
rglmdf_read_general_data_from_binary_file_t (ut_test_t *const t)
{
  const char *dirname = "test/data/ut_rglm_data_files";
  const char *modelname = "b7_y20_ucms_fintercept-mobility_pdiag3";
  const char *extension = "dat";
  const char *hashext = "SHA3-256";

  char filename[1024];
  char hashname[1024];

  bool verbose;

  int ccount, ret;

  rglmdf_general_data_t gds;
  rglmdf_general_data_t *gd;

  gd = &gds;

  verbose = (ut_run_time_is_verbose(t)) ? true : false;

  ccount = snprintf(filename, sizeof filename, "%s/%s.%s", dirname, modelname, extension);
  ut_assert(t, ccount < sizeof filename);
  ut_assert(t, fut_file_exists(filename));

  ccount = snprintf(hashname, sizeof hashname, "%s.%s", filename, hashext);
  ut_assert(t, ccount < sizeof hashname);
  ut_assert(t, fut_file_exists(hashname));

  rglmdf_general_data_init(gd);

  ret = rglmdf_read_general_data_from_binary_file(gd, filename, verbose);
  ut_assert(t, ret == 0);

  if (false) printf("rglmdf_get_file_creation_time(gd)=%ld\n", rglmdf_get_file_creation_time(gd));
  ut_assert(t, 0 == rglmdf_get_file_creation_time(gd));

  char creation_time_as_string[25];
  rglmdf_get_file_creation_time_as_string(gd, creation_time_as_string);
  ut_assert(t, strcmp("Thu Jan  1 00:00:00 1970", creation_time_as_string) == 0);

  ut_assert(t, 1 == rglmdf_get_batch_id_cnt(gd));

  uint64_t *batch_ids;
  batch_ids = rglmdf_get_batch_ids(gd);
  ut_assert(t, 7 == batch_ids[0]);

  ut_assert(t, 20 == rglmdf_get_empty_count(gd));

  ut_assert(t, 1 == rglmdf_get_position_status_cnt(gd));

  char** position_statuses;
  position_statuses = rglmdf_get_position_statuses(gd);
  ut_assert(t, strcmp("CMS", position_statuses[0]) == 0);

  ut_assert(t, 2 == rglmdf_get_feature_cnt(gd));

  board_feature_id_t *features;
  features = rglmdf_get_features(gd);
  ut_assert(t, BOARD_FEATURE_INTERCEPT == features[0]);
  ut_assert(t, BOARD_FEATURE_MOBILITY == features[1]);

  ut_assert(t, 2 == rglmdf_get_glm_f_variable_cnt(gd));

  ut_assert(t, 1 == rglmdf_get_pattern_cnt(gd));

  board_pattern_id_t *patterns;
  patterns = rglmdf_get_patterns(gd);
  ut_assert(t, BOARD_PATTERN_DIAG3 == patterns[0]);

  ut_assert(t, 16 == rglmdf_get_glm_p_variable_cnt(gd));

  ut_assert(t, 1 == rglmdf_get_position_summary_ntuples(gd));

  rglmdf_position_summary_record_t *psrs;
  psrs = rglmdf_get_position_summary_records(gd);
  ut_assert(t, 7 == psrs[0].batch_id);
  ut_assert(t, strcmp("CMS", psrs[0].status) == 0);
  ut_assert(t, 10 == psrs[0].game_position_cnt);
  ut_assert(t, 10 == psrs[0].classified_cnt);

  ut_assert(t, 18 == rglmdf_get_entity_freq_summary_ntuples(gd));

  rglmdf_entity_freq_summary_record_t *pfsrs;
  pfsrs = rglmdf_get_entity_freq_summary_records(gd);

  ut_assert(t,                       0 == pfsrs[0].glm_variable_id);
  ut_assert(t,                       0 == pfsrs[0].entity_class);
  ut_assert(t, BOARD_FEATURE_INTERCEPT == pfsrs[0].entity_id);
  ut_assert(t,                       0 == pfsrs[0].principal_index_value);
  ut_assert(t,                      10 == pfsrs[0].total_cnt);
  ut_assert(t,                     1.0 == pfsrs[0].relative_frequency);
  ut_assert(t,                     1.0 == pfsrs[0].theoretical_probability);
  ut_assert(t,                     0.0 == pfsrs[0].weight);

  ut_assert(t,                      1 == pfsrs[1].glm_variable_id);
  ut_assert(t,                      0 == pfsrs[1].entity_class);
  ut_assert(t, BOARD_FEATURE_MOBILITY == pfsrs[1].entity_id);
  ut_assert(t,                      0 == pfsrs[1].principal_index_value);
  ut_assert(t,                     10 == pfsrs[1].total_cnt);
  ut_assert(t,                    1.0 == pfsrs[1].relative_frequency);
  ut_assert(t,                    1.0 == pfsrs[1].theoretical_probability);
  ut_assert(t,                    0.0 == pfsrs[1].weight);

  ut_assert(t,                   2 == pfsrs[2].glm_variable_id);
  ut_assert(t,                   1 == pfsrs[2].entity_class);
  ut_assert(t, BOARD_PATTERN_DIAG3 == pfsrs[2].entity_id);
  ut_assert(t,                   0 == pfsrs[2].principal_index_value);
  ut_assert(t,                   2 == pfsrs[2].total_cnt);
  ut_assert(t,                0.05 == pfsrs[2].relative_frequency);
  ut_assert(t,                1.0  >= pfsrs[2].theoretical_probability);
  ut_assert(t,                0.0  == pfsrs[2].weight);

  ut_assert(t,                  17 == pfsrs[17].glm_variable_id);
  ut_assert(t,                   1 == pfsrs[17].entity_class);
  ut_assert(t, BOARD_PATTERN_DIAG3 == pfsrs[17].entity_id);
  ut_assert(t,                  26 == pfsrs[17].principal_index_value);
  ut_assert(t,                   2 == pfsrs[17].total_cnt);
  ut_assert(t,                0.05 == pfsrs[17].relative_frequency);
  ut_assert(t,                1.0  >= pfsrs[17].theoretical_probability);
  ut_assert(t,                0.0  == pfsrs[17].weight);

  ut_assert(t, 10 == rglmdf_get_positions_ntuples(gd));

  rglmdf_solved_and_classified_gp_record_t *rs;
  rs = rglmdf_get_positions_records(gd);

  ut_assert(t, 0 == rs[0].row_n);
  ut_assert(t, 68230056 == rs[0].gp_id);
  ut_assert(t, 4611717676283199524 == rs[0].mover);
  ut_assert(t, -7855295674223658936 == rs[0].opponent);
  ut_assert(t, 10 == rs[0].game_value);

  for (int i = 0; i < 10; i++) ut_assert(t, i == rs[i].row_n);

  ut_assert(t, 2 == rglmdf_get_positions_n_fvalues_per_record(gd));

  ut_assert(t, 4 == rglmdf_get_positions_n_index_values_per_record(gd));

  int32_t *pis;
  pis = rglmdf_get_positions_i0array(gd);

  ut_assert(t,  9 == pis[0]);
  ut_assert(t,  7 == pis[1]);
  ut_assert(t,  6 == pis[2]);
  ut_assert(t,  0 == pis[3]);

  ut_assert(t, 25 == pis[36]);
  ut_assert(t, 14 == pis[37]);
  ut_assert(t,  2 == pis[38]);
  ut_assert(t,  9 == pis[39]);

  double *pfs;
  pfs = rglmdf_get_positions_farray(gd);

  ut_assert(t, 1.000 == pfs[0]);
  ut_assert(t, 0.650 == pfs[1]);

  ut_assert(t, 1.000 == pfs[18]);
  ut_assert(t, 0.400 == pfs[19]);

  rglmdf_general_data_release(gd);
}

static void
rglmdf_model_weights_read_from_binary_file_t (ut_test_t *const t)
{
  const char *dirname = "test/data/ut_rglm_data_files";
  const char *modelname = "A2050_01.w";
  const char *extension = "dat";
  const char *hashext = "SHA3-256";

  char filename[1024];
  char hashname[1024];

  bool verbose;

  int ccount, ret;

  rglmdf_model_weights_t mws;
  rglmdf_model_weights_t *mw;

  mw = &mws;

  verbose = (ut_run_time_is_verbose(t)) ? true : false;

  ccount = snprintf(filename, sizeof filename, "%s/%s.%s", dirname, modelname, extension);
  ut_assert(t, ccount < sizeof filename);
  ut_assert(t, fut_file_exists(filename));

  ccount = snprintf(hashname, sizeof hashname, "%s.%s", filename, hashext);
  ut_assert(t, ccount < sizeof hashname);
  ut_assert(t, fut_file_exists(hashname));

  rglmdf_model_weights_init(mw);

  ret = rglmdf_model_weights_read_from_binary_file(mw, filename, verbose, true);
  ut_assert(t, ret == 0);

  if (verbose) rglmdf_model_weights_summary_to_stream(mw, stdout);

  rglmdf_model_weights_release(mw);
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

  ut_suite_t *const s = ut_suite_new(&config, "rglm_data_files");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_dummy", rglmdf_dummy_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_set_batch_ids", rglmdf_set_batch_ids_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_set_position_statuses", rglmdf_set_position_statuses_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_set_features", rglmdf_set_features_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_set_patterns", rglmdf_set_patterns_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_get_endianness", rglmdf_get_endianness_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_verify_type_sizes", rglmdf_verify_type_sizes_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_general_data_init", rglmdf_general_data_init_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_check_sha3_file_digest", rglmdf_check_sha3_file_digest_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "rglmdf_read_general_data_from_binary_file", rglmdf_read_general_data_from_binary_file_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "rglmdf_model_weights_read_from_binary_file_t", rglmdf_model_weights_read_from_binary_file_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  return failure_count;
}

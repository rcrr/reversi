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
 * @copyright 2020 Roberto Corradini. All rights reserved.
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
#include "file_utils.h"
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
  ut_assert(t, true);
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

  ret = rglmdf_check_sha3_file_digest(filename);
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

  rglmdf_general_data_release(gd);
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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_get_endianness", rglmdf_get_endianness_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_verify_type_sizes", rglmdf_verify_type_sizes_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_general_data_init", rglmdf_general_data_init_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_check_sha3_file_digest", rglmdf_check_sha3_file_digest_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmdf_read_general_data_from_binary_file", rglmdf_read_general_data_from_binary_file_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  return failure_count;
}

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

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  return failure_count;
}

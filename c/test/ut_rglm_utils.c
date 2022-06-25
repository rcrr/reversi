/**
 * @file
 *
 * @brief Reversi Generalized Linear Model utilities unit test suite.
 * @details Collects tests and helper methods for the rglm utils module.
 *
 * @par ut_rglm_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019, 2022 Roberto Corradini. All rights reserved.
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
#include <math.h>

#include "unit_test.h"
#include "rglm_utils.h"
#include "file_utils.h"
#include "rglm_data_files.h"



/*
 * Test structure.
 */



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
dummy_test_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
rglmut_logistic_function_t (ut_test_t *const t)
{
  double v, e;
  size_t n;

  const double eps = 0.0001;

  const double data_x[] = { 0.0000, 0.1000, 0.2000, 0.3000, 0.4000, 0.5000, 0.6000, 0.7000, 0.8000, 0.9000, 1.0000, 2.0000, 3.0000, 4.0000, 5.0000, -1.0000, -5.0000 };
  const double data_y[] = { 0.5000, 0.5250, 0.5498, 0.5744, 0.5987, 0.6225, 0.6457, 0.6682, 0.6900, 0.7109, 0.7311, 0.8808, 0.9526, 0.9820, 0.9933,  0.2689,  0.0067 };

  n = sizeof(data_x) / sizeof(data_x[0]);

  ut_assert(t, n == sizeof(data_y) / sizeof(data_y[0]));
  ut_assert(t, 0.5 == rglmut_logistic_function(0.0));

  for (int i = 0; i < n; i++) {
    v = rglmut_logistic_function(data_x[i]);
    e = fabs(data_y[i] - v);
    if (e > eps) fprintf(stderr, "\ni=%d, data_x=%f, data_y=%f, v=%f, e=%f, eps=%f\n", i, data_x[i], data_y[i], v, e, eps);
    ut_assert(t, e <= eps);
  }

}

static void
rglmut_gv_scale_t (ut_test_t *const t)
{
  double v, e;
  size_t n;

  const double eps = 0.000001;

  const int data_x[]    = {    0,  -64,  +64 };
  const double data_y[] = { 0.50, 0.01, 0.99 };

  n = sizeof(data_x) / sizeof(data_x[0]);

  ut_assert(t, n == sizeof(data_y) / sizeof(data_y[0]));

  for (int i = 0; i < n; i++) {
    v = rglmut_gv_scale(data_x[i]);
    e = fabs(data_y[i] - v);
    if (e > eps) fprintf(stderr, "\ni=%d, data_x=%d, data_y=%f, v=%f, e=%f, eps=%f\n", i, data_x[i], data_y[i], v, e, eps);
    ut_assert(t, e <= eps);
  }
}

static void
rglmut_eval_gp_using_model_weights_t (ut_test_t *const t)
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

  /*
   * Sample board.
   *
   * ROW_N    GP_ID                MOVER             OPPONENT GAME_VALUE GAME_VALUE_TRANSFORMED EVALUATION_FUNCTION       RESIDUAL EGV
   *    28 62089772      600540516071168     2336253951166719          0              0.5000000           0.4689332   0.0310667753  -4
   *    59 62091678    36107996742225664  6645118932134928511          8          0.56124997139      0.492535740137 0.068714231253   0
   */
  board_t bs;
  board_t *b = &bs;

  board_set_square_sets(b, 600540516071168ULL, 2336253951166719ULL);
  const double expected_ef = 0.4689332;
  const int expected_egv = -4;

  //board_set_square_sets(b, 36107996742225664ULL, 6645118932134928511ULL);
  //const double expected_ef = 0.492535740137;
  //const int expected_egv = 0;

  double ef;
  int egv;

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

  ef = rglmut_eval_gp_using_model_weights(mw, b);
  egv = rglmut_gv_scale_back_i(ef);

  //printf("\n");
  //printf("ef = %10.8f\n", ef);
  //printf("egv = %d\n", egv);

  ut_assert(t, egv == expected_egv);
  ut_assert(t, fabs(ef - expected_ef) < 0.0001);

  rglmdf_model_weights_release(mw);
}

static void
rglmut_eval_gp_negascout_t (ut_test_t *const t)
{
  const char *dirname = "test/data/ut_rglm_data_files";
  const char *modelname = "A1850_01.w";
  const char *extension = "dat";
  const char *hashext = "SHA3-256";

  char filename[1024];
  char hashname[1024];

  bool verbose;

  int ccount, ret;

  rglmdf_model_weights_t mws;
  rglmdf_model_weights_t *mw;

  mw = &mws;

  /*
   * Sample board.
   *
   * ROW_N    GP_ID                MOVER             OPPONENT GAME_VALUE GAME_VALUE_TRANSFORMED EVALUATION_FUNCTION       RESIDUAL EGV
   *    28 62089772      600540516071168     2336253951166719          0              0.5000000           0.4689332   0.0310667753  -4
   *    59 62091678    36107996742225664  6645118932134928511          8          0.56124997139      0.492535740137 0.068714231253   0
   *
   */
  board_t bs;
  board_t *b = &bs;
  //board_set_square_sets(b, 600540516071168ULL, 2336253951166719ULL);
  //const double expected_ef = 0.4689332;
  //const int expected_egv = -4;

  board_set_square_sets(b, 36107996742225664ULL, 6645118932134928511ULL);
  const double expected_ef = 0.559602;
  const int expected_egv = +8;

  double ef;
  int egv;

  verbose = (ut_run_time_is_verbose(t)) ? true : false;
  //printf("\n");

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

  ef = rglmut_eval_gp_negascout(b, mw);
  egv = rglmut_gv_scale_back_i(ef);

  //printf("ef  = %f\n", ef);
  //printf("egv = %d\n", egv);

  ut_assert(t, egv == expected_egv);
  ut_assert(t, fabs(ef - expected_ef) < 0.0001);

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

  ut_suite_t *const s = ut_suite_new(&config, "rglm_utils");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy_test", dummy_test_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmut_logistic_function", rglmut_logistic_function_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "rglmut_gv_scale", rglmut_gv_scale_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "rglmut_eval_gp_using_model_weights", rglmut_eval_gp_using_model_weights_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "rglmut_eval_gp_negascout", rglmut_eval_gp_negascout_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}

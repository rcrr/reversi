/**
 * @file
 *
 * @todo Notice: RGLM and REGAB to-do list has to go somehow together.
 *
 * @todo Both REGAB and RGLM main functions have to be reduced to a call to two respective new functions.
 *       These new functions are going to enable an API for the two programs that is then usable by
 *       test modules.
 *
 * @todo More comments are needed in order to better explain the algorithm.
 *
 * @todo Residual = Observed value - predicted value
 *       Are we consistent ?
 *
 * @todo Reopened: The INTERCEPT is 'hyper-static', and does not take the value corresponding to the average game value.
 *       This behavior is not expected, and should be investigated further.
 *       The INTERCEPT feature taken alone does fit the average game value.
 *       What happens is that low relevant patterns ( e.g. DIAG3 ) are fitted with weights assuming very high value,
 *       all the instances belonging to a pattern take high values within a small range, and the INTERCEPT compensates
 *       this behaviour with a value having the same modulus but different size.
 *       Why this happens is not clear.
 *
 * Here an analysis run with R, dt_a is the WEIGHTS data.table computed on model A1850 (2M positions),
 * dt_b is model B1850 (4M pos.), dt_c is C1850 (6M pos.).
 * The model has all patterns and is competed on empty_count = 18.
 * V1 column is the mean of the weight grouped by the pattern_id.
 * There are two points to notice:
 *   - 1 - All the contributions are positive, with the exception of the INTERCEPT.
 *   - 2 - Mobility starts flat and grows a lot after 5. At 7 we are above 500. seems to me to much ...
 *
 * > dt_a[INDEX_VALUE==PRINCIPAL_INDEX_VALUE & TOTAL_CNT > 0, mean(WEIGHT), by = list(ENTITY_CLASS, ENTITY_ID)]
 *     ENTITY_CLASS ENTITY_ID           V1
 *  1:            0         0 -90.52614079
 *  2:            0         3   0.49295071
 *  3:            1         1   0.11353875
 *  4:            1         2   0.02384584
 *  5:            1         3   0.17677227
 *  6:            1         4   0.17841503
 *  7:            1         5   0.39537714
 *  8:            1         6  14.14114405
 *  9:            1         7   4.52611704
 * 10:            1         8   1.57065974
 * 11:            1         9   0.78761965
 * 12:            1        10   0.20141021
 * 13:            1        11   0.03084498
 * > dt_b[INDEX_VALUE==PRINCIPAL_INDEX_VALUE & TOTAL_CNT > 0, mean(WEIGHT), by = list(ENTITY_CLASS, ENTITY_ID)]
 *     ENTITY_CLASS ENTITY_ID            V1
 *  1:            0         0 -236.79886656
 *  2:            0         3    0.49370139
 *  3:            1         1    0.33835303
 *  4:            1         2    0.06915251
 *  5:            1         3    0.53053781
 *  6:            1         4    0.53416219
 *  7:            1         5    1.18507909
 *  8:            1         6   34.47111123
 *  9:            1         7   13.44621296
 * 10:            1         8    4.75240017
 * 11:            1         9    2.35388405
 * 12:            1        10    0.60000846
 * 13:            1        11    0.09081492
 * > dt_c[INDEX_VALUE==PRINCIPAL_INDEX_VALUE & TOTAL_CNT > 0, mean(WEIGHT), by = list(ENTITY_CLASS, ENTITY_ID)]
 *     ENTITY_CLASS ENTITY_ID            V1
 *  1:            0         0 -245.55161311
 *  2:            0         3    0.49274027
 *  3:            1         1    0.45952941
 *  4:            1         2    0.09265962
 *  5:            1         3    0.72139033
 *  6:            1         4    0.73226532
 *  7:            1         5    1.63074611
 *  8:            1         6   30.64418093
 *  9:            1         7   15.81527700
 * 10:            1         8    6.39677146
 * 11:            1         9    3.25066545
 * 12:            1        10    0.82053433
 * 13:            1        11    0.12244913
 *
 *
 * @todo A much more powerful mobility measure could in principle better asses positions like 35900656.
 *       This position has a value of +64, but the best evaluator with ALL the patterns give an evaluation of
 *       a DRAW.
 *       Look at the position for details ....
 *
 *       tst_regab=> SELECT game_position_pp_mop(mover, opponent, player) FROM regab_prng_gp WHERE seq = 35900656;
 *       game_position_pp_mop
 *       -----------------------
 *           a b c d e f g h   +
 *        1  . O @ @ @ @ @ @   +
 *        2  @ @ @ @ @ @ @ .   +
 *        3  @ @ @ @ @ @ @ @   +
 *        4  @ @ @ @ @ . @ .   +
 *        5  . . @ @ @ O O O   +
 *        6  . . @ . O . @ .   +
 *        7  . . . O @ @ @ O   +
 *        8  . . O . @ . . .   +
 *        Player to move: BLACK
 *       (1 row)
 *
 *       One option would be to llassify the legal moves into types.
 * @code
 * .    a    b    c    d    e    f    g    h
 *   =========================================
 * 1 =  V .. C .. A .. B .. B .. A .. C .. V =
 *   =========================================
 * 2 =  C .. X .. F .. H .. H .. F .. X .. C =
 *   =========================================
 * 3 =  A .. F .. S .. E .. E .. S .. F .. A =
 *   =========================================
 * 4 =  B .. H .. E .. O .. O .. E .. H .. B =
 *   =========================================
 * 5 =  B .. H .. E .. O .. O .. E .. H .. B =
 *   =========================================
 * 6 =  A .. F .. S .. E .. E .. S .. F .. A =
 *   =========================================
 * 7 =  C .. X .. F .. H .. H .. F .. X .. C =
 *   =========================================
 * 8 =  V .. C .. A .. B .. B .. A .. C .. V =
 *   =========================================
 * @endcode
 *       Or even to compute the adversarial possible moves in case of a "theorethical pass".
 *
 * @todo The evaluation function in the current implementation computes the expected game outcome.
 *       This is the reason why it is better to use Minimum Mean Square Error (MMSE),
 *       it has a formal matemathical theory behind the model.
 *
 *       A second model could be based on computing the probability of win. In such a model
 *       the natural algorithm to be used to compute the weights would be the Maximum
 *       Likelihood Estimation (MLE).

 *       In this second design, the logistic regression should model the probability
 *       of a dichotomous outcome. To realize it we should develop the model as an ordinal logit regression.
 *       The model could be realized ( see Agresti book ) with a sequence of dicotomous models
 *       of the type:
 *
 *         - model  0: [-64:-64] = loss , [-62:+64] = win
 *         - model  1: [-64:-62] = loss , [-60:+64] = win
 *         - model  2: [-64:-60] = loss , [-58:+64] = win
 *         - model  3: [-64:-58] = loss , [-56:+64] = win
 *         - ...
 *         - model 63: [-64:+62] = loss , [+64:+64] = win
 *
 *       The idea is to identify a Target game Value (TV) and classify the position data-set
 *       using the recorded Game True Value (GTV): Pr(GTV > TV) = Pr(y = 1) = e(w) , where
 *       e(w) is the value of the evaluation function given the vector of weights w.
 *       Solve then the GLM problem for TV in the range [-64..+62] (when TV is 64, all positions
 *       are classified as y = 0, fail, loss, defeat, ...). there are 64 regressions to carry out in
 *       order to fully profile the model.
 *       This approach, being formally sound, brings in many challenges and changes.
 *
 *       The first comment regards a formal issue that is going to emerge.
 *       There will be values of TV, most likely at the extreme of the range, that shall
 *       incur into the "Complete Separation" issue. What does it mean ?
 *       There will be feature configurations occurring only on positions classified either
 *       as win or loss. The model doesn't have a solution then, the corresponding parameters
 *       will go to plus or minus infinite. The interpretation is that the feature configuration
 *       classifies the outcome without any possible error as a win or a loss, of course given
 *       the data-set.
 *       The proposed solution is to filter these configurations from the regression.
 *       When found during the application of the model to new positions the evaluation
 *       functions has to pre-check the configurations and return immediately a value of
 *       1 or 0 accordingly.
 *       It means that these configuration has to be detected upfront, and stored in the
 *       binary file. There is a change to be applied in the REGAB program as well as in the
 *       binary file format and in the RGLM utility that read the file.
 *       The entity_freq_summary table has to be extended with the frequencies of positions
 *       by outcome value, it is an array of 65 integers for each glm variable.
 *
 *       There is a huge literatire on dealing with Complete Separation. There are basicly two reason why
 *       we could incurr in this case: the feature fully separate the model, or we have few data for rare events.
 *       The second one is the issue. Approches to cope with it follow into two main categories: prior knowledge,
 *       and Penalized Logistic Regression ( Ridge or Lasso methods ).
 *
 *       A more automated procedure is then needed to orchestrate all the regressions, storing the
 *       results ( I mean the weights ) by TV, and also the regression KPI for future understanding.
 *
 *       A complete different approach is to compute the probability mass function as a Beta distribution knowing
 *       the mean and the variance, that we know with the application of the first design.
 *       Would it be a model describing the reality in an usefull manner ?
 *
 * --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 *
 * @todo [2020-05-15 - done] There is an error in the definition of one vector of size emme instead of enne.
 *                           Change done.
 *
 * @todo [2020-05-15 - done] The vector p is not needed, most likely. Removed.
 *
 * @todo [2020-05-16 - done] There is an error in the math formula used in function rglmut_big_b_eval
 *                           The formula was correct , but written in an alternative way.
 *                           The formula has been written as in the documentation.
 *                           The definition of residual is changed from r = e - v to r = v - e.
 *
 * @todo [2020-06-19 - done] The optimization loop, that implements the Newton-Raphson algorithm
 *                           doesn't have the proper order of printf statements.
 *                           Measured quantities are computed and printed in the wrong place or order.
 *
 * @todo [2020-06-19 - done] Reorganize verbose output and variable declarations.
 *
 * @todo [2020-06-19 - done] CSV files dumped from the program are somehow redundant and not well organized.
 *                           Moreover the FATURES are not dumped ...
 *
 * @todo [2020-06-19 - done] Add the intercept parameter w_0
 *                           Add the new concept of feature.
 *                           Add to features the legal_move_count as MOBILITY, MOBILITY2, MOBILITY3 and INTERCEPT as the w_0 intercept parameter.
 *                           These three changes has been joined together.
 *
 * @todo [2020-06-21 - done] Consider if is better to remove the end_game positions.
 *                           Are strong outliers.
 *                           Were not that many! And the change reveled to be not so relevant. But doing it I have found a terrible bug in the
 *                           game_value generation, when the first move in the game position is a pass, the stack was not initialized properly
 *                           and the game espansion was truncated after the first pass move.
 *                           Also this proved not to be a game changer ... around one hundred among two millions game position were affected.
 *
 *                           The evaluation function could check if the game position is a leaf, if yes the value is the disc difference.
 *                           In this scenario having end_game position into the data set just pollutes it.
 *
 *                           An option could be:
 *                           end_game           :: special case in ef(w) , it return the true value ( as it must be ! )
 *                                                 removed from the regressed data-set
 *                           pass               :: LMOCNT = 1
 *                                                 keept in the data set
 *                           legal_move_cnt > 0 :: standard case , LMOCNT := legal_move_cnt
 *                                                 keept in the data set
 *
 * @todo [2020-07-13 - done] Move the options of the main program from static declarations
 *                           to the main function. The reason is to avoid potential collisions,
 *                           and as a general rule it is a way to increase encapsulation.
 *
 * @todo [2020-11-15 - done] Insert the RGLM_VALID_A, RGLM_VALID_B , .... values into the binary files.
 *                           They are guards to increment the robustness of the format.
 *                           Add the file format version RGLMDF_BINARY_DATA_FILE_FORMAT_VERSION.
 *
 * @todo [2020-11-22 - done] The dump of the weights is done in the same way by the -B, --extract-efs-table, and -W, --extract-weights, arguments.
 *                           In both cases the program call the rglmdf_fpfs_table_to_csv_file() function.
 *                           The difference is that the -B options dumps the CVS file before solving the GLM problem, the -W one does id after.
 *                           It is better to remove the -W option and to follow a different practice.
 *                           Solve the GLM with the -s option togeter with an -o option. Then run the rglm program again without the -s and -B options
 *                           having as input the output of the previous run.
 *                           Action: remove the -W option.
 *
 * @todo [2020-11-22 - done] The flags -T, --extract-gp-ttable, and -R, --extract-residuals, are equal.
 *                           Action: remove the -R option.
 *                           The fields extracted to the CSV file: GAME_VALUE_TRANSFORMED; EVALUATION_FUNCTION; RESIDUAL are
 *                           not populated when the GLM problem has not been resolved.
 *                           Action: populate them as soon as the first file is created.
 *
 * @todo [2020-11-28 - done] When appropriate substitutes "_pattern_" with "_entity_" into structures, and field names.
 *                           The underlining reason is the introduction of features .... we do not have any longer just patterns.
 *
 * @todo [2020-11-28 - done] The REGAB extraction with the -g option is subject to having the game position being classified.
 *                           The classification is performed after the REGAB solve and offspring actions, and is done at the
 *                           SQL prompt using the regab_gp_populate_pattern_class_table stored procedure.
 *                           Removing the dependency and adding the classify action to the REGAB program is not a priority.
 *                           Action: we keep as it is.
 *
 * @todo [2020-12-05 - done] Refactor rglm_utils removing references to the inner definition of rglmdf_general_data_t.
 *
 * @todo [2020-12-06 - done] Add documentation to rglm_utils.h
 *
 * @todo [2020-12-06 - done] Rename pattern_freq_summary into entity_freq_summary.
 *
 * @todo [2020-12-06 - done] Remove the rglmdf_iarray_data_type_t enum. Having introduced the i0array, i1array,and i2array, it doesn't have any more any meaning.
 *
 * @todo [2020-12-06 - done] The General Data structure has three principal states, described by the iarray_data_type field into the
 *                           positions table.
 *                           Values are in the range _IS_INDES, _IS_PINCIPAL_INDEX, _IS_GLM_VARIABLE_ID, _IS_MISSING.
 *                           Transitions happens calling the rglmdf_transform_piv_to_glm_variable_id() function.
 *                           This is saving space in memory and in the file at the price of:
 *                           being cumbersome, losing the history, duplicating the code to output the CSV files.
 *                           Action: add to the positions table two more "iarray like" fields, (i0array, i1array, i2array)
 *                           where i0array is the INDEX array, i1array is the PRINCIPAL INDEX array, and i2array is the GLM VAIABLE ID array.
 *                           All the fnction accessing the iarray has to be rewritten ....
 *
 * @todo [2020-12-07 - done] Remove two among the three flags -P -T -Q, they now should do the same.
 *                           The format of the game positions table is now invariant.
 *                           Action: remove flags -Q and -T from the options of the RGLM program.
 *
 * @todo [2020-12-07 - done] Use consistently int16_t, int32_t, and int64_t for the fields:
 *                           entity_class, entity_id, principal_index_value, glm_variable_id, reverse_map_a_f, reverse_map_a_p,
 *                           and reverse_map_b. And also for i0array, i1array, and i2array.
 *
 * @todo [2020-12-07 - done] Change glm_variable_id from 64 to 32 bits.
 *                           The index_value, principal_indix_value, and i.array are 32 bits, there is no reason to have
 *                           glm_variable_id being 64.
 *
 * @todo [2020-12-13 - done] Create new data structure `rglmdf_model_weights_t` to hold the computed model, having:
 *                           file_creation_time, empty_count, feature_cnt, features, pattern_cnt, patterns, and the weights table.
 *                           The table hosts: entity_class, entity_id, index_value, principal_index_value, glm_variable_id, weight.
 *
 * @todo [2020-12-13 - done] Complete the rglmdf_weight_record_t with the statistical data in the general data frequency table.
 *
 * @todo [2020-12-13 - done] Complete the rglmdf_model_weights_t with the data taken from the solved gd ( the gp count ).
 *
 * @todo [2020-12-15 - done] Write a function that "enters" the rglmdf_model_weights_t with the key:
 *                           (entity_class, entity_id, index_value) and returns a pointer to record or NULL if not found.
 *                           The function needs the appropriate reverse_map structures.
 *
 * @todo [2020-12-19 - done] Write the function that writes to a file binary the rglmdf_model_weights_t structure.
 *
 * @todo [2020-12-19 - done] Write the function that reads from file the rglmdf_model_weights_t structure.
 *
 * @todo [2020-12-19 - done] Document the new RGLMDF_MODEL_WEIGHTS file format.
 *
 * @todo [2020-12-28 - done] Write the function that writes to a CSV file the weights table contained into the rglmdf_model_weights_t structure.
 *
 * @todo [2020-12-28 - done] Verify if there is a function already written that takes a GAME POSITION and maps it into a value.
 *                           function (GP, MODEL) -> game value
 *                           Action: verify and eventually write it.
 *                           Resolution: the function was missing. it has been written: rglmut_eval_gp_using_model_weights(), part of rglm_utils module.
 *
 * @todo [2020-12-28 - done] Evaluate if it would be better to insert the checksum data into the rglm files.
 *                           Evaluated. It is better as it is now. The checksum, in order to be checked with rhash --sha3-256 <nome-file>, needs to be
 *                           external to the file itself.
 *
 * @todo [2020-12-28 - done] Functions are not commented in file rglm_utils.h
 *
 *
 * @todo [2020-12-28 - done] Write a function in rglm_data_files that reads, and one that also writes, the rglm
 *                           binary data file.
 *                           Formats for binary data files are more than one. They are:
 *                           - 'general'
 *                             Read and write functions are written.
 *                             Read function has all the tests. Write function needs the tests.
 *                             The functions has to replace the duplicated code in the clients.
 *                           - 'game-positions'
 *                             Read and write functions are the same as for the general case.
 *                           - 'weights'
 *                             A complete review is done.
 *                             The program rglmw reads the output of rglm when using the -w <file-name> option.
 *                             Weighs are generated by the rglm program using the -w, --model-weights-output-file, option.
 *                             Positions are generated by the regab pogram using the -g, --game-positions, option.
 *                           - 'hessian'
 *                             It is just a dump of the matrix using the linear_algebra utility functions. It is ok.
 *                           Add the data format type.
 *                           Should be all documented and rationalized.
 *
 * @todo [2021-01-11 - done] A regression convergence createria has to be studied. The goodness of fit statistics, either
 *                           residuals, deviance, or chi-square, or something else.
 *                           Comment: deviance would be the way to go in case of a max likelihood ordinal GLM.
 *                           But currently we are using a min residual square loss ... and most relevant overall, we are
 *                           fitting the game value, not the winning probability.
 *                           The choice at the moment is to use the standard deviation of the residual of a validation data set.
 *                           It seams to be the right choice.
 *
 * @todo [2021-01-11 - done] Complete the rglmdf_general_data_t data structure with the solution KPI (Effe, Residual mod., Gradient mod.).
 *                           We need to generate a better measure of the fitting properties of the model.
 *                           Store them in the RGLM file, read/write them.
 *                           Copy them to the rglmdf_weight_record_t structure.
 *                           Action: cancelled.
 *                           Having developed the rglm.sh and rglm_batch.sh scripts, that record all the data steps with appropriate logs
 *                           there is no stringent need to keep in the binary file the optimization history.
 *
 * @todo [2021-01-11 - done] The INTERCEPT is 'hyper-static', and does not take the value corresponding to the average game value.
 *                           This behavior is not expected, and should be investigated further.
 *                           Checked: it is not true. The INTERCEPT feature taken alone does fit the average game value.
 *                           What happens is that low relevant patterns ( e.g. DIAG3 ) are fitted with weights assuming very high value,
 *                           all the instances belonging to a pattern take high values within a small range, and the INTERCEPT compensates
 *                           this behaviour with a value having the same modulus but different size.
 *                           Why this happens is not fully clear.
 *
 *
 *
 * @brief RGLM, Revrsi Generalized Linear Model.
 * @details Solves the problem stated in the input file ...
 *
 * @par rglm.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018, 2019, 2020, 2021, 2025 Roberto Corradini. All rights reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>
#include <time.h>

#include "time_utils.h"
#include "main_option_parse.h"
#include "rglm_utils.h"
#include "linear_algebra.h"



/**
 * @cond
 */

/* Static constants. */


/* Static variables. */


/* Static functions. */

/*
 * The Kahan summation algorithm, also known as compensated summation, reduces the numerical error in the
 * total obtained by adding a sequence of finite-precision floating-point numbers, compared to the obvious approach.
 * This is done by keeping a separate running compensation (a variable to accumulate small errors).
 *
 * The formula for the variance, first computes the sample mean,
 * and then computes the sum of the squares of the differences from the mean.
 * This two pass algorithm applies the Kahan summation in both of them.
 *
 * The no-rounding-math attribute should be the default, anyway it is better to reaffirm the intention.
 *
 * On the contrary, the attribute fast-math compiles to a very different code.
 *     __attribute__((optimize ("fast-math")))
 */
__attribute__((optimize ("no-rounding-math")))
static void
rglm_residual_compute_mean_and_var (double *v,
                                    size_t n,
                                    double *mean,
                                    double *var)
{
  double s, c, m;

  if (!mean && !var) return;
  if (n == 0) {
    if (mean) *mean = 0.0;
    if (var) *var = 0.0;
    return;
  }

  /* Kahan summation algorithm. */
  s = 0.0;
  c = 0.0;
  for (size_t i = 0; i < n; i++) {
    const double y = v[i] - c;
    const double t = s + y;
    c = (t - s) - y;
    s = t;
  }
  m = s / (double) n;
  if (mean) *mean = m;

  if (!var) return;
  if (n == 1) {
    *var = 0.0;
    return;
  }

  /* Kahan summation algorithm again. */
  s = 0.0;
  c = 0.0;
  for (size_t i = 0; i < n; i++) {
    const double x = v[i] - m;
    const double z = x * x;
    const double y = z - c;
    const double t = s + y;
    c = (t - s) - y;
    s = t;
  }
  *var = s / (double) (n - 1);
}

/**
 * @endcond
 */



/**
 * @brief Main entry for the RGLM ( Reversi Generalized Linear Model ) program.
 */
int
main (int argc,
      char *argv[])
{
  int ret_code;

  /* Sets LINEBUFFER mode on stdout. It is a nice feature when using tee ... */
  ret_code = setvbuf(stdout, NULL, _IOLBF, 0);
  if (ret_code != 0) {
    fprintf(stdout, "Unable to set line-buffer mode on stdout. Exiting ... \n");
    return EXIT_FAILURE;
  }

  rglmdf_general_data_t data;
  rglmdf_general_data_init(&data);

  time_t saved_time;

  bool verbose = false;

  FILE *ofp = NULL;

  int opt;
  int oindex = -1;

  mop_options_t options;
  int h_flag = false;
  int v_flag = false;
  int s_flag = false;
  int i_flag = false;
  char *i_arg = NULL;
  int o_flag = false;
  char *o_arg = NULL;
  int t_flag = false;
  int w_flag = false;
  char *w_arg = NULL;
  int A_flag = false;
  char *A_arg = NULL;
  int B_flag = false;
  char *B_arg = NULL;
  int P_flag = false;
  char *P_arg = NULL;
  int H_flag = false;
  char *H_arg = NULL;

  mop_options_long_t opt_list[] =
    {
     {"help",                      'h', MOP_NONE},
     {"verbose",                   'v', MOP_NONE},
     {"solve",                     's', MOP_NONE},
     {"input-file",                'i', MOP_REQUIRED},
     {"output-file",               'o', MOP_REQUIRED},
     {"no-time-out-file",          't', MOP_NONE},
     {"model-weights-output-file", 'w', MOP_REQUIRED},
     {"extract-ps-table",          'A', MOP_REQUIRED},
     {"extract-efs-table",         'B', MOP_REQUIRED},
     {"extract-gp-table",          'P', MOP_REQUIRED},
     {"dump-hessian-matrix",       'H', MOP_REQUIRED},
     {0, 0, 0}
    };

  const char *documentation =
    "Usage:\n"
    "rglm [OPTION...] - Reversi Generalized Linear Model solver\n"
    "\n"
    "Options:\n"
    "  -h, --help                      Show help options\n"
    "  -v, --verbose                   Verbose output\n"
    "  -s, --solve                     Solve the Generalized Linear Model\n"
    "  -i, --input-file                Input file name - Mandatory\n"
    "  -o, --output-file               Output file name\n"
    "  -t, --no-time-out-file          Write a 'zero time' in the output file\n"
    "  -w, --model-weights-output-file RGLM parameters values output file name\n"
    "  -A, --extract-ps-table          Extract the position summary table in a CSV format\n"
    "  -B, --extract-efs-table         Extract the entity (feature and pattern) frequency summary table in a CSV format\n"
    "  -P, --extract-gp-table          Extract the solved and classified game position table in a CSV format, with original pattern indexes\n"
    "  -H, --dump-hessian-matrix       Dump the unresolved Hessian matrix to a binary file and exits\n"
    "\n"
    "Description:\n"
    "The Reversi Generalized Linear Model solver is the main entry to a group of utilities dedicated to process a set of solved and classified game position retrieved from a binary imput file.\n"
    "\n"
    "Author:\n"
    "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
    "\n"
    "Copyright (c) 2018, 2019, 2020, 2021 Roberto Corradini. All rights reserved.\n"
    "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
    "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
    ;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, opt_list, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
      break;
    case 's':
      s_flag = true;
      break;
    case 'i':
      i_flag = true;
      i_arg = options.optarg;
      break;
    case 'o':
      o_flag = true;
      o_arg = options.optarg;
      break;
    case 't':
      t_flag = true;
      break;
    case 'w':
      w_flag = true;
      w_arg = options.optarg;
      break;
    case 'A':
      A_flag = true;
      A_arg = options.optarg;
      break;
    case 'B':
      B_flag = true;
      B_arg = options.optarg;
      break;
    case 'P':
      P_flag = true;
      P_arg = options.optarg;
      break;
    case 'H':
      H_flag = true;
      H_arg = options.optarg;
      break;
    case ':':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return -1;
    case '?':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return -2;
    default:
      fprintf(stderr, "Unexpectd error parsing the arguments. Aborting ...\n");
      abort();
    }
  }

  /* Outputs verbose comments. */
  if (v_flag) verbose = true;

  /* Prints documentation and returns, when help option is active. */
  if (h_flag) {
    fprintf(stderr, "%s", documentation);
    return 0;
  }

  /* Checks command line options for consistency. */
  if (!i_flag) {
    fprintf(stderr, "Option -i, --input-file is mandatory.\n");
    return -3;
  }

  /* Checks command line options for consistency: t flag requires o flag.*/
  if (t_flag) {
    if (!o_flag) {
      fprintf(stderr, "Option -t, --no-time-out-file requires option -o, --out-file.\n");
      return EXIT_FAILURE;
    }
  }

  /* Checks command line options for consistency: H flag is exclusive. */
  if (H_arg) {
    if (o_flag || w_flag || A_flag || B_flag || P_flag || s_flag ) {
      fprintf(stderr, "Option -H, --dump-hessian-matrix is not compatible with other selected flags.\n");
      return -4;
    }
  }

  /* Checks size of types. */
  if (!rglmdf_verify_type_sizes()) {
    fprintf(stderr, "Data types read from, or written to, the binary file have a size that is not consistent with the original one.\n");
    return EXIT_FAILURE;
  }

  /* Reads the binary input file. */
  ret_code = rglmdf_read_general_data_from_binary_file(&data, i_arg, verbose);
  if (ret_code != EXIT_SUCCESS) {
      fprintf(stderr, "Unable to read properly the binary input file: %s\n", i_arg);
      return EXIT_FAILURE;
  }
  if (s_flag) {
    rglmdf_file_data_format_type_t format = rglmdf_get_format(&data);
    if (format != RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL) {
      fprintf(stderr, "When the -s, --solve, option is selected the format of the data file must be GENERAL.\n");
      return EXIT_FAILURE;
    }
  }

  /* If A flag is turned on, dumps the position summary table to the output file. */
  if (A_arg) {
    ofp = fopen(A_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", A_arg);
      return EXIT_FAILURE;
    }
    rglmdf_ps_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Position summary table dumped to CSV file: \"%s\".\n", A_arg);
  }

  /* If B flag is turned on, dumps the feature and pattern frequency summary table to the output file. */
  if (B_arg) {
    ofp = fopen(B_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", B_arg);
      return EXIT_FAILURE;
    }
    rglmdf_fpfs_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Feature and pattern frequencies summary table dumped to CSV file: \"%s\".\n", B_arg);
  }

  /* If P flag is turned on, dumps the game position table to the output file. */
  if (P_arg) {
    ofp = fopen(P_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", P_arg);
      return EXIT_FAILURE;
    }
    rglmdf_gp_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Game positions dumped to CSV file: \"%s\".\n", P_arg);
  }

  /*
   *
   * Ready to compute the weights ...
   *
   */

  if (s_flag || H_flag) {

    /* Max number of iterations allowed to the Newton-Raphson algorithm. */
    size_t max_newton_iter;

    /* Stopwatch variables. */
    timespec_t time_0, time_1, delta_cpu_time, start_time, end_time, delta_time;

    /* All the Algorithm variables. */
    double *v, *w, *e, *de, *r, *minus_grad_f, **big_b;
    double effe, effe_last, delta_effe, lambda, grad_magnitude, g_abs_min, g_abs_max, r_magnitude, p_magnitude;
    double r_abs_min, r_abs_max, p_abs_min, p_abs_max;
    size_t r_abs_min_pos, r_abs_max_pos, g_abs_min_pos, g_abs_max_pos, p_abs_min_pos, p_abs_max_pos;
    double ridge_reg_param, effe_residuals, effe_ridge;

    double epsilon_on_gradient_modulus;

    bool evf_already_computed;
    bool cholesky_fact_ok;

    /* enne: number of parameters to be fitted. */
    const size_t enne = data.entity_freq_summary.ntuples;

    /* emme: number of solved and classified game positions. */
    const size_t emme = data.positions.ntuples;

    /*
     * DPOTRF/DPOTRS - Cholesky factorization and solution parameters.
     * Are all constant.
     */
    char chol_upper = 'L'; // dpotrf follows the fortran convention of column major storage, so L translated in raw major mode means upper.
    int ret = 0;           // Function return value. 0 means NO ERROR.
    int enne_int = enne;   // The rank of the matrix need to be passed as a pointer to int.
    int nrhs = 1;          // Number of right hand side columns.
    int tc = 4;            // Thread count.

    /* v: transformd game value for each solved position. */
    v = lial_allocate_vector(emme);
    if (!v) abort();
    rglmut_gv_init(&data, emme, v);

    /* w: weigths.*/
    w = lial_allocate_vector(enne);
    if (!w) abort();
    for (size_t i = 0; i < enne; i++) w[i] = data.entity_freq_summary.records[i].weight;

    /* e: evaluation function for the game positions.*/
    e = lial_allocate_vector(emme);
    if (!e) abort();

    /* de: derivative of the evaluation function for the game positions, de = e * (1 - e).*/
    de = lial_allocate_vector(emme);
    if (!de) abort();

    /* r: residual value for the game positions, r = e - v.*/
    r = lial_allocate_vector(emme);
    if (!r) abort();

    /* minus_grad_f: right hand side of the liner system solved at each iteration of the Newton-Raphson algorithm. */
    minus_grad_f = lial_allocate_vector(enne);
    if (!minus_grad_f) abort();

    /* big_b: square matrix generated by the matrix multiplication jacobian transposed * jacobian. */
    big_b = lial_allocate_square_matrix(enne);
    if (!big_b) abort();

    /* Computes the constant v vector. */
    rglmut_gv_init(&data, emme, v);

    /* lambda: scalar parameter used by the Levemberg-Marquardt algorith. */
    lambda = 1.e-9;

    /* Termination criteria. */
    epsilon_on_gradient_modulus = 1.0e-9;

    /* max_newton_iter: max number of iterations allowed to the Newton-Raphson algorithm. */
    max_newton_iter = 12;

    /* At the beginning of the first iteration the evaluation function is not already computed. */
    evf_already_computed = false;

    /* At the beginning of the first iteration the flag is set to true. */
    cholesky_fact_ok = true;

    /* Ridge regularization coefficient. */
    ridge_reg_param = 0.01;

    if (verbose) {
      printf("Dumping factor for the diagonal of the Hessian matrix (Levemberg-Marquardt): lambda = %e\n", lambda);
      printf("Ridge regularization coefficient: ridge_reg_param = %e\n", ridge_reg_param);
      printf("Max number of Newton-Raphson algorithm iterations = %zu\n", max_newton_iter);
      printf("Termination criteria:\n");
      printf("   Epsilon on modulus of gradient: %e\n", epsilon_on_gradient_modulus);
    }

    for (size_t iter = 0; iter < max_newton_iter; iter++) {

      if (verbose) {
        printf("Iteration[%03zu]:\n", iter);
        if (!cholesky_fact_ok) printf("   New value assigned to the lambda parameter: lambda = %e\n", lambda);
      }

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      if (!evf_already_computed) {
        rglmut_evaluation_function_eval(&data, enne, w, emme, e);
        rglmut_evaluation_function_derivative_eval(emme, e, de);
        rglmut_residual_value_eval(emme, e, v, r);
        rglmut_minus_grad_f_eval(&data, minus_grad_f, enne, emme, r, de, w, ridge_reg_param);
        effe_residuals = 0.0;
        effe_ridge = 0.0;
        for (size_t i = 0; i < emme; i++) effe_residuals += r[i]*r[i];
        for (size_t i = 0; i < enne; i++) effe_ridge += w[i]*w[i];
        effe = 0.5 * (effe_residuals + ridge_reg_param * effe_ridge);
        r_magnitude = lial_vector_magnitude(r, emme, &r_abs_min, &r_abs_min_pos, &r_abs_max, &r_abs_max_pos);
        grad_magnitude = lial_vector_magnitude(minus_grad_f, enne, &g_abs_min, &g_abs_min_pos, &g_abs_max, &g_abs_max_pos);
      }

      rgmlut_big_b_eval(&data, big_b, enne, emme, e, de, v);

      /* Increases the diagonal of the Hessian matrix. */
      for (size_t i = 0; i < enne; i++) big_b[i][i] += lambda + ridge_reg_param;

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
      clock_gettime(CLOCK_REALTIME, &end_time);

      if (verbose) {
        double r_mean, r_var, r_sd;
        rglm_residual_compute_mean_and_var(r, emme, &r_mean, &r_var);
        r_sd = sqrt(r_var);
        printf("   Residual: mean = %15.12f, variance = %15.12f, standard deviation = %15.12f (%5.2f) [%2d]\n", r_mean, r_var, r_sd, rglmut_gv_scale_back_f(0.5 + r_sd), rglmut_gv_scale_back_i(0.5 + r_sd));
        printf("   Effe             = %30.18f\n", effe);
        printf("   Residual modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", r_magnitude, r_abs_min, r_abs_min_pos, r_abs_max, r_abs_max_pos);
        printf("   Gradient modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", grad_magnitude, g_abs_min, g_abs_min_pos, g_abs_max, g_abs_max_pos);
      }

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&delta_time, &start_time, &end_time);
      timespec_diff(&delta_cpu_time, &time_0, &time_1);
      if (verbose) {
        printf("   Function, Gradient, Hessian, and Residual evaluation CPU time: ");
        printf("[%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
               (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
      }

      /* Dumps the modified Hessian matrix and terminates if flag H is on. */
      if (H_arg) {
        /* Copies the upper triangle into the lower one. */
        for (size_t i = 1; i < enne; i++) {
          for (size_t j = 0; j < i; j++) {
            big_b[i][j] = big_b[j][i];
          }
        }
        int h_ret_code;
        lial_dump_matrix(big_b, enne, enne, H_arg, &h_ret_code);
        if (h_ret_code != 0) abort();
        if (verbose) fprintf(stdout, "Hessian matrix dumped to binary file: \"%s\".\n", H_arg);
        goto end_of_optimization_loop;
      }

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Factorizes the symmetrical Hessian matrix applying the Cholesky decomposition. */
      lial_dpotrf_blis(&chol_upper, &enne_int, *big_b, &enne_int, &ret, tc);
      if (ret != 0) {
        cholesky_fact_ok = false;
        lambda *= 10.0;
      } else {
        cholesky_fact_ok = true;
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
      clock_gettime(CLOCK_REALTIME, &end_time);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&delta_time, &start_time, &end_time);
      timespec_diff(&delta_cpu_time, &time_0, &time_1);
      if (verbose) {
        printf("   Cholesky Factorization %s, CPU time:                           ", cholesky_fact_ok ? "ok" : "ko");
        printf("[%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
               (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
      }
      if (!cholesky_fact_ok) continue;

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Solve the linear system by back substitution. */
      lial_dpotrs(&chol_upper, &enne_int, &nrhs, *big_b, &enne_int, minus_grad_f, &enne_int, &ret);
      if (ret != 0) {
        fprintf(stderr, "lial_dpotrs return code = %d, aborting ...", ret);
        abort();
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
      clock_gettime(CLOCK_REALTIME, &end_time);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&delta_time, &start_time, &end_time);
      timespec_diff(&delta_cpu_time, &time_0, &time_1);
      if (verbose) {
        printf("   Cholesky Solution CPU time:                                    ");
        printf("[%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
               (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
      }

      p_magnitude = lial_vector_magnitude(minus_grad_f, enne, &p_abs_min, &p_abs_min_pos, &p_abs_max, &p_abs_max_pos);
      if (verbose) printf("   Delta w modulus  = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", p_magnitude, p_abs_min, p_abs_min_pos, p_abs_max, p_abs_max_pos);

      /* Updates the vector of weights with the delta. */
      for (size_t i = 0; i < enne; i++) w[i] += minus_grad_f[i];

      /* After the update of w, re-computes e, de, r, and minus_grad_f. */
      rglmut_evaluation_function_eval(&data, enne, w, emme, e);
      rglmut_evaluation_function_derivative_eval(emme, e, de);
      rglmut_residual_value_eval(emme, e, v, r);
      rglmut_minus_grad_f_eval(&data, minus_grad_f, enne, emme, r, de, w, ridge_reg_param);
      effe_last = effe;
      effe_residuals = 0.0;
      effe_ridge = 0.0;
      for (size_t i = 0; i < emme; i++) effe_residuals += r[i]*r[i];
      for (size_t i = 0; i < enne; i++) effe_ridge += w[i]*w[i];
      effe = 0.5 * (effe_residuals + ridge_reg_param * effe_ridge);
      delta_effe = effe_last - effe;

      r_magnitude = lial_vector_magnitude(r, emme, &r_abs_min, &r_abs_min_pos, &r_abs_max, &r_abs_max_pos);
      grad_magnitude = lial_vector_magnitude(minus_grad_f, enne, &g_abs_min, &g_abs_min_pos, &g_abs_max, &g_abs_max_pos);

      if (verbose) printf("   Delta Effe       = %30.18f\n", delta_effe);

      evf_already_computed = true;

      if (grad_magnitude <= epsilon_on_gradient_modulus) {
        break;
      }
    }

    if (verbose) {
      r_magnitude = lial_vector_magnitude(r, emme, &r_abs_min, &r_abs_min_pos, &r_abs_max, &r_abs_max_pos);
      grad_magnitude = lial_vector_magnitude(minus_grad_f, enne, &g_abs_min, &g_abs_min_pos, &g_abs_max, &g_abs_max_pos);
      if (grad_magnitude <= epsilon_on_gradient_modulus) {
        printf("Termination criteria reached:\n");
      } else {
        printf("Max number of iterations reached:\n");
      }
      double r_mean, r_var, r_sd;
      rglm_residual_compute_mean_and_var(r, emme, &r_mean, &r_var);
      r_sd = sqrt(r_var);
      printf("   Residual: mean = %15.12f, variance = %15.12f, standard deviation = %15.12f (%5.2f) [%2d]\n", r_mean, r_var, r_sd, rglmut_gv_scale_back_f(0.5 + r_sd), rglmut_gv_scale_back_i(0.5 + r_sd));
      printf("   Effe             = %30.18f\n", effe);
      printf("   Residual modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", r_magnitude, r_abs_min, r_abs_min_pos, r_abs_max, r_abs_max_pos);
      printf("   Gradient modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", grad_magnitude, g_abs_min, g_abs_min_pos, g_abs_max, g_abs_max_pos);
    }

  end_of_optimization_loop:
    ;

    /* Copies the optimized weighs into the general data structure. */
    for (size_t i = 0; i < enne; i++) data.entity_freq_summary.records[i].weight = w[i];

    /* Copies residual and game value into the general data structure. */
    rglmut_evaluation_function_eval(&data, enne, w, emme, e);
    rglmut_residual_value_eval(emme, e, v, r);
    for (size_t i = 0; i < emme; i++) {
      data.positions.records[i].evaluation_function = e[i];
      data.positions.records[i].residual = r[i];
    }

    lial_free_matrix(big_b, enne);
    lial_free_vector(minus_grad_f);
    lial_free_vector(r);
    lial_free_vector(de);
    lial_free_vector(e);
    lial_free_vector(w);
    lial_free_vector(v);

  }

  /*
   *
   * Weights computed.
   *
   */

  /* Writes the binary output file. */
  if (o_arg) {
    saved_time = t_flag ? (time_t) 0 : time(NULL);
    ret_code = rglmdf_write_general_data_to_binary_file(&data, o_arg, saved_time);
    if (ret_code == EXIT_SUCCESS) {
      if (verbose) fprintf(stdout, "Binary output file written to %s, computed SHA3-256 digest, written to file %s.sha3-256.\n", o_arg, o_arg);
    } else {
      fprintf(stderr, "Unable to write correctly binary output file: %s\n", o_arg);
      return ret_code;
    }
  }

  /* Writes the model weights binary file. */
  if (w_arg) {
    rglmdf_model_weights_t mw;
    rglmdf_model_weights_init(&mw);
    ret_code = rglmdf_model_veights_load(&mw, &data);
    if (ret_code == EXIT_FAILURE) {
      fprintf(stderr, "Unable to load the RGLM model weights data structure.\n");
      return ret_code;
    }
    saved_time = t_flag ? (time_t) 0 : time(NULL);
    ret_code = rglmdf_model_weights_write_to_binary_file(&mw, w_arg, saved_time);
    if (ret_code == EXIT_SUCCESS) {
      if (verbose) fprintf(stdout, "RGLM model weights binary file written to %s\n", w_arg);
    } else {
      fprintf(stderr, "Unable to write correctly RGLM model weights binary file: %s\n", w_arg);
      return ret_code;
    }
    rglmdf_model_weights_release(&mw);
  }

  /* Frees resources. */
  rglmdf_general_data_release(&data);

  return EXIT_SUCCESS;
}

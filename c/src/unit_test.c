/**
 * @file
 *
 * @todo Implement the -v (verbose) and -q (quiet) modes.
 *
 * @todo Implement the -m flag standard|perf.
 *
 * @todo Implement the standard test, having the data pointer, and the fixture
 *       prepare and terdown functions.
 *
 * @todo Verify that tests behave correctly when assertions fails or the program aborts.
 *
 * @todo Complete the utest program.
 *
 * @todo Write unit tests for the ut module.
 *
 *
 *
 * @brief Unit test module implementation.
 *
 * @details This module defines a suite entity and a test entity, respectively
 *          #ut_suite_t, and #ut_test_t.
 *          A suite is a collection of tests, it is created by calling #ut_suite_new.
 *          Test are added to the suite by calling
 *          the function #ut_suite_add_simple_test, that allocates the memory for
 *          the test.
 *
 *
 * @par unit_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <string.h>

#include "unit_test.h"



/******************************************************/
/* Internal static constant declarations.             */
/******************************************************/

static const timespec_t ut_quickness_ranges[] =
  {
    {          0,   1000000 }, // UT_QUICKNESS_0001
    {          0,  10000000 }, // UT_QUICKNESS_001
    {          0, 100000000 }, // UT_QUICKNESS_01
    {          1,         0 }, // UT_QUICKNESS_1
    {         10,         0 }, // UT_QUICKNESS_10
    {        100,         0 }, // UT_QUICKNESS_100
    {       1000,         0 }, // UT_QUICKNESS_1000
    { 3153600000,         0 }  // UT_QUICKNESS_OUT_OF_RANGE - 100 years
  };

/******************************************************/
/* Internal static variable declarations.             */
/******************************************************/

static ut_prog_arg_config_t arg_config;



/******************************************************/
/* Internal function declarations.                    */
/******************************************************/

static void
parse_args (int *argc_p,
            char ***argv_p);

static unsigned long
ut_suite_full_path_max_length (ut_suite_t *s);



/***********************************************************/
/* Function implementations for the ut_quickness_t entity. */
/***********************************************************/

/**
 * @brief Returns the proper speed class.
 *
 * @invariant Parameter `ts` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] ts the value to be categorized
 * @return        the proper quickness class
 */
ut_quickness_t
ut_quickness_range (const timespec_t *const ts)
{
  assert(ts);

  const timespec_t *const r = ut_quickness_ranges;

  int rc = 0; //range_class

  for (; rc < UT_QUICKNESS_OUT_OF_RANGE; rc++) {
    if (timespec_get_sec(ts) <  timespec_get_sec(&r[rc])) break;
    if (timespec_get_sec(ts) == timespec_get_sec(&r[rc]) && timespec_get_nsec(ts) < timespec_get_nsec(&r[rc])) break;
  }

  return rc;
}

timespec_t
ut_quickness_boundary (const ut_quickness_t q)
{
  assert(q >= 0);
  assert(q <= UT_QUICKNESS_OUT_OF_RANGE);
  return ut_quickness_ranges[q];
}



/******************************************************/
/* Function implementations for the ut_test_t entity. */
/******************************************************/

/**
 * @brief Test structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * test structure is not `NULL`.
 *
 * @return a pointer to a new test structure
 */
ut_test_t *
ut_test_new (char *label,
             ut_simple_test_f tfun,
             ut_mode_t mode,
             ut_quickness_t speed,
             ut_suite_t *s)
{
  ut_test_t *t;
  static const size_t size_of_t = sizeof(ut_test_t);
  t = (ut_test_t *) malloc(size_of_t);
  assert(t);
  t->failure_count = 0;
  t->assertion_count = 0;
  t->label = label;
  t->test = tfun;
  t->suite = s;
  t->mode = mode;
  t->speed = speed;
  timespec_set(&t->duration, 0, 0);
  return t;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #ut_test_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] t the pointer to be deallocated
 */
void
ut_test_free (ut_test_t *t)
{
  free(t);
}



/*******************************************************/
/* Function implementations for the ut_suite_t entity. */
/*******************************************************/

/**
 * @brief The array reallocation increase.
 */
static const size_t array_alloc_chunk_size = 2;

/**
 * @brief Suite structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * suite structure is not `NULL`.
 *
 * @return a pointer to a new suite structure
 */
ut_suite_t *
ut_suite_new (char *label)
{
  ut_suite_t *s;
  static const size_t size_of_t = sizeof(ut_suite_t);
  s = (ut_suite_t *) malloc(size_of_t);
  assert(s);
  s->label = label;
  s->count = 0;
  s->size = array_alloc_chunk_size;
  s->tests = (void *) malloc(s->size * sizeof(void *));
  assert(s->tests);
  return s;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #ut_suite_new.
 *
 * @details Deallocates also all the referenced tests.
 *          If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] s the pointer to be deallocated
 */
void
ut_suite_free (ut_suite_t *s)
{
  for (int i = 0; i < s->count; i++) {
    ut_test_t **tests_p = s->tests + i;
    ut_test_free(*tests_p);
  }
  free(s->tests);
  free(s);
}

/**
 * @brief Adds a simple test to the suite.
 *
 * @param [in,out] s     the test suite
 * @param [in]     mode  the test mode
 * @param [in]     speed the test expected quickness
 * @param [in]     label the test label
 * @param [in]     tfun  the the test function
 */
void
ut_suite_add_simple_test (ut_suite_t *s,
                          ut_mode_t mode,
                          ut_quickness_t speed,
                          char *label,
                          ut_simple_test_f tfun)
{
  assert(s);
  assert(label);
  assert(tfun);

  ut_test_t *const t = ut_test_new(label, tfun, mode, speed, s);

  if (s->count == s->size) {
    s->size += array_alloc_chunk_size;
    s->tests = (void *) realloc(s->tests, s->size * sizeof(void *));
    assert(s->tests);
  }

  ut_test_t **tests_p = s->tests + s->count;
  *tests_p = t;
  s->count++;
}

/**
 * @brief Runs all tests contained by the suite.
 *
 * @param [in,out] s the test suite
 * @return           the count of failed tests
 */
int
ut_suite_run (ut_suite_t *s)
{
  if (!s) return 0;

  timespec_t time_0, time_1;
  int ret;

  const unsigned long res_msg_def_print_column = 80;
  const unsigned long len = ut_suite_full_path_max_length(s);
  const unsigned long res_msg_print_column = (res_msg_def_print_column > len) ? res_msg_def_print_column : len;

  char *full_path = (char *) malloc((len + 1) * sizeof(char));
  assert(full_path);

  for (int i = 0; i < s->count; i++) {
    ut_test_t *t = *(s->tests + i);

    sprintf(full_path, "/%s/%s", s->label, t->label);
    bool selected = false;
    if (llist_length(arg_config.test_paths) == 0) selected = true;
    for (llist_elm_t *e = arg_config.test_paths->head; e; e = e->next) {
      char *test_path = (char *)(e->data);
      char *match = strstr(full_path, test_path);
      if (match == full_path) {
        selected = true;
        break;
      }
    }
    for (llist_elm_t *e = arg_config.skip_paths->head; e; e = e->next) {
      char *skip_path = (char *)(e->data);
      char *match = strstr(full_path, skip_path);
      if (match == full_path) {
        selected = false;
        break;
      }
    }

    if (selected) {
      if (arg_config.print_test_list) { /* Lists the test. */
        if (arg_config.utest) fprintf(stdout, "  ");
        fprintf(stdout, "%s\n", full_path);
      } else { /* Runs the test. */
        if (arg_config.utest) fprintf(stdout, "  ");
        fprintf(stdout, "%s: ", full_path);
        fflush(stdout);

        /* Starts the stop-watch. */
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

        t->test(t);

        /* Stops the stop-watch. */
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

        /* Computes the time taken, and updates the test duration. */
        ret = timespec_diff(&t->duration, &time_0, &time_1);
        (void) ret; assert(ret == 0);

        fprintf(stdout, "%*c", (int)(res_msg_print_column - strlen(full_path)), ' ');
        fprintf(stdout, "[%6lld.%9ld] ", (long long) timespec_get_sec(&t->duration), timespec_get_nsec(&t->duration));
        const ut_quickness_t actual_speed = ut_quickness_range(&t->duration);
        const int time_perf = (actual_speed > t->speed) - (actual_speed < t->speed);
        char time_perf_c = ' ';
        if (time_perf > 0) time_perf_c = '+';
        if (time_perf < 0) time_perf_c = '-';
        fprintf(stdout, "[%d.%d]%c ", t->speed, actual_speed, time_perf_c);
        if (t->failure_count) {
          s->failed_test_count++;
          fprintf(stdout, "FAILED - Failure count = %d\n", t->failure_count);
        } else {
          fprintf(stdout, "OK\n");
        }
      }
    }

  }

  free(full_path);

  return s->failed_test_count;
}



/********************************************/
/* Assertion implementations.               */
/********************************************/

/**
 * @brief When assertion is not true, the count of failures fot the test is increased.
 *
 * @param [in,out] t         the test case
 * @param [in,out] assertion the result of the assertion
 */
void
ut_assert (ut_test_t *t,
           int assertion)
{
  t->assertion_count++;
  if (!assertion) t->failure_count++;
}

/********************************************/
/* Module functions.                        */
/********************************************/

/**
 * @brief Has to be called by main as the first step for running the test suite.
 *
 * @param [in]     argc_p address of the argc parameter of the main() function.
 * @param [in,out] argv_p address of the argv parameter of the main() function.
 */
void
ut_init (int *argc_p,
         char ***argv_p)
{
  parse_args(argc_p, argv_p);
  return;
}

/**
 * @brief Returns true when performance tests are included in the run.
 *
 * @return true if test is in performance mode.
 */
bool
ut_is_mode_equal_to_perf (void)
{
  return arg_config.mode >= UT_MODE_PERF_0 ? true : false;
}



/********************************************/
/* Internal functions.                      */
/********************************************/

static void
parse_args (int *argc_p,
            char ***argv_p)
{
  int argc = *argc_p;
  char **argv = *argv_p;

  arg_config.print_test_list = false;
  arg_config.mode = UT_MODE_STND;
  arg_config.test_paths = llist_new(NULL);
  arg_config.skip_paths = llist_new(NULL);
  arg_config.verb = UT_VEROSITY_STND;
  arg_config.utest = false;

  /* Parses known args. */
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-l") == 0) {
      arg_config.print_test_list = true;
      argv[i] = NULL;
    } else if (strcmp("-m", argv[i]) == 0 || strncmp("-m=", argv[i], 3) == 0) {
      char *equal = argv[i] + 2;
      char *mode = NULL;
      if (*equal == '=')
        mode = equal + 1;
      else if (i + 1 < argc) {
        argv[i++] = NULL;
        mode = argv[i];
      } else {
        fprintf(stderr, "%s: missing mode value after -m flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      if (strcmp(mode, "perf") == 0) arg_config.mode = UT_MODE_PERF_0;
      else if (strcmp(mode, "standard") == 0) arg_config.mode = UT_MODE_STND;
      else {
        fprintf(stderr, "%s: mode value \"%s\" is invalid.\n", argv[0], mode);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
    } else if (strcmp("-p", argv[i]) == 0 || strncmp ("-p=", argv[i], 3) == 0) {
      char *equal = argv[i] + 2;
      char *test_path = NULL;
      if (*equal == '=')
        test_path = equal + 1;
      else if (i + 1 < argc) {
        argv[i++] = NULL;
        test_path = argv[i];
      } else {
        fprintf(stderr, "%s: missing TESTPATH value after -p flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
      if (test_path) llist_add(arg_config.test_paths, test_path);
    } else if (strcmp("-s", argv[i]) == 0 || strncmp ("-s=", argv[i], 3) == 0) {
      char *equal = argv[i] + 2;
      char *skip_path = NULL;
      if (*equal == '=')
        skip_path = equal + 1;
      else if (i + 1 < argc) {
        argv[i++] = NULL;
        skip_path = argv[i];
      } else {
        fprintf(stderr, "%s: missing TESTPATH value after -s flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
      if (skip_path) llist_add(arg_config.skip_paths, skip_path);
    } else if (strcmp("-q", argv[i]) == 0 || strcmp("--quiet", argv[i]) == 0) {
      arg_config.verb = UT_VEROSITY_LOW;
      argv[i] = NULL;
    } else if (strcmp("-v", argv[i]) == 0 || strcmp("--verbose", argv[i]) == 0) {
      arg_config.verb = UT_VEROSITY_HIGHT;
      argv[i] = NULL;
    } else if (strcmp("--utest", argv[i]) == 0) {
      arg_config.utest = true;
      argv[i] = NULL;
    } else if (strcmp("-?", argv[i]) == 0 ||
               strcmp("-h", argv[i]) == 0 ||
               strcmp("--help", argv[i]) == 0) {
      fprintf(stdout,
              "Usage:\n"
              "  %s [OPTION...]\n\n"
              "Help Options:\n"
              "  -h, --help                  Show help options\n\n"
              "Test Options:\n"
              "  -l                          List test cases available in a test executable\n"
              "  -m {perf|standard}          Execute tests according to mode\n"
              "  -p TESTPATH                 Only start test cases matching TESTPATH\n"
              "  -s TESTPATH                 Skip all tests matching TESTPATH\n"
              "  -q, --quiet                 Run tests quietly\n"
              "  -v, --verbose               Run tests verbosely\n",
              argv[0]);
      exit(0);
    }
  }

  /* Packs argv removing null values. */
  int count = 1;
  for (int i = 1; i < argc; i++) {
    if (argv[i]) {
      argv[count++] = argv[i];
      if (i >= count)
        argv[i] = NULL;
    }
  }
  *argc_p = count;

}

static unsigned long
ut_suite_full_path_max_length (ut_suite_t *s)
{
  unsigned long max_label_len = 0;
  unsigned long full_path_len = 2 + strlen(s->label);
  for (int i = 0; i < s->count; i++) {
    ut_test_t *t = *(s->tests + i);
    unsigned long t_label_len = strlen(t->label);
    if (t_label_len > max_label_len) max_label_len = t_label_len;
  }
  full_path_len += max_label_len;
  return full_path_len;
}

/**
 * @file
 *
 * @todo [done] Implement the -v (verbose) and -q (quiet) modes.
 *
 * @todo [done] Implement the -m flag standard|perf.
 *
 * @todo [done] Implement the standard test, having the data pointer, and the fixture
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

#include "main_option_parse.h"
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



/******************************************************/
/* Internal function declarations.                    */
/******************************************************/

static unsigned long
ut_suite_full_path_max_length (ut_suite_t *s);

static ut_test_t *
ut_test_new (char *label,
             ut_test_f tfun,
             ut_mode_t mode,
             ut_quickness_t qck_class,
             ut_suite_t *s);

static void
ut_test_free (ut_test_t *t);




/***********************************************************/
/* Function implementations for the ut_quickness_t entity. */
/***********************************************************/

/**
 * @brief Returns the proper quickness class.
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
 * @brief Raises the failure count for the test.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] t the pointer to the test
 */
void
ut_test_fail (ut_test_t *const t)
{
  if (t) t->failure_count++;
}



/*******************************************************/
/* Function implementations for the ut_suite_t entity. */
/*******************************************************/

/**
 * @brief The array reallocation increase.
 */
static const size_t array_alloc_chunk_size = 16;

/**
 * @brief Suite structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * suite structure is not `NULL`.
 *
 * @param [in] config program runtime configuration
 * @param [in] label  suite label
 * @return            a pointer to a new suite structure
 */
ut_suite_t *
ut_suite_new (ut_prog_arg_config_t *config,
              char *label)
{
  ut_suite_t *s;
  static const size_t size_of_t = sizeof(ut_suite_t);
  s = (ut_suite_t *) malloc(size_of_t);
  assert(s);
  s->label = label;
  s->config = config;
  s->count = 0;
  s->size = array_alloc_chunk_size;
  s->tests = (void *) malloc(s->size * sizeof(void *));
  assert(s->tests);
  s->failed_test_count = 0;
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
  if (s) {
    for (int i = 0; i < s->count; i++) {
      ut_test_t **tests_p = s->tests + i;
      ut_test_free(*tests_p);
    }
    free(s->tests);
    if (s->config) {
      if (s->config->test_paths) llist_free(s->config->test_paths);
      if (s->config->skip_paths) llist_free(s->config->skip_paths);
    }
    free(s);
  }
}

/**
 * @brief Adds a simple test to the suite.
 *
 * @param [in,out] s         the test suite
 * @param [in]     mode      the test mode
 * @param [in]     qck_class the test expected quickness
 * @param [in]     label     the test label
 * @param [in]     tfun      the the test function
 * @return                       the newly created test
 */
ut_test_t *
ut_suite_add_simple_test (ut_suite_t *s,
                          ut_mode_t mode,
                          ut_quickness_t qck_class,
                          char *label,
                          ut_test_f tfun)
{
  assert(s);
  assert(label);
  assert(tfun);

  ut_test_t *const t = ut_test_new(label, tfun, mode, qck_class, s);

  if (s->count == s->size) {
    s->size += array_alloc_chunk_size;
    s->tests = (void *) realloc(s->tests, s->size * sizeof(void *));
    assert(s->tests);
  }

  ut_test_t **tests_p = s->tests + s->count;
  *tests_p = t;
  s->count++;

  return t;
}

/**
 * @brief Adds a test to the suite.
 *
 * @param [in,out] s             the test suite
 * @param [in]     mode          the test mode
 * @param [in]     qck_class     the test expected quickness
 * @param [in]     label         the test label
 * @param [in]     provided_data the user provided data
 * @param [in]     setup         the setup function
 * @param [in]     tfun          the test function
 * @param [in]     teardown      the teardown function
 * @return                       the newly created test
 */
ut_test_t *
ut_suite_add_regular_test (ut_suite_t *s,
                           ut_mode_t mode,
                           ut_quickness_t qck_class,
                           char *label,
                           const void *const provided_data,
                           ut_fixture_setup_f setup,
                           ut_test_f tfun,
                           ut_fixture_teardown_f teardown)
{
  ut_test_t *const t = ut_suite_add_simple_test(s, mode, qck_class, label, tfun);
  t->setup = setup;
  t->teardown = teardown;
  t->provided_data = (void *) provided_data;
  return t;
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
    if (llist_length(s->config->test_paths) == 0) selected = true;
    for (llist_elm_t *e = s->config->test_paths->head; e; e = e->next) {
      char *test_path = (char *)(e->data);
      char *match = strstr(full_path, test_path);
      if (match == full_path) {
        selected = true;
        break;
      }
    }
    for (llist_elm_t *e = s->config->skip_paths->head; e; e = e->next) {
      char *skip_path = (char *)(e->data);
      char *match = strstr(full_path, skip_path);
      if (match == full_path) {
        selected = false;
        break;
      }
    }

    /* Checks mode. */
    switch (s->config->mode) {
    case UT_MODE_STND:
      switch (t->mode) {
      case UT_MODE_STND:
        ; // do-nothing
        break;
      case UT_MODE_PERF:
        selected = false;
        break;
      default:
        abort();
      }
      break;
    case UT_MODE_PERF:
      switch (t->mode) {
      case UT_MODE_STND:
        selected = false;
        break;
      case UT_MODE_PERF:
        ; // do-nothing
        break;
      default:
        abort();
      }
      break;
    case UT_MODE_ALL:
      ; // do-nothing
      break;
    default:
      abort();
    }

    if (selected && s->config->max_quickness >= t->quickness_class) {
      if (s->config->print_test_list) { /* Lists the test. */
        if (s->config->utest) fprintf(stdout, "  ");
        fprintf(stdout, "%s\n", full_path);
      } else { /* Runs the test. */
        if (!ut_run_time_is_quiet(t)) {
          if (s->config->utest) fprintf(stdout, "  ");
          fprintf(stdout, "%s: ", full_path);
          fflush(stdout);
        }
        if (ut_run_time_is_verbose(t)) fprintf(stdout, "\n");

        if (t->setup) t->setup(t);

        /* Sets the test start time. */
        clock_gettime(CLOCK_REALTIME, &t->start_time);

        /* Starts the stop-watch. */
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

        t->test(t);

        /* Stops the stop-watch. */
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

        /* Sets the test end time. */
        clock_gettime(CLOCK_REALTIME, &t->end_time);

        if (t->teardown) t->teardown(t);

        /* Computes the time taken, and updates the test cpu_time. */
        ret = timespec_diff(&t->cpu_time, &time_0, &time_1);
        (void) ret; assert(ret == 0);

        if (!ut_run_time_is_quiet(t)) {
          if (ut_run_time_is_verbose(t)) {
            if (s->config->utest) fprintf(stdout, "  ");
            fprintf(stdout, "%s: ", full_path);
          }
          fprintf(stdout, "%*c", (int)(res_msg_print_column - strlen(full_path)), ' ');
          fprintf(stdout, "[%6lld.%9ld] ", (long long) timespec_get_sec(&t->cpu_time), timespec_get_nsec(&t->cpu_time));
          const ut_quickness_t actual_qck_class = ut_quickness_range(&t->cpu_time);
          const int time_perf = (actual_qck_class > t->quickness_class) - (actual_qck_class < t->quickness_class);
          char time_perf_c = ' ';
          if (time_perf > 0) time_perf_c = '+';
          if (time_perf < 0) time_perf_c = '-';
          fprintf(stdout, "[%d.%d]%c ", t->quickness_class, actual_qck_class, time_perf_c);
          if (t->failure_count) {
            s->failed_test_count++;
            fprintf(stdout, "FAILED - Failure count = %d\n", t->failure_count);
          } else {
            fprintf(stdout, "OK\n");
          }
          if (ut_run_time_is_verbose(t)) fprintf(stdout, "\n");
        }
      }
    }

  }

  free(full_path);

  return s->failed_test_count;
}



/********************************************/
/* Module functions.                        */
/********************************************/

static const mop_options_long_t olist[] = {
  {"help",          'h', MOP_NONE},
  {"list",          'l', MOP_NONE},
  {"mode",          'm', MOP_REQUIRED},
  {"path",          'p', MOP_REQUIRED},
  {"skip",          's', MOP_REQUIRED},
  {"max-quickness", 'k', MOP_REQUIRED},
  {"verbose",       'v', MOP_NONE},
  {"quiet",         'q', MOP_NONE},
  {"utest",         'u', MOP_NONE},
  {0, 0, 0}
};

static void
ut_documentation_to_stream (FILE *stream,
                            const char *const prog_name,
                            const ut_prog_arg_config_t *const config)
{
  assert(stream);
  assert(prog_name);
  assert(config);

  const char *usage_generic =
    "[OPTION]...";
  const char *usage_utest =
    "[OPTION]... [FILE]...";

  if (config->prog_description) {
    fprintf(stream, "Description:\n");
    if (config->prog_name) fprintf(stream, "  %s - ", config->prog_name);
    fprintf(stream, "%s\n\n", config->prog_description);
  }

  fprintf(stream,
          "Usage:\n"
          "  %s %s\n\n",
          prog_name,
          config->utest ? usage_utest : usage_generic);

  fprintf(stream,
          "Options:\n"
          "  -h, --help                       Show help options\n"
          "  -l, --list                       List test cases available in a test executable\n"
          "  -m, --mode {perf|standard|all}   Execute tests according to mode\n"
          "  -p, --path TESTPATH              Only start test cases matching TESTPATH\n"
          "  -s, --skip TESTPATH              Skip all tests matching TESTPATH\n"
          "  -k, --max-quickness              Cap value for quickness class\n"
          "  -q, --quiet                      Run tests quietly\n"
          "  -v, --verbose                    Run tests verbosely\n");

  if (config->prog_long_desc) fprintf(stream,
                                      "\nDetails:\n"
                                      "%s\n", config->prog_long_desc);

  if (config->prog_author) fprintf(stream,
                                   "\nAuthor:\n"
                                   "%s\n", config->prog_author);

  if (config->prog_copyright || config->prog_license) fprintf(stream, "\n");
  if (config->prog_copyright) fprintf(stream, "%s\n", config->prog_copyright);
  if (config->prog_license) fprintf(stream, "%s\n", config->prog_license);
}

/**
 * @brief Parses standard args.
 *
 * @details Returns `0` when everithing is ok, `1` when option `-h` is active,
 *          and a negative value on invalid options.
 *
 * @param [out] config         the program argument configuration
 * @param [in]  stream         file output pointer
 * @param [in]  argc_p         a pointer to argc
 * @param [in]  argv_p         a pointer to argv
 * @param [out] next_arg_index when not NULL returns the next argument to process
 * @return                     parsing status
 */
int
ut_parse_args (ut_prog_arg_config_t *config,
               FILE *stream,
               int *argc_p,
               char ***argv_p,
               int *next_arg_index)
{
  mop_options_t options;
  int opt;

  int oindex = -1;
  int argc = *argc_p;
  char **argv = *argv_p;

  /* help */
  int h_flag = false;

  /* list */
  int l_flag = false;

  /* mode */
  int m_flag = false;
  char *m_arg = NULL;

  /* path */
  int p_flag = false;
  char *p_arg = NULL;

  /* skip */
  int s_flag = false;
  char *s_arg = NULL;

  /* max-quickness */
  int k_flag = false;
  char *k_arg = NULL;

  /* verbose */
  int v_flag = false;

  /* quiet */
  int q_flag = false;

  /* utest */
  int u_flag = false;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'l':
      l_flag = true;
      break;
    case 'm':
      m_flag = true;
      m_arg = options.optarg;
      break;
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
      break;
    case 's':
      s_flag = true;
      s_arg = options.optarg;
      break;
    case 'k':
      k_flag = true;
      k_arg = options.optarg;
      break;
    case 'v':
      v_flag = true;
      break;
    case 'q':
      q_flag = true;
      break;
    case 'u':
      u_flag = true;
      break;
    case ':':
      fprintf(stream, "Option parsing failed: %s\n", options.errmsg);
      return -1;
    case '?':
      fprintf(stream, "Option parsing failed: %s\n", options.errmsg);
      return -2;
    default:
      fprintf(stream, "Unexpectd error. Aborting ...\n");
      abort();
    }

    if (u_flag) config->utest = true;

    /* Prints documentation and returns when help option is detected. */
    if (h_flag) {
      ut_documentation_to_stream(stream, argv[0], config);
      return 1;
    }

    if (l_flag) config->print_test_list = true;

    if (m_flag) {
      if (strcmp(m_arg, "perf") == 0) config->mode = UT_MODE_PERF;
      else if (strcmp(m_arg, "standard") == 0) config->mode = UT_MODE_STND;
      else if (strcmp(m_arg, "all") == 0) config->mode = UT_MODE_ALL;
      else {
        fprintf(stream, "%s: mode value \"%s\" is invalid.\n", argv[0], m_arg);
        return -3;
      }
    }

    if (p_flag) llist_add(config->test_paths, p_arg);

    if (s_flag) llist_add(config->skip_paths, s_arg);

    if (k_flag) {
      char *endptr;
      long int max_quickness = strtol(k_arg, &endptr, 10);
      if (endptr - k_arg != strlen(k_arg)) {
        fprintf(stream, "Argument for option -k: %s is invalid.\n", k_arg);
        return -4;
      }
      if (max_quickness < 0) {
        fprintf(stream, "Argument for option -k is %ld, it must be a non negative integer.\n", max_quickness);
        return -5;
      }
      config->max_quickness = max_quickness;
    }

    if (v_flag) config->verb = UT_VEROSITY_HIGHT;

    if (q_flag) config->verb = UT_VEROSITY_LOW;

    if (u_flag) config->utest = true;

  } // end while loop.

  if (next_arg_index) *next_arg_index = options.optind;

  return 0;
}

/**
 * @brief Has to be called by main as the first step for running the test suite.
 *
 * @details The `config` structures is initialized parsing the argument list.
 *
 * @param [out]    config a reference to a configuration structure
 * @param [in]     argc_p address of the argc parameter of the main() function.
 * @param [in,out] argv_p address of the argv parameter of the main() function.
 */
void
ut_init (ut_prog_arg_config_t *config,
         int *argc_p,
         char ***argv_p)
{
  ut_prog_arg_config_init(config, false);

  int status = ut_parse_args(config, stdout, argc_p, argv_p, NULL);
  if (status == 1) exit(EXIT_SUCCESS);
  if (status < 0) exit(EXIT_FAILURE);
  return;
}

bool
ut_run_time_is_verbose (const ut_test_t *const t)
{
  assert(t);
  assert(t->suite);
  assert(t->suite->config);
  return t->suite->config->verb == UT_VEROSITY_HIGHT ? true : false;
}

bool
ut_run_time_is_quiet (const ut_test_t *const t)
{
  assert(t);
  assert(t->suite);
  assert(t->suite->config);
  return t->suite->config->verb == UT_VEROSITY_LOW ? true : false;
}

ut_verbosity_t
ut_run_time_verbosity (const ut_test_t *const t)
{
  assert(t);
  assert(t->suite);
  assert(t->suite->config);
  return t->suite->config->verb;
}



/*****************************************************************/
/* Function implementations for the ut_prog_arg_config_t entity. */
/*****************************************************************/

void
ut_prog_arg_config_init (ut_prog_arg_config_t *const config,
                         const bool utest)
{
  config->print_test_list = false;
  config->mode = UT_MODE_STND;
  config->max_quickness = UT_QUICKNESS_01;
  config->test_paths = llist_new(NULL);
  config->skip_paths = llist_new(NULL);
  config->verb = UT_VEROSITY_STND;
  config->utest = utest;

  config->prog_name = NULL;
  config->prog_version = NULL;
  config->prog_description = NULL;
  config->prog_copyright = NULL;
  config->prog_long_desc = NULL;
  config->prog_license = NULL;
  config->prog_author = NULL;
}

void
ut_prog_arg_config_set_prog_name (ut_prog_arg_config_t *const config,
                                  const char *const name)
{
  assert(config);
  config->prog_name = name;
}


const char *
ut_prog_arg_config_get_prog_name (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_name;
}

void
ut_prog_arg_config_set_prog_version (ut_prog_arg_config_t *const config,
                                     const char *const version)
{
  assert(config);
  config->prog_version = version;
}

const char *
ut_prog_arg_config_get_prog_version (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_version;
}

void
ut_prog_arg_config_set_prog_description (ut_prog_arg_config_t *const config,
                                         const char *const description)
{
  assert(config);
  config->prog_description = description;
}

const char *
ut_prog_arg_config_get_prog_description (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_description;
}

void
ut_prog_arg_config_set_prog_copyright (ut_prog_arg_config_t *const config,
                                       const char *const copyright)
{
  assert(config);
  config->prog_copyright = copyright;
}

const char *
ut_prog_arg_config_get_prog_copyright (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_copyright;
}

void
ut_prog_arg_config_set_prog_long_desc (ut_prog_arg_config_t *const config,
                                       const char *const long_desc)
{
  assert(config);
  config->prog_long_desc = long_desc;
}

const char *
ut_prog_arg_config_get_prog_long_desc (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_long_desc;
}

void
ut_prog_arg_config_set_prog_license (ut_prog_arg_config_t *const config,
                                     const char *const license)
{
  assert(config);
  config->prog_license = license;
}

const char *
ut_prog_arg_config_get_prog_license (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_license;
}

void
ut_prog_arg_config_set_prog_author (ut_prog_arg_config_t *const config,
                                    const char *const author)
{
  assert(config);
  config->prog_author = author;
}

const char *
ut_prog_arg_config_get_prog_author (const ut_prog_arg_config_t *const config)
{
  assert(config);
  return config->prog_author;
}



/********************************************/
/* Internal functions.                      */
/********************************************/

static ut_test_t *
ut_test_new (char *label,
             ut_test_f tfun,
             ut_mode_t mode,
             ut_quickness_t qck_class,
             ut_suite_t *s)
{
  assert(label);
  assert(tfun);
  assert(mode >= UT_MODE_STND && mode <= UT_MODE_PERF);
  assert(qck_class >= UT_QUICKNESS_0001 && qck_class <= UT_QUICKNESS_OUT_OF_RANGE);
  assert(s);

  ut_test_t *t;
  static const size_t size_of_t = sizeof(ut_test_t);
  t = (ut_test_t *) malloc(size_of_t);
  assert(t);
  t->suite = s;
  t->label = label;
  t->test = tfun;
  t->setup = NULL;
  t->teardown = NULL;
  t->provided_data = NULL;
  t->fixture = NULL;
  t->failure_count = 0;
  t->assertion_count = 0;
  t->mode = mode;
  t->quickness_class = qck_class;
  timespec_set(&t->start_time, 0, 0);
  timespec_set(&t->end_time, 0, 0);
  timespec_set(&t->cpu_time, 0, 0);
  return t;
}

static void
ut_test_free (ut_test_t *t)
{
  free(t);
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

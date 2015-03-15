/**
 * @file
 *
 * @todo
 *
 * @brief Unit test module implementation.
 *
 * @details This module defines a suite entity and a test entity, respectively
 *          #ut_suite_t, and #ut_test_t.
 *          A suite is a collection of tests, it is created by colling #ut_suite_new.
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
 * @copyright 2015 Roberto Corradini. All rights reserved.
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
/* Internal static variable declarations.             */
/******************************************************/

static ut_prog_arg_config_t arg_config;



/******************************************************/
/* Internal function declarations.                    */
/******************************************************/

static void
parse_args (int *argc_p,
            char ***argv_p);



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
ut_test_new (label, tfun, s)
     char *label;
     ut_simple_test_f tfun;
     ut_suite_t *s;
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
ut_test_free (t)
     ut_test_t *t;
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
ut_suite_new (label)
     char *label;
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
ut_suite_free (s)
     ut_suite_t *s;
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
 * @param [in]     label the test label
 * @param [in]     tfun  the the test function
 */
void
ut_suite_add_simple_test (s, label, tfun)
     ut_suite_t *s;
     char *label;
     ut_simple_test_f tfun;
{
  assert(s);
  assert(label);
  assert(tfun);

  ut_test_t *const t = ut_test_new(label, tfun, s);

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
ut_suite_run (s)
     ut_suite_t *s;
{
  if (!s) return 0;
  for (int i = 0; i < s->count; i++) {
    ut_test_t *t = *(s->tests + i);
    printf("/%s/%s: ", s->label, t->label);
    t->test(t);
    if (t->failure_count) {
      s->failed_test_count++;
      printf("failure count = %d.\n", t->failure_count);
    } else {
      printf("OK\n");
    }
  }
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
ut_assert (t, assertion)
     ut_test_t *t;
     int assertion;
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
ut_init (argc_p, argv_p)
     int *argc_p;
     char ***argv_p;
{
  parse_args(argc_p, argv_p);
  return;
}



/********************************************/
/* Internal functions.                      */
/********************************************/

static void
parse_args (argc_p, argv_p)
     int *argc_p;
     char ***argv_p;
{
  int argc = *argc_p;
  char **argv = *argv_p;

  arg_config.print_test_list = false;
  arg_config.mode = UT_MODE_STND;
  arg_config.path = NULL;
  arg_config.skip = NULL;
  arg_config.verb = UT_VEROSITY_STND;

  /* Parses known args. */
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-l") == 0) {
      arg_config.print_test_list = true;
      argv[i] = NULL;
    } else if (strcmp("-m", argv[i]) == 0) {
      ; // TBD
      argv[i] = NULL;
    } else if (strcmp("-p", argv[i]) == 0) {
      ; // TBD
      argv[i] = NULL;
    } else if (strcmp("-s", argv[i]) == 0) {
      ; // TBD
      argv[i] = NULL;
    } else if (strcmp("-q", argv[i]) == 0 || strcmp("--quiet", argv[i]) == 0) {
      ; // TBD
      argv[i] = NULL;
    } else if (strcmp("-v", argv[i]) == 0 || strcmp("--verbose", argv[i]) == 0) {
      ; // TBD
      argv[i] = NULL;
    } else if (strcmp("-?", argv[i]) == 0 ||
               strcmp("-h", argv[i]) == 0 ||
               strcmp("--help", argv[i]) == 0) {
      printf("Usage:\n"
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

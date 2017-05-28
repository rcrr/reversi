/**
 * @file
 *
 * @brief Unit test module definitions.
 * @details This module defines the #ut_test_t entity.
 *
 * @par unit_test.h
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

#ifndef UNIT_TEST_H
#define UNIT_TEST_H

#include <stdbool.h>

#include "linked_list.h"
#include "time_utils.h"



/**********************************************/
/* Structure declarations.                    */
/**********************************************/

/**
 * @brief Test structure.
 */
struct ut_test_t_;

/**
 * @brief Suite structure.
 */
struct ut_suite_t_;



/**********************************************/
/* Function declarations.                     */
/**********************************************/

/**
 * @brief Simple test function.
 *
 * @details A test functions without further data.
 *
 * @param t the unit test
 */
typedef void
(*ut_simple_test_f) (struct ut_test_t_ *t);

/**
 * @brief Regular test function.
 *
 * @details A test functions with data.
 *
 * @param t              the unit test
 * @param fixture        data used or consumed by the test
 * @param provaided_data data given when registering the test
 */
typedef void
(*ut_regular_test_f) (struct ut_test_t_ *t,
                      void *const fixture,
                      const void *const provided_data );

/**
 * @brief Prepares the fixture for the test.
 *
 * @details Using the data provided by the user, construts the fixture.
 *
 * @param [out] fixture       data used or consumed by the test
 * @param [in]  provided_data data given when registering the test
 */
typedef void
(*ut_fixture_setup_f) (void *const fixture,
                       const void *const provided_data );

/**
 * @brief Teardowns the fixture formerly prepared for the test.
 *
 * @param [in] fixture       data used or consumed by the test
 * @param [in] provided_data data given when registering the test
 */
typedef void
(*ut_fixture_teardown_f) (void *const fixture,
                          const void *const provided_data );



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @enum ut_mode_t
 * @brief The mode for running the group of tests.
 */
typedef enum {
  UT_MODE_STND = 0,   /**< Standard mode. */
  UT_MODE_PERF_0,     /**< Performance mode, fast. */
  UT_MODE_PERF_1,     /**< Performance mode, medium speed. */
  UT_MODE_PERF_2      /**< Performance mode, slow. */
} ut_mode_t;

/**
 * @enum ut_verbosity_t
 * @brief The mode for running the group of tests.
 */
typedef enum {
  UT_VEROSITY_LOW,     /**< Low verbosity. */
  UT_VEROSITY_STND,    /**< Standard verbosity. */
  UT_VEROSITY_HIGHT    /**< Hight verbosity. */
} ut_verbosity_t;

/**
 * @enum ut_quickness_t
 * @brief The time approximately required to complete the test.
 */
typedef enum {
  UT_QUICKNESS_0001 = 0,    /**< An almost instantaneous test, running in less than 0.001 seconds. */
  UT_QUICKNESS_001,         /**< A test running in less than 0.01 seconds. */
  UT_QUICKNESS_01,          /**< A test running in less than 0.1 seconds. */
  UT_QUICKNESS_1,           /**< A test running in less than 1 second. */
  UT_QUICKNESS_10,          /**< A test running in less than 10 second. */
  UT_QUICKNESS_100,         /**< A test running in less than 100 second. */
  UT_QUICKNESS_1000,        /**< A test running in less than 1000 second. */
  UT_QUICKNESS_OUT_OF_RANGE /**< A test running in more than 1000 second. */
} ut_quickness_t;

/**
 * @brief A unit test.
 */
typedef struct ut_test_t_ {
  struct ut_suite_t_ *suite;      /**< @brief The parent suite. */
  char *label;                    /**< @brief The test label. */
  ut_simple_test_f simple_test;   /**< @brief The test function, when it is a simple test. */
  ut_regular_test_f regular_test; /**< @brief The test function. when it is a regular test. */
  ut_fixture_setup_f setup;       /**< @brief Setup function. */
  ut_fixture_teardown_f teardown; /**< @brief Teardown function. */
  void *provided_data;            /**< @brief User provided data. */
  int failure_count;              /**< @brief The number of assertion failures. */
  int assertion_count;            /**< @brief The number of assertions. */
  ut_mode_t mode;                 /**< @brief Standard vs performance test. */
  ut_quickness_t quickness_class; /**< @brief The quickness class of the test. */
  timespec_t start_time;          /**< @brief Test start time duration. */
  timespec_t end_time;            /**< @brief Test end time duration. */
  timespec_t cpu_time;            /**< @brief Test cpu process consumption. */
} ut_test_t;

/**
 * @brief A test suite.
 */
typedef struct ut_suite_t_ {
  char *label;                /**< @brief The suite label. */
  size_t count;               /**< @brief Number of tests in the array. */
  size_t size;                /**< @brief Size of the array. */
  ut_test_t **tests;          /**< @brief An array of pointers to tests. */
  int failed_test_count;      /**> @brief Count of failed tests. */
} ut_suite_t;

/**
 * @brief Program argument configuration.
 */
typedef struct ut_prog_arg_config_t_ {
  bool print_test_list;                  /**< @brief Print test list. */
  ut_mode_t mode;                        /**< @brief Running mode. */
  llist_t *test_paths;                   /**< @brief Only start test cases matching a path in the list. */
  llist_t *skip_paths;                   /**< @brief Skip all tests matching a path in the list. */
  ut_verbosity_t verb;                   /**< @brief Output verbosity. */
  bool utest;                            /**< @brief Called by utest program. */
} ut_prog_arg_config_t;



/******************************************************/
/* Function prototypes for the ut_quickness_t entity. */
/******************************************************/

extern ut_quickness_t
ut_quickness_range (const timespec_t *const ts);

extern timespec_t
ut_quickness_boundary (const ut_quickness_t q);



/*************************************************/
/* Function prototypes for the ut_test_t entity. */
/*************************************************/

extern ut_test_t *
ut_test_simple_new (char *label,
                    ut_simple_test_f tfun,
                    ut_mode_t mode,
                    ut_quickness_t speed,
                    ut_suite_t *s);

extern void
ut_test_free (ut_test_t *t);

extern void
ut_test_fail (ut_test_t *t);



/**************************************************/
/* Function prototypes for the ut_suite_t entity. */
/**************************************************/

extern ut_suite_t *
ut_suite_new (char *label);

extern void
ut_suite_free (ut_suite_t *s);

extern void
ut_suite_add_simple_test (ut_suite_t *s,
                          ut_mode_t mode,
                          ut_quickness_t speed,
                          char *label,
                          ut_simple_test_f tfun);

extern int
ut_suite_run (ut_suite_t *s);



/**************************************************/
/* Prototypes for module's functions.             */
/**************************************************/

extern void
ut_init (int *argc_p,
         char ***argv_p);

extern bool
ut_is_mode_equal_to_perf (void);



/**************************************************/
/* Module's macros.                               */
/**************************************************/

/**
 * @brief When `expr` is not true, the program is ended with an abortion.
 *
 * @details Parameter `t` mast be a valid pointer to a `ut_test_t` type.
 *
 * @param [out] t    the test case
 * @param [in]  expr the expression to test, it must be an integer type
 */
#define ut_assert(t, expr) do {                                         \
    assert(t);                                                          \
    if (expr)                                                           \
      t->assertion_count++;                                             \
    else {                                                              \
      t->failure_count++;                                               \
      fprintf(stdout,                                                   \
              "\nERROR:%s:%d:%s:_assertion_failed_(%s)\n",              \
              __FILE__, __LINE__, __FUNCTION__, #expr);                 \
      fflush(stdout);                                                   \
      abort();                                                          \
    }                                                                   \
  } while (0)



#endif /* UNIT_TEST_H */

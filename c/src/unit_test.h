/**
 * @file
 *
 * @brief Unit test module definitions.
 * @details This module defines the `ut_test_t` entity.
 *
 * @par unit_test.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2017, 2020 Roberto Corradini. All rights reserved.
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
(*ut_test_f) (struct ut_test_t_ *t);

/**
 * @brief Prepares the fixture for the test.
 *
 * @details Using the data provided by the user, construts the fixture.
 *
 * @param t the unit test
 */
typedef void
(*ut_fixture_setup_f) (struct ut_test_t_ *t);

/**
 * @brief Teardowns the fixture formerly prepared for the test.
 *
 * @param t the unit test
 */
typedef void
(*ut_fixture_teardown_f) (struct ut_test_t_ *t);



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @enum ut_mode_t
 * @brief The mode for running the group of tests.
 */
typedef enum {
  UT_MODE_STND = 0,   /**< Standard mode. */
  UT_MODE_PERF,       /**< Performance mode. */
  UT_MODE_ALL         /**< Everything. */
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
  struct ut_suite_t_ *suite;             /**< @brief The parent suite. */
  char *label;                           /**< @brief The test label. */
  ut_test_f test;                        /**< @brief The test function, when it is a simple test. */
  ut_fixture_setup_f setup;              /**< @brief Setup function. */
  ut_fixture_teardown_f teardown;        /**< @brief Teardown function. */
  void *provided_data;                   /**< @brief User provided data. */
  void *fixture;                         /**< @brief Test fixture. */
  int failure_count;                     /**< @brief The number of assertion failures. */
  int assertion_count;                   /**< @brief The number of assertions. */
  ut_mode_t mode;                        /**< @brief Standard vs performance test. */
  ut_quickness_t quickness_class;        /**< @brief The quickness class of the test. */
  timespec_t start_time;                 /**< @brief Test start time duration. */
  timespec_t end_time;                   /**< @brief Test end time duration. */
  timespec_t delta_time;                 /**< @brief Test elapsed duration (end_time - start_time). */
  timespec_t cpu_time;                   /**< @brief Test cpu process consumption. */
} ut_test_t;

/**
 * @brief A test suite.
 */
typedef struct ut_suite_t_ {
  char *label;                           /**< @brief The suite label. */
  struct ut_prog_arg_config_t_ *config;  /**< @brief The runtime configutration taken from the program arguments. */
  size_t count;                          /**< @brief Number of tests in the array. */
  size_t size;                           /**< @brief Size of the array. */
  ut_test_t **tests;                     /**< @brief An array of pointers to tests. */
  int failed_test_count;                 /**> @brief Count of failed tests. */
} ut_suite_t;

/**
 * @brief Program argument configuration.
 */
typedef struct ut_prog_arg_config_t_ {
  bool print_test_list;                  /**< @brief Print test list. */
  ut_mode_t mode;                        /**< @brief Running mode. */
  ut_quickness_t max_quickness;          /**< @brief Max quickness allowed. */
  llist_t *test_paths;                   /**< @brief Only start test cases matching a path in the list. */
  llist_t *skip_paths;                   /**< @brief Skip all tests matching a path in the list. */
  ut_verbosity_t verb;                   /**< @brief Output verbosity. */
  bool utest;                            /**< @brief Called by utest program. */
  const char *prog_name;                 /**< @brief Program name string. */
  const char *prog_version;              /**< @brief Program version string. */
  const char *prog_description;          /**< @brief Program description string. */
  const char *prog_copyright;            /**< @brief Program copyright string. */
  const char *prog_long_desc;            /**< @brief Program long description string. */
  const char *prog_license;              /**< @brief Program license string. */
  const char *prog_author;               /**< @brief Program author string. */
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

extern void
ut_test_fail (ut_test_t *const t);



/**************************************************/
/* Function prototypes for the ut_suite_t entity. */
/**************************************************/

extern ut_suite_t *
ut_suite_new (ut_prog_arg_config_t *config,
              char *label);

extern void
ut_suite_free (ut_suite_t *s);

extern ut_test_t *
ut_suite_add_simple_test (ut_suite_t *s,
                          ut_mode_t mode,
                          ut_quickness_t qck_class,
                          char *label,
                          ut_test_f tfun);

extern ut_test_t *
ut_suite_add_regular_test (ut_suite_t *s,
                           ut_mode_t mode,
                           ut_quickness_t qck_class,
                           char *label,
                           const void *const provided_data,
                           ut_fixture_setup_f setup,
                           ut_test_f tfun,
                           ut_fixture_teardown_f teardown);

extern int
ut_suite_run (ut_suite_t *s);



/************************************************************/
/* Function prototypes for the ut_prog_arg_config_t entity. */
/************************************************************/

extern void
ut_prog_arg_config_init (ut_prog_arg_config_t *const config,
                         const bool utest);

extern void
ut_prog_arg_config_set_prog_name (ut_prog_arg_config_t *const config,
                                  const char *const name);

extern const char *
ut_prog_arg_config_get_prog_name (const ut_prog_arg_config_t *const config);

extern void
ut_prog_arg_config_set_prog_version (ut_prog_arg_config_t *const config,
                                     const char *const version);

extern const char *
ut_prog_arg_config_get_prog_version (const ut_prog_arg_config_t *const config);

extern void
ut_prog_arg_config_set_prog_description (ut_prog_arg_config_t *const config,
                                         const char *const description);

extern const char *
ut_prog_arg_config_get_prog_description (const ut_prog_arg_config_t *const config);

extern void
ut_prog_arg_config_set_prog_copyright (ut_prog_arg_config_t *const config,
                                       const char *const copyright);

extern const char *
ut_prog_arg_config_get_prog_copyright (const ut_prog_arg_config_t *const config);

extern void
ut_prog_arg_config_set_prog_long_desc (ut_prog_arg_config_t *const config,
                                       const char *const long_desc);

extern const char *
ut_prog_arg_config_get_prog_long_desc (const ut_prog_arg_config_t *const config);

extern void
ut_prog_arg_config_set_prog_license (ut_prog_arg_config_t *const config,
                                     const char *const license);

extern const char *
ut_prog_arg_config_get_prog_license (const ut_prog_arg_config_t *const config);

extern void
ut_prog_arg_config_set_prog_author (ut_prog_arg_config_t *const config,
                                     const char *const author);

extern const char *
ut_prog_arg_config_get_prog_author (const ut_prog_arg_config_t *const config);



/**************************************************/
/* Prototypes for module's functions.             */
/**************************************************/

extern void
ut_init (ut_prog_arg_config_t *config,
         int *argc_p,
         char ***argv_p);

extern int
ut_parse_args (ut_prog_arg_config_t *config,
               FILE *stream,
               int *argc_p,
               char ***argv_p,
               int *next_arg_index);

extern bool
ut_run_time_is_verbose (const ut_test_t *const t);

extern bool
ut_run_time_is_quiet (const ut_test_t *const t);

extern ut_verbosity_t
ut_run_time_verbosity (const ut_test_t *const t);



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
              __FILE__, __LINE__, __func__, #expr);                     \
      fflush(stdout);                                                   \
      abort();                                                          \
    }                                                                   \
  } while (0)



#endif /* UNIT_TEST_H */

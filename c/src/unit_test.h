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

#ifndef UNIT_TEST_H
#define UNIT_TEST_H

#include <stdbool.h>



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
 * @param s the test suite
 * @param t the unit test
 */
typedef void
(*ut_simple_test_f) (struct ut_test_t_ *t);



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @brief A unit test.
 */
typedef struct ut_test_t_ {
  struct ut_suite_t_ *suite;  /**< @brief The parent suite. */
  char *label;                /**< @brief The test label. */
  ut_simple_test_f test;      /**< @brief The test function. */
  int failure_count;          /**< @brief The number of assertion failures. */
  int assertion_count;        /**< @brief The number of assertions. */
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
 * @enum ut_mode_t
 * @brief The mode for running the group of tests.
 */
typedef enum {
  UT_MODE_STND,   /**< Standard mode. */
  UT_MODE_PERF    /**< Performance mode. */
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
 * @brief Program argument configuration.
 */
typedef struct ut_prog_arg_config_t_ {
  bool print_test_list;                  /**< @brief Print test list. */
  ut_mode_t mode;                        /**< @brief Running mode. */
  char *path;                            /**< @brief Only start test cases matching it. */
  char *skip;                            /**< @brief Skip all tests matching it. */
  ut_verbosity_t verb;                   /**< @brief Output verbosity. */
} ut_prog_arg_config_t;



/*************************************************/
/* Function prototypes for the ut_test_t entity. */
/*************************************************/

extern ut_test_t *
ut_test_new (char *label,
             ut_simple_test_f tfun,
             ut_suite_t *s);

extern void
ut_test_free (ut_test_t *t);



/**************************************************/
/* Function prototypes for the ut_suite_t entity. */
/**************************************************/

extern ut_suite_t *
ut_suite_new (char *label);

extern void
ut_suite_free (ut_suite_t *s);

extern void
ut_suite_add_simple_test (ut_suite_t *s,
                          char *label,
                          ut_simple_test_f tfun);

extern int
ut_suite_run (ut_suite_t *s);



/**************************************************/
/* Function prototypes for assertions.            */
/**************************************************/

extern void
ut_assert (ut_test_t *t,
           int assertion);



/**************************************************/
/* Prototypes for module's functions.             */
/**************************************************/

extern void
ut_init (int *argc_p,
         char ***argv_p);



#endif /* UNIT_TEST_H */

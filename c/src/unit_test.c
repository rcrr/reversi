/**
 * @file
 *
 * @brief Unit test module implementation.
 * @details This module defines a unit test module implementation.
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

#include "unit_test.h"



/***************************************************/
/* Function implementations for the test_t entity. */
/***************************************************/

/**
 * @brief Test structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * test structure is not `NULL`.
 *
 * @return a pointer to a new test structure
 */
ut_test_t *
ut_test_new (label, tfun)
     char *label;
     ut_simple_test_f tfun;
{
  ut_test_t *t;
  static const size_t size_of_t = sizeof(ut_test_t);
  t = (ut_test_t *) malloc(size_of_t);
  assert(t);
  t->label = label;
  t->test = tfun;
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
ut_suite_new (void)
{
  ut_suite_t *s;
  static const size_t size_of_t = sizeof(ut_suite_t);
  s = (ut_suite_t *) malloc(size_of_t);
  assert(s);
  s->count = 0;
  s->size = array_alloc_chunk_size;
  s->tests = (void *) malloc(s->size * sizeof(void *));
  assert(s->tests);
  return s;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #ut_suite_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
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
 * @details Adds a simple test to the suite.
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

  ut_test_t *const t = ut_test_new(label, tfun);

  if (s->count == s->size) {
    s->size += array_alloc_chunk_size;
    s->tests = (void *) realloc(s->tests, s->size * sizeof(void *));
    assert(s->tests);
  }

  ut_test_t **tests_p = s->tests + s->count;
  *tests_p = t;
  s->count++;
}

int
ut_suite_run (s)
     ut_suite_t *s;
{
  for (int i = 0; i < s->count; i++) {
    ut_test_t *t = *(s->tests + i);
    printf("test label: %s\n", t->label);
    t->test(s);
  }
  return 0;
}



/********************************************/
/* Assertion implementations.               */
/********************************************/

int
ut_assert (assertion)
     int assertion;
{
  return assertion;
}

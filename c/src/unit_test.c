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
 * linked list structure is not `NULL`.
 *
 * @return a pointer to a new test structure
 */
ut_test_t *
ut_test_new (void)
{
  ut_test_t *t;
  static const size_t size_of_t = sizeof(ut_test_t);
  t = (ut_test_t *) malloc(size_of_t);
  assert(t);
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

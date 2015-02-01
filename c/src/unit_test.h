/**
 * @file
 *
 * @brief Unit test module definitions.
 * @details This module defines the #test_t entity.
 *
 * @par test.h
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



/**********************************************/
/* Function declarations.                     */
/**********************************************/

/**
 * @brief Simple test function.
 *
 * @details A test functions without further data.
 *
 * @return the count of faiilures
 */
typedef int
(*ut_simple_test_f) (void);



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @brief A unit test.
 */
typedef struct {
  char *label;             /**< @brief The test label. */
  ut_simple_test_f test;   /**< @brief The test function. */
} ut_test_t;



/**********************************************/
/* Function prototypes for the test_t entity. */
/**********************************************/

extern ut_test_t *
ut_test_new (void);

extern void
ut_test_free (ut_test_t *t);



#endif /* UNIT_TEST_H */

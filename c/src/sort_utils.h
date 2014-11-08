/**
 * @file
 *
 * @brief Sort Utils module definitions.
 *
 * @details This module provides sorting functions.
 *
 * @par sort_utils.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#ifndef SORT_UTILS_H
#define SORT_UTILS_H

/**
 * @brief Function pointer type for comparing values.
 *
 * @details Function implementation can have different forms:
 *          - Compare: has to ensure that when `a` is equal `b`,
 *          zero is returned, when `a` is greater than `b` a positive integer,
 *          and otherwise a negative one.
 *          - Less then: has to assure that when `a` is less than `b`,
 *          `TRUE` is returned, and `FALSE` otherwise.
 *
 * @param a first value
 * @param b second value
 * @return result of comparing values
 */
typedef int (*sort_utils_compare_function) (const void *const a,
                                            const void *const b);



/********************************/
/* Compare function signatures. */
/********************************/

extern int
sort_utils_double_eq (const void *const a,
                      const void *const b);

extern int
sort_utils_double_gt (const void *const a,
                      const void *const b);

extern int
sort_utils_double_lt (const void *const a,
                      const void *const b);

extern int
sort_utils_double_cmp (const void *const a,
                       const void *const b);

extern void
sort_utils_heapsort (void *const a,
                     const size_t count,
                     const size_t element_size,
                     const sort_utils_compare_function cmp);

extern void
sort_utils_heapsort_asc_d (double *const a,
                           const int count);

extern void
sort_utils_heapsort_dsc_d (double *const a,
                           const int count);

extern void
sort_utils_insertionsort_d (double *const a,
                            const int n);

extern void
sort_utils_heapsort_d (double *const a,
                       const int count);

extern void
sort_utils_heapsort_p (void **const a,
                       const int count);

extern void
sort_utils_smoothsort_d (double *const a,
                         const int count);

#endif /* SORT_UTILS_H */

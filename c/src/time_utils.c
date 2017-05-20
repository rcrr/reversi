/**
 * @file
 *
 * @brief Time utils module implementation.
 *
 * @par time_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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

#include "time_utils.h"


/**
 * @brief Computes `timespec` delta between `end` and `start`.
 *
 * @details The `result` pointer receives the elapsed time between end and start.
 *
 * A way of obtaining the `timespec_t` structure value is:
 *
 * @code
 * timespec_t time_0;
 *
 * clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);
 * @endcode
 *
 * @invariant Parameters `result`, `start` and `end` must be not `NULL`.
 * The invariants are guarded by assertions.
 *
 * @param [out] result a pointer to the result structure
 * @param [in]  start  starting time
 * @param [in]  end    ending time
 * @return             1 if the difference is negative, otherwise 0
 */
int
timespec_diff (timespec_t *const result,
               const timespec_t *const start,
               const timespec_t *const end)
{
  assert(result);
  assert(start);
  assert(end);
  if ((end->tv_sec - start->tv_sec) < 0) return 1;
  if ((end->tv_sec - start->tv_sec) == 0 &&
      (end->tv_nsec - start->tv_nsec) < 0) return 1;

  if ((end->tv_nsec - start->tv_nsec) < 0) {
    result->tv_sec = end->tv_sec - start->tv_sec - 1;
    result->tv_nsec = 1000000000 + end->tv_nsec - start->tv_nsec;
  } else {
    result->tv_sec = end->tv_sec - start->tv_sec;
    result->tv_nsec = end->tv_nsec - start->tv_nsec;
  }
  return 0;
}

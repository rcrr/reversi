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
#include <string.h>
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

/**
 * @brief Gets the value from the seconds field.
 *
 * @param [in] ts a pointer to the timespec structure
 * @return        the value from the seconds field
 */
time_t
timespec_get_sec (const timespec_t *const ts)
{
  return ts->tv_sec;
}

/**
 * @brief Gets the value from the nanoseconds field.
 *
 * @param [in] ts a pointer to the timespec structure
 * @return        the value from the nanoseconds field
 */
long
timespec_get_nsec (const timespec_t *const ts)
{
  return ts->tv_nsec;
}

/**
 * @brief Sets `ts` values.
 *
 * @invariant Parameter `ts` must be not `NULL`.
 * @invariant Parameter `seconds` must be not negative.
 * @invariant Parameter `nanoseconds` must be not negative.
 * The invariants are guarded by assertions.
 *
 * @param [out] ts           a pointer to the timespec structure
 * @param [in]  seconds      value for the `tv_sec` field
 * @param [in]  nanoseconds  value for the `tv_nsec` field
 */
void
timespec_set (timespec_t *const ts,
              const time_t seconds,
              const long nanoseconds)
{
  assert(ts);
  assert(seconds >= 0);
  assert(nanoseconds >= 0);

  ts->tv_sec = seconds;
  ts->tv_nsec = nanoseconds;
}

/**
 * @brief Compares timespec_t values pointed by `a` and `b`.
 *
 * @details Compare function that returns:
 *          - `+1` when `a` is greater than `b`
 *          - ` 0` when `a` is equal to `b`
 *          - `-1` when `a` is less then `b`
 *
 * @invariant Parameters `a` and `b` must be not `NULL`.
 * The invariants are guarded by assertions.
 *
 * @param [in] a a pointer to the first value
 * @param [in] b a pointer to the second value
 * @return       a value in `{-1, 0, +1}` based on the comparison of `a` and `b`
 */
int
timespec_cmp (const timespec_t *const a,
              const timespec_t *const b)
{
  assert(a);
  assert(b);

  if (a->tv_sec > b->tv_sec) return +1;
  if (a->tv_sec < b->tv_sec) return -1;
  return (a->tv_nsec > b->tv_nsec) - (a->tv_nsec < b->tv_nsec);
}

/**
 * @brief Prints the current time to `output_stream`.
 *
 * @details If `output_stream` is null the function does nothing.
 *
 * @param [in] output_stream output stream
 */
void
timespec_print_local_time (FILE *output_stream)
{
  if (output_stream) {
    time_t mytime = time(NULL);
    char *time_str = ctime(&mytime);
    time_str[strlen(time_str)-1] = '\0';
    fprintf(output_stream, "%s", time_str);
  }
}

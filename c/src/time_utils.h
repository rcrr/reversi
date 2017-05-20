/**
 * @file
 *
 * @brief Time utils module definitions.
 * @details This module defines a miscellaneous of utilities related
 * to time twiking.
 *
 * @par time_utils.h
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

#ifndef TIME_UTILS_H
#define TIME_UTILS_H

#include <time.h>



/**
 * @brief Time spec type definition. See sys/time.h for more details.
 *
 * @details the `timespec` structure is declared in `time.h` and has the following members:
 *
 * - `time_t`     `tv_sec`    This represents the number of whole seconds of elapsed time.
 * - `long int`   `tv_nsec`   This is the rest of the elapsed time (a fraction of a second),
 *                            represented as the number of nanoseconds. It is always less than one billion.
 */
typedef struct timespec timespec_t;



extern int
timespec_diff (timespec_t *const result,
               const timespec_t *const start,
               const timespec_t *const end);


#endif /* TIME_UTILS_H */

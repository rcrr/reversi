/**
 * @file
 *
 * @brief Architecture module definitions.
 * @details This module defines utilities and macros for dealing with different architectures.
 *
 * @par arch.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2016 Roberto Corradini. All rights reserved.
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

#ifndef ARCH_H
#define ARCH_H

#include <stdbool.h>
#include <cpuid.h>

#if __linux__ == 1 &&                           \
  __x86_64__ == 1 &&                            \
  __AVX2__ == 1 &&                              \
  __GNUC__ >= 4

#define ARCH_AT_COMPILE_TIME_IS_SUPPORTED 1

#else

#pragma GCC error "Architecture is not supported."

#endif

/**********************************************/
/* Global constants.                          */
/**********************************************/

/**
 * @brief Architecture at compile time is supported, it is always 1.
 */
static const int arch_at_compile_time_is_supported  = ARCH_AT_COMPILE_TIME_IS_SUPPORTED;



extern bool
arch_runtime_is_supported (void);



#endif /* ARCH_H */

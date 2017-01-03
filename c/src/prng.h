/**
 * @file
 *
 * @brief PRNG: Pseudo Random Number Generator module definitions.
 * @details This module defines functions and structures that deals
 * with randomness.
 *
 * @par prng.h
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

#ifndef PRNG_H
#define PRNG_H



extern void
prng_mt19937_init_by_seed (uint64_t seed);

extern void
prng_mt19937_init_by_array (const uint64_t init_key[],
                            const size_t key_length);

extern uint64_t
prng_mt19937_get_uint64 (void);

extern double
prng_mt19937_get_double_in_c0_c1 (void);

extern double
prng_mt19937_get_double_in_c0_o1 (void);

extern double
prng_mt19937_get_double_in_o0_o1 (void);



#endif /* PRNG_H */

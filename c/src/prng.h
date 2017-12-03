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

#include <stdint.h>



/**
 * @brief It holds the state of a Mersenne Twister Pseudo-Random generator.
 *
 * @details The Mersenne Twister 19937 algorithm is a 64-bit implementation of
 * a 623-Dimensionally Equidistributed Uniform Pseudo-Random Number Generator.
 *
 * The structure must be initialized before usage.
 */
typedef struct {
  uint64_t *mt;   /**< @brief A pointer to the Mersenne Twister working area. */
  size_t   mti;   /**< @brief The working index on the `mt` array. */
} prng_mt19937_t;


extern void
prng_stdlib_init_seed (void);

extern void
prng_stdlib_init_seed_with_value (const unsigned int seed);

extern int
prng_stdlib_get_number_in_range (const int low,
                                 const int high);

extern void
prng_stdlib_shuffle_array_uint8 (uint8_t *const array,
                                 const int n);

extern unsigned long int
prng_uint64_from_clock_random_seed (void);

extern prng_mt19937_t *
prng_mt19937_new (void);

extern void
prng_mt19937_free (prng_mt19937_t *st);

extern void
prng_mt19937_init_by_seed (prng_mt19937_t *st,
                           const uint64_t seed);

extern void
prng_mt19937_init_by_array (prng_mt19937_t *st,
                            const uint64_t init_key[],
                            const size_t key_length);

extern uint64_t
prng_mt19937_get_uint64 (prng_mt19937_t *st);

extern double
prng_mt19937_get_double_in_c0_c1 (prng_mt19937_t *st);

extern double
prng_mt19937_get_double_in_c0_o1 (prng_mt19937_t *st);

extern double
prng_mt19937_get_double_in_o0_o1 (prng_mt19937_t *st);

extern uint64_t
prng_mt19937_random_choice_from_finite_set (prng_mt19937_t *prng,
                                            const uint64_t k);

extern void
prng_mt19937_shuffle_array_uint8 (prng_mt19937_t *prng,
                                  uint8_t *const array,
                                  const size_t n);

extern void
prng_mt19937_shuffle_array_p (prng_mt19937_t *prng,
                              void *array,
                              const size_t n);

extern void
prng_mt19937_shuffle_array_double (prng_mt19937_t *prng,
                                   double *const array,
                                   const size_t n);

extern void
prng_mt19937_shuffle_array_int (prng_mt19937_t *prng,
                                int *const array,
                                const size_t n);

extern void
prng_mt19937_random_string_az (prng_mt19937_t *prng,
                               char *const rs,
                               const size_t n);

extern uint8_t
prng_mt19937_extract_from_set64 (prng_mt19937_t *prng,
                                 uint64_t *set);



#endif /* PRNG_H */

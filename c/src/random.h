/**
 * @file
 *
 * @brief Random module definitions.
 * @details This module defines utilities that are based on
 * random number generation.
 *
 * @par random.h
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

#ifndef RANDOM_H
#define RANDOM_H

#include <gsl/gsl_rng.h>

#include "bit_works.h"


/**
 * @brief An entity that holds the state of a random number generator.
 */
typedef struct {
  gsl_rng           *r;           /**< @brief A reference to a GNU GSL random number generator. */
  unsigned long int  seed;        /**< @brief The seed used to createthe RNG instance. */
} RandomNumberGenerator;


extern void
random_init_seed (void);

extern void
random_init_seed_with_value (const unsigned int seed);

extern int
random_get_number_in_range (const int low,
                            const int high);

extern void
random_shuffle_array_uint8 (uint8_t *array,
                            const int n);

extern RandomNumberGenerator *
rng_new (const unsigned long int seed);

extern void
rng_free (RandomNumberGenerator *rng);

extern unsigned long int
rng_random_seed (void);

extern unsigned long int
rng_random_choice_from_finite_set (RandomNumberGenerator *rng,
                                   const unsigned long int k);

extern void
rng_shuffle_array_uint8 (RandomNumberGenerator *rng,
                         uint8_t *array,
                         const int n);

extern void
rng_shuffle_array_p (RandomNumberGenerator *const rng,
                     void **const array,
                     const int n);

extern void
rng_shuffle_array_double (RandomNumberGenerator *const rng,
                          double *const array,
                          const int n);

extern void
rng_shuffle_array_int (RandomNumberGenerator *const rng,
                       int *const array,
                       const int n);

#endif /* RANDOM_H */

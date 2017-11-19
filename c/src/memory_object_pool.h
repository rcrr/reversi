/**
 * @file
 *
 * @brief Memory object pool module definitions.
 *
 * @details A memory object pool is an allocated space that provides a mechanism
 * for memory allocation and de-allocation specific for a given structure definition.
 *
 * The pool is created with a call to #mopool_create()
 *
 * @par memory_object_pool.h
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

#ifndef MEMORY_OBJECT_POOL_H
#define MEMORY_OBJECT_POOL_H

#include <stdbool.h>

/**
 * @brief A memory object pool structure.
 *
 * @details Collects the memory for a pool of objects.
 */
typedef struct mopool_s mopool_t;

/**
 * @enum mopool_ext_policy_t
 * @brief The policy that governs the extension of the memory pool.
 */
typedef enum {
  MOPOOL_EXT_POLICY_FIXED,      /**< No extension is allowed. */
  MOPOOL_EXT_POLICY_LIMITED,    /**< Extension is allowed up to the walue of `extension_limit`. */
  MOPOOL_EXT_POLICY_UNLIMITED   /**< Unlimited extension is allowed. */
} mopool_ext_policy_t;

/**
 * @brief Returns the number of objects that the pool can collects without performing an incremental extension.
 *
 * @details It is the allocated space in terms of object count.
 *
 * @param [in] mop the reference to the pool
 * @return         the current size of the pool
 */
extern size_t
mopool_get_nobjs (mopool_t *mop);

/**
 * @brief Returns the object size.
 *
 * @details When the pool is created, the client passes the object size, that is then
 *          recorded unchanged for the complete lifecycle of the pool.
 *
 * @param [in] mop the reference to the pool
 * @return         the object size
 */
extern size_t
mopool_get_obj_size (mopool_t *mop);

/**
 * @brief Returns the slot size.
 *
 * @details When the pool is created, the memory footprint for one object is computed
 *          by padding the object size to the next eight byte multiple.
 *          The slot size is then the real memory consumption for one object in the pool,
 *          after being computed it is recorded unchanged for the complete lifecycle of the pool.
 *
 * @param [in] mop the reference to the pool
 * @return         the slot size
 */
extern size_t
mopool_get_slot_size (mopool_t *mop);

/**
 * @brief Returns the count of new objects that can be added to the pool without an incremental extension.
 *
 * @details It is the free space in terms of object count.
 *
 * @param [in] mop the reference to the pool
 * @return         the free slots
 */
extern size_t
mopool_get_nfree (mopool_t *mop);

/**
 * @brief Returns the policy of the pool.
 *
 * @details When the pool is created, the client passes the policy of the pool, that is then
 *          recorded unchanged for the complete lifecycle of the pool.
 *
 * @param [in] mop the reference to the pool
 * @return         the policy of the pool
 */
extern mopool_ext_policy_t
mopool_get_policy (mopool_t *mop);

/**
 * @brief Returns the initial pool capacity.
 *
 * @details When the pool is created, the client passes the initial pool size in terms of
 *          object capacity, that is then recorded unchanged for the complete lifecycle of the pool.
 *
 * @param [in] mop the reference to the pool
 * @return         the initial pool capacity
 */
extern size_t
mopool_get_nobjs_initial (mopool_t *mop);

/**
 * @brief Returns the incremental capacity given by one extension.
 *
 * @details When the pool is created, the client passes the incremental capacity given by one extension.
 *
 * @param [in] mop the reference to the pool
 * @return         the extension size
 */
extern size_t
mopool_get_nobjs_extension (mopool_t *mop);

extern size_t
mopool_get_nobjs_limit (mopool_t *mop);

extern bool
mopool_check_consistency (mopool_t *mop);

extern mopool_t *
mopool_create (size_t obj_size,
               mopool_ext_policy_t policy,
               size_t nobjs_initial,
               size_t nobjs_extension,
               size_t nobjs_limit);

extern void
mopool_destroy (mopool_t *mop);

extern void *
mopool_malloc (mopool_t *mop);

extern void
mopool_free (mopool_t *mop,
             void *ptr);

extern size_t
mopool_get_nobjs (mopool_t *mop);

#endif

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

/**
 * @brief Returns the capacity limit of the pool.
 *
 * @details When the pool is created, the client passes the capacity limit of the pool.
 *
 * @param [in] mop the reference to the pool
 * @return         the size limit
 */
extern size_t
mopool_get_nobjs_limit (mopool_t *mop);

/**
 * @brief Performs consistency checks on the pool.
 *
 * @details The function performs the following checks:
 *          - `mop` is not `NULL`
 *          - the memory segments are all connected and reachable following the linked list
 *          - all the free slots are belonging to the segments
 *          - the count of free slots is consistent
 *          - allocated slot count are no more then allocated capacity
 *
 *          Further checks not yet implemented are:
 *          - collects the free slots in a dictionary
 *          - computes by difference the set of allocated slots
 *          - performs a call-back to a function provided by the client on all allocated objects
 *
 * @param [in] mop the reference to the pool
 * @return         `true` when the pool is consistent
 */
extern bool
mopool_check_consistency (mopool_t *mop);

/**
 * @brief Returns a newly created and allocated memory pool
 *
 * @details
 *
 * @param [in] obj_size         the size in bytes of the object hosted by the pool
 * @param [in] policy           the extension policy to apply when space is exhausted
 * @param [in] nobjs_initial    the size allocated, in terms of object count, when the pool is created
 * @param [in] nobjs_extension  the size further allocated when an extension is performed
 * @param [in] nobjs_limit      the upper bound of the pool
 * @return
 */
extern mopool_t *
mopool_create (size_t obj_size,
               mopool_ext_policy_t policy,
               size_t nobjs_initial,
               size_t nobjs_extension,
               size_t nobjs_limit);

/**
 * @brief Destroies the pool, by releasing to the OS all the allocated resources.
 *
 * @details Deallocates a pool created by a call to mopool_create().
 *
 *          If `mop` does not point to a valid #mopool_t structure allocated with the above functions,
 *          undefined behavior occurs.
 *
 *          If `mop` is a null pointer, the function does nothing.
 *
 *          Notice that this function does not change the value of `mop` itself, hence it still
 *          points to the same (now invalid) location.
 *
 * @param [in] mop the reference to the pool
 */
extern void
mopool_destroy (mopool_t *mop);

/**
 * @brief Returns a pointer to the newly allocated memory for an object
 *
 * @details Allocates a slot from the pool, returning a pointer to the object.
 *          The newly allocated object is not initialized, remaining with indeterminate values.
 *
 * @param [in] mop the reference to the pool
 * @return         a pointer to the allocated object
 */
extern void *
mopool_malloc (mopool_t *mop);

/**
 * @brief Release the space, allocated by an object pointed by `ptr`, to the pool.
 *
 * @details A block of memory previously allocated by a call to mopool_malloc(), is deallocated,
 *          and returned to the pool making it available again for further allocations.
 *
 *          If `ptr` does not point to a slot of memory allocated with the above functions, the pool gets corrupted.
 *          Calling the function on a valid slot, but already deallocated causes corruption.
 *
 *          If `ptr` is a null pointer, the function does nothing.
 *
 *          Notice that this function does not change the value of `ptr` itself, hence it still
 *          points to the same (now invalid) location.
 *
 * @param [in] mop the reference to the pool
 * @param [in] ptr the reference to the released object
 */
extern void
mopool_free (mopool_t *mop,
             void *ptr);

#endif

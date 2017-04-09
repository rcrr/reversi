/**
 * @file
 *
 * @brief SHA3 (Secure Hash Algorithm v3) module definitions.
 * @details This module defines a minimum set of functions that implements
 * the SHA-3 NIST standard.
 *
 * This work has been derived from the implementation reachable at the
 * GitHub page: <a href="https://github.com/mjosaarinen/tiny_sha3" target="_blank">
 * tiny_sha3</a>.
 *
 * @par sha3.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Markku-Juhani O. Saarinen mailto:mjos@iki.fi
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2011-2015 Markku-Juhani O. Saarinen. All rights reserved.
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

#ifndef SHA3_H
#define SHA3_H

#include <stddef.h>
#include <stdint.h>



/**
 * @brief State context for SHA-3 algorithms.
 *
 * @details As described in the document page 7 section 3.
 */
typedef struct {
  union {
    uint8_t b[200];                     /**< @brief State array viewed as 200 bytes. */
    uint64_t q[25];                     /**< @brief State array viewed as 25 QWORDs. */
  } st;                                 /**< @brief State array formed by 1600 bits. */
  int pt;                               /**< @brief An index that moves in circle in [0..rsiz) for each byte processed in the message. */
  int rsiz;                             /**< @brief A costant equal to `200 - (2 * md_len)`. */
  int md_len;                           /**< @brief Message digest length in bytes */
} sha3_ctx_t;



/**********************************************/
/* Global constants.                          */
/**********************************************/

/**
 * @brief SHA3-224 digest lenght in bytes.
 */
static const size_t sha3_224_digest_lenght = 28;

/**
 * @brief SHA3-256 digest lenght in bytes.
 */
static const size_t sha3_256_digest_lenght = 32;

/**
 * @brief SHA3-384 digest lenght in bytes.
 */
static const size_t sha3_384_digest_lenght = 48;

/**
 * @brief SHA3-512 digest lenght in bytes.
 */
static const size_t sha3_512_digest_lenght = 64;



extern void
sha3_224 (void *const md,
          const void *const msg,
          const size_t msg_len);

extern void
sha3_256 (void *const md,
          const void *const msg,
          const size_t msg_len);

extern void
sha3_384 (void *const md,
          const void *const msg,
          const size_t msg_len);

extern void
sha3_512 (void *const md,
          const void *const msg,
          const size_t msg_len);

extern void
sha3_init (sha3_ctx_t *const c,
           const int md_len);

extern void
sha3_update (sha3_ctx_t *const c,
             const void *const msg_chunk,
             const size_t msg_chunk_len);

extern void
sha3_final (sha3_ctx_t *const c,
            void *md);

inline static void
sha3_shake128_init (sha3_ctx_t *const c)
{
  sha3_init(c, 16);
}

inline static void
sha3_shake256_init (sha3_ctx_t *const c)
{
  sha3_init(c, 32);
}

inline static void
sha3_shake_update (sha3_ctx_t *const c,
                   const void *const msg_chunk,
                   const size_t msg_chunk_len)
{
  sha3_update(c, msg_chunk, msg_chunk_len);
}

extern void
sha3_shake_xof (sha3_ctx_t *c);

extern void
sha3_shake_out (sha3_ctx_t *const c,
                void *const md,
                const size_t md_len);

extern void
sha3_msg_digest_to_string (char *const md_as_string,
                           const char *const md,
                           const size_t md_len);


#endif

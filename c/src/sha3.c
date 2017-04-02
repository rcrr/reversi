/**
 * @file
 *
 * @brief SHA3 (Secure Hash Algorithm v3) module implementation.
 * @details This module provides a set of function that implements the specification
 * of the SHA3 family of hashing algorithms.
 *
 * The specification is published by NIST (National Institute of Standards and Technology),
 * a measurement standards laboratory, and a non-regulatory agency of the
 * United States Department of Commerce.
 * Its home page can be reached at: <a href="https://www.nist.gov/" target="_blank">
 * NIST</a>.
 * The document describing the algorithm, downloadable from the NIST's site as a `PDF` document,
 * is named: <a href="http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.202.pdf" target="_blank">
 * FIPS 202</a>.
 *
 * This work has been derived from the implementation written by Markku-Juhani O. Saarinen,
 * that is reachable at the
 * GitHub page: <a href="https://github.com/mjosaarinen/tiny_sha3" target="_blank">
 * tiny_sha3</a>.
 *
 * The code is designed and limited to little endian systems.
 *
 * A first group of functions implements the four
 * SHA3 functions as described in `FIPS 202, paragraph 6, page 20`:
 *
 *  - #sha3_224()
 *  - #sha3_256()
 *  - #sha3_384()
 *  - #sha3_512()
 *
 * A sample call to the #sha3_256() function is here exemplified:
 *
 * @code
 * char *msg = "I am the message to be hashed.";
 *
 * char digest[32]; // 32 bites are 256 bits.
 *
 * sha3_256(digest, msg, strlen(msg));
 * @endcode
 *
 *
 * @par sha3.c
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

#include "sha3.h"
#include "bit_works.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

void
sha3_keccakf (uint64_t st[25]);



/*
 * Internal variables and constants.
 */

/* Number of rounds used by the keccakf function. */
static const size_t keccakf_rounds = 24;

/**
 * @endcond
 */



/**
 * @brief Initializes the context for SHA3.
 *
 * @details Common values for parameter `mdlen` are:
 *            - 16 for SHA3-256
 *            - 32 for SHA3-512
 *
 * @param [in,out] c     sha3 context
 * @param [in]     mdlen message digest output length in bytes
 */
void
sha3_init (sha3_ctx_t *const c,
           const int mdlen)
{
  for (int i = 0; i < 25; i++)
    c->st.q[i] = 0;
  c->mdlen = mdlen;
  c->rsiz = 200 - 2 * mdlen;
  c->pt = 0;
}

/**
 * @brief Updates state with more data.
 *
 * @details The SHA3 context is updated based on the new data received.
 *          The data has to come to this function in chunks of any size,
 *          in the correct sequence, and being complete.
 *
 * @param [in,out] c         sha3 context
 * @param [in]     msg_chunk a chunk of data taken sequentially from the message
 * @param [in]     len       message chunk length in bytes
 */
void
sha3_update (sha3_ctx_t *const c,
             const void *const msg_chunk,
             const size_t len)
{
  int j = c->pt;
  for (size_t i = 0; i < len; i++) {
    c->st.b[j++] ^= ((const uint8_t *) msg_chunk)[i];
    if (j >= c->rsiz) {
      sha3_keccakf(c->st.q);
      j = 0;
    }
  }
  c->pt = j;
}

/**
 * @brief Finalizes the SHA3 context and outputs the message digest.
 *
 * @details The SHA3 context is updated and finalized.
 *          The message digest is copied to the `md` buffer.
 *
 * @param [in,out] c  sha3 context
 * @param [out]    md the output buffer that receives the digest
 */
void
sha3_final (sha3_ctx_t *const c,
            void *const md)
{
  c->st.b[c->pt] ^= 0x06;
  c->st.b[c->rsiz - 1] ^= 0x80;
  sha3_keccakf(c->st.q);

  for (int i = 0; i < c->mdlen; i++) {
    ((uint8_t *) md)[i] = c->st.b[i];
  }
}

/**
 * @brief Computes the SHA3-224 hash.
 *
 * @details The SHA-3 hash function that produces 224-bit digests.
 *
 * @param [out] md      the output buffer that receives the digest
 * @param [in]  msg     the message
 * @param [in]  msg_len the lenght of the message in bytes
 */
void
sha3_224 (void *const md,
          const void *const msg,
          const size_t msg_len)
{
  sha3_ctx_t sha3;
  sha3_init(&sha3, 28);
  sha3_update(&sha3, msg, msg_len);
  sha3_final(&sha3, md);
}

/**
 * @brief Computes the SHA3-256 hash.
 *
 * @details The SHA-3 hash function that produces 256-bit digests.
 *
 * @param [out] md      the output buffer that receives the digest
 * @param [in]  msg     the message
 * @param [in]  msg_len the lenght of the message in bytes
 */
void
sha3_256 (void *const md,
          const void *const msg,
          const size_t msg_len)
{
  sha3_ctx_t sha3;
  sha3_init(&sha3, 32);
  sha3_update(&sha3, msg, msg_len);
  sha3_final(&sha3, md);
}

/**
 * @brief Computes the SHA3-384 hash.
 *
 * @details The SHA-3 hash function that produces 384-bit digests.
 *
 * @param [out] md      the output buffer that receives the digest
 * @param [in]  msg     the message
 * @param [in]  msg_len the lenght of the message in bytes
 */
void
sha3_384 (void *const md,
          const void *const msg,
          const size_t msg_len)
{
  sha3_ctx_t sha3;
  sha3_init(&sha3, 48);
  sha3_update(&sha3, msg, msg_len);
  sha3_final(&sha3, md);
}

/**
 * @brief Computes the SHA3-512 hash.
 *
 * @details The SHA-3 hash function that produces 512-bit digests.
 *
 * @param [out] md      the output buffer that receives the digest
 * @param [in]  msg     the message
 * @param [in]  msg_len the lenght of the message in bytes
 */
void
sha3_512 (void *const md,
          const void *const msg,
          const size_t msg_len)
{
  sha3_ctx_t sha3;
  sha3_init(&sha3, 64);
  sha3_update(&sha3, msg, msg_len);
  sha3_final(&sha3, md);
}

// SHAKE128 and SHAKE256 extensible-output functionality

void
shake_xof (sha3_ctx_t *c)
{
  c->st.b[c->pt] ^= 0x1F;
  c->st.b[c->rsiz - 1] ^= 0x80;
  sha3_keccakf(c->st.q);
  c->pt = 0;
}

void
shake_out (sha3_ctx_t *c,
           void *out,
           size_t len)
{
  size_t i;
  int j;

  j = c->pt;
  for (i = 0; i < len; i++) {
    if (j >= c->rsiz) {
      sha3_keccakf(c->st.q);
      j = 0;
    }
    ((uint8_t *) out)[i] = c->st.b[j++];
  }
  c->pt = j;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Compression function.
 *
 * @details Updates the state with given number of rounds.
 *
 * @param [in,out] st the internal state
 */
void
sha3_keccakf (uint64_t st[25])
{
  /* Constants. */
  const uint64_t keccakf_rndc[24] = {
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
    0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008
  };

  const int keccakf_rotc[24] = {
    1,  3,  6,  10, 15, 21, 28, 36, 45, 55, 2,  14,
    27, 41, 56, 8,  25, 43, 62, 18, 39, 61, 20, 44
  };

  const int keccakf_piln[24] = {
    10, 7,  11, 17, 18, 3, 5,  16, 8,  21, 24, 4,
    15, 23, 19, 13, 12, 2, 20, 14, 22, 9,  6,  1
  };

  /* Variables. */
  int i, j, r;
  uint64_t t, bc[5];

  /* Actual iteration. */
  for (r = 0; r < keccakf_rounds; r++) {

    /* Theta */
    for (i = 0; i < 5; i++)
      bc[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20];

    for (i = 0; i < 5; i++) {
      t = bc[(i + 4) % 5] ^ bit_works_rol_64(bc[(i + 1) % 5], 1);
      for (j = 0; j < 25; j += 5)
        st[j + i] ^= t;
    }

    /* Rho Pi */
    t = st[1];
    for (i = 0; i < 24; i++) {
      j = keccakf_piln[i];
      bc[0] = st[j];
      st[j] = bit_works_rol_64(t, keccakf_rotc[i]);
      t = bc[0];
    }

    /* Chi */
    for (j = 0; j < 25; j += 5) {
      for (i = 0; i < 5; i++)
        bc[i] = st[j + i];
      for (i = 0; i < 5; i++)
        st[j + i] ^= (~bc[(i + 1) % 5]) & bc[(i + 2) % 5];
    }

    /* Iota */
    st[0] ^= keccakf_rndc[r];
  }
}

/**
 * @endcond
 */

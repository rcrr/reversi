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

#ifndef KECCAKF_ROUNDS
#define KECCAKF_ROUNDS 24
#endif

#ifndef ROTL64
#define ROTL64(x, y) (((x) << (y)) | ((x) >> (64 - (y))))
#endif

// state context
typedef struct {
  union {                               // state:
    uint8_t b[200];                     // 8-bit bytes
    uint64_t q[25];                     // 64-bit words
  } st;
  int pt, rsiz, mdlen;                  // these don't overflow
} sha3_ctx_t;

// Compression function.
void sha3_keccakf(uint64_t st[25]);

// OpenSSL - like interfece
int sha3_init(sha3_ctx_t *c, int mdlen);    // mdlen = hash output in bytes
int sha3_update(sha3_ctx_t *c, const void *data, size_t len);
int sha3_final(void *md, sha3_ctx_t *c);    // digest goes to md

// compute a sha3 hash (md) of given byte length from "in"
void *sha3(const void *in, size_t inlen, void *md, int mdlen);

// SHAKE128 and SHAKE256 extensible-output functions
#define shake128_init(c) sha3_init(c, 16)
#define shake256_init(c) sha3_init(c, 32)
#define shake_update sha3_update

void shake_xof(sha3_ctx_t *c);
void shake_out(sha3_ctx_t *c, void *out, size_t len);

#endif

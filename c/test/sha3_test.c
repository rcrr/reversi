/**
 * @file
 *
 * @brief SHA3 (Secure Hash Algorithm v3) unit test suite.
 * @details Collects tests and helper methods for the module.
 *
 * @par sha3_test.c
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <glib.h>

#include "sha3.h"



/* Test function prototypes. */

static void dummy_test (void);
static void sha3_224_test (void);
static void sha3_256_test (void);
static void sha3_384_test (void);
static void sha3_512_test (void);
static void sha3_256_abc_test (void);
static void sha3_512_abc_test (void);
static void sha3_256_empty_string_test (void);
static void sha3_512_empty_string_test (void);
static void sha3_256_empty_896_bits_test (void);
static void sha3_256_empty_1M_a_test (void);
static void sha3_256_extremely_long_message_test (void);
static void shake128_variable_output_test (void);
static void shake256_variable_output_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/sha3/dummy", dummy_test);
  g_test_add_func("/sha3/sha3_224_test", sha3_224_test);
  g_test_add_func("/sha3/sha3_256_test", sha3_256_test);
  g_test_add_func("/sha3/sha3_384_test", sha3_384_test);
  g_test_add_func("/sha3/sha3_512_test", sha3_512_test);
  g_test_add_func("/sha3/sha3_256_abc_test", sha3_256_abc_test);
  g_test_add_func("/sha3/sha3_512_abc_test", sha3_512_abc_test);
  g_test_add_func("/sha3/sha3_256_empty_string_test", sha3_256_empty_string_test);
  g_test_add_func("/sha3/sha3_512_empty_string_test", sha3_512_empty_string_test);
  g_test_add_func("/sha3/sha3_256_empty_896_bits_test", sha3_256_empty_896_bits_test);
  g_test_add_func("/sha3/sha3_256_empty_1M_a_test", sha3_256_empty_1M_a_test);
  g_test_add_func("/sha3/shake128_variable_output_test", shake128_variable_output_test);
  g_test_add_func("/sha3/shake256_variable_output_test", shake256_variable_output_test);

  if (g_test_slow()) {
    g_test_add_func("/sha3/sha3_256_extremely_long_message_test", sha3_256_extremely_long_message_test);
  }

  return g_test_run();
}



/*
 * Test functions.
 */

static void
dummy_test (void)
{
  g_assert(TRUE);
}

static void
sha3_224_test (void)
{
  const size_t msg_digest_len = 28; // SHA3-256 are 28 bytes.

  char *msg = "abc";
  char *expected_msg_digest_as_string =
    "e642824c3f8cf24a" "d09234ee7d3c766f" "c9a3a5168d0c94ad" "73b46fdf";

  char msg_digest[msg_digest_len];
  char msg_digest_as_string[msg_digest_len * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_224(msg_digest, msg, msg_len);

  char *c = msg_digest_as_string;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", msg_digest[i]);

  g_assert(strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_test (void)
{
  const size_t msg_digest_len = 32; // SHA3-256 are 32 bytes.

  char *msg = "abc";
  char *expected_msg_digest_as_string =
    "3a985da74fe225b2" "045c172d6bd390bd" "855f086e3e9d525b" "46bfe24511431532";

  char msg_digest[msg_digest_len];
  char msg_digest_as_string[msg_digest_len * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_256(msg_digest, msg, msg_len);

  char *c = msg_digest_as_string;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", msg_digest[i]);

  g_assert(strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_384_test (void)
{
  const size_t msg_digest_len = 48; // SHA3-256 are 48 bytes.

  char *msg = "abc";
  char *expected_msg_digest_as_string =
    "ec01498288516fc9" "26459f58e2c6ad8d" "f9b473cb0fc08c25" "96da7cf0e49be4b2"
    "98d88cea927ac7f5" "39f1edf228376d25";

  char msg_digest[msg_digest_len];
  char msg_digest_as_string[msg_digest_len * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_384(msg_digest, msg, msg_len);

  char *c = msg_digest_as_string;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", msg_digest[i]);

  g_assert(strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_512_test (void)
{
  const size_t msg_digest_len = 64; // SHA3-256 are 64 bytes.

  char *msg = "abc";
  char *expected_msg_digest_as_string =
    "b751850b1a57168a" "5693cd924b6b096e" "08f621827444f70d" "884f5d0240d2712e"
    "10e116e9192af3c9" "1a7ec57647e39340" "57340b4cf408d5a5" "6592f8274eec53f0";

  char msg_digest[msg_digest_len];
  char msg_digest_as_string[msg_digest_len * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_512(msg_digest, msg, msg_len);

  char *c = msg_digest_as_string;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", msg_digest[i]);

  g_assert(strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_abc_test (void)
{
  char msg[] = { "abc" };
  char expected_msg_digest[] = { "3a985da74fe225b2" "045c172d6bd390bd" "855f086e3e9d525b" "46bfe24511431532" };

  size_t msg_digest_len = 32; // SHA3-256  ###  256 = 32 * 8
  char msg_digest[64 + 1];    // Two hex digit per byte plus string termination.
  char sha[32];               // Same size as msg_digest_len

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
sha3_512_abc_test (void)
{
  char msg[] = { "abc" };
  char expected_msg_digest[] = { "b751850b1a57168a" "5693cd924b6b096e"
                                 "08f621827444f70d" "884f5d0240d2712e"
                                 "10e116e9192af3c9" "1a7ec57647e39340"
                                 "57340b4cf408d5a5" "6592f8274eec53f0" };

  size_t msg_digest_len = 64; // SHA3-512  ###  512 = 64 * 8
  char msg_digest[128 + 1];   // Two hex digit per byte plus string termination.
  char sha[64];               // Same size as msg_digest_len

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
sha3_256_empty_string_test (void)
{
  char msg[] = { "" };
  char expected_msg_digest[] = { "a7ffc6f8bf1ed766" "51c14756a061d662" "f580ff4de43b49fa" "82d80a4b80f8434a" };

  size_t msg_digest_len = 32; // SHA3-256  ###  256 = 32 * 8
  char msg_digest[64 + 1];    // Two hex digit per byte plus string termination.
  char sha[32];               // Same size as msg_digest_len

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
sha3_512_empty_string_test (void)
{
  char msg[] = { "" };
  char expected_msg_digest[] = { "a69f73cca23a9ac5" "c8b567dc185a756e"
                                 "97c982164fe25859" "e0d1dcc1475c80a6"
                                 "15b2123af1f5f94c" "11e3e9402c3ac558"
                                 "f500199d95b6d3e3" "01758586281dcd26" };

  size_t msg_digest_len = 64; // SHA3-512  ###  512 = 64 * 8
  char msg_digest[128 + 1];   // Two hex digit per byte plus string termination.
  char sha[64];               // Same size as msg_digest_len

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
sha3_256_empty_896_bits_test (void)
{
  char msg[] = { "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu" };
  char expected_msg_digest[] = { "916f6061fe879741" "ca6469b43971dfdb" "28b1a32dc36cb325" "4e812be27aad1d18" };

  size_t msg_digest_len = 32; // SHA3-256  ###  256 = 32 * 8
  char msg_digest[64 + 1];    // Two hex digit per byte plus string termination.
  char sha[32];               // Same size as msg_digest_len

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
sha3_256_empty_1M_a_test (void)
{
  char msg[1000000 + 1];
  for (int i = 0; i < 1000000; i++) {
    msg[i] = 'a';
  }
  msg[1000000] = '\0';
  char expected_msg_digest[] = { "5c8875ae474a3634" "ba4fd55ec85bffd6" "61f32aca75c6d699" "d0cdcb6c115891c1" };

  size_t msg_digest_len = 32; // SHA3-256  ###  256 = 32 * 8
  char msg_digest[64 + 1];    // Two hex digit per byte plus string termination.
  char sha[32];               // Same size as msg_digest_len

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
sha3_256_extremely_long_message_test (void)
{

  char pattern[] = { "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno" };

  const size_t repeats = 16777216;
  const size_t pattern_len = sizeof(pattern) - 1;

  char *msg = (char *) malloc(repeats * pattern_len + 1);
  char *p = msg;
  for (int i = 0; i < repeats; i++) {
    for (int j = 0; j < pattern_len; j++) {
      *p++ = pattern[j];
    }
  }
  *p = '\0';
  p++;

  char expected_msg_digest[] = { "ecbbc42cbf296603" "acb2c6bc0410ef43" "78bafb24b710357f" "12df607758b33e2b" };

  size_t msg_digest_len = 32; // SHA3-256  ###  256 = 32 * 8
  char msg_digest[64 + 1];    // Two hex digit per byte plus string termination.
  char sha[32];               // Same size as msg_digest_len

  size_t msg_len = repeats * pattern_len + 1;
  sha3_ctx_t ctx;

  sha3_init(&ctx, msg_digest_len);
  sha3_update(&ctx, msg, msg_len - 1);
  sha3_final(&ctx, sha);

  free(msg);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
shake128_variable_output_test (void)
{
  char msg[] = { "e9474a9e7a8bd81dbcca534d8c78267a" };
  char expected_msg_digest[] = { "7ad49919b30fea6c3acdd6100d883e41f3" };

  const size_t output_len = 17; // 17 * 8 = 136

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  char msg_digest[output_len * 2 + 1];    // Two hex digit per byte plus string termination.
  char sha[output_len];                   // Same size as output_len

  shake128_init(&ctx);
  shake_update(&ctx, msg, msg_len - 1);
  shake_xof(&ctx);
  shake_out(&ctx, sha, output_len);

  char *c = msg_digest;
  for (int i = 0; i < output_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

static void
shake256_variable_output_test (void)
{
  char msg[] = { "Per me si va ne la citta' dolente,\n"
                 "per me si va ne l'etterno dolore,\n"
                 "per me si va tra la perduta gente." };

  char expected_msg_digest[] = { "eeab2b4561d6d512ec5cdb10ab50a8d04de78f4018346c1efc166a55efe9e4f063b193e746b9e6c2" };

  const size_t output_len = 40; // 40 * 8 = 320

  size_t msg_len = sizeof(msg);
  sha3_ctx_t ctx;

  char msg_digest[output_len * 2 + 1];    // Two hex digit per byte plus string termination.
  char sha[output_len];                   // Same size as output_len

  shake256_init(&ctx);
  shake_update(&ctx, msg, msg_len - 1);
  shake_xof(&ctx);
  shake_out(&ctx, sha, output_len);

  char *c = msg_digest;
  for (int i = 0; i < output_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

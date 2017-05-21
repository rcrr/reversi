/**
 * @file
 *
 * @brief SHA3 (Secure Hash Algorithm v3) unit test suite.
 * @details Collects tests and helper methods for the module.
 *
 * @par ut_sha3.c
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
#include <assert.h>

#include "unit_test.h"
#include "sha3.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
sha3_224_t (ut_test_t *const t)
{
  const char *const msg = "abc";
  const char *const expected_msg_digest_as_string =
    "e642824c3f8cf24a" "d09234ee7d3c766f" "c9a3a5168d0c94ad" "73b46fdf";

  char msg_digest[sha3_224_digest_lenght];
  char msg_digest_as_string[sha3_224_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  const size_t msg_len = strlen(msg);

  sha3_224(msg_digest, msg, msg_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_224_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_t (ut_test_t *const t)
{
  const char *const msg = "abc";
  const char *const expected_msg_digest_as_string =
    "3a985da74fe225b2" "045c172d6bd390bd" "855f086e3e9d525b" "46bfe24511431532";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_224_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_256(msg_digest, msg, msg_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_384_t (ut_test_t *const t)
{
  const char *const msg = "abc";
  const char *const expected_msg_digest_as_string =
    "ec01498288516fc9" "26459f58e2c6ad8d" "f9b473cb0fc08c25" "96da7cf0e49be4b2"
    "98d88cea927ac7f5" "39f1edf228376d25";

  char msg_digest[sha3_384_digest_lenght];
  char msg_digest_as_string[sha3_384_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_384(msg_digest, msg, msg_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_384_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_512_t (ut_test_t *const t)
{
  const char *const msg = "abc";
  const char *const expected_msg_digest_as_string =
    "b751850b1a57168a" "5693cd924b6b096e" "08f621827444f70d" "884f5d0240d2712e"
    "10e116e9192af3c9" "1a7ec57647e39340" "57340b4cf408d5a5" "6592f8274eec53f0";

  char msg_digest[sha3_512_digest_lenght];
  char msg_digest_as_string[sha3_512_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_512(msg_digest, msg, msg_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_512_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_abc_t (ut_test_t *const t)
{
  const char *const msg = "abc";
  const char *const expected_msg_digest_as_string =
    "3a985da74fe225b2" "045c172d6bd390bd" "855f086e3e9d525b" "46bfe24511431532";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_512_abc_t (ut_test_t *const t)
{
  const char *const msg = "abc";
  const char *const expected_msg_digest_as_string =
    "b751850b1a57168a" "5693cd924b6b096e" "08f621827444f70d" "884f5d0240d2712e"
    "10e116e9192af3c9" "1a7ec57647e39340" "57340b4cf408d5a5" "6592f8274eec53f0";

  char msg_digest[sha3_512_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_512_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_512_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_empty_string_t (ut_test_t *const t)
{
  const char *const msg = "";
  const char *const expected_msg_digest_as_string =
    "a7ffc6f8bf1ed766" "51c14756a061d662" "f580ff4de43b49fa" "82d80a4b80f8434a";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_512_empty_string_t (ut_test_t *const t)
{
  const char *const msg = "";
  const char *const expected_msg_digest_as_string =
    "a69f73cca23a9ac5" "c8b567dc185a756e" "97c982164fe25859" "e0d1dcc1475c80a6"
    "15b2123af1f5f94c" "11e3e9402c3ac558" "f500199d95b6d3e3" "01758586281dcd26";

  char msg_digest[sha3_512_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_512_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_512_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_896_bits_t (ut_test_t *const t)
{
  const char *const msg = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu";
  const char *const expected_msg_digest_as_string =
    "916f6061fe879741" "ca6469b43971dfdb" "28b1a32dc36cb325" "4e812be27aad1d18";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_1M_a_0_t (ut_test_t *const t)
{
  char msg[1000000 + 1];
  for (int i = 0; i < 1000000; i++) {
    msg[i] = 'a';
  }
  msg[1000000] = '\0';
  const char *const expected_msg_digest_as_string =
    "5c8875ae474a3634" "ba4fd55ec85bffd6" "61f32aca75c6d699" "d0cdcb6c115891c1";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_1M_a_1_t (ut_test_t *const t)
{
  char msg[1000 + 1];
  for (int i = 0; i < 1000; i++) {
    msg[i] = 'a';
  }
  msg[1000] = '\0';
  const char *const expected_msg_digest_as_string =
    "5c8875ae474a3634" "ba4fd55ec85bffd6" "61f32aca75c6d699" "d0cdcb6c115891c1";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  for (int i = 0; i < 1000; i++)
    sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_extremely_long_message_0_t (ut_test_t *const t)
{
  const char *const pattern = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno";

  const size_t repeats = 16777216;
  const size_t pattern_len = strlen(pattern);

  char *msg = (char *) malloc(repeats * pattern_len + 1);
  char *p = msg;
  for (int i = 0; i < repeats; i++) {
    for (int j = 0; j < pattern_len; j++) {
      *p++ = pattern[j];
    }
  }
  *p = '\0';
  p++;

  const char *const expected_msg_digest_as_string =
    "ecbbc42cbf296603" "acb2c6bc0410ef43" "78bafb24b710357f" "12df607758b33e2b";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.
  size_t msg_len = strlen(msg);

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  sha3_update(&ctx, msg, msg_len);
  sha3_final(&ctx, msg_digest);

  free(msg);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_256_extremely_long_message_1_t (ut_test_t *const t)
{
  const char *const pattern = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno";

  const size_t repeats = 16777216;
  const size_t pattern_len = strlen(pattern);

  const char *const expected_msg_digest_as_string =
    "ecbbc42cbf296603" "acb2c6bc0410ef43" "78bafb24b710357f" "12df607758b33e2b";

  char msg_digest[sha3_256_digest_lenght];
  char msg_digest_as_string[sha3_256_digest_lenght * 2 + 1]; // Two hex digit per byte plus string termination.

  sha3_ctx_t ctx;

  sha3_init(&ctx, sha3_256_digest_lenght);
  for (int i = 0; i < repeats; i++) {
    sha3_update(&ctx, pattern, pattern_len);
  }
  sha3_final(&ctx, msg_digest);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, sha3_256_digest_lenght);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_shake128_variable_output_t (ut_test_t *const t)
{
  const char *const msg = "e9474a9e7a8bd81dbcca534d8c78267a";
  const char *const expected_msg_digest_as_string = "7ad49919b30fea6c3acdd6100d883e41f3";
  const size_t output_len = 17; // 17 * 8 = 136

  size_t msg_len = strlen(msg);
  char msg_digest[output_len];
  char msg_digest_as_string[output_len * 2 + 1]; // Two hex digit per byte plus string termination.

  sha3_ctx_t ctx;

  sha3_shake128_init(&ctx);
  sha3_shake_update(&ctx, msg, msg_len);
  sha3_shake_xof(&ctx);
  sha3_shake_out(&ctx, msg_digest, output_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, output_len);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_shake256_variable_output_0_t (ut_test_t *const t)
{
  const char *const msg =
    "Per me si va ne la citta' dolente,\n"
    "per me si va ne l'etterno dolore,\n"
    "per me si va tra la perduta gente.";

  const char *const expected_msg_digest_as_string =
    "eeab2b4561d6d512ec5cdb10ab50a8d04de78f4018346c1efc166a55efe9e4f063b193e746b9e6c2";

  const size_t output_len = 40; // 40 * 8 = 320

  size_t msg_len = strlen(msg);
  char msg_digest[output_len];
  char msg_digest_as_string[output_len * 2 + 1]; // Two hex digit per byte plus string termination.

  sha3_ctx_t ctx;

  sha3_shake256_init(&ctx);
  sha3_shake_update(&ctx, msg, msg_len);
  sha3_shake_xof(&ctx);
  sha3_shake_out(&ctx, msg_digest, output_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, output_len);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}

static void
sha3_shake256_variable_output_1_t (ut_test_t *const t)
{
  const char *const msg0 =
    "Per me si va ne la citta' dolente,\n";
  const char *const msg1 =
    "per me si va ne l'etterno dolore,\n";
  const char *const msg2 =
    "per me si va tra la perduta gente.";

  const char *const expected_msg_digest_as_string =
    "eeab2b4561d6d512ec5cdb10ab50a8d04de78f4018346c1efc166a55efe9e4f063b193e746b9e6c2";

  const size_t output_len = 40;

  char msg_digest[output_len];
  char msg_digest_as_string[output_len * 2 + 1];

  sha3_ctx_t ctx;

  sha3_shake256_init(&ctx);

  sha3_shake_update(&ctx, msg0, strlen(msg0));
  sha3_shake_update(&ctx, msg1, strlen(msg1));
  sha3_shake_update(&ctx, msg2, strlen(msg2));

  sha3_shake_xof(&ctx);
  sha3_shake_out(&ctx, msg_digest, output_len);

  sha3_msg_digest_to_string(msg_digest_as_string, msg_digest, output_len);

  ut_assert(t, strcmp(expected_msg_digest_as_string, msg_digest_as_string) == 0);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("sha3");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_224", sha3_224_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256", sha3_256_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_384", sha3_384_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_512", sha3_512_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_abc", sha3_256_abc_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_512_abc", sha3_512_abc_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_empty_string", sha3_256_empty_string_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_512_empty_string", sha3_512_empty_string_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_896_bits", sha3_256_896_bits_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_1M_a_0", sha3_256_1M_a_0_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_1M_a_1", sha3_256_1M_a_1_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_extremely_long_message_0", sha3_256_extremely_long_message_0_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_256_extremely_long_message_1", sha3_256_extremely_long_message_1_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_shake128_variable_output", sha3_shake128_variable_output_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_shake256_variable_output_0", sha3_shake256_variable_output_0_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "sha3_shake256_variable_output_1", sha3_shake256_variable_output_1_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

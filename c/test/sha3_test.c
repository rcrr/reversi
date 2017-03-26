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
static void sha3_256_abc_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/sha3/dummy", dummy_test);
  g_test_add_func("/sha3/sha3_256_abc_test", sha3_256_abc_test);

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
  sha3_final(sha, &ctx);

  char *c = msg_digest;
  for (int i = 0; i < msg_digest_len; i++)
    c += sprintf(c, "%02hhx", sha[i]);

  g_assert(strcmp(expected_msg_digest, msg_digest) == 0);
}

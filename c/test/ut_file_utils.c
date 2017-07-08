/**
 * @file
 *
 * @brief Unit test suite for the file utils module.
 * @details Collects tests and helper methods for the file utils module.
 *
 * @par ut_file_utils.c
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
#include <errno.h>
#include <assert.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "prng.h"
#include "file_utils.h"
#include "unit_test.h"



#define TEST_DIR_NAME_LEN 16

static const char *base_dir_name = "/tmp";

static char test_dir_name[TEST_DIR_NAME_LEN + 1];
static char test_dir_full_path[TEST_DIR_NAME_LEN + 16];

static prng_mt19937_t *prng;

/*
 * Auxiliary functions.
 */

static void
aux_setup (void)
{
  int status;

  const unsigned long int random_seed = prng_uint64_from_clock_random_seed();
  prng =  prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, random_seed);

  prng_mt19937_random_string_az(prng, test_dir_name, TEST_DIR_NAME_LEN);

  strcpy(test_dir_full_path, base_dir_name);
  strcat(test_dir_full_path, "/");
  strcat(test_dir_full_path, test_dir_name);

  status = mkdir(test_dir_full_path, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
  if (status != 0) {
    printf("Error creating the directory for testing. Directory name is: %s, %s.\n", test_dir_full_path, strerror(errno));
    abort();
  }
}

static void
aux_teardown (void)
{
  int status;

  status = rmdir(test_dir_full_path);
  if (status != 0) {
    printf("Error removing the directory used tor testing. Directory name is: %s, %s.\n", test_dir_full_path, strerror(errno));
    abort();
  }

  prng_mt19937_free(prng);
}


/*
 * Test functions.
 */

static void
fut_file_exists_t (ut_test_t *const t)
{
  char pathname[1024];

  const char *file_name = "fut_file_exists_t.tmp";

  strcpy(pathname, test_dir_full_path);
  strcat(pathname, "/");
  strcat(pathname, file_name);

  FILE *fp = fopen(pathname, "ab+");
  fclose(fp);

  ut_assert(t, fut_file_exists(pathname));

  unlink(pathname);

  ut_assert(t, !fut_file_exists(pathname));
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  aux_setup();

  ut_suite_t *const s = ut_suite_new(&config, "file_utils");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "fut_file_exists", fut_file_exists_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  aux_teardown();

  return failure_count;
}

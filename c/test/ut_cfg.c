/**
 * @file
 *
 * @brief Config unit test suite.
 * @details Collects tests and helper methods for the cfg module.
 *
 * @par ut_cfg.c
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
#include <unistd.h>

#include "unit_test.h"
#include "cfg.h"



static const char *cfg_test_file_path = "build/tmp/cfg_test_file.cfg";



/*
 * Auxiliary functions.
 */

static void
prepare_config_file (void)
{
  const char *config_data =
    "; changed on Monday 27th November 2017 by The Wizard of Oz\n"
    "\n"
    "slippers=ruby\n"
    "\n"
    "[owner]\n"
    "name = Doroty\n"
    "city = The Emerald City\n"
    "\n"
    "[data]\n"
    "; data is update daily\n"
    "server = 192.0.3.44\n"
    "port = 80\n"
    "file = \"game-data.txt\"\n";

  FILE *fp = fopen(cfg_test_file_path, "w+");
  assert(fp);
  fprintf(fp, "%s", config_data);
  fclose(fp);
}

static void
remove_config_file (void)
{
  unlink(cfg_test_file_path);
}

static void
cfg_fixture_setup (ut_test_t *const t)
{
  assert(t);
  prepare_config_file();
}

static void
cfg_fixture_teardown (ut_test_t *const t)
{
  assert(t);
  remove_config_file();
}



/*
 * Test functions.
 */

static void
load_t (ut_test_t *const t)
{
  int ret;

  cfg_t *config = cfg_load(cfg_test_file_path);
  ut_assert(t, config);

  const char *name = cfg_get(config, "owner", "name");
  ut_assert(t, name);
  ut_assert(t, 0 == strcmp("Doroty", name));

  const char *slippers = cfg_get(config, NULL, "slippers");
  ut_assert(t, slippers);
  ut_assert(t, 0 == strcmp("ruby", slippers));

  slippers = cfg_get(config, NULL, "sliPPers");
  ut_assert(t, slippers);
  ut_assert(t, 0 == strcmp("ruby", slippers));

  const char *missing = cfg_get(config, NULL, "missing");
  ut_assert(t, !missing);
  missing = cfg_get(config, "owner", "server");
  ut_assert(t, !missing);
  missing = cfg_get(config, NULL, "server");
  ut_assert(t, missing);

  char server[128];
  int port = 0;
  char file[256];

  ret = cfg_sget(config, "data", "server", "%s", &server);
  ut_assert(t, 1 == ret);
  ret = cfg_sget(config, "data", "port", "%d", &port);
  ut_assert(t, 1 == ret);
  ret = cfg_sget(config, "data", "file", "%s", &file);
  ut_assert(t, 1 == ret);

  ut_assert(t, 0 == strcmp("192.0.3.44", server));
  ut_assert(t, 80 == port);
  ut_assert(t, 0 == strcmp("game-data.txt", file));

  ret = cfg_sget(config, "data", "port-fake", "%d", &port);
  ut_assert(t, 0 == ret);
  ut_assert(t, 80 == port);

  cfg_free(config);
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

  ut_suite_t *const s = ut_suite_new(&config, "cfg");

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "load",
                            NULL,
                            cfg_fixture_setup,
                            load_t,
                            cfg_fixture_teardown);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

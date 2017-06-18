/**
 * @file
 *
 * @brief Main option parse unit test suite.
 * @details Collects tests and helper methods for the main option parse module.
 *
 * @par ut_main_option_parse.c
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
#include "main_option_parse.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
a00_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
}

static void
a01_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 1);
  ut_assert(t, opt == -1);
}

static void
a02_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "foo", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 1);
  ut_assert(t, opt == -1);
}

static void
a03_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "foo", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 1);
  ut_assert(t, opt == -1);
}

static void
a04_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "g";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, options.optopt == 'f');
  ut_assert(t, strcmp(options.errmsg, "invalid option -- 'f'") == 0);
  ut_assert(t, opt == '?');
}

static void
a05_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
}

static void
a06_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f:";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, options.optopt == 'f');
  ut_assert(t, strcmp(options.errmsg, "option requires an argument -- 'f'") == 0);
  ut_assert(t, opt == ':');
}

static void
a07_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", "arg", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f:";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 3);
  ut_assert(t, opt == 'f');
  ut_assert(t, strcmp(options.optarg, "arg") == 0);
}

static void
a08_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-farg", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f::";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
  ut_assert(t, strcmp(options.optarg, "arg") == 0);
}

static void
a09_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f::";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
  ut_assert(t, options.optarg == NULL);
}

static void
a10_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", "-b", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "f::";

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
  ut_assert(t, options.optarg == NULL);
}

static void
a11_t (ut_test_t *const t)
{
  mop_options_t options;
  int opt;
  bool a_flag, b_flag, d_flag, f_flag, g_flag;
  char *f_arg, *g_arg;

  char *argv[] = { "./prog_name", "-f", "arg", "-b", "-c", "-a", "-g", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "abdf:g:";

  a_flag = false;
  b_flag = false;
  d_flag = false;
  f_flag = false;
  f_arg  = NULL;
  g_flag = false;
  g_arg  = NULL;

  mop_init(&options, argc, argv);

  while ((opt = mop_parse(&options, optionstring)) != -1) {
    switch (opt) {
    case 'a':
      a_flag = true;
      break;
    case 'b':
      b_flag = true;
      break;
    case 'd':
      b_flag = true;
      break;
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
      break;
    case 'g':
      g_flag = true;
      g_arg = options.optarg;
      break;
    case ':':
      ut_assert(t, options.optopt == 'g');
      break;
    case '?':
      ut_assert(t, options.optind == 5);
      ut_assert(t, options.optopt == 'c');
      ut_assert(t, strcmp(options.errmsg, "invalid option -- 'c'") == 0);
      ut_assert(t, opt == '?');
      break;
    default:
      ut_assert(t, false);
    }
  }

  ut_assert(t, a_flag == true);
  ut_assert(t, b_flag == true);
  ut_assert(t, d_flag == false);
  ut_assert(t, f_flag == true);
  ut_assert(t, strcmp(f_arg, "arg") == 0);
  ut_assert(t, g_flag == false);
  ut_assert(t, g_arg == NULL);

  while ((mop_arg(&options))) {
    ut_assert(t, false);
  }

}

static void
a12_t (ut_test_t *const t)
{
  mop_options_t options;
  int opt;
  bool a_flag, b_flag, d_flag, f_flag, g_flag;
  char *f_arg, *g_arg;
  char *arg;
  int i;

  char *argv[] = { "./prog_name", "-f", "arg", "-b", "--", "-c", "-a", "-g", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  char *optionstring = "abdf:g:";

  char *expected_args[] = { "-c", "-a", "-g" };

  a_flag = false;
  b_flag = false;
  d_flag = false;
  f_flag = false;
  f_arg  = NULL;
  g_flag = false;
  g_arg  = NULL;

  mop_init(&options, argc, argv);

  while ((opt = mop_parse(&options, optionstring)) != -1) {
    switch (opt) {
    case 'a':
      a_flag = true;
      break;
    case 'b':
      b_flag = true;
      break;
    case 'd':
      b_flag = true;
      break;
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
      break;
    case 'g':
      g_flag = true;
      g_arg = options.optarg;
      break;
    case ':':
      break;
    case '?':
      break;
    default:
      ut_assert(t, false);
    }
  }

  ut_assert(t, a_flag == false);
  ut_assert(t, b_flag == true);
  ut_assert(t, d_flag == false);
  ut_assert(t, f_flag == true);
  ut_assert(t, strcmp(f_arg, "arg") == 0);
  ut_assert(t, g_flag == false);
  ut_assert(t, g_arg == NULL);

  i = 0;
  while ((arg = mop_arg(&options))) {
    ut_assert(t, strcmp(arg, expected_args[i]) == 0);
    i++;
  }

}

static void
b00_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-a", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  int long_index = -1;

  mop_options_long_t long_options_array[] = {
    {"amend", 'a', MOP_NONE},
    {"brief", 'b', MOP_NONE},
    {"color", 'c', MOP_REQUIRED},
    {"delay", 'd', MOP_OPTIONAL},
    {0, 0, 0}
  };

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse_long(&options, long_options_array, &long_index);
  ut_assert(t, long_index == 0);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'a');
}

static void
b01_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "-b", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  int long_index = -1;

  mop_options_long_t long_options_array[] = {
    {"amend", 'a', MOP_NONE},
    {"brief", 'b', MOP_NONE},
    {"color", 'c', MOP_REQUIRED},
    {"delay", 'd', MOP_OPTIONAL},
    {0, 0, 0}
  };

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse_long(&options, long_options_array, &long_index);
  ut_assert(t, long_index == 1);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'b');
}

static void
b02_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "--amend", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  int long_index = -1;

  mop_options_long_t long_options_array[] = {
    {"amend", 'a', MOP_NONE},
    {"brief", 'b', MOP_NONE},
    {"color", 'c', MOP_REQUIRED},
    {"delay", 'd', MOP_OPTIONAL},
    {0, 0, 0}
  };

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse_long(&options, long_options_array, &long_index);
  ut_assert(t, long_index == 0);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'a');
}

static void
b03_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", "--ame", NULL };
  int argc = sizeof(argv) / sizeof(argv[0]);
  int long_index = -1;

  mop_options_long_t long_options_array[] = {
    {"amend", 'a', MOP_NONE},
    {"brief", 'b', MOP_NONE},
    {"color", 'c', MOP_REQUIRED},
    {"delay", 'd', MOP_OPTIONAL},
    {0, 0, 0}
  };

  mop_init(&options, argc, argv);
  ut_assert(t, options.optind == 1);
  const int opt = mop_parse_long(&options, long_options_array, &long_index);
  ut_assert(t, long_index == -1);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == '?');
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

  ut_suite_t *const s = ut_suite_new(&config, "main_option_parse");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a00", a00_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a01", a01_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a02", a02_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a03", a03_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a04", a04_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a05", a05_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a06", a06_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a07", a07_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a08", a08_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a09", a09_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a10", a10_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a11", a11_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "a12", a12_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "b00", b00_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "b01", b01_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "b02", b02_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "b03", b03_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

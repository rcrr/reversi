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

static void
aux_dummy (void)
{
  return;
}

static void
print_argv (char **argv)
{
  while (*argv)
    printf("%s ", *argv++);
  printf("\n");
}

static void
try_optparse (char **argv)
{
  int opt;
  char *arg;
  mop_options_t options;

  print_argv(argv);
  mop_init(&options, argv);
  while ((opt = mop_parse(&options, "abc:d::")) != -1) {
    if (opt == '?')
      printf("%s: %s\n", argv[0], options.errmsg);
    printf("%c (%d) = '%s'\n", opt, options.optind, options.optarg);
  }
  printf("optind = %d\n", options.optind);
  while ((arg = mop_arg(&options)))
    printf("argument: %s\n", arg);
}

static void
try_optparse_long (char **argv)
{
  char *arg;
  int opt, longindex;
  mop_options_t options;
  mop_options_long_t longopts[] = {
    {"amend", 'a', MOP_NONE},
    {"brief", 'b', MOP_NONE},
    {"color", 'c', MOP_REQUIRED},
    {"delay", 'd', MOP_OPTIONAL},
    {0, 0, 0}
  };

  print_argv(argv);
  mop_init(&options, argv);
  while ((opt = mop_parse_long(&options, longopts, &longindex)) != -1) {
    if (opt == '?')
      printf("%s: %s\n", argv[0], options.errmsg);
    printf("%c (%d, %d) = '%s'\n",
           opt, options.optind, longindex, options.optarg);
  }
  printf("optind = %d\n", options.optind);
  while ((arg = mop_arg(&options)))
    printf("argument: %s\n", arg);
}



/*
 * Test functions.
 */

static void
dummy_t (ut_test_t *const t)
{
  aux_dummy();
  ut_assert(t, true);
}

static void
base_t (ut_test_t *const t)
{
  char *long_argv[] = {
    "./prog", "--amend", "-b", "--color", "red", "--delay=22",
    "subcommand", "example.txt", "--amend", NULL
  };

  printf("\nOPTPARSE\n");
  try_optparse(long_argv);

  ut_assert(t, true);
}

static void
base_long_t (ut_test_t *const t)
{
  char *long_argv[] = {
    "./prog", "--amend", "-b", "--color", "red", "--delay=22",
    "subcommand", "example.txt", "--amend", NULL
  };

  printf("\nOPTPARSE LONG\n");
  try_optparse_long(long_argv);

  ut_assert(t, true);
}

static void
abc_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;
  char *arg;

  char *test_argv[] = {
    "./prog_name", "--amend", "-b", "--color", "red", "--delay=22",
    "subcommand", "example.txt", "--amend", NULL
  };

  char *optionstring = "abc:d::";

  printf("\n\n");
  print_argv(test_argv);
  mop_init(&options, test_argv);
  while ((opt = mop_parse(&options, optionstring)) != -1) {
    if (opt == '?')
      printf("%s: %s\n", test_argv[0], options.errmsg);
    printf("%c (%d) = '%s'\n", opt, options.optind, options.optarg);
  }
  printf("optind = %d\n", options.optind);
  while ((arg = mop_arg(&options)))
    printf("argument: %s\n", arg);

  ut_assert(t, true);
}

static void
a00_t (ut_test_t *const t)
{
  mop_options_t options;

  char *argv[] = { "./prog_name", NULL };

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
}

static void
a01_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", NULL };
  char *optionstring = "";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 1);
  ut_assert(t, opt == -1);
}

static void
a02_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "foo", NULL };
  char *optionstring = "";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 1);
  ut_assert(t, opt == -1);
}

static void
a03_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "foo", NULL };
  char *optionstring = "f";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 1);
  ut_assert(t, opt == -1);
}

static void
a04_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  char *optionstring = "g";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, options.optopt == 'f');
  ut_assert(t, strcmp(options.errmsg, "invalid option -- 'f'") == 0);
  ut_assert(t, opt == '?');
}

static void
a05_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  char *optionstring = "f";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
}

static void
a06_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  char *optionstring = "f:";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, options.optopt == 'f');
  ut_assert(t, strcmp(options.errmsg, "option requires an argument -- 'f'") == 0);
  ut_assert(t, opt == '?');
}

static void
a07_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  char *optionstring = ":f:";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 2);
  ut_assert(t, options.optopt == 'f');
  ut_assert(t, strcmp(options.errmsg, "option requires an argument -- 'f'") == 0);
  ut_assert(t, opt == '?');
}

static void
a08_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", "argument-for-the-f-option", NULL };
  char *optionstring = "f:";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  ut_assert(t, options.optind == 3);
  ut_assert(t, opt == 'f');
  ut_assert(t, strcmp(options.optarg, "argument-for-the-f-option") == 0);
}

static void
a09_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-farg", "argument-for-the-f-option", NULL };
  char *optionstring = "f::";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  printf("\n\n\nopt=%d, opt_as_char=%c\n", opt, (char) opt);
  printf("options.errmsg=%s\n", options.errmsg);
  printf("options.optind=%d\n", options.optind);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
  printf("options.optarg=%s\n", options.optarg);
  ut_assert(t, strcmp(options.optarg, "arg") == 0);
}

static void
a10_t (ut_test_t *const t)
{
  int opt;
  mop_options_t options;

  char *argv[] = { "./prog_name", "-f", NULL };
  char *optionstring = "f::";

  mop_init(&options, argv);
  ut_assert(t, options.optind == 1);
  opt = mop_parse(&options, optionstring);
  printf("\n\n\nopt=%d, opt_as_char=%c\n", opt, (char) opt);
  printf("options.errmsg=%s\n", options.errmsg);
  printf("options.optind=%d\n", options.optind);
  ut_assert(t, options.optind == 2);
  ut_assert(t, opt == 'f');
  ut_assert(t, options.optarg == NULL);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("main_option_parse");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", dummy_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "base", base_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "base_long", base_long_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "abc", abc_t);
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

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

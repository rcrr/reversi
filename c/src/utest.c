/**
 * @file
 *
 * @brief Runs unit tests.
 * @details This executable runs a group of tests.
 *
 * @par utest.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2017 Roberto Corradini. All rights reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <sys/wait.h>

#include "unit_test.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static char **
prepare_args (char *prog_name);



/*
 * Internal variables and constants.
 */

static ut_prog_arg_config_t arg_config;

static const char const* program_name = "utest";
static const char const* program_version = "1.0";
static const char const* program_description = "Unit test runner";
static const char const* program_long_description =
  "  The utest executable is part of the Reversi program.\n"
  "  This program is designed to run a test suite.\n"
  "  Type utest -h to learn how to use it.\n"
  "  Visit the web site http://github.com/rcrr/reversi for more info, and to obtain the source code.";
static const char const* program_author =
  "  Written by Roberto Corradini <rob_corradini@yahoo.it>";
static const char const* program_copyright =
  "Copyright (c) 2015, 2017 Roberto Corradini. All rights reserved.";
static const char const* program_license =
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n";

/**
 * @endcond
 */



/**
 * @brief Main entry for the utest program.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_init(&arg_config, true);
  ut_prog_arg_config_set_prog_name(&arg_config, program_name);
  ut_prog_arg_config_set_prog_version(&arg_config, program_version);
  ut_prog_arg_config_set_prog_description(&arg_config, program_description);
  ut_prog_arg_config_set_prog_copyright(&arg_config, program_copyright);
  ut_prog_arg_config_set_prog_long_desc(&arg_config, program_long_description);
  ut_prog_arg_config_set_prog_license(&arg_config, program_license);
  ut_prog_arg_config_set_prog_author(&arg_config, program_author);

  int next_arg_index = 0;
  int parse_status = ut_parse_args(&arg_config, stdout, &argc, &argv, &next_arg_index);

  if (parse_status < 0) return parse_status;
  if (parse_status == 1) return 0;

  int err_count = 0;

  for (int i = next_arg_index; i < argc; i++) {
    char *test_program_name = argv[i];
    bool test_pass = false;
    if (access(test_program_name, F_OK) == -1 ) {
      perror("Function access()");
      fprintf(stderr, "%s: test program %s doesn't exist.\n", argv[0], test_program_name);
      exit(EXIT_FAILURE);
    }
    if (access(test_program_name, X_OK) == -1 ) {
      perror("Function access");
      fprintf(stderr, "%s: test program %s is not executable.\n", argv[0], test_program_name);
      exit(EXIT_FAILURE);
    }

    pid_t child_pid = fork();
    if (child_pid == -1) {
      perror("Function fork()");
      fprintf(stderr, "%s, fork failed.", argv[0]);
      exit(EXIT_FAILURE);
    } else if (child_pid == 0) {
      char **argv_tprog = prepare_args(test_program_name);
      argv_tprog[0] = test_program_name;
      int ret = execv(test_program_name, argv_tprog);
      if (ret == -1) {
        perror("Function execv()");
        fprintf(stderr, "%s: returning from fork gave an error.", argv[0]);
        exit(EXIT_FAILURE);
      } else { // This should never happen.
        perror("Function execv()");
        fprintf(stderr, "%s: returning from fork gave an error. Return value is %d.\n", argv[0], ret);
        abort();
      }
    } else {
      fprintf(stdout, "TEST - %s: launching test program %s ... ( pid = %zu )\n", argv[0], test_program_name, (size_t) child_pid);
      fflush(stdout);
      int status;
      wait(&status);
      const int exit_value = WEXITSTATUS(status);
      char exit_msg[32] = {'\0'};
      if (WIFSIGNALED(status)) {
        fprintf(stdout, "\nKILLED - Killed by signal %d\n", WTERMSIG(status));
        if (WTERMSIG(status) == SIGABRT) sprintf(exit_msg, " program aborted (SIGABRT).");
        err_count++;
      } else {
        if (!exit_value) test_pass = true;
        sprintf(exit_msg, " exit code: %d", exit_value);
        if (exit_value > 0) err_count += exit_value;
        if (exit_value < 0) err_count++;
      }
      char *pass_or_fail_string = (test_pass) ? "PASS" : "FAIL";
      fprintf(stdout, "%s - %s: test program %s, child process ( pid = %zu )%s\n",
              pass_or_fail_string, argv[0], test_program_name, (size_t) child_pid, exit_msg);
    }
  }

  if (err_count != 0) fprintf(stdout, "ERROR: utest encountered one or more errors or failing tests.\n");
  return err_count;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static char **
prepare_args (char *prog_name)
{
  static char max_quickness_s[2];
  int argc;

  if (arg_config.print_test_list) {
    argc = 2;
    char **argv = (char **) malloc((argc + 1) * sizeof(char *));
    argv[0] = prog_name;
    argv[1] = "-l";
    argv[2] = NULL;
    return argv;
  }

  /*
   * argc = program_name + 2 * MODE  + UTEST_FLAG + 2 * test_paths + 2 * skip_paths + 2
   *      = 6 + 2 * (test_paths + skip_paths)
   * argc++ if (-v or -q flags are turned on);
   */
  argc = 6 + 2 * (llist_length(arg_config.test_paths) + llist_length(arg_config.skip_paths));
  if (arg_config.verb != UT_VEROSITY_STND) argc++;

  char **argv = (char **) malloc((argc + 1) * sizeof(char *));

  argv[0] = prog_name;
  argv[1] = "-m";
  if (arg_config.mode == UT_MODE_STND) argv[2] = "standard";
  else if (arg_config.mode >= UT_MODE_PERF) argv[2] = "perf";
  else if (arg_config.mode >= UT_MODE_ALL) argv[2] = "all";
  else {
    fprintf(stderr, "Invalid arg_config.mode \"%d\" option.\n", arg_config.mode);
    abort();
  }

  argv[3] = "--utest";

  int index = 4;
  if (arg_config.verb != UT_VEROSITY_STND) {
    if (arg_config.verb == UT_VEROSITY_LOW) argv[index++] = "-q";
    else if (arg_config.verb == UT_VEROSITY_HIGHT) argv[index++] = "-v";
    else {
      fprintf(stderr, "Invalid arg_config.verb \"%d\" option.\n", arg_config.verb);
      abort();
    }
  }

  for (llist_elm_t *e = (arg_config.test_paths)->head; e; e = e->next) {
    char *path = (char *) e->data;
    argv[index++] = "-p";
    argv[index++] = path;
  }

  for (llist_elm_t *e = (arg_config.skip_paths)->head; e; e = e->next) {
    char *path = (char *) e->data;
    argv[index++] = "-s";
    argv[index++] = path;
  }

  sprintf(max_quickness_s, "%d", arg_config.max_quickness);
  argv[index++] = "-k";
  argv[index++] = max_quickness_s;

  assert(index == argc);
  argv[index] = NULL;
  return argv;
}

/**
 * @endcond
 */

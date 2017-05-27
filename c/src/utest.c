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
#include <unistd.h>
#include <sys/wait.h>

#include "unit_test.h"



/******************************************************/
/* Internal static variable declarations.             */
/******************************************************/

static ut_prog_arg_config_t arg_config;

static const char const* program_name = "utest";
static const char const* program_version = "0.1";
static const char const* program_description = "Unit test runner.";
static const char const* program_copyright = "Copyright (c) 2015 Roberto Corradini. All rights reserved.";
static const char const* program_long_description =
  "The utest executable is part of the Reversi program.\n"
  "This program is designed to run a test suite.\n"
  "Type utest -h to learn how to use it.\n"
  "Visit the web site http://github.com/rcrr/reversi for more info, and to obtain the source code.";
static const char const* program_license =
  "This program is free software; you can redistribute it and/or modify it\n"
  "under the terms of the GNU General Public License as published by the\n"
  "Free Software Foundation; either version 3, or (at your option) any\n"
  "later version.\n"
  "\n"
  "This program is distributed in the hope that it will be useful,\n"
  "but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
  "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the\n"
  "GNU General Public License for more details.\n"
  "\n"
  "You should have received a copy of the GNU General Public License\n"
  "along with this program; if not, write to the Free Software\n"
  "Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA\n"
  "or visit the site <http://www.gnu.org/licenses/>.";



static void
print_program_version (void)
{
  fprintf(stdout, "%s version %s\n", program_name, program_version);
}



static void
print_program_license (void)
{
  fprintf(stdout, "%s\n", program_license);
}



static void
print_program_copyright (void)
{
  fprintf(stdout, "%s\n", program_copyright);
}



static void
print_program_info (void)
{
  fprintf(stdout, "%s version %s - %s\n", program_name, program_version, program_description);
  fprintf(stdout, "%s\n", program_copyright);
  fprintf(stdout, "%s\n", program_long_description);
}



static void
parse_args (int *argc_p,
            char ***argv_p)
{
  int argc = *argc_p;
  char **argv = *argv_p;

  arg_config.print_test_list = false;
  arg_config.mode = UT_MODE_STND;
  arg_config.test_paths = llist_new(NULL);
  arg_config.skip_paths = llist_new(NULL);
  arg_config.verb = UT_VEROSITY_STND;

  /* Parses known args. */
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-l") == 0) {
      arg_config.print_test_list = true;
      argv[i] = NULL;
    } else if (strcmp("-m", argv[i]) == 0 || strncmp("-m=", argv[i], 3) == 0) {
      char *equal = argv[i] + 2;
      char *mode = NULL;
      if (*equal == '=')
        mode = equal + 1;
      else if (i + 1 < argc) {
        argv[i++] = NULL;
        mode = argv[i];
      } else {
        fprintf(stderr, "%s: missing MODE value after -m flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      if (strcmp(mode, "perf") == 0) arg_config.mode = UT_MODE_PERF_0;
      else if (strcmp(mode, "standard") == 0) arg_config.mode = UT_MODE_STND;
      else {
        fprintf(stderr, "%s: MODE value \"%s\" is invalid.\n", argv[0], mode);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
    } else if (strcmp("-p", argv[i]) == 0 || strncmp("-p=", argv[i], 3) == 0) {
      char *equal = argv[i] + 2;
      char *test_path = NULL;
      if (*equal == '=')
        test_path = equal + 1;
      else if (i + 1 < argc) {
        argv[i++] = NULL;
        test_path = argv[i];
      } else {
        fprintf(stderr, "%s: missing TESTPATH value after -l flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
      if (test_path) llist_add(arg_config.test_paths, test_path);
    } else if (strcmp("-s", argv[i]) == 0 || strncmp("-s=", argv[i], 3) == 0) {
      char *equal = argv[i] + 2;
      char *skip_path = NULL;
      if (*equal == '=')
        skip_path = equal + 1;
      else if (i + 1 < argc) {
        argv[i++] = NULL;
        skip_path = argv[i];
      } else {
        fprintf(stderr, "%s: missing TESTPATH value after -s flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
      if (skip_path) llist_add(arg_config.skip_paths, skip_path);
    } else if (strcmp("-q", argv[i]) == 0 || strcmp("--quiet", argv[i]) == 0) {
      arg_config.verb = UT_VEROSITY_LOW;
      argv[i] = NULL;
    } else if (strcmp("-v", argv[i]) == 0 || strcmp("--verbose", argv[i]) == 0) {
      arg_config.verb = UT_VEROSITY_HIGHT;
      argv[i] = NULL;
    } else if (strcmp("--license", argv[i]) == 0) {
      print_program_license();
      argv[i] = NULL;
    } else if (strcmp("--version", argv[i]) == 0) {
      print_program_version();
      argv[i] = NULL;
    } else if (strcmp("--copyright", argv[i]) == 0) {
      print_program_copyright();
      argv[i] = NULL;
    } else if (strcmp("--info", argv[i]) == 0) {
      print_program_info();
      argv[i] = NULL;
    } else if (strcmp("-?", argv[i]) == 0 ||
               strcmp("-h", argv[i]) == 0 ||
               strcmp("--help", argv[i]) == 0) {
      printf("Usage:\n"
             "  %s [OPTIONS] testprogram...\n\n"
             "Help Options:\n"
             "  -h, --help                  Show help options\n\n"
             "Info Options:\n"
             "  --license                   Show software license\n"
             "  --version                   Show software version\n"
             "  --copyright                 Show software copyright\n"
             "  --info                      Show software info\n\n"
             "Test Options:\n"
             "  -l                          List test cases available in a test executable\n"
             "  -m {perf|standard}          Execute tests according to mode\n"
             "  -p TESTPATH                 Only start test cases matching TESTPATH\n"
             "  -s TESTPATH                 Skip all tests matching TESTPATH\n"
             "  -q, --quiet                 Run tests quietly\n"
             "  -v, --verbose               Run tests verbosely\n",
             argv[0]);
      exit(0);
    }
  }

  /* Packs argv removing null values. */
  int count = 1;
  for (int i = 1; i < argc; i++) {
    if (argv[i]) {
      argv[count++] = argv[i];
      if (i >= count)
        argv[i] = NULL;
    }
  }
  *argc_p = count;

}



static char **
prepare_args (void)
{
  int argc;

  if (arg_config.print_test_list) {
    argc = 2;
    char **argv = (char **) malloc((argc + 1) * sizeof(char *));
    argv[0] = "TBD";
    argv[1] = "-l";
    argv[2] = NULL;
    return argv;
  }

  /*
   * argc = program_name + 2 * MODE  + UTEST_FLAG + 2 * test_paths + 2 * skip_paths
   *      = 4 + 2 * (test_paths + skip_paths)
   * argc++ if (-v or -q flags are turned on);
   */
  argc = 4 + 2 * (llist_length(arg_config.test_paths) + llist_length(arg_config.skip_paths));
  if (arg_config.verb != UT_VEROSITY_STND) argc++;

  char **argv = (char **) malloc((argc + 1) * sizeof(char *));

  argv[0] = "TBD";
  argv[1] = "-m";
  if (arg_config.mode == UT_MODE_STND) argv[2] = "standard";
  else if (arg_config.mode >= UT_MODE_PERF_0) argv[2] = "perf";
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

  argv[index] = NULL;
  return argv;
}



/**
 * @brief Main entry for the utest program.
 */
int
main (int argc,
      char **argv)
{
  parse_args(&argc, &argv);

  int err_count = 0;

  for (int i = 1; i < argc; i++) {
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
      char **argv_tprog = prepare_args();
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

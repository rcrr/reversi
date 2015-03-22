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
 * @copyright 2015 Roberto Corradini. All rights reserved.
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
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>

/* Static constants. */



/* Static variables. */



/**
 * @brief Main entry for the unit test utility.
 */
int
main (int argc, char *argv[])
{
  pid_t cpid, w;
  int ret;
  int status;

  printf("utest: start\n");

  ret = 0;

  /*
  char *argv_new[] = { "ls", "-l", (char *)0 };
  ret = execv("/bin/ls", argv_new);
  */

  cpid = fork();

  if (cpid == 0) {
    //char *argv_new[] = { "llist_test", "-l", (char *)NULL };
    printf("utest: launching llist_test ...\n");

    char **argv_new = (char **) malloc((argc + 1) * sizeof(char *));
    argv_new[0] = "llist_test";
    for (int i = 1; i < argc; i++) {
      argv_new[i] = argv[i];
    }
    //char *argv_new[] = { "llist_test", argv[1] };
    ret = execv("./build/test/bin/llist_test", argv_new);
  } else {
    do {
      w = waitpid(cpid, &status, WUNTRACED | WCONTINUED);
      if (w == -1) {
        perror("waitpid");
        exit(EXIT_FAILURE);
      }

      if (WIFEXITED(status)) {
        printf("exited, status=%d\n", WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        printf("killed by signal %d\n", WTERMSIG(status));
      } else if (WIFSTOPPED(status)) {
        printf("stopped by signal %d\n", WSTOPSIG(status));
      } else if (WIFCONTINUED(status)) {
        printf("continued\n");
      }
    } while (!WIFEXITED(status) && !WIFSIGNALED(status));
    printf("\n");
    printf("ret=%d\n", ret);
    exit(EXIT_SUCCESS);
  }

  return 0;
}

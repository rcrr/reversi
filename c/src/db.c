/**
 * @file
 *
 * @brief Data Base utilities and programs.
 * @details This executable read and write game position db.
 *
 * @par db.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013 Roberto Corradini. All rights reserved.
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
#include "board.h"

#define MY_BUFSIZ 8;

/**
 * Data Base main entry.
 * 
 * Has to be completly developed.
 */
int main(int argc, char *argv[])
{

  FILE *fp;
  char *line;

  char *getline(FILE *);

  printf("Hello, db user!\n");

  if (argc == 1)
    //filecopy(stdin, stdout);
    line = getline(stdin);
  else
    while (--argc > 0)
      if ((fp = fopen(*++argv, "r")) == NULL) {
        printf("db: can't open %s\n", *argv);
        return 1;
      } else {
        //filecopy(fp, stdout);
        line = getline(fp);
        printf("line = %s", line);
        fclose(fp);
      }

  return 0;

}

char *getline(FILE *f)
{
  size_t size = 0;
  char * buf  = NULL;

  int c;
  int char_counter;

  char_counter = 0;
  for (;;) {
    if (char_counter + 1 - size > 0) {
      size += MY_BUFSIZ;
      buf = realloc(buf, size);
    }
    if (((c = fgetc(f)) != '\n' && c != EOF)) {
      buf[char_counter++] = c;
    } else {
      if (c != EOF) buf[char_counter++] = c;
      buf[char_counter++] = '\0';
      return buf;
    }
  }
}

/*
void filecopy(FILE *ifp, FILE *ofp)
{
  int c;

  while ((c = getc(ifp)) != EOF)
    putc(c, ofp);
}
*/

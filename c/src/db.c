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

#define BUFFER_SIZE 64;

typedef struct {
  char   **lines;
  int      line_counter;
  size_t   allocated_size;
} LineList;

typedef struct {
  char   *id;
  Board  *board;
  Player *player;
  char   *desc;
} GamePositionDbEntry;

typedef struct {
  GamePositionDbEntry **positions;
  int                   number_of_positions;
  char                 *desc;
} GamePositionDb;

const static char field_separator = ';'; /* Field separator for records in the game position db. */

int   db_validate(const LineList *const llp);
char *get_line(FILE *f);
void  load_line_list(FILE *f, LineList *llp);

/**
 * Data Base main entry.
 * 
 * Has to be completly developed.
 */
int main(int argc, char *argv[])
{
  LineList *llp;
  FILE *fp;

  llp = malloc(sizeof(LineList *));

  if (argc == 1) {
    load_line_list(stdin, llp);
  } else {
    while (--argc > 0)
      if ((fp = fopen(*++argv, "r")) == NULL) {
        printf("db: can't open %s\n", *argv);
        return 1;
      } else {
        load_line_list(fp, llp);
        fclose(fp);
      }
  }

  db_validate(llp);

  /* Release the memory allocated. */
  // to be done ....

  return 0;
}

int db_validate(const LineList *const llp)
{
  char                *line;
  char                *db_record;
  char                 c;
  char                *cp0;
  char                *cp1;
  int                  char_counter;
  int                  line_counter;
  GamePositionDbEntry *game_position_db_entry;
  char                 tmp_board[65];

  tmp_board[64] = '\0';

  for (line_counter = 0; line_counter < llp->line_counter; line_counter++) {
    line = llp->lines[line_counter];

    db_record = NULL;
    char_counter = 0;
    while ((c = line[char_counter])) {
      if (c == '#' || c == '\n') {
        break;
      }
      char_counter++;
    }

    if (char_counter > 0) {
      db_record = malloc((char_counter + 1) * sizeof(db_record));
      sprintf(db_record, "%.*s", char_counter, line);
      printf("%s\n", db_record);

      game_position_db_entry = malloc(sizeof(game_position_db_entry));

      cp0 = db_record;
      if ((cp1 = strchr(cp0, field_separator)) != NULL) {
        strncpy(game_position_db_entry->id = malloc((cp1 - cp0 + 1) * sizeof(game_position_db_entry->id)), cp0, cp1 - cp0);
        game_position_db_entry->id[cp1 - cp0] = '\0';
      } else {
        printf("ERROR!\n\n");
      }
      printf("game_position_db_entry->id=%s\n", game_position_db_entry->id);
      cp0 = cp1 + 1;
      if ((cp1 = strchr(cp0, field_separator)) != NULL) {
        if ((cp1 - cp0) != 64) {
          printf("ERROR: board pieces must be 64! Found %ld\n", cp1 - cp0);
        }
        strncpy(tmp_board, cp0, 64);
        printf("tmp_board=%s\n", tmp_board);
        SquareSet blacks = 0ULL;
        SquareSet whites = 0ULL;
        for (int i = 0; i < 64; i++) {
          c = tmp_board[i];
          switch (c) {
          case 'b':
            blacks |= 1ULL << i;
            break;
          case 'w':
            whites |= 1ULL << i;
            break;
          case '.':
            break;
          default:
            printf("ERROR: board pieces must be in 'b', 'w', or '.' character set. Found %c\n", c);
          }
        }
        game_position_db_entry->board = new_board(blacks, whites);
        printf("Board:\n%s\n", board_print(game_position_db_entry->board));

      } else {
        printf("ERROR!\n\n");
      }


    }

    //printf("line %6d:%s", i, line);
    free(db_record);
  }
  return 0;
}

void load_line_list(FILE *fp, LineList *llp)
{
  while (!feof(fp)) {
    if (llp->line_counter + 1 - (int) llp->allocated_size > 0) {
      llp->allocated_size += BUFFER_SIZE;
      llp->lines = realloc(llp->lines, llp->allocated_size * sizeof(char **));
    }
    llp->lines[llp->line_counter++] = get_line(fp);
  }
}

char *get_line(FILE *f)
{
  size_t  size;
  char   *buf;
  int     char_counter;
  int     c;

  size         = 0;
  buf          = NULL;
  char_counter = 0;

  for (;;) {
    if (char_counter + 2 - (int) size > 0) { // +2 takes into account the next c that will be read and the appended \0.
      size += BUFFER_SIZE;
      buf = realloc(buf, size);
    }
    if (((c = fgetc(f)) != EOF)) {
      buf[char_counter++] = c;
    }
    if (c == '\n' || c == EOF) {
      buf[char_counter++] = '\0';
      return buf;
    }
  }
}

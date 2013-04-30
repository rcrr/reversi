/**
 * @file
 *
 * @brief Data Base utilities and programs for `GamePosition` structures.
 * @details This executable read and write game position db.
 *
 * @par game_position_db.c
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
#include "rb.h"

#include <glib.h>

#include "game_position_db.h"

typedef struct {
  char   **lines;
  int      line_counter;
  size_t   allocated_size;
} LineList;

const static char field_separator = ';'; /* Field separator for records in the game position db. */
const static  int buffer_size     = 64;  /* Buffer size for allocating dynamic space. */

 int   db_validate(const LineList *const llp);
char * get_line(FILE *f);
void   load_line_list(FILE *f, LineList *llp);
 int   compare_entries (const void *pa, const void *pb, void *param);

/**
 * Data Base main entry.
 * 
 * Has to be completly developed.
 */
int main(int argc, char *argv[])
{
  LineList *llp;
  FILE *fp;

  // *** Test usage of the GLib library.
  GSList* list = NULL;
  printf("The list is now %d items long\n", g_slist_length(list));
  list = g_slist_prepend(list, "first");
  printf("The list is now %d items long\n", g_slist_length(list));
  list = g_slist_append(list, "second");
  printf("The list is now %d items long\n", g_slist_length(list));
  g_slist_free(list);
  // ***



  llp = malloc(sizeof(LineList *));
  llp->line_counter = 0;
  llp->allocated_size = 0;
  llp->lines = NULL;

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


  // It is a quick-and-dirty test for the GLib file library ....
  gpdb_load(fp = fopen("./db/test-db.txt", "r"), NULL);
  fclose(fp);


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

  // Has to be passed as function parameter.
  // Create a new Red-Black Tree used as a dictionary for Game Position Entries.
  struct rb_table *tree;
  struct libavl_allocator *allocator = NULL;
  void *param = NULL;
  tree = rb_create(compare_entries, param, allocator);
  if (tree == NULL) {
    printf ("ERROR! Out of memory creating tree.\n");
    return 1;
  }


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
        printf("ERROR! The record is missing the id field.\n");
        goto error;
      }
      printf("game_position_db_entry->id=%s\n", game_position_db_entry->id);

      cp0 = cp1 + 1;
      if ((cp1 = strchr(cp0, field_separator)) != NULL) {
        if ((cp1 - cp0) != 64) {
          printf("ERROR: board pieces must be 64! Found %ld\n", cp1 - cp0);
          goto error;
        }
        strncpy(tmp_board, cp0, 64);
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
            goto error;
          }
        }
        game_position_db_entry->board = board_new(blacks, whites);
        printf("Board:\n%s\n", board_print(game_position_db_entry->board));
      } else {
        printf("ERROR! The record is missing the board field.\n");
        goto error;
      }

      cp0 = cp1 + 1;
      if ((cp1 = strchr(cp0, field_separator)) != NULL) {
        if ((cp1 - cp0) != 1) {
          printf("ERROR: player field must be one char! Found %ld\n", cp1 - cp0);
          goto error;
        }
        Player p;
        c = *cp0;
        switch (c) {
        case 'b':
          p = BLACK_PLAYER;
          break;
        case 'w':
          p = WHITE_PLAYER;
          break;
        default:
          printf("ERROR: player must be in 'b', or 'w' character set. Found %c\n", c);
          goto error;
        }
        game_position_db_entry->player = p;
        printf("Player: %s\n", player_description(game_position_db_entry->player));
      } else {
        printf("ERROR! The record is missing the player field.\n");
        goto error;
      }

      cp0 = cp1 + 1;
      if ((cp1 = strchr(cp0, field_separator)) != NULL) {
        game_position_db_entry->desc = malloc(((cp1 - cp0) + 1) * sizeof(game_position_db_entry->desc));
        strncpy(game_position_db_entry->desc, cp0, cp1 - cp0);
        printf("Description: %s\n", game_position_db_entry->desc);
      } else {
        printf("ERROR! The record is missing the description field.\n");
        goto error;
      }

      // Add the game position db entry to the dictionary.
      // How the dictionary is managed?
      GamePositionDbEntry *entry;
      if ((entry = rb_find(tree, game_position_db_entry)) != NULL) {
        printf("Item already inserted into the tree.\n");
      } else {
        printf("Item is new.\n");
        void **p = rb_probe(tree, game_position_db_entry);
        if (p == NULL)
          printf("ERROR! Allocating space for tree entry failed.\n");
      }

      printf("\n\n");

    }

  error:
    free(db_record);
  }
  return 0;
}

void load_line_list(FILE *fp, LineList *llp)
{
  while (!feof(fp)) {
    if (llp->line_counter + 1 - (int) llp->allocated_size > 0) {
      llp->allocated_size += buffer_size;
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
      size += buffer_size;
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

/* Comparison function for entries.*/
int compare_entries(const void *pa, const void *pb, void *param)
{
  const GamePositionDbEntry *a = pa;
  const GamePositionDbEntry *b = pb;

  return strcmp(a->id, b->id);
}

int gpdb_load(FILE *fp, GError **e)
{
  GIOChannel *channel;
  GError     *err;
  GIOStatus   ret;
  gchar      *msg;
  gsize       len;

  channel = NULL;
  err     = NULL;

  channel = g_io_channel_unix_new(fileno(fp));

  do {
    ret = g_io_channel_read_line(channel, &msg, &len, NULL, &err);
    if (ret == G_IO_STATUS_ERROR)
      g_error("Error reading: %s\n", err->message);

    printf("Read %lu bytes: %s\n", len, msg);
    g_free(msg);
  } while (ret != G_IO_STATUS_EOF);

  g_io_channel_shutdown(channel, TRUE, &err);
  if (err) {
    g_propagate_error(e, err);
    return EXIT_FAILURE;
  }

  g_io_channel_unref(channel);
  return EXIT_SUCCESS;
}

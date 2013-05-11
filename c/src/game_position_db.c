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

#include <glib.h>

#include "game_position_db.h"

static gint extract_entry_from_line(gchar                           *line,
                                    GamePositionDbEntry             *entry,
                                    GamePositionDbEntrySyntaxError **syntax_error);

static gint compare_entries(gconstpointer pa,
                            gconstpointer pb);

const static char field_separator = ';'; /* Field separator for records in the game position db. */

GamePositionDbEntrySyntaxError
*gpdb_entry_syntax_error_new(GamePositionDbEntrySyntaxErrorType  error_type,
                             char                               *source,
                             int                                 line_number,
                             char                               *line,
                             char                               *error_message)
{
  GamePositionDbEntrySyntaxError *e;
  static const size_t size_of_e = sizeof(GamePositionDbEntrySyntaxError);

  e = (GamePositionDbEntrySyntaxError*) g_malloc(size_of_e);
  g_assert(e);

  e->error_type    = error_type;
  e->source        = source;
  e->line_number   = line_number;
  e->line          = line;
  e->error_message = error_message;

  return e;
}

/**
 * @brief GamePositionDb structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * game position database structure is not `NULL`.
 *
 * @param [in] desc a string descibing the database
 * @return          a pointer to a new game position database structure
 */
GamePositionDb *gpdb_new(char *desc)
{
  GamePositionDb *db;
  static const size_t size_of_db = sizeof(GamePositionDb);

  db = (GamePositionDb*) g_malloc(size_of_db);
  g_assert(db);

  db->tree = g_tree_new(compare_entries);
  db->desc = desc;

  return db;
}

/**
 * @brief Inserts the entries found in file `fp` into the `db` database.
 *
 * An assertion checks that the received pointer to the allocated
 * game position database structure is not `NULL`.
 *
 * @param [in]     fp                a pointer to the file that is loaded 
 * @param [in,out] db                a pointer to the data base that is updated
 * @param [out]    syntax_error_log  the list of syntax errors 
 * @param [out]    e                 a location to return an error reference 
 * @return                           the return code
 */
int gpdb_load(FILE *fp,  GamePositionDb *db, GSList *syntax_error_log, GError **e)
{
  GIOChannel *channel;
  GIOStatus   ret;
  GError     *err;
  gchar      *line;
  gsize       line_len;
  GTree      *tree;
  int         line_number;

  if (!db)
    db = gpdb_new(NULL);
  tree = db->tree;
  channel = g_io_channel_unix_new(fileno(fp));
  line_number = 0;

  do {
    line_number++;

    err = NULL;
    ret = g_io_channel_read_line(channel, &line, &line_len, NULL, &err);
    if (ret == G_IO_STATUS_ERROR)
      g_error("Error reading: %s\n", err->message);

    GamePositionDbEntrySyntaxError *syntax_error = NULL;
    GamePositionDbEntry *entry = NULL;
    extract_entry_from_line(line, entry, &syntax_error);
    if (syntax_error) {
      syntax_error->source = "NULL";
      syntax_error->line_number = line_number;
      syntax_error_log = g_slist_append(syntax_error_log, syntax_error);
    }

    if (entry) {
      g_tree_insert(tree, entry->id, entry);
    }

  } while (ret != G_IO_STATUS_EOF);

  err = NULL;
  g_io_channel_shutdown(channel, TRUE, &err);
  if (err) {
    g_propagate_error(e, err);
    return EXIT_FAILURE;
  }

  g_io_channel_unref(channel);
  return EXIT_SUCCESS;
}

/* Comparison function for entries.*/
static gint compare_entries(gconstpointer pa,
                            gconstpointer pb)
{
  const GamePositionDbEntry *a = pa;
  const GamePositionDbEntry *b = pb;

  return strcmp(a->id, b->id);
}

static gint extract_entry_from_line(gchar *line,
                                    GamePositionDbEntry *entry,
                                    GamePositionDbEntrySyntaxError **syntax_error)
{
  gchar *record;
  int    record_length;
  gchar  c;
  gchar *cp0;
  gchar *cp1;

  /* If line is null return. */
  if (!line)
    return EXIT_SUCCESS;

  /* Variable initialization. */
  record = NULL;
  record_length = 0;
  cp0 = NULL;
  cp1 = NULL;

  /* Computes the record_length, removing everything following a dash. */
  while ((c = line[record_length])) {
    if (c == '#' || c == '\n') {
      break;
    }
    record_length++;
  }

  /* Prepares the record by stripping the comments, or exit if the line is empty or a comment. */
  if (record_length == 0)
    return EXIT_SUCCESS;
  record = g_malloc((record_length + 1) * sizeof(record));
  sprintf(record, "%.*s", record_length, line);
  entry = g_malloc0(sizeof(entry));

  /* Extracts the key (id field). */
  cp0 = record;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    strncpy(entry->id = g_malloc((cp1 - cp0 + 1) * sizeof(entry->id)), cp0, cp1 - cp0);
    entry->id[cp1 - cp0] = '\0';
  } else {
    *syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_ON_ID,
                                                NULL,
                                                -1,
                                                line,
                                                "The record does't have the proper separator identifying the id field.");
    g_free(entry->id);
    g_free(entry);
    g_free(record);
    entry = NULL;
    return EXIT_FAILURE;
  }

  /* Extracts the board field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    if ((cp1 - cp0) != 64) {
      *syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64,
                                                  NULL,
                                                  -1,
                                                  line,
                                                  "The record has the field board composed by x chars.");
      g_free(entry->id);
      g_free(entry);
      g_free(record);
      entry = NULL;
      return EXIT_FAILURE;
    }
    SquareSet blacks = 0ULL;
    SquareSet whites = 0ULL;
    for (int i = 0; i < 64; i++) {
      c = cp0[i];
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
        //goto error;
        break;
      }
    }
    entry->board = board_new(blacks, whites);
  } else {
    printf("ERROR! The record is missing the board field.\n");
    //goto error;
  }

  /* Extracts the player field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    if ((cp1 - cp0) != 1) {
      printf("ERROR: player field must be one char! Found %ld\n", cp1 - cp0);
      //goto error;
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
      //goto error;
      break;
    }
    entry->player = p;
    printf("Player: %s\n", player_description(entry->player));
  } else {
    printf("ERROR! The record is missing the player field.\n");
    //goto error;
  }

  /* Extracts the description field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    entry->desc = g_malloc(((cp1 - cp0) + 1) * sizeof(entry->desc));
    strncpy(entry->desc, cp0, cp1 - cp0);
    printf("Description: %s\n", entry->desc);
  } else {
    printf("ERROR! The record is missing the description field.\n");
    //goto error;
  }

  return EXIT_SUCCESS;
}

GString *gpdb_entry_syntax_error_print(GamePositionDbEntrySyntaxError *syntax_error)
{
  gchar et_string[64];

  GString *msg = g_string_new("");

  if (!syntax_error)
    return msg;

  GamePositionDbEntrySyntaxErrorType et = syntax_error->error_type;

  gchar *em = syntax_error->error_message;
  if (!em)
    strcpy(em, "NULL");

  gchar *sl = syntax_error->source;
  if (!sl)
    strcpy(sl, "NULL");

  gchar ln_string[64];
  int ln = syntax_error->line_number;
  if (ln > 0) {
    sprintf(ln_string, "%d", ln);
  } else {
    strcpy(ln_string, "UNDEFINED LINE NUMBER");
  }

  GString *l;
  gchar *line_end = strchr(syntax_error->line, '\n');
  if (line_end)
    l = g_string_new_len(syntax_error->line, line_end - syntax_error->line);
  else
    l = g_string_new(syntax_error->line);

  switch (et) {
  case GPDB_ENTRY_SYNTAX_ERROR_ON_ID:
    strcpy(et_string, "The id field is not correctly assigned.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64:
    strcpy(et_string, "The board field must have 64 chars.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_C:
    strcpy(et_string, "C");
    break;
  default:
    abort();
    break;
  }

  g_string_append_printf(msg, "Error type:    %s\n", et_string);
  g_string_append_printf(msg, "Error message: %s\n", em);
  g_string_append_printf(msg, "Source label:  %s\n", sl);
  g_string_append_printf(msg, "Line number:   %s\n", ln_string);
  g_string_append_printf(msg, "Line:          %s\n", l->str);

  g_string_free(l, TRUE);

  return msg;
}

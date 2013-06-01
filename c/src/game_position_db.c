/**
 * @file
 *
 * @brief Data Base utilities and programs for `GamePosition` structures.
 * @details This executable read and write game position db.
 *
 * @todo Complete documentation for game_position_db.c
 * @todo Take a final approach for testing glib vs cunit.
 * @todo Retrofit board.c and board.h with the new style.
 * @todo Complete the gpdb_game_samples.txt database.
 * @todo Write a program that read and validate a database file.
 * @todo Verify that a new entry is not replacing an existing one.
 * @todo Write a print function for the GamePosition.
 * @todo Write a print function for the Entry.
 * @todo Replace Board and Player with GamePosition in an Entry.
 * @todo Write al the missing tests.
 * @todo Make the makefile more friendly for the tests. 
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

/*
 * Internal variables and constants.
 */

/**
 * Field separator for records in the game position db.
 */
const static char
field_separator = ';';

/*
 * Prototypes for internal functions.
 */

static gint
extract_entry_from_line (gchar                           *line,
                         GamePositionDbEntry            **p_entry,
                         GamePositionDbEntrySyntaxError **p_syntax_error);

static gint
compare_entries (gconstpointer pa,
                 gconstpointer pb,
                 gpointer      user_data);


static void
gpdb_tree_value_destroy_function (gpointer data);

static void
gpdb_tree_key_destroy_function (gpointer data);


/*
 * Public functions.
 */

/***************************************************************************/
/* Function implementations for the GamePositionDbEntrySyntaxError entity. */ 
/***************************************************************************/

/**
 * @brief Game position database syntax error structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * game position database syntax error structure is not `NULL`.
 *
 * Parameters `source`, `line`, and `error_message` must be dynamically allocated
 * if the `gpdb_entry_syntax_error_delete` function will be called on the returned
 * structure's pointer.
 *
 * @param [in] error_type    the type of the error
 * @param [in] source        the label identifying the source input
 * @param [in] line_number   the line number that rises the error 
 * @param [in] line          a string holding the line that rises the error 
 * @param [in] error_message a detailed error message
 * @return                   a pointer to a new game position database syntax error structure
 */
GamePositionDbEntrySyntaxError *
gpdb_entry_syntax_error_new (GamePositionDbEntrySyntaxErrorType  error_type,
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
 * @brief Game position database syntax error structure structure destructor.
 *
 * The fields belonging to the error parameter `error` must
 * be not shared elsewhere. This function frees them all.
 *
 * @invariant Parameter `error` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] error the pointer to be deallocated
 * @return           always the NULL pointer
 */
GamePositionDbEntrySyntaxError *
gpdb_entry_syntax_error_delete (GamePositionDbEntrySyntaxError *error)
{
  g_assert(error);

  g_free(error->source);
  g_free(error->line);
  g_free(error->error_message);

  g_free(error);
  error = NULL;

  return error;
}

/**
 * @brief Game position database structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * game position database structure is not `NULL`.
 *
 * @param [in] desc a string descibing the database
 * @return          a pointer to a new game position database structure
 */
GamePositionDb *
gpdb_new (char *desc)
{
  GamePositionDb *db;
  static const size_t size_of_db = sizeof(GamePositionDb);

  db = (GamePositionDb*) g_malloc(size_of_db);
  g_assert(db);

  gpointer key_compare_data = NULL;
  db->tree = g_tree_new_full((GCompareDataFunc) compare_entries,
                             key_compare_data,
                             (GDestroyNotify) gpdb_tree_key_destroy_function,
                             (GDestroyNotify) gpdb_tree_value_destroy_function);
  db->desc = desc;

  return db;
}

/**
 * @brief Game position database structure destructor.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] db           the pointer to be deallocated
 * @param [in] free_segment if yes also frees the data stored in the db
 * @return                  always the NULL pointer
 */
GamePositionDb *
gpdb_delete (GamePositionDb *db,
             gboolean        free_segment)
{
  g_assert(db);

  if (free_segment) {
    if (db->desc)
      g_free(db->desc);
    if (db->tree) {
      g_tree_destroy(db->tree);
    }
  }

  g_free(db);
  db = NULL;

  return db;
}

/**
 * @brief Inserts the entries found in file `fp` into the `db` database.
 *
 * When the received pointer to the allocated game position database
 * structure is `NULL` return code is `EXIT_FAILURE`.
 *
 * @param [in]     fp                a pointer to the file that is loaded 
 * @param [in]     source            a string documenting the source of the loaded records 
 * @param [in,out] db                a pointer to the data base that is updated
 * @param [out]    syntax_error_log  the list of syntax errors 
 * @param [out]    p_e               a location to return an error reference 
 * @return                           the return code
 */
int
gpdb_load (FILE            *fp,
           gchar           *source,
           GamePositionDb  *db,
           GSList          *syntax_error_log,
           GError         **p_e)
{
  GIOChannel *channel;
  GIOStatus   ret;
  GError     *err;
  gchar      *line;
  gsize       line_len;
  GTree      *tree;
  int         line_number;

  if (!db)
    return EXIT_FAILURE;

  tree = db->tree;
  channel = g_io_channel_unix_new(fileno(fp));
  line_number = 0;

  do {
    line_number++;

    err = NULL;
    ret = g_io_channel_read_line(channel, &line, &line_len, NULL, &err);
    if (err) {
      g_propagate_error(p_e, err);
      return EXIT_FAILURE;
    }

    GamePositionDbEntrySyntaxError *syntax_error = NULL;
    GamePositionDbEntry *entry = NULL;
    extract_entry_from_line(line, &entry, &syntax_error);
    if (syntax_error) {
      syntax_error->source = source;
      syntax_error->line_number = line_number;
      syntax_error_log = g_slist_append(syntax_error_log, syntax_error);
    } else {
      g_free(line);
      if (entry) {
        g_tree_insert(tree, entry->id, entry);
      }
    }
    line = NULL;

  } while (ret != G_IO_STATUS_EOF);

  err = NULL;
  g_io_channel_shutdown(channel, TRUE, &err);
  if (err) {
    g_propagate_error(p_e, err);
    return EXIT_FAILURE;
  }

  g_io_channel_unref(channel);

  return EXIT_SUCCESS;
}

/**
 * @brief Game position database entry structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * game position database entry structure is not `NULL`.
 *
 * @return a pointer to a new empty game position database entry structure
 */
GamePositionDbEntry *
gpdb_entry_new (void)
{
  GamePositionDbEntry *entry;
  static const size_t size_of_entry = sizeof(GamePositionDbEntry);

  entry = (GamePositionDbEntry *) g_malloc0(size_of_entry);
  g_assert(entry);

  return entry;
}

/**
 * @brief Game position database entry structure destructor.
 *
 * @invariant Parameter `entry` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] entry        the pointer to be deallocated
 * @param [in] free_segment if yes also free the data referenced by the entry
 * @return                  always the NULL pointer
 */
GamePositionDbEntry *
gpdb_entry_delete (GamePositionDbEntry *entry,
                   gboolean             free_segment)
{
  g_assert(entry);

  if (free_segment) {
    g_free(entry->id);
    board_delete(entry->board);
    g_free(entry->desc);
  }

  g_free(entry);
  entry = NULL;

  return entry;
}

/**
 * @brief Returns a formatted `GString` holding a represention of the syntax error.
 *
 * The returned structure has a dynamic extent set by a call to malloc.
 * It must then properly garbage collected by a call to free when no more referenced.
 *
 * Parameter `syntax_error` can be `NULL`, then the returned string is empty.
 *
 * @param [in] syntax_error a pointer to the syntax error structure
 * @return                  a message describing the error structure
 */
GString *
gpdb_entry_syntax_error_print (const GamePositionDbEntrySyntaxError const *syntax_error)
{
  gchar et_string[128];

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

  gchar ln_string[16];
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
  case GPDB_ENTRY_SYNTAX_ERROR_SQUARE_CHAR_IS_INVALID:
    strcpy(et_string, "The board field must be composed of 'b', 'w', and '.' chars only.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_BOARD_FIELD_IS_INVALID:
    strcpy(et_string, "The board field is not correctly assigned or terminated.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_PLAYER_IS_NOT_ONE_CHAR:
    strcpy(et_string, "The player field is not one char.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_PLAYER_CHAR_IS_INVALID:
    strcpy(et_string, "The player char is not 'b' or 'w'.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_PLAYER_FIELD_IS_INVALID:
    strcpy(et_string, "The player field is not correctly assigned or terminated.");
    break;
  case GPDB_ENTRY_SYNTAX_ERROR_DESC_FIELD_IS_INVALID:
    strcpy(et_string, "The description field is not correctly assigned or terminated.");
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

/**
 * @brief Prints the syntax error log.
 *
 * @todo Function implementation must be done! 
 */
GString *
gpdb_print_syntax_error_log (GSList *syntax_error_log)
{
  return g_string_new("");
}


/*
 * Internal functions.
 */

/**
 * @brief Comparison function for game position database entries.
 *
 * @invariant Parameters `pa` and `pb` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param pa        the constant pointer to the key of the first structure
 * @param pb        the constant pointer to the key of the second structure
 * @param user_data not used
 * @return          `-1` if `pa` precedes `pb`, `+1` if `pa` is greater than `pb`,
 *                  and `0` if the two object are equal
 */
static gint
compare_entries (gconstpointer pa,
                 gconstpointer pb,
                 gpointer      user_data)
{
  g_assert(pa && pb);

  const char *a = (char *) pa;
  const char *b = (char *) pb;

  return strcmp(a, b);
}

/**
 * @brief Extracts a game position database entry from the input line.
 *
 * @param line
 * @param p_entry
 * @param p_syntax_error
 * @return             the status of the operation
 */
static gint
extract_entry_from_line (gchar                           *line,
                         GamePositionDbEntry            **p_entry,
                         GamePositionDbEntrySyntaxError **p_syntax_error)
{
  gchar   *record;
  int      record_length;
  gchar    c;
  gchar   *cp0;
  gchar   *cp1;
  GString *error_msg;

  GamePositionDbEntry *entry;

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
  entry = gpdb_entry_new();

  /* Extracts the key (id field). */
  cp0 = record;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    strncpy(entry->id = g_malloc((cp1 - cp0 + 1) * sizeof(entry->id)), cp0, cp1 - cp0);
    entry->id[cp1 - cp0] = '\0';
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record does't have the proper separator identifying the id field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_ON_ID,
                                                  NULL,
                                                  -1,
                                                  line,
                                                  error_msg->str);
    g_free(entry->id);
    g_free(entry);
    g_free(record);
    g_string_free(error_msg, FALSE);
    entry = NULL;
    return EXIT_FAILURE;
  }

  /* Extracts the board field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    if ((cp1 - cp0) != 64) {
      error_msg = g_string_new("");
      g_string_append_printf(error_msg, "The record has the field board composed by %d chars.", (int) (cp1 - cp0));
      *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64,
                                                    NULL,
                                                    -1,
                                                    line,
                                                    error_msg->str);
      g_free(entry->id);
      g_free(entry);
      g_free(record);
      g_string_free(error_msg, FALSE);
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
        error_msg = g_string_new("");
        g_string_append_printf(error_msg, "Board pieces must be in 'b', 'w', or '.' character set. Found %c", c);
        *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_SQUARE_CHAR_IS_INVALID,
                                                      NULL,
                                                      -1,
                                                      line,
                                                      error_msg->str);
        g_free(entry->id);
        g_free(entry);
        g_free(record);
        g_string_free(error_msg, FALSE);
        entry = NULL;
        return EXIT_FAILURE;
      }
    }
    entry->board = board_new(blacks, whites);
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record doesn't have a proper terminated board field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_BOARD_FIELD_IS_INVALID,
                                                  NULL,
                                                  -1,
                                                  line,
                                                  error_msg->str);
    g_free(entry->id);
    g_free(entry);
    g_free(record);
    g_string_free(error_msg, FALSE);
    entry = NULL;
    return EXIT_FAILURE;
  }

  /* Extracts the player field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    if ((cp1 - cp0) != 1) {
      error_msg = g_string_new("");
      g_string_append_printf(error_msg, "The record has the field player composed by %d chars.", (int) (cp1 - cp0));
      *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_PLAYER_IS_NOT_ONE_CHAR,
                                                    NULL,
                                                    -1,
                                                    line,
                                                    error_msg->str);
      g_free(entry->id);
      board_delete(entry->board);
      g_free(entry);
      g_free(record);
      g_string_free(error_msg, FALSE);
      entry = NULL;
      return EXIT_FAILURE;
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
      error_msg = g_string_new("");
      g_string_append_printf(error_msg, "Player must be in 'b', or 'w' character set. Found %c", c);
      *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_PLAYER_CHAR_IS_INVALID,
                                                    NULL,
                                                    -1,
                                                    line,
                                                    error_msg->str);
      g_free(entry->id);
      board_delete(entry->board);
      g_free(entry);
      g_free(record);
      g_string_free(error_msg, FALSE);
      entry = NULL;
      return EXIT_FAILURE;
    }
    entry->player = p;
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record doesn't have a proper terminated player field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_PLAYER_FIELD_IS_INVALID,
                                                  NULL,
                                                  -1,
                                                  line,
                                                  error_msg->str);
    g_free(entry->id);
    board_delete(entry->board);
    g_free(entry);
    g_free(record);
    g_string_free(error_msg, FALSE);
    entry = NULL;
    return EXIT_FAILURE;
  }

  /* Extracts the description field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    entry->desc = g_malloc(((cp1 - cp0) + 1) * sizeof(entry->desc));
    strncpy(entry->desc, cp0, cp1 - cp0);
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record doesn't have a proper terminated description field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_DESC_FIELD_IS_INVALID,
                                                  NULL,
                                                  -1,
                                                  line,
                                                  error_msg->str);
    g_free(entry->id);
    board_delete(entry->board);
    g_free(entry);
    g_free(record);
    g_string_free(error_msg, FALSE);
    entry = NULL;
    return EXIT_FAILURE;
  }

  g_free(record);
  *p_entry = entry;
  return EXIT_SUCCESS;
}

/**
 * @brief `GDestroyNotify` function used by `g_tree_new_full`
 *        in `gpdb_new` for the value field.
 *
 * @param data a pointer to an entry
 */
static void
gpdb_tree_value_destroy_function (gpointer data)
{
  GamePositionDbEntry *entry = (GamePositionDbEntry *) data;
  gpdb_entry_delete(entry, TRUE);
}

/**
 * @brief `GDestroyNotify` function used by `g_tree_new_full`
 *        in `gpdb_new` for the key field.
 *
 * The function does nothing and could be substituted by a null pointer.
 *
 * @param data a pointer to the id field of the entry
 */
static void
gpdb_tree_key_destroy_function (gpointer data)
{
  char *id = (char *) data;
  if (id) ; // Nothing to do here.
}

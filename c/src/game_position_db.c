/**
 * @file
 *
 * @brief Data Base utilities and programs for `GamePositionX` structures.
 * @details This executable read and write game position db.
 *
 * @par game_position_db.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <string.h>

#include "game_position_db.h"



/**
 * @cond
 */

/*
 * Internal variables and constants.
 */

/* Field separator for records in the game position db. */
static const char field_separator = ';';



/*
 * Prototypes for internal functions.
 */

static gint
gpdb_extract_entry_from_line (gchar *line,
                              int line_number,
                              gchar *source,
                              GamePositionDbEntry **p_entry,
                              gpdb_entry_syntax_error_t **p_syntax_error);

static gint
gpdb_compare_entries (gconstpointer pa,
                      gconstpointer pb,
                      gpointer user_data);


static void
gpdb_tree_value_destroy_function (gpointer data);

static void
gpdb_tree_key_destroy_function (gpointer data);

static void
gpdb_syntax_error_log_destroy_function (gpointer data);

static gboolean
gpdb_print_entry_helper_fn (gchar *key,
                            GamePositionDbEntry *entry,
                            GString **p_msg);

/**
 * @endcond
 */



/*
 * Public functions.
 */

/*************************************************************************/
/* Function implementations for the GamePositionDbSyntaxErrorLog entity. */
/*************************************************************************/

/**
 * @brief Game position database syntax error log constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * syntax error log is not `NULL`.
 *
 * @return a pointer to a new syntax error log
 */
GamePositionDbSyntaxErrorLog *
gpdb_syntax_error_log_new (void)
{
  GamePositionDbSyntaxErrorLog *syntax_error_log;
  syntax_error_log = NULL;
  return syntax_error_log;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_syntax_error_log_new.
 *
 * @details The error structures, content of the log,  must
 * be not shared elsewhere. This function frees them all.
 *
 * If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] syntax_error_log the pointer to be deallocated
 */
void
gpdb_syntax_error_log_free (GamePositionDbSyntaxErrorLog *syntax_error_log)
{
  if (syntax_error_log) {
    g_slist_free_full(syntax_error_log, (GDestroyNotify) gpdb_syntax_error_log_destroy_function);
  }
}

/**
 * @brief Returns a formatted string holding a represention of the syntax error log.
 *
 * The returned structure has a dynamic extent set by a call to g_malloc.
 * It must then properly garbage collected by a call to g_free when no more referenced.
 *
 * Parameter `syntax_error_log` can be `NULL`, then the returned string is empty.
 *
 * @param [in] syntax_error_log a pointer to the syntax error log structure
 * @return                      a message describing the error log structure
 */
gchar *
gpdb_syntax_error_log_print (GamePositionDbSyntaxErrorLog *syntax_error_log)
{
  gchar   *result;
  GString *msg;
  GSList  *element;

  msg = g_string_new("");
  element = syntax_error_log;

  if (!element) {
    msg = g_string_append(msg, "No errors detected.\n");
  } else {
    do {
      gpdb_entry_syntax_error_t *syntax_error = (gpdb_entry_syntax_error_t *) element->data;
      if (syntax_error != NULL) {
        gchar *s = gpdb_entry_syntax_error_print(syntax_error);
        msg = g_string_append(msg, s);
        g_free(s);
      }
    } while ((element = g_slist_next(element)) != NULL);
  }

  result = msg->str;
  g_string_free(msg, FALSE);
  return result;
}

/**
 * @brief Returns the number of items contained by the error log.
 *
 * Parameter `syntax_error_log` can be `NULL`, then the returned length is zero.
 *
 * @param [in] syntax_error_log a pointer to the syntax error log structure
 * @return                      the length of the error log
 */
int
gpdb_syntax_error_log_length (GamePositionDbSyntaxErrorLog *syntax_error_log)
{
  return g_slist_length(syntax_error_log);
}



/***************************************************************************/
/* Function implementations for the gpdb_entry_syntax_error_t entity. */
/***************************************************************************/

/**
 * @brief Game position database syntax error structure constructor.
 *
 * @invariant Parameter `error_type` must be in the range defined by the enum type.
 * The invariant is guarded by an assertion.
 *
 * An assertion checks that the received pointer to the allocated
 * game position database syntax error structure is not `NULL`.
 *
 * Parameters `source`, `line`, and `error_message` must be dynamically allocated
 * if the #gpdb_entry_syntax_error_free destructor function will be called on the
 * returnedstructure's pointer.
 * When one of this parameter is `NULL` the value `"N/A"` is assigned.
 * These parameters are property of the structure and must be freed only by the
 * structure destructor.
 *
 * @param [in] error_type    the type of the error
 * @param [in] source        the label identifying the source input
 * @param [in] line_number   the line number that rises the error
 * @param [in] line          a string holding the line that rises the error
 * @param [in] error_message a detailed error message
 * @return                   a pointer to a new game position database syntax error structure
 */
gpdb_entry_syntax_error_t *
gpdb_entry_syntax_error_new (gpdb_entry_syntax_error_type_t error_type,
                             char *source,
                             int line_number,
                             char *line,
                             char *error_message)
{
  g_assert(error_type >= GPDB_ENTRY_SYNTAX_ERROR_ON_ID &&
           error_type <= GPDB_ENTRY_SYNTAX_ERROR_DUPLICATE_ENTRY_KEY);

  gpdb_entry_syntax_error_t *e;

  static const size_t size_of_e = sizeof(gpdb_entry_syntax_error_t);
  static const gchar *NA = "N/A";

  e = (gpdb_entry_syntax_error_t*) g_malloc(size_of_e);
  g_assert(e);

  e->error_type = error_type;

  if (source)
    e->source = source;
  else
    e->source = g_strdup(NA);

  e->line_number = line_number;

  if (line)
    e->line = line;
  else
    e->line = g_strdup(NA);

  if (error_message)
    e->error_message = error_message;
  else
    e->error_message = g_strdup(NA);

  return e;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_entry_syntax_error_new.
 *
 * @details The fields belonging to the error parameter `error` must
 * be not shared elsewhere. This function frees them all.
 *
 * If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] error the pointer to be deallocated
 */
void
gpdb_entry_syntax_error_free (gpdb_entry_syntax_error_t *error)
{
  if (error) {
    g_free(error->source);
    g_free(error->line);
    g_free(error->error_message);
    g_free(error);
  }
}

/**
 * @brief Returns a formatted string holding a represention of the syntax error.
 *
 * The returned structure has a dynamic extent set by a call to g_malloc.
 * It must then properly garbage collected by a call to g_free when no more referenced.
 *
 * Parameter `syntax_error` can be `NULL`, then the returned string is empty.
 *
 * @param [in] syntax_error a pointer to the syntax error structure
 * @return                  a message describing the error structure
 */
gchar *
gpdb_entry_syntax_error_print (const gpdb_entry_syntax_error_t const *syntax_error)
{
  gchar *result;

  gchar et_string[128];

  GString *msg = g_string_new("");

  if (!syntax_error) {
    result = msg->str;
    g_string_free(msg, FALSE);
    return result;
  }

  gpdb_entry_syntax_error_type_t et = syntax_error->error_type;

  gchar *em = syntax_error->error_message;
  if (!em)
    em = g_strdup("NULL");

  gchar *sl = syntax_error->source;
  if (!sl)
    sl = g_strdup("NULL");

  gchar ln_string[32];
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
  case GPDB_ENTRY_SYNTAX_ERROR_DUPLICATE_ENTRY_KEY:
    strcpy(et_string, "The key for the entry has been already loaded.");
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

  result = msg->str;
  g_string_free(msg, FALSE);
  return result;
}



/***********************************************************/
/* Function implementations for the GamePositionDb entity. */
/***********************************************************/

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

  gpointer compare_entries_data = NULL;
  db->tree = g_tree_new_full((GCompareDataFunc) gpdb_compare_entries,
                             compare_entries_data,
                             (GDestroyNotify) gpdb_tree_key_destroy_function,
                             (GDestroyNotify) gpdb_tree_value_destroy_function);
  db->desc = desc;

  return db;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] db           the pointer to be deallocated
 * @param [in]     free_segment if yes also frees the data stored in the db
 */
void
gpdb_free (GamePositionDb *db,
           gboolean free_segment)
{
  if (db) {
    if (free_segment) {
      if (db->desc)
        g_free(db->desc);
      if (db->tree) {
        g_tree_destroy(db->tree);
      }
    }
    g_free(db);
  }
}

/**
 * @brief Lookups into the `db` database.
 * It searches for an entry that match with the `entry_id` key.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] db       a pointer to the data base that is updated
 * @param [in] entry_id the entry key to search for
 * @return              the matching db entry or null when the query fails
 */
GamePositionDbEntry *
gpdb_lookup (GamePositionDb *db,
             gchar *entry_id)
{
  g_assert(db);

  GamePositionDbEntry *entry;
  entry = (GamePositionDbEntry *) g_tree_lookup(db->tree, entry_id);
  return entry;
}

/**
 * @brief Inserts the entries found in file `fp` into the `db` database.
 *
 * When the received pointer to the allocated game position database
 * structure is `NULL` return code is `EXIT_FAILURE`.
 *
 * @param [in]     fp                  a pointer to the file that is loaded
 * @param [in]     source              a string documenting the source of the loaded records
 * @param [in,out] db                  a pointer to the data base that is updated
 * @param [out]    p_syntax_error_log  a location to return the list of syntax errors
 * @param [out]    p_e                 a location to return an error reference
 * @return                             the return code
 */
int
gpdb_load (FILE *fp,
           gchar *source,
           GamePositionDb *db,
           GamePositionDbSyntaxErrorLog **p_syntax_error_log,
           GError **p_e)
{
  GIOChannel *channel;
  GIOStatus   ret;
  GError     *err;
  gchar      *line;
  gsize       line_len;
  GTree      *tree;
  int         line_number;
  GSList     *tmp_syntax_error_log;

  if (!db)
    return EXIT_FAILURE;

  tree = db->tree;
  channel = g_io_channel_unix_new(fileno(fp));
  line_number = 0;
  tmp_syntax_error_log = gpdb_syntax_error_log_new();

  do {
    line_number++;

    err = NULL;
    ret = g_io_channel_read_line(channel, &line, &line_len, NULL, &err);
    if (err) {
      g_propagate_error(p_e, err);
      return EXIT_FAILURE;
    }

    gpdb_entry_syntax_error_t *syntax_error = NULL;
    GamePositionDbEntry *entry = NULL;
    gpdb_extract_entry_from_line(line, line_number, source, &entry, &syntax_error);
    if (entry) {
      if (g_tree_lookup(tree, entry->id)) {
        syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_DUPLICATE_ENTRY_KEY,
                                                   g_strdup(source),
                                                   line_number,
                                                   line,
                                                   g_strdup_printf("id \"%s\" is duplicated.", entry->id));
        gpdb_entry_free(entry, TRUE);
      } else {
        g_tree_insert(tree, g_strdup(entry->id), entry);
        g_free(line);
      }
    }

    if (syntax_error) {
      tmp_syntax_error_log = g_slist_prepend(tmp_syntax_error_log, syntax_error);
    }

    if (!entry && !syntax_error)
      g_free(line);

    line = NULL;

  } while (ret != G_IO_STATUS_EOF);

  err = NULL;
  g_io_channel_shutdown(channel, TRUE, &err);
  if (err) {
    g_propagate_error(p_e, err);
    return EXIT_FAILURE;
  }

  *p_syntax_error_log = g_slist_concat(*p_syntax_error_log, g_slist_reverse(tmp_syntax_error_log));

  g_io_channel_unref(channel);

  return EXIT_SUCCESS;
}

/**
 * @brief Returns a formatted string holding a represention of the game position db.
 *
 * The returned structure has a dynamic extent set by a call to g_malloc.
 * It must then properly garbage collected by a call to g_free when no more referenced.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] db a pointer to the db structure
 * @return        a message describing the db structure
 */
gchar *
gpdb_print (GamePositionDb *db)
{
  g_assert(db);

  gchar   *result;
  GTree   *t;
  GString *msg;

  msg = g_string_new("");
  t = db->tree;

  g_tree_foreach(t, (GTraverseFunc) gpdb_print_entry_helper_fn, &msg);

  result = msg->str;
  g_string_free(msg, FALSE);

  return result;
}

/**
 * @brief Returns a formatted string holding a short represention of the game position db.
 *
 * The returned structure has a dynamic extent set by a call to g_malloc.
 * It must then properly garbage collected by a call to g_free when no more referenced.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] db a pointer to the db structure
 * @return        a short message describing the db structure
 */
gchar *
gpdb_print_summary (GamePositionDb *db)
{
  g_assert(db);

  gchar   *result;
  GTree   *t;
  int      entry_count;
  GString *msg;

  msg = g_string_new("");
  t = db->tree;
  entry_count = g_tree_nnodes(t);

  g_string_append_printf(msg, "The Game Position Database has %d entr%s.\n",
                         entry_count, (entry_count == 1) ? "y" : "ies");

  g_string_append_printf(msg, "Database Description: %s\n", db->desc);

  result = msg->str;
  g_string_free(msg, FALSE);

  return result;
}

/**
 * @brief Returns the number of items contained by db.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] db a pointer to the db structure
 * @return        the number of entries in the db
 */
int
gpdb_length (GamePositionDb *db)
{
  g_assert(db);
  return g_tree_nnodes(db->tree);
}



/****************************************************************/
/* Function implementations for the GamePositionDbEntry entity. */
/****************************************************************/

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
 * @brief Deallocates the memory previously allocated by a call to #gpdb_entry_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] entry        the pointer to be deallocated
 * @param [in]     free_segment if yes also frees the data stored in the db
 */
void
gpdb_entry_free (GamePositionDbEntry *entry,
                 gboolean free_segment)
{
  if (entry) {
    if (free_segment) {
      g_free(entry->id);
      game_position_x_free(entry->gpx);
      g_free(entry->desc);
    }
    g_free(entry);
  }
}

/**
 * @brief Returns a formatted string holding a represention of the db entry.
 *
 * The returned structure has a dynamic extent set by a call to g_malloc.
 * It must then properly garbage collected by a call to g_free when no more referenced.
 *
 * @param [in] entry a pointer to the game position db entry structure
 * @return           a message describing the entry structure
 */
gchar *
gpdb_entry_print (GamePositionDbEntry *entry)
{
  gchar   *result;
  GString *msg;

  char game_position_to_string[512];
  size_t length;

  length = game_position_x_print(game_position_to_string, entry->gpx);

  assert(length < 512);
  (void) length;

  msg = g_string_new("");

  g_string_append_printf(msg, "Entry id:    %s\n", entry->id);
  g_string_append_printf(msg, "Description: %s\n", entry->desc);
  g_string_append_printf(msg, "Game Position:\n%s\n", game_position_to_string);

  result = msg->str;
  g_string_free(msg, FALSE);

  return result;
}

/**
 * @brief Returns a newly allocated game position corresponding to the database entry.
 *
 * The returned structure has a dynamic extent set by a call to malloc.
 * It must then properly garbage collected by a call to free when no more referenced.
 *
 * @param [in] entry a pointer to the game position db entry structure
 * @return           the game position relative to the given db entry
 */
GamePositionX *
gpdb_get_gpx (GamePositionDbEntry *entry)
{
  g_assert(entry);
  return game_position_x_clone(entry->gpx);
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Comparison function for game position database entries.
 *
 * @invariant Parameters `pa` and `pb` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] pa        the constant pointer to the key of the first structure
 * @param [in] pb        the constant pointer to the key of the second structure
 * @param [in] user_data not used
 * @return               `-1` if `pa` precedes `pb`, `+1` if `pa` is greater than `pb`,
 *                       and `0` if the two object are equal
 */
static gint
gpdb_compare_entries (gconstpointer pa,
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
 * @param [in]  line           a string containing the db line
 * @param [in]  line_number    the line number used for logging in case of error
 * @param [in]  source         a string identifying the source of the line
 * @param [out] p_entry        a reference to a pointer to the database entry
 * @param [out] p_syntax_error a reference to a pointer to the syntax error
 * @return                     the status of the operation
 */
static gint
gpdb_extract_entry_from_line (gchar                           *line,
                              int                              line_number,
                              gchar                           *source,
                              GamePositionDbEntry            **p_entry,
                              gpdb_entry_syntax_error_t **p_syntax_error)
{
  gchar               *record;
  int                  record_length;
  gchar                c;
  gchar               *cp0;
  gchar               *cp1;
  GString             *error_msg;
  GamePositionDbEntry *entry;
  SquareSet            blacks;
  SquareSet            whites;
  Player               player;


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
                                                  g_strdup(source),
                                                  line_number,
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
                                                    g_strdup(source),
                                                    line_number,
                                                    line,
                                                    error_msg->str);
      g_free(entry->id);
      g_free(entry);
      g_free(record);
      g_string_free(error_msg, FALSE);
      entry = NULL;
      return EXIT_FAILURE;
    }
    blacks = 0ULL;
    whites = 0ULL;
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
                                                      g_strdup(source),
                                                      line_number,
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
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record doesn't have a proper terminated board field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_BOARD_FIELD_IS_INVALID,
                                                  g_strdup(source),
                                                  line_number,
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
                                                    g_strdup(source),
                                                    line_number,
                                                    line,
                                                    error_msg->str);
      g_free(entry->id);
      g_free(entry);
      g_free(record);
      g_string_free(error_msg, FALSE);
      entry = NULL;
      return EXIT_FAILURE;
    }
    c = *cp0;
    switch (c) {
    case 'b':
      player = BLACK_PLAYER;
      break;
    case 'w':
      player = WHITE_PLAYER;
      break;
    default:
      error_msg = g_string_new("");
      g_string_append_printf(error_msg, "Player must be in 'b', or 'w' character set. Found %c", c);
      *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_PLAYER_CHAR_IS_INVALID,
                                                    g_strdup(source),
                                                    line_number,
                                                    line,
                                                    error_msg->str);
      g_free(entry->id);
      g_free(entry);
      g_free(record);
      g_string_free(error_msg, FALSE);
      entry = NULL;
      return EXIT_FAILURE;
    }
    entry->gpx = game_position_x_new(blacks, whites, player);
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record doesn't have a proper terminated player field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_PLAYER_FIELD_IS_INVALID,
                                                  g_strdup(source),
                                                  line_number,
                                                  line,
                                                  error_msg->str);
    g_free(entry->id);
    g_free(entry);
    g_free(record);
    g_string_free(error_msg, FALSE);
    entry = NULL;
    return EXIT_FAILURE;
  }

  /* Extracts the description field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    entry->desc = g_malloc0(((cp1 - cp0) + 1) * sizeof(entry->desc));
    strncpy(entry->desc, cp0, cp1 - cp0);
  } else {
    error_msg = g_string_new("");
    g_string_append_printf(error_msg, "The record doesn't have a proper terminated description field.");
    *p_syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_DESC_FIELD_IS_INVALID,
                                                  g_strdup(source),
                                                  line_number,
                                                  line,
                                                  error_msg->str);
    g_free(entry->id);
    game_position_x_free(entry->gpx);
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
 * The function frees the tree's entry.
 *
 * @param [in,out] data a pointer to an entry
 */
static void
gpdb_tree_value_destroy_function (gpointer data)
{
  GamePositionDbEntry *entry = (GamePositionDbEntry *) data;
  gpdb_entry_free(entry, TRUE);
}

/**
 * @brief `GDestroyNotify` function used by `g_tree_new_full`
 *        in `gpdb_new` for the key field.
 *
 * The function frees the string used as the key for the tree's entry.
 *
 * @param [in,out] data a pointer to the id field of the entry
 */
static void
gpdb_tree_key_destroy_function (gpointer data)
{
  char *id = (char *) data;
  if (id)
    g_free(id);
}

/**
 * @brief `GDestroyNotify` function used by `g_slist_free_full`
 *        in `gpdb_syntax_error_log_free` for the list content.
 *
 * The function frees the content of the list.
 *
 * @param [in,out] data a pointer to the id field of the syntax error
 */
static void
gpdb_syntax_error_log_destroy_function (gpointer data)
{
  gpdb_entry_syntax_error_t *e = (gpdb_entry_syntax_error_t *) data;
  if (e)
    gpdb_entry_syntax_error_free(e);
}

/**
 * @brief `GTraverseFunc` function used by `g_tree_foreach`
 *        in `gpdb_print` for the list content.
 *
 * @param [in]  key   a pointer to the key
 * @param [in]  entry a pointer to the value
 * @param [out] p_msg a location to return a pointer for the message
 * @return            always `FALSE`
 */
static gboolean
gpdb_print_entry_helper_fn (gchar *key,
                            GamePositionDbEntry *entry,
                            GString **p_msg)
{
  GString *msg;
  gchar *entry_to_string;

  msg = *p_msg;
  entry_to_string = gpdb_entry_print(entry);

  g_string_append_printf(msg, "%s", entry_to_string);

  g_free(entry_to_string);

  return FALSE;
}

/**
 * @endcond
 */

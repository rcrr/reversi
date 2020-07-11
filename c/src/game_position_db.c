/**
 * @file
 *
 * @brief Data Base utilities for reading game positions from files.
 *
 * @details This module introduces five entities:
 *          - #gpdb_dictionary_t
 *          - #gpdb_entry_t
 *          - #gpdb_syntax_err_log_t
 *          - #gpdb_syntax_err_t
 *          - #gpdb_syntax_err_type_t
 *
 * #gpdb_dictionary_t is a table (sorted set) of entries.
 *
 * #gpdb_entry_t is an element of the dictionary, defined by three fields, a key (id), a
 *               description, and a game position.
 *
 * The dictionary can be populated by reading a database file by mean of
 * the function #gpdb_dictionary_load(). A sample usage scenario is:
 *
 * @code
 *
 * const bool duplicates_are_errors = true;
 * const bool replace_duplicates = false;
 * const bool stop_on_error = false;
 *
 * const char *const file_name = "db/gpdb-test-db.txt";
 * const char *const db_desc = "Test game position database";
 *
 * gpdb_dictionary_t *db = gpdb_dictionary_new(db_desc);
 *
 * gpdb_syntax_err_log_t *elog = gpdb_syntax_err_log_new();
 *
 * insertions = gpdb_dictionary_load(db,
 *                                   elog,
 *                                   file_name,
 *                                   duplicates_are_errors,
 *                                   replace_duplicates,
 *                                   stop_on_error);
 *
 * // Use the syntax error log.
 * gpdb_syntax_err_log_free(elog);
 *
 * // Use the dictionary and hits entries.
 * gpdb_dictionary_free(db);
 *
 * @endcode
 *
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
#include <errno.h>

#include "file_utils.h"
#include "game_position_db.h"



/**
 * @cond
 */

/* Used for file reading, it contains also string termination. */
#define MAX_LINE_LENGTH 4096

/*
 * Internal variables and constants.
 */



/*
 * Prototypes for internal functions.
 */

static void
gpdb_syntax_err_free_proxy (void *const element,
                            void *const aux);

static void
gpdb_syntax_err_print_proxy (void *const element,
                             void *const aux);

static int
gpdb_compare_entries (const void *item_a,
                      const void *item_b,
                      void *param);

static void
gpdb_tree_item_destroy_function (void *item,
                                 void *param);

static int
gpdb_parse_line (char *line,
                 const char *const file_name,
                 size_t line_number,
                 char *entry_id,
                 char *entry_description,
                 GamePositionX *entry_gpx,
                 gpdb_syntax_err_t **err);

static void
gpdb_syntax_err_log_reverse (gpdb_syntax_err_log_t *log);

/**
 * @endcond
 */



/*
 * Public functions.
 */

/*********************************************************/
/* Function implementations for the gpdb_entry_t entity. */
/*********************************************************/

/**
 * @brief Game position database entry structure constructor.
 *
 * @details The constructor allocates memory for the entry structure and then for the
 *          `id` and `description` fields.
 *          This memory is then released when the entry is freed by a call to #gpdb_entry_free().
 *          No dependency is left on the data referenced by the three arguments.
 *
 *          An assertion checks that the received pointer to the allocated
 *          game position database entry structure is not `NULL`.
 *
 * @invariant Parameters `id`, `description` and `gpx` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] id          entry unique key
 * @param [in] description entry description
 * @param [in] gpx         the game position associated to the entry
 * @return                 a pointer to a new empty game position database entry structure
 */
gpdb_entry_t *
gpdb_entry_new (const char *const id,
                const char *const description,
                const GamePositionX *const gpx)
{
  assert(id);
  assert(description);
  assert(gpx);

  int len;

  gpdb_entry_t *entry = (gpdb_entry_t *) malloc(sizeof(gpdb_entry_t));
  assert(entry);

  len = strlen(id);
  entry->id = malloc(len + 1);
  assert(entry->id);
  strcpy(entry->id, id);

  len = strlen(description);
  entry->description = malloc(len + 1);
  assert(entry->description);
  strcpy(entry->description, description);

  entry->gpx.blacks = gpx->blacks;
  entry->gpx.whites = gpx->whites;
  entry->gpx.player = gpx->player;

  return entry;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_entry_new().
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] entry the pointer to be deallocated
 */
void
gpdb_entry_free (gpdb_entry_t *entry)
{
  if (entry) {
    free(entry->id);
    free(entry->description);
    free(entry);
  }
}

/**
 * @brief Gets the `id` field.
 *
 * @invariant Parameter `entry` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] entry reference to the object
 * @return           the value of the `id` field
 */
char *
gpdb_entry_get_id (gpdb_entry_t *const entry)
{
  assert(entry);
  return entry->id;
}

/**
 * @brief Gets the `description` field.
 *
 * @invariant Parameter `entry` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] entry reference to the object
 * @return           the value of the `description` field
 */
char *
gpdb_entry_get_description (gpdb_entry_t *const entry)
{
  assert(entry);
  return entry->description;
}

/**
 * @brief Gets a `gpx` reference.
 *
 * @invariant Parameter `entry` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] entry reference to the object
 * @return           a pointer to the `gpx` structure
 */
GamePositionX *
gpdb_entry_get_gpx (gpdb_entry_t *const entry)
{
  assert(entry);
  return &entry->gpx;
}

/**
 * @brief Prints to the given `stream` a text representation of `entry`.
 *
 * @details When `verbose` is `false` the output stays on one line and
 *          it has the format of the record stored in game position db files.
 *          When `verbose` is `true` the output is multiline, and the
 *          game position is represented as a two dimensions 8x8 board.
 *
 * @invariant Parameter `entry` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] entry   reference to the entry
 * @param [in] stream  the target of the output
 * @param [in] verbose true for more output
 */
void
gpdb_entry_print (const gpdb_entry_t *const entry,
                  FILE *const stream,
                  const bool verbose)
{
  assert(entry);

  char buf[256]; // see game_position_x_print documentation ...

  if (verbose) {
    game_position_x_print(buf, &entry->gpx);
    fprintf(stream, "Entry id: %s\n", entry->id);
    fprintf(stream, "Description: %s\n", entry->description);
    fprintf(stream, "Game Position:\n%s\n", buf);
  } else {
    fprintf(stream, "%s;", entry->id);
    for (int pos = 0; pos < 64; pos++) {
      const SquareSet sq = (SquareSet) 1 << pos;
      char color = '.';
      if (sq & entry->gpx.blacks) {
        color = 'b';
      } else if (sq & entry->gpx.whites) {
        color = 'w';
      }
      fprintf(stream, "%c", color);
    }
    fprintf(stream, ";%c;", (entry->gpx.player == BLACK_PLAYER) ? 'b' : 'w');
    fprintf(stream, "%s;\n", entry->description);
  }
}



/**************************************************************/
/* Function implementations for the gpdb_dictionary_t entity. */
/**************************************************************/

/**
 * @brief Dictionary (a data base of game positions) structure constructor.
 *
 * @details The parameter `description` must be a pointer to a well
 *          formed string. The string is copied into freshly allocated
 *          memory. The parameter can be `NULL`.
 *          This memory is then released when the dictionary is freed by a call to #gpdb_dictionary_free().
 *          No dependency is left on the data referenced by the argument.
 *
 *          An assertion checks that the received pointer to the allocated
 *          game position database structure is not `NULL`.
 *
 * @param [in] description a string describing the database
 * @return                 a pointer to a new dictionary structure
 */
gpdb_dictionary_t *
gpdb_dictionary_new (const char *const description)
{
  gpdb_dictionary_t *db = (gpdb_dictionary_t*) malloc(sizeof(gpdb_dictionary_t));
  assert(db);

  /* Sets description. */
  db->description = NULL;
  gpdb_dictionary_set_description(db, description);

  /* Creates a new empty table. */
  rbt_item_compare_f *cmp = gpdb_compare_entries;
  void *cmp_param = NULL;
  mem_allocator_t *alloc = NULL; // when NULL default allocator is used.
  db->table = rbt_create(cmp, cmp_param, alloc);

  return db;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_dictionary_new().
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] db the pointer to be deallocated
 */
void
gpdb_dictionary_free (gpdb_dictionary_t *db)
{
  if (db) {
    if (db->description) free(db->description);
    rbt_destroy(db->table, gpdb_tree_item_destroy_function);
    free(db);
  }
}

/**
 * @brief Gets the `description` field.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] db reference to the object
 * @return        the value of the `description` field
 */
char *
gpdb_dictionary_get_description (const gpdb_dictionary_t *const db)
{
  assert(db);
  return db->description;
}

/**
 * @brief Sets the `description` field.
 *
 * @invariant Parameter `db` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in,out] db          reference to the object
 * @param [in]     description new value for the field
 */
void
gpdb_dictionary_set_description (gpdb_dictionary_t *const db,
                                 const char *const description)
{
  assert(db);

  if (db->description) free(db->description);
  if (description) {
    int len = strlen(description);
    db->description = malloc(len + 1);
    assert(db->description);
    strcpy(db->description, description);
  } else {
    db->description = NULL;
  }
}

/**
 * @brief Inserts `entry` into `db`, replacing any duplicate item.
 *
 * @details Returns `NULL` if `entry` was inserted without replacing a duplicate, or if a memory
 *          allocation error occurred. Otherwise, returns the item that was replaced.
 *
 * @invariant The `db` and `entry` arguments cannot be `NULL`.
 *
 * @param [in,out] db     the dictionary
 * @param [in]     entry  the element to be inserterted
 * @return               `NULL` or a pointer to the replaced `entry`
 */
gpdb_entry_t *
gpdb_dictionary_add_or_replace_entry (gpdb_dictionary_t *const db,
                                      gpdb_entry_t *entry)
{
  assert(db);
  assert(entry);

  gpdb_entry_t *replaced = rbt_replace(db->table, entry);

  return replaced;
}

/**
 * @brief Deletes from `db` and returns an entry matching `entry`.
 *
 * @details Returns a null pointer if no matching entry found.
 *
 * @invariant The `db` and `entry` arguments cannot be `NULL`.
 *
 * @param [in,out] db     the dictionary
 * @param [in]     entry  the element to be inserterted
 * @return                 a pointer to the deleted `entry` or `NULL`
 */
gpdb_entry_t *
gpdb_dictionary_delete_entry (gpdb_dictionary_t *const db,
                              const gpdb_entry_t *const entry)
{
  assert(db);
  assert(entry);

  gpdb_entry_t *deleted = rbt_delete(db->table, entry);

  return deleted;
}

/**
 * @brief Returns the number of entries collected by `db`.
 *
 * @invariant The `db` argument cannot be `NULL`.
 *
 * @param [in] db the dictionary
 * @return        dictionary entry count
 */
size_t
gpdb_dictionary_entry_count (const gpdb_dictionary_t *const db)
{
  assert(db);
  return rbt_count(db->table);
}

/**
 * @brief Searches `db` for an entry having the key matching `id` and returns it if found.
 *
 * @details Returns a null pointer if no matching entry exists in the dictionary.
 *
 * @invariant The `db` argument cannot be `NULL`.
 *
 * @param [in] db the dictionary
 * @param [in] id the key searched for
 * @return        a pointer to the matching  entry or `NULL`
 */
gpdb_entry_t *
gpdb_dictionary_find_entry (gpdb_dictionary_t *const db,
                            const char *const id)
{
  assert(db);
  if (!id) return NULL;

  gpdb_entry_t key = { .id = (char *) id };

  gpdb_entry_t *selected = (gpdb_entry_t *) rbt_find(db->table, &key);

  return selected;
}

/**
 * @brief Inserts the entries found in file `file_name` into the `db` dictionary.
 *
 * @details The function loads entries into the dictionary reading the file line by line.
 *          Lines can be an entry record or a comment.
 *          Comments are signaled by a line beginning with a `#` character.
 *          Well formed records are loaded, on the contrary syntax errors are logged
 *          in the `elog` list.
 *
 *          A well formed record has four fields separated by the `;` character:
 *          - id
 *          - squares
 *          - player
 *          - description
 *
 *          Everything coming after the `;` following the description is ignored.
 *
 *          The id field is a string, it is used as the entry's `id`. All characters
 *          are accepted with the exception of `;` and `#`.
 *
 *          The squares field must be a string of sixty-four characters. Accepted values
 *          are `w` for white, `b` for black, or `.` for empty.
 *
 *          The player field must be a string of one character. Accepted values
 *          are `w` for white, or `b` for black.
 *
 *          Fields squares and player are combined to define the game position contained
 *          into the loaded entry.
 *
 *          The description field is a string, it is used as the entry's `description`.
 *          All characters are accepted with the exception of `;` and `#`.
 *
 *          Arguments `duplicates_are_errors` and `replace_duplicates` define the behavior
 *          of the loader when a valid record has a key that is already part of the `db`.
 *          When `duplicates_are_errors` is `true` a specific error is logged and the record
 *          is discarded. When it is `false` the argument `replace_duplicates` specifies
 *          if the newly encountered record has to replace the already loaded one, or not.
 *          When replacement occurs, the existing entry is deallocated.
 *
 * @invariant Parameters `db`, `elog` and `file_name` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in,out] db                    a pointer to the dictionary that is updated
 * @param [in,out] elog                  a pointer to the error log
 * @param [in]     file_name             file name that is loaded
 * @param [in]     duplicates_are_errors duplicate keys are logged as errors
 * @param [in]     replace_duplicates    duplicate entries are replaced
 * @param [in]     stop_on_error         stops loading on first error
 * @return                               count of loaded entries
 */
size_t
gpdb_dictionary_load (gpdb_dictionary_t *const db,
                      gpdb_syntax_err_log_t *const elog,
                      const char *const file_name,
                      const bool duplicates_are_errors,
                      const bool replace_duplicates,
                      const bool stop_on_error)
{
  assert(db);
  assert(elog);
  assert(file_name);

  FILE *fp;
  gpdb_syntax_err_t *err;
  gpdb_entry_t *entry;

  size_t insertions = 0;
  size_t line_number = 0;

  if (!fut_file_exists(file_name)) return insertions;

  /* Opens the source file for reading. */
  fp = fopen(file_name, "r");

  char line[MAX_LINE_LENGTH] = ""; /* Initialize all line elements to 0 ('\0'). */
  char entry_id[MAX_LINE_LENGTH] = "";
  char entry_description[MAX_LINE_LENGTH] = "";
  GamePositionX gpx;
  char error_message[MAX_LINE_LENGTH * 2];


  while (fgets(line, sizeof(line), fp)) {
    if (strlen(line) == MAX_LINE_LENGTH - 1) {
      fprintf(stderr, "Input line %zu, in file %s is longer than MAX_LINE_LENGTH - 1 (%d). Aborting ...\n",
              line_number, file_name, MAX_LINE_LENGTH - 1);
      abort();
    }
    err = NULL;
    entry_id[0] = '\0';
    gpdb_parse_line(line, file_name, line_number, entry_id, entry_description, &gpx, &err);
    if (err) {
      gpdb_syntax_err_log_add(elog, err);
      if (stop_on_error) goto out;
    } else if (entry_id[0] != '\0') { // The line is not a comment.
      entry = gpdb_dictionary_find_entry(db, entry_id);
      if (entry) {
        /* Entry is a duplicate. */
        if (duplicates_are_errors) {
          sprintf(error_message, "The line has a duplicate key, duplications are considered as errors. Key is: \"%s\"", entry_id);
          err = gpdb_syntax_err_new(file_name,
                                    line_number,
                                    line,
                                    GPDB_SYNTAX_ERR_DUPLICATE_ENTRY_KEY,
                                    error_message);
          gpdb_syntax_err_log_add(elog, err);
          if (stop_on_error) goto out;
        } else {
          if (replace_duplicates) {
            /* Replaces entry. */
            entry = gpdb_entry_new(entry_id, entry_description, &gpx);
            entry = gpdb_dictionary_add_or_replace_entry(db, entry);
            gpdb_entry_free(entry);
            insertions++;
          } else {
            /* Does nothing. */
            ;
          }
        }
      } else {
        /* Entry is new. */
        entry = gpdb_entry_new(entry_id, entry_description, &gpx);
        gpdb_dictionary_add_or_replace_entry(db, entry);
        insertions++;
      }
    }
    line_number++;
  }
 out:

  fclose(fp);

  gpdb_syntax_err_log_reverse(elog);

  return insertions;
}

/**
 * @brief Prints to `stream` a text representation of `db`.
 *
 * @details Prints all the entries in the `db`, one by one in the
 *          lexicographic order of the keys.
 *          For each entry prints the output obtained calling #gpdb_entry_print().
 *
 * @invariant Parameter `db` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] db      reference to the dictionary
 * @param [in] stream  the target of the output
 * @param [in] verbose true for more output
 */
void
gpdb_dictionary_print (const gpdb_dictionary_t *const db,
                       FILE *const stream,
                       const bool verbose)
{
  assert(db);

  gpdb_entry_t *entry;

  rbt_traverser_t traverser;
  rbt_traverser_t *tr = &traverser;

  rbt_t_init(tr, db->table);

  while ((entry = rbt_t_next(tr))) {
    gpdb_entry_print(entry, stream, verbose);
  }
}

/**
 * @brief Prints to `stream` a formatted summary holding a short represention of the `db`.
 *
 * @details The printed message has the form:
 *
 * @code
 * The Game Position Database has 37 entries.
 * Database Description: This describe the database of positions.
 * Number of errors: 0
 * @endcode
 *
 * @invariant Parameter `db` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] db     pointer to the db
 * @param [in] elog   pointer to the error log
 * @param [in] stream the target of the output
 */
void
gpdb_dictionary_print_summary (const gpdb_dictionary_t *const db,
                               const gpdb_syntax_err_log_t *const elog,
                               FILE *const stream)
{
  assert(db);

  const size_t entry_count = gpdb_dictionary_entry_count(db);

  fprintf(stream, "The Game Position Database has %zu entr%s.\n",
          entry_count, (entry_count == 1) ? "y" : "ies");
  fprintf(stream, "Database Description: %s\n", gpdb_dictionary_get_description(db));

  if (elog) {
    const size_t error_count = gpdb_syntax_err_log_length(elog);
    fprintf(stream, "Number of errors: %zu\n", error_count);
  }
}



/**************************************************************/
/* Function implementations for the gpdb_syntax_err_t entity. */
/**************************************************************/

/**
 * @brief Game position database syntax error structure constructor.
 *
 * @details The constructor allocates memory for the syntax error structure and then for the
 *          `file_name`, `line`, and `message` fields.
 *          This memory is then released when the entry is freed by a call to #gpdb_syntax_err_free().
 *          No dependency is left on the data referenced by the three arguments.
 *
 *          An assertion checks that the received pointer to the allocated
 *          game position database syntax error structure is not `NULL`.
 *
 * @invariant Parameters `file_name`, `line` and `message` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @invariant Parameters `type` must be in the range defined by #gpdb_syntax_err_type_t enum.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] file_name   file name
 * @param [in] line_number line number
 * @param [in] line        line text
 * @param [in] type        error type
 * @param [in] message     error message
 * @return                 a pointer to a new empty game position database syntax error structure
 */
gpdb_syntax_err_t *
gpdb_syntax_err_new (const char *const file_name,
                     const size_t line_number,
                     const char *const line,
                     const gpdb_syntax_err_type_t type,
                     const char *const message)
{
  assert(file_name);
  assert(line);
  assert(message);
  assert(type >= 0 && type < GPDB_SYNTAX_ERR_COUNT);

  int len;

  gpdb_syntax_err_t *e = (gpdb_syntax_err_t *) malloc(sizeof(gpdb_syntax_err_t));
  assert(e);

  len = strlen(file_name);
  e->file_name = malloc(len + 1);
  assert(e->file_name);
  strcpy(e->file_name, file_name);

  e->line_number = line_number;

  len = strlen(line);
  e->line = malloc(len + 1);
  assert(e->line);
  strcpy(e->line, line);

  e->type = type;

  len = strlen(message);
  e->message = malloc(len + 1);
  assert(e->message);
  strcpy(e->message, message);

  return e;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_syntax_err_new().
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] error the pointer to be deallocated
 */
void
gpdb_syntax_err_free (gpdb_syntax_err_t *error)
{
  if (error) {
    free(error->file_name);
    free(error->line);
    free(error->message);
    free(error);
  }
}

/**
 * @brief Gets the `file_name` field.
 *
 * @invariant Parameter `error` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] error reference to the object
 * @return           the value of the `file_name` field
 */
char *
gpdb_syntax_err_get_file_name (gpdb_syntax_err_t *const error)
{
  assert(error);
  return error->file_name;
}

/**
 * @brief Gets the `line_number` field.
 *
 * @invariant Parameter `error` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] error reference to the object
 * @return           the value of the `line_number` field
 */
size_t
gpdb_syntax_err_get_line_number (gpdb_syntax_err_t *const error)
{
  assert(error);
  return error->line_number;
}

/**
 * @brief Gets the `line` field.
 *
 * @invariant Parameter `error` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] error reference to the object
 * @return           the value of the `line` field
 */
char *
gpdb_syntax_err_get_line (gpdb_syntax_err_t *const error)
{
  assert(error);
  return error->line;
}

/**
 * @brief Gets the `type` field.
 *
 * @invariant Parameter `error` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] error reference to the object
 * @return           the value of the `type` field
 */
gpdb_syntax_err_type_t
gpdb_syntax_err_get_type (gpdb_syntax_err_t *const error)
{
  assert(error);
  return error->type;
}

/**
 * @brief Gets the `message` field.
 *
 * @invariant Parameter `error` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] error reference to the object
 * @return           the value of the `message` field
 */
char *
gpdb_syntax_err_get_message (gpdb_syntax_err_t *const error)
{
  assert(error);
  return error->message;
}

void
gpdb_syntax_err_print (const gpdb_syntax_err_t *const error,
                       FILE *const stream)
{
  assert(error);

  fprintf(stream, "\n");
  fprintf(stream, "Line number: %zu\n", error->line_number);
  fprintf(stream, "Line:        %s", error->line);

  fprintf(stream, "Error type:  ");
  switch (error->type) {
  case GPDB_SYNTAX_ERR_INCOMPLETE_ENTRY:
    fprintf(stream, "GPDB_SYNTAX_ERR_INCOMPLETE_ENTRY - There are less than four fields.");
    break;
  case GPDB_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64:
    fprintf(stream, "GPDB_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64 - The board field doesn't have sixty-four characters.");
    break;
  case GPDB_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID:
    fprintf(stream, "GPDB_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID - In the board field, one or more characters are out of range.");
    break;
  case GPDB_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR:
    fprintf(stream, "GPDB_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR - Player field doesn't have a single character.");
    break;
  case GPDB_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID:
    fprintf(stream, "GPDB_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID - Out of range character for player.");
    break;
  case GPDB_SYNTAX_ERR_DUPLICATE_ENTRY_KEY:
    fprintf(stream, "GPDB_SYNTAX_ERR_DUPLICATE_ENTRY_KEY - Duplicate key.");
    break;
  default:
    fprintf(stderr, "Error type is out of range. Aborting ...\n");
    abort();
    break;
  }
  fprintf(stream, "\n");

  fprintf(stream, "Message:     %s\n", error->message);
}



/******************************************************************/
/* Function implementations for the gpdb_syntax_err_log_t entity. */
/******************************************************************/

/**
 * @brief Syntax error log constructor.
 *
 * @details The memory allocated is then released when the log is freed by a call to #gpdb_syntax_err_log_free().
 *          No dependency is left on the data referenced by the argument.
 *
 *          An assertion checks that the received pointer to the allocated
 *          log is not `NULL`.
 *
 * @return a pointer to a new syntax error log
 */
gpdb_syntax_err_log_t *
gpdb_syntax_err_log_new (void)
{
  gpdb_syntax_err_log_t *log = (gpdb_syntax_err_log_t *) malloc(sizeof(gpdb_syntax_err_log_t));
  assert(log);

  llist_compare_f cmp = NULL;
  log->list = llist_new(cmp);

  return log;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #gpdb_syntax_err_log_new().
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] log the pointer to be deallocated
 */
void
gpdb_syntax_err_log_free (gpdb_syntax_err_log_t *log)
{
  if (log) {
    llist_foreach(log->list, gpdb_syntax_err_free_proxy, NULL);
    llist_free(log->list);
    free(log);
  }
}

/**
 * @brief Adds a new element at the beginning of the log.
 *
 * @details When `err` is `NULL` nothing happens.
 *
 * @invariant Parameter `log` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in,out] log syntax error log
 * @param [in]     err syntax error
 */
void
gpdb_syntax_err_log_add (gpdb_syntax_err_log_t *log,
                         gpdb_syntax_err_t *err)
{
  assert(log);
  if (err) {
    llist_add(log->list, err);
  }
}

/**
 * @brief Returns the number of errors within the log.
 *
 * @param [in] log syntax error log
 * @return         the length of the log
 */
size_t
gpdb_syntax_err_log_length (const gpdb_syntax_err_log_t *const log)
{
  assert(log);
  return llist_length(log->list);
}

/**
 * @brief Prints to `stream` a text representation of `log`.
 *
 * @details Prints all the entries in the `log`, one by one.
 *          For each entry prints the output obtained calling #gpdb_syntax_err_print().
 *
 * @invariant Parameter `log` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] log     reference to the syntax error log
 * @param [in] stream  the target of the output
 */
void
gpdb_syntax_err_log_print (const gpdb_syntax_err_log_t *const log,
                           FILE *const stream)
{
  assert(log);
  llist_foreach(log->list, gpdb_syntax_err_print_proxy, (void *) stream);
}

/**
 * @brief Gets the `list` field.
 *
 * @invariant Parameter `log` cannot be `NULL`.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] log reference to the object
 * @return         the value of the `list` field
 */
llist_t *
gpdb_syntax_err_log_get_list (gpdb_syntax_err_log_t *const log)
{
  assert(log);
  return log->list;
}


/*
 * Internal functions.
 */

/**
 * @cond
 */

static void
gpdb_syntax_err_free_proxy (void *const element,
                            void *const aux)
{
  gpdb_syntax_err_t *error = (gpdb_syntax_err_t *) element;
  gpdb_syntax_err_free(error);
}

static void
gpdb_syntax_err_print_proxy (void *const element,
                             void *const aux)
{
  const gpdb_syntax_err_t *const error = (const gpdb_syntax_err_t *const) element;
  FILE *const stream = (FILE *const) aux;
  gpdb_syntax_err_print(error, stream);
}

static int
gpdb_compare_entries (const void *item_a,
                      const void *item_b,
                      void *param)
{
  assert(item_a && item_b);
  const gpdb_entry_t *a = (gpdb_entry_t *) item_a;
  const gpdb_entry_t *b = (gpdb_entry_t *) item_b;
  assert(a->id && b->id);

  return strcmp(a->id, b->id);
}

static void
gpdb_tree_item_destroy_function (void *item,
                                 void *param)
{
  gpdb_entry_t *entry = (gpdb_entry_t *) item;
  gpdb_entry_free(entry);
}

static int
gpdb_parse_line (char *line,
                 const char *const file_name,
                 size_t line_number,
                 char *entry_id,
                 char *entry_description,
                 GamePositionX *entry_gpx,
                 gpdb_syntax_err_t **err)
{
  static const size_t record_field_count = 4;

  char *record_separators[record_field_count];
  size_t field_count;
  size_t record_length;
  char c;
  char error_message[1024];
  bool line_is_an_entry;
  size_t len;
  char *cursor;

  /* If line is null return. */
  if (!line) return EXIT_SUCCESS;

  /* Variable initialization. */
  entry_gpx->blacks = empty_square_set;
  entry_gpx->whites = empty_square_set;
  entry_gpx->player = BLACK_PLAYER;
  record_length = 0;
  field_count = 0;
  line_is_an_entry = false;
  for (int i = 0; i < record_field_count; i++) record_separators[i] = NULL;

  /* Finds the positions of all field separators. */
  while ((c = line[record_length])) {
    switch (c) {
    case '#':
      goto line_scan_completed;
    case '\n':
      goto line_scan_completed;
    case ';':
      record_separators[field_count++] = line + record_length;
      if (field_count >= record_field_count) goto line_scan_completed;
    default:
      line_is_an_entry = true;
    }
    record_length++;
  }

 line_scan_completed:
  ;

  /* The line doesn't contain a record. */
  if (!line_is_an_entry) return EXIT_SUCCESS;

  if (field_count != record_field_count) {
    sprintf(error_message, "The line has an incomplete record. The number of fields is %zu", field_count);
    *err = gpdb_syntax_err_new(file_name,
                               line_number,
                               line,
                               GPDB_SYNTAX_ERR_INCOMPLETE_ENTRY,
                               error_message);
    return EXIT_FAILURE;
  }

  /* The line is a record and has four fields. */

  /* Prepares the id field. */
  len = record_separators[0] - line;
  memcpy(entry_id, line, len);
  entry_id[len] = '\0';

  /* Prepares the board field. */
  cursor = record_separators[0] + 1;
  len = record_separators[1] - cursor;
  if (len != 64) {
    sprintf(error_message, "The board field has %zu squares, length must be 64", len);
    *err = gpdb_syntax_err_new(file_name,
                               line_number,
                               line,
                               GPDB_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64,
                               error_message);
    return EXIT_FAILURE;
  }
  for (int i = 0; i < 64; i++) {
    c = *(cursor + i);
    switch (c) {
    case 'b':
      entry_gpx->blacks |= 1ULL << i;
      break;
    case 'w':
      entry_gpx->whites |= 1ULL << i;
      break;
    case '.':
      break;
    default:
      sprintf(error_message, "Board pieces must be in the character set [bw.]. Found %c", c);
      *err = gpdb_syntax_err_new(file_name,
                                 line_number,
                                 line,
                                 GPDB_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID,
                                 error_message);
      return EXIT_FAILURE;
    }
  }

  /* Prepares the player field. */
  cursor = record_separators[1] + 1;
  len = record_separators[2] - cursor;
  if (len != 1) {
    sprintf(error_message, "The player field has %zu characters, length must be 1", len);
    *err = gpdb_syntax_err_new(file_name,
                               line_number,
                               line,
                               GPDB_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR,
                               error_message);
    return EXIT_FAILURE;
  }
  c = *cursor;
  switch (c) {
  case 'b':
    entry_gpx->player = BLACK_PLAYER;
    break;
  case 'w':
    entry_gpx->player = WHITE_PLAYER;
    break;
  default:
    sprintf(error_message, "Player must be in the character set [bw]. Found %c", c);
    *err = gpdb_syntax_err_new(file_name,
                               line_number,
                               line,
                               GPDB_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID,
                               error_message);
    return EXIT_FAILURE;
  }

  /* Prepares the description field. */
  cursor = record_separators[2] + 1;
  len = record_separators[3] - cursor;
  memcpy(entry_description, cursor, len);
  entry_description[len] = '\0';

  return EXIT_SUCCESS;
}

static void
gpdb_syntax_err_log_reverse (gpdb_syntax_err_log_t *log)
{
  assert(log);
  llist_reverse(log->list);
}

/**
 * @endcond
 */

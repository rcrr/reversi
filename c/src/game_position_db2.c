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
#include <errno.h>

#include "game_position_db2.h"

/* Used for file reading, it contains also string termination. */
#define MAX_LINE_LENGTH 4096



/**
 * @cond
 */

/*
 * Internal variables and constants.
 */



/*
 * Prototypes for internal functions.
 */

static void
gpdb2_syntax_err_free_proxy (void *const element,
                             void *const aux);

static void
gpdb2_syntax_err_print_proxy (void *const element,
                              void *const aux);

static bool
file_exists (const char *const file_name);

static int
gpdb2_compare_entries (const void *item_a,
                       const void *item_b,
                       void *param);

static void
gpdb2_tree_item_destroy_function (void *item,
                                  void *param);

static int
gpdb2_parse_line (char *line,
                  const char *const file_name,
                  size_t line_number,
                  char *entry_id,
                  char *entry_description,
                  GamePositionX *entry_gpx,
                  gpdb2_syntax_err_t **err);

static void
gpdb2_syntax_err_log_reverse (gpdb2_syntax_err_log_t *log);

/**
 * @endcond
 */



/*
 * Public functions.
 */

/*********************************************************/
/* Function implementations for the gpdb2_entry_t entity. */
/*********************************************************/

gpdb2_entry_t *
gpdb2_entry_new (const char *const id,
                 const char *const description,
                 const GamePositionX *const gpx)
{
  assert(id);
  assert(description);
  assert(gpx);

  int len;

  gpdb2_entry_t *entry = (gpdb2_entry_t *) malloc(sizeof(gpdb2_entry_t));
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

void
gpdb2_entry_free (gpdb2_entry_t *entry)
{
  if (entry) {
    free(entry->id);
    free(entry->description);
    free(entry);
  }
}

void
gpdb2_entry_print (const gpdb2_entry_t *const entry,
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
/* Function implementations for the gpdb2_dictionary_t entity. */
/**************************************************************/

gpdb2_dictionary_t *
gpdb2_dictionary_new (const char *const description)
{
  gpdb2_dictionary_t *db = (gpdb2_dictionary_t*) malloc(sizeof(gpdb2_dictionary_t));
  assert(db);

  /* Sets description. */
  db->description = NULL;
  gpdb2_dictionary_set_description(db, description);

  /* Creates a new empty table. */
  rbt_item_compare_f *cmp = gpdb2_compare_entries;
  void *cmp_param = NULL;
  mem_allocator_t *alloc = NULL; // when NULL default allocator is used.
  db->table = rbt_create(cmp, cmp_param, alloc);

  return db;
}

void
gpdb2_dictionary_free (gpdb2_dictionary_t *db)
{
  if (db) {
    if (db->description) free(db->description);
    rbt_destroy(db->table, gpdb2_tree_item_destroy_function);
    free(db);
  }
}

char *
gpdb2_dictionary_get_description (const gpdb2_dictionary_t *const db)
{
  return db->description;
}

void
gpdb2_dictionary_set_description (gpdb2_dictionary_t *const db,
                                  const char *const description)
{
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

gpdb2_entry_t *
gpdb2_dictionary_add_or_replace_entry (gpdb2_dictionary_t *const db,
                                       gpdb2_entry_t *entry)
{
  assert(db);
  assert(entry);

  gpdb2_entry_t *replaced = rbt_replace(db->table, entry);

  return replaced;
}

gpdb2_entry_t *
gpdb2_dictionary_delete_entry (gpdb2_dictionary_t *const db,
                               const gpdb2_entry_t *const entry)
{
  assert(db);
  assert(entry);

  gpdb2_entry_t *deleted = rbt_delete(db->table, entry);

  return deleted;
}

size_t
gpdb2_dictionary_entry_count (const gpdb2_dictionary_t *const db)
{
  assert(db);
  return rbt_count(db->table);
}

gpdb2_entry_t *
gpdb2_dictionary_find_entry (gpdb2_dictionary_t *const db,
                             const char *const id)
{
  assert(db);
  if (!id) return NULL;

  gpdb2_entry_t key = { .id = (char *) id };

  gpdb2_entry_t *selected = (gpdb2_entry_t *) rbt_find(db->table, &key);

  return selected;
}

size_t
gpdb2_dictionary_load (gpdb2_dictionary_t *const db,
                       gpdb2_syntax_err_log_t *const elog,
                       const char *const file_name,
                       const bool duplicates_are_errors,
                       const bool replace_duplicates,
                       const bool stop_on_error)
{
  assert(db);
  assert(elog);
  assert(file_name);

  FILE *fp;
  gpdb2_syntax_err_t *err;
  gpdb2_entry_t *entry;

  size_t insertions = 0;
  size_t line_number = 0;

  if (!file_exists(file_name)) return insertions;

  /* Opens the source file for reading. */
  fp = fopen(file_name, "r");

  char line[MAX_LINE_LENGTH] = ""; /* Initialize all line elements to 0 ('\0'). */
  char entry_id[MAX_LINE_LENGTH] = "";
  char entry_description[MAX_LINE_LENGTH] = "";
  GamePositionX gpx;
  char error_message[1024];


  while (fgets(line, sizeof(line), fp)) {
    if (strlen(line) == MAX_LINE_LENGTH - 1) {
      fprintf(stderr, "Input line %zu, in file %s is longer than MAX_LINE_LENGTH - 1 (%d). Aborting ...\n",
              line_number, file_name, MAX_LINE_LENGTH - 1);
      abort();
    }
    err = NULL;
    entry_id[0] = '\0';
    gpdb2_parse_line(line, file_name, line_number, entry_id, entry_description, &gpx, &err);
    if (err) {
      gpdb2_syntax_err_log_add(elog, err);
      if (stop_on_error) goto out;
    } else if (entry_id[0] != '\0') { // The line is not a comment.
      entry = gpdb2_dictionary_find_entry(db, entry_id);
      if (entry) {
        /* Entry is a duplicate. */
        if (duplicates_are_errors) {
          sprintf(error_message, "The line has a duplicate key, duplications are considered as errors. Key is: \"%s\"", entry_id);
          err = gpdb2_syntax_err_new(file_name,
                                     line_number,
                                     line,
                                     GPDB2_SYNTAX_ERR_DUPLICATE_ENTRY_KEY,
                                     error_message);
          gpdb2_syntax_err_log_add(elog, err);
          if (stop_on_error) goto out;
        } else {
          if (replace_duplicates) {
            /* Replaces entry. */
            entry = gpdb2_entry_new(entry_id, entry_description, &gpx);
            entry = gpdb2_dictionary_add_or_replace_entry(db, entry);
            gpdb2_entry_free(entry);
            insertions++;
          } else {
            /* Does nothing. */
            ;
          }
        }
      } else {
        /* Entry is new. */
        entry = gpdb2_entry_new(entry_id, entry_description, &gpx);
        gpdb2_dictionary_add_or_replace_entry(db, entry);
        insertions++;
      }
    }
    line_number++;
  }
 out:

  fclose(fp);

  gpdb2_syntax_err_log_reverse(elog);

  return insertions;
}

void
gpdb2_dictionary_print (const gpdb2_dictionary_t *const db,
                        FILE *const stream,
                        const bool verbose)
{
  assert(db);

  gpdb2_entry_t *entry;

  rbt_traverser_t traverser;
  rbt_traverser_t *tr = &traverser;

  rbt_t_init(tr, db->table);

  while ((entry = rbt_t_next(tr))) {
    gpdb2_entry_print(entry, stream, verbose);
  }
}

void
gpdb2_dictionary_print_summary (const gpdb2_dictionary_t *const db,
                                const gpdb2_syntax_err_log_t *const elog,
                                FILE *const stream)
{
  assert(db);

  const size_t entry_count = gpdb2_dictionary_entry_count(db);

  fprintf(stream, "The Game Position Database has %zu entr%s.\n",
          entry_count, (entry_count == 1) ? "y" : "ies");
  fprintf(stream, "Database Description: %s\n", gpdb2_dictionary_get_description(db));

  if (elog) {
    const size_t error_count = gpdb2_syntax_err_log_length(elog);
    fprintf(stream, "Number of errors: %zu\n", error_count);
  }
}



/**************************************************************/
/* Function implementations for the gpdb2_syntax_err_t entity. */
/**************************************************************/

gpdb2_syntax_err_t *
gpdb2_syntax_err_new (const char *const file_name,
                      const size_t line_number,
                      const char *const line,
                      const gpdb2_syntax_err_type_t type,
                      const char *const message)
{
  assert(file_name);
  assert(line);
  assert(message);
  assert(type >= 0 && type < GPDB2_SYNTAX_ERR_COUNT);

  int len;

  gpdb2_syntax_err_t *e = (gpdb2_syntax_err_t *) malloc(sizeof(gpdb2_syntax_err_t));
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

void
gpdb2_syntax_err_free (gpdb2_syntax_err_t *error)
{
  if (error) {
    free(error->file_name);
    free(error->line);
    free(error->message);
    free(error);
  }
}

void
gpdb2_syntax_err_print (const gpdb2_syntax_err_t *const error,
                        FILE *const stream)
{
  assert(error);

  fprintf(stream, "\n");
  fprintf(stream, "Line number: %zu\n", error->line_number);
  fprintf(stream, "Line:        %s", error->line);

  fprintf(stream, "Error type:  ");
  switch (error->type) {
  case GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY:
    fprintf(stream, "GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY - There are less than four fields.");
    break;
  case GPDB2_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64:
    fprintf(stream, "GPDB2_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64 - The board field doesn't have sixty-four characters.");
    break;
  case GPDB2_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID:
    fprintf(stream, "GPDB2_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID - In the board field, one or more characters are out of range.");
    break;
  case GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR:
    fprintf(stream, "GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR - Player field doesn't have a single character.");
    break;
  case GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID:
    fprintf(stream, "GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID - Out of range character for player.");
    break;
  case GPDB2_SYNTAX_ERR_DUPLICATE_ENTRY_KEY:
    fprintf(stream, "GPDB2_SYNTAX_ERR_DUPLICATE_ENTRY_KEY - Duplicate key.");
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
/* Function implementations for the gpdb2_syntax_err_log_t entity. */
/******************************************************************/

gpdb2_syntax_err_log_t *
gpdb2_syntax_err_log_new (void)
{
  gpdb2_syntax_err_log_t *log = (gpdb2_syntax_err_log_t *) malloc(sizeof(gpdb2_syntax_err_log_t));
  assert(log);

  llist_compare_f cmp = NULL;
  log->list = llist_new(cmp);

  return log;
}

void
gpdb2_syntax_err_log_free (gpdb2_syntax_err_log_t *log)
{
  if (log) {
    llist_foreach(log->list, gpdb2_syntax_err_free_proxy, NULL);
    llist_free(log->list);
    free(log);
  }
}

void
gpdb2_syntax_err_log_add (gpdb2_syntax_err_log_t *log,
                          gpdb2_syntax_err_t *err)
{
  assert(log);
  if (err) {
    llist_add(log->list, err);
  }
}

size_t
gpdb2_syntax_err_log_length (const gpdb2_syntax_err_log_t *const log)
{
  assert(log);
  return llist_length(log->list);
}

void
gpdb2_syntax_err_log_print (const gpdb2_syntax_err_log_t *const log,
                            FILE *const stream)
{
  assert(log);
  llist_foreach(log->list, gpdb2_syntax_err_print_proxy, (void *) stream);
}



/*
 * Internal functions.
 */

static void
gpdb2_syntax_err_free_proxy (void *const element,
                             void *const aux)
{
  gpdb2_syntax_err_t *error = (gpdb2_syntax_err_t *) element;
  gpdb2_syntax_err_free(error);
}

static void
gpdb2_syntax_err_print_proxy (void *const element,
                                  void *const aux)
{
  const gpdb2_syntax_err_t *const error = (const gpdb2_syntax_err_t *const) element;
  FILE *const stream = (FILE *const) aux;
  gpdb2_syntax_err_print(error, stream);
}

static bool
file_exists (const char *const file_name)
{
  FILE *f;
  if ((f = fopen(file_name, "r"))) {
    fclose(f);
    return true;
  }
  return false;
}

static int
gpdb2_compare_entries (const void *item_a,
                       const void *item_b,
                       void *param)
{
  assert(item_a && item_b);
  const gpdb2_entry_t *a = (gpdb2_entry_t *) item_a;
  const gpdb2_entry_t *b = (gpdb2_entry_t *) item_b;
  assert(a->id && b->id);

  return strcmp(a->id, b->id);
}

static void
gpdb2_tree_item_destroy_function (void *item,
                                  void *param)
{
  gpdb2_entry_t *entry = (gpdb2_entry_t *) item;
  gpdb2_entry_free(entry);
}

static int
gpdb2_parse_line (char *line,
                  const char *const file_name,
                  size_t line_number,
                  char *entry_id,
                  char *entry_description,
                  GamePositionX *entry_gpx,
                  gpdb2_syntax_err_t **err)
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
    *err = gpdb2_syntax_err_new(file_name,
                                line_number,
                                line,
                                GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY,
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
    *err = gpdb2_syntax_err_new(file_name,
                                line_number,
                                line,
                                GPDB2_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64,
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
      *err = gpdb2_syntax_err_new(file_name,
                                  line_number,
                                  line,
                                  GPDB2_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID,
                                  error_message);
      return EXIT_FAILURE;
    }
  }

  /* Prepares the player field. */
  cursor = record_separators[1] + 1;
  len = record_separators[2] - cursor;
  if (len != 1) {
    sprintf(error_message, "The player field has %zu characters, length must be 1", len);
    *err = gpdb2_syntax_err_new(file_name,
                                line_number,
                                line,
                                GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR,
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
    *err = gpdb2_syntax_err_new(file_name,
                                line_number,
                                line,
                                GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID,
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
gpdb2_syntax_err_log_reverse (gpdb2_syntax_err_log_t *log)
{
  assert(log);
  llist_reverse(log->list);
}

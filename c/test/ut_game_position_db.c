/**
 * @file
 *
 * @brief Game position database unit test suite.
 * @details Collects tests and helper methods for the game_position_db module.
 *
 * @par ut_game_position_db.c
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "unit_test.h"
#include "game_position_db.h"
#include "linked_list.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
basic_t (ut_test_t *const t)
{
  gpdb2_entry_t *e, *r;
  size_t count;
  char *id;
  char *desc;
  GamePositionX *g;

  GamePositionX gpx = {0, 0, BLACK_PLAYER};
  e = gpdb2_entry_new("one", "The first entry", &gpx);

  id = gpdb2_entry_get_id(e);
  ut_assert(t, strcmp("one", id) == 0);

  desc = gpdb2_entry_get_description(e);
  ut_assert(t, strcmp("The first entry", desc) == 0);

  g = gpdb2_entry_get_gpx(e);
  ut_assert(t, game_position_x_compare(g, &gpx) == 0);

  const char *const db_description = "Test database";

  gpdb2_dictionary_t *db = gpdb2_dictionary_new(db_description);

  desc = gpdb2_dictionary_get_description(db);
  ut_assert(t, strcmp(db_description, desc) == 0);

  count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, 0 == count);

  r = gpdb2_dictionary_add_or_replace_entry(db, e);
  ut_assert(t, r == NULL);

  count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, 1 == count);

  gpdb2_dictionary_set_description(db, "Changed description");
  desc = gpdb2_dictionary_get_description(db);
  ut_assert(t, strcmp("Changed description", desc) == 0);

  gpdb2_dictionary_free(db);
}

static void
find_t (ut_test_t *const t)
{
  gpdb2_entry_t *e, *r;

  GamePositionX gpx = {0, 0, BLACK_PLAYER};
  e = gpdb2_entry_new("one", "The first entry", &gpx);

  gpdb2_dictionary_t *db = gpdb2_dictionary_new(NULL);

  r = gpdb2_dictionary_add_or_replace_entry(db, e);

  r = NULL;
  r = gpdb2_dictionary_find_entry(db, "one");
  ut_assert(t, e == r);

  r = NULL;
  r = gpdb2_dictionary_find_entry(db, "two");
  ut_assert(t, NULL == r);

  gpdb2_dictionary_free(db);
}

static void
delete_t (ut_test_t *const t)
{
  gpdb2_entry_t *e, *r;
  size_t count;

  GamePositionX gpx = {0, 0, BLACK_PLAYER};
  e = gpdb2_entry_new("one", "The first entry", &gpx);

  gpdb2_dictionary_t *db = gpdb2_dictionary_new("Test db");

  r = gpdb2_dictionary_add_or_replace_entry(db, e);
  ut_assert(t, r == NULL);

  count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, 1 == count);

  gpdb2_dictionary_delete_entry(db, e);

  count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, 0 == count);

  gpdb2_entry_free(e);
  gpdb2_dictionary_free(db);
}

static void
replace_t (ut_test_t *const t)
{
  gpdb2_entry_t *e0, *e1, *e2, *r;
  size_t count;

  GamePositionX gpx0 = {0, 0, BLACK_PLAYER};
  e0 = gpdb2_entry_new("one", "The first entry", &gpx0);

  GamePositionX gpx1 = {1, 0, BLACK_PLAYER};
  e1 = gpdb2_entry_new("one", "A replacement for the first entry", &gpx1);

  GamePositionX gpx2 = {2, 0, BLACK_PLAYER};
  e2 = gpdb2_entry_new("two", "A second entry", &gpx2);

  gpdb2_dictionary_t *db = gpdb2_dictionary_new("Test db");

  r = gpdb2_dictionary_add_or_replace_entry(db, e0);
  ut_assert(t, r == NULL);

  r = gpdb2_dictionary_add_or_replace_entry(db, e2);
  ut_assert(t, r == NULL);

  count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, 2 == count);

  r = gpdb2_dictionary_add_or_replace_entry(db, e1);
  ut_assert(t, r == e0);
  gpdb2_entry_free(r);

  count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, 2 == count);

  r = NULL;
  r = gpdb2_dictionary_find_entry(db, "one");
  ut_assert(t, e1 == r);

  r = NULL;
  r = gpdb2_dictionary_find_entry(db, "two");
  ut_assert(t, e2 == r);

  gpdb2_dictionary_free(db);
}

static void
load_a_t (ut_test_t *const t)
{
  size_t insertions;
  size_t error_count;
  size_t entry_count;

  const bool duplicates_are_errors = true;
  const bool replace_duplicates = false;
  const bool stop_on_error = false;

  const size_t expected_insertions = 6;
  const size_t expected_error_count = 11;
  const size_t expected_entry_count = 6;

  const struct {
    size_t line_number;
    gpdb2_syntax_err_type_t error_type;
  } expected_errors[] = {
    { 35, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID },
    { 40, GPDB2_SYNTAX_ERR_DUPLICATE_ENTRY_KEY },
    { 50, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 51, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 52, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 53, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 57, GPDB2_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64 },
    { 61, GPDB2_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID },
    { 65, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID },
    { 66, GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR },
    { 67, GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR }
  };

  const char *const file_name = "db/gpdb-test-db.txt";

  gpdb2_dictionary_t *db = gpdb2_dictionary_new("Test db from file: db/gpdb-test-db.txt");
  assert(db);

  gpdb2_syntax_err_log_t *elog = gpdb2_syntax_err_log_new();
  assert(elog);

  insertions = gpdb2_dictionary_load(db,
                                     elog,
                                     file_name,
                                     duplicates_are_errors,
                                     replace_duplicates,
                                     stop_on_error);

  ut_assert(t, insertions == expected_insertions);

  error_count = gpdb2_syntax_err_log_length(elog);
  ut_assert(t, error_count == expected_error_count);

  entry_count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, entry_count == expected_entry_count);

  llist_t *l = gpdb2_syntax_err_log_get_list(elog);
  for (int i = 0; i < expected_error_count; i++) {
    gpdb2_syntax_err_t *error = llist_nth_data(l, i);
    ut_assert(t, error);
    ut_assert(t, expected_errors[i].line_number == gpdb2_syntax_err_get_line_number(error));
    ut_assert(t, expected_errors[i].error_type == gpdb2_syntax_err_get_type(error));
  }

  gpdb2_dictionary_free(db);
  gpdb2_syntax_err_log_free(elog);
}

static void
load_b_t (ut_test_t *const t)
{
  size_t insertions;
  size_t error_count;
  size_t entry_count;

  const bool duplicates_are_errors = false;
  const bool replace_duplicates = false;
  const bool stop_on_error = false;

  const size_t expected_insertions = 6;
  const size_t expected_error_count = 10;
  const size_t expected_entry_count = 6;

  const struct {
    size_t line_number;
    gpdb2_syntax_err_type_t error_type;
  } expected_errors[] = {
    { 35, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID },
    { 50, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 51, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 52, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 53, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 57, GPDB2_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64 },
    { 61, GPDB2_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID },
    { 65, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID },
    { 66, GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR },
    { 67, GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR }
  };

  const char *const file_name = "db/gpdb-test-db.txt";

  gpdb2_dictionary_t *db = gpdb2_dictionary_new("Test db from file: db/gpdb-test-db.txt");
  assert(db);

  gpdb2_syntax_err_log_t *elog = gpdb2_syntax_err_log_new();
  assert(elog);

  insertions = gpdb2_dictionary_load(db,
                                     elog,
                                     file_name,
                                     duplicates_are_errors,
                                     replace_duplicates,
                                     stop_on_error);

  ut_assert(t, insertions == expected_insertions);

  error_count = gpdb2_syntax_err_log_length(elog);
  ut_assert(t, error_count == expected_error_count);

  entry_count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, entry_count == expected_entry_count);

  llist_t *l = gpdb2_syntax_err_log_get_list(elog);
  for (int i = 0; i < expected_error_count; i++) {
    gpdb2_syntax_err_t *error = llist_nth_data(l, i);
    ut_assert(t, error);
    ut_assert(t, expected_errors[i].line_number == gpdb2_syntax_err_get_line_number(error));
    ut_assert(t, expected_errors[i].error_type == gpdb2_syntax_err_get_type(error));
  }

  gpdb2_entry_t *r = gpdb2_dictionary_find_entry(db, "duplicate-entry");
  ut_assert(t, r);
  ut_assert(t, strcmp("Test inserting a position twice: first time", gpdb2_entry_get_description(r)) == 0);

  gpdb2_dictionary_free(db);
  gpdb2_syntax_err_log_free(elog);
}

static void
load_c_t (ut_test_t *const t)
{
  size_t insertions;
  size_t error_count;
  size_t entry_count;

  const bool duplicates_are_errors = true;
  const bool replace_duplicates = false;
  const bool stop_on_error = true;

  const size_t expected_insertions = 2;
  const size_t expected_error_count = 1;
  const size_t expected_entry_count = 2;

  const struct {
    size_t line_number;
    gpdb2_syntax_err_type_t error_type;
  } expected_errors[] = {
    { 35, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID }
  };

  const char *const file_name = "db/gpdb-test-db.txt";

  gpdb2_dictionary_t *db = gpdb2_dictionary_new("Test db from file: db/gpdb-test-db.txt");
  assert(db);

  gpdb2_syntax_err_log_t *elog = gpdb2_syntax_err_log_new();
  assert(elog);

  insertions = gpdb2_dictionary_load(db,
                                     elog,
                                     file_name,
                                     duplicates_are_errors,
                                     replace_duplicates,
                                     stop_on_error);

  ut_assert(t, insertions == expected_insertions);

  error_count = gpdb2_syntax_err_log_length(elog);
  ut_assert(t, error_count == expected_error_count);

  entry_count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, entry_count == expected_entry_count);

  llist_t *l = gpdb2_syntax_err_log_get_list(elog);
  for (int i = 0; i < expected_error_count; i++) {
    gpdb2_syntax_err_t *error = llist_nth_data(l, i);
    ut_assert(t, error);
    ut_assert(t, expected_errors[i].line_number == gpdb2_syntax_err_get_line_number(error));
    ut_assert(t, expected_errors[i].error_type == gpdb2_syntax_err_get_type(error));
  }

  gpdb2_dictionary_free(db);
  gpdb2_syntax_err_log_free(elog);
}

static void
load_d_t (ut_test_t *const t)
{
  size_t insertions;
  size_t error_count;
  size_t entry_count;

  const bool duplicates_are_errors = false;
  const bool replace_duplicates = true;
  const bool stop_on_error = false;

  const size_t expected_insertions = 7;
  const size_t expected_error_count = 10;
  const size_t expected_entry_count = 6;

  const struct {
    size_t line_number;
    gpdb2_syntax_err_type_t error_type;
  } expected_errors[] = {
    { 35, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID },
    { 50, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 51, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 52, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 53, GPDB2_SYNTAX_ERR_INCOMPLETE_ENTRY },
    { 57, GPDB2_SYNTAX_ERR_BOARD_SIZE_IS_NOT_64 },
    { 61, GPDB2_SYNTAX_ERR_SQUARE_CHAR_IS_INVALID },
    { 65, GPDB2_SYNTAX_ERR_PLAYER_CHAR_IS_INVALID },
    { 66, GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR },
    { 67, GPDB2_SYNTAX_ERR_PLAYER_IS_NOT_ONE_CHAR }
  };

  const char *const file_name = "db/gpdb-test-db.txt";

  gpdb2_dictionary_t *db = gpdb2_dictionary_new("Test db from file: db/gpdb-test-db.txt");
  assert(db);

  gpdb2_syntax_err_log_t *elog = gpdb2_syntax_err_log_new();
  assert(elog);

  insertions = gpdb2_dictionary_load(db,
                                     elog,
                                     file_name,
                                     duplicates_are_errors,
                                     replace_duplicates,
                                     stop_on_error);

  ut_assert(t, insertions == expected_insertions);

  error_count = gpdb2_syntax_err_log_length(elog);
  ut_assert(t, error_count == expected_error_count);

  entry_count = gpdb2_dictionary_entry_count(db);
  ut_assert(t, entry_count == expected_entry_count);

  llist_t *l = gpdb2_syntax_err_log_get_list(elog);
  for (int i = 0; i < expected_error_count; i++) {
    gpdb2_syntax_err_t *error = llist_nth_data(l, i);
    ut_assert(t, error);
    ut_assert(t, expected_errors[i].line_number == gpdb2_syntax_err_get_line_number(error));
    ut_assert(t, expected_errors[i].error_type == gpdb2_syntax_err_get_type(error));
  }

  gpdb2_entry_t *r = gpdb2_dictionary_find_entry(db, "duplicate-entry");
  ut_assert(t, r);
  ut_assert(t, strcmp("Test inserting a position twice: second time", gpdb2_entry_get_description(r)) == 0);

  gpdb2_dictionary_free(db);
  gpdb2_syntax_err_log_free(elog);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "game_position_db2");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "basic", basic_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "find", find_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "delete", delete_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "replace", replace_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "load_a", load_a_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "load_b", load_b_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "load_c", load_c_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "load_d", load_d_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

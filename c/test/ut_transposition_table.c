/**
 * @file
 *
 * @brief Transposition table unit test suite.
 * @details Collects basic tests fo the transposition table module.
 *
 * @par ut_transposition_table.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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
#include "transposition_table.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
tratab_table_create_t (ut_test_t *const t)
{
  tratab_table_t *table;
  size_t size;

  table = NULL;
  size = (size_t) 1024;

  table = tratab_table_create(size);
  ut_assert(t, table);

  tratab_table_init(table);

  tratab_table_destroy(table);
  table = NULL;
}

static void
tratab_insert_item_t (ut_test_t *const t)
{
  tratab_table_t *table;
  size_t size;

  GamePositionX gpx_s = {
    .blacks = 1,
    .whites = 4,
    .player = WHITE_PLAYER
  };
  GamePositionX *gpx = &gpx_s;

  const uint64_t hash = game_position_x_hash(gpx);

  table = NULL;
  size = (size_t) 1024;

  table = tratab_table_create(size);
  ut_assert(t, table);

  tratab_table_init(table);

  tratab_insert_item(table, hash, gpx, 0, 0, 0, invalid_move);

  tratab_table_destroy(table);
  table = NULL;
}

/* ### ### ### ### */

static void
ttab_new_free_t (ut_test_t *const t)
{
  const int log_size = 4; // 0 --> 1, 1 --> 2, 2 --> 4; 3 --> 8; 4 --> 16 .....

  ttab_t table;

  table = ttab_new(log_size);
  ut_assert(t, table);

  ttab_free(&table);
  ut_assert(t, table == NULL);
}

static void
ttab_summary_to_stream_t (ut_test_t *const t)
{
  const int log_size = 4;

  bool verbose = (ut_run_time_is_verbose(t)) ? true : false;

  ttab_t table;
  FILE *f;

  table = ttab_new(log_size);
  ut_assert(t, table);

  f = verbose ? stdout : NULL;

  ttab_summary_to_stream(table, f);

  ttab_free(&table);
  ut_assert(t, table == NULL);
}

static void
ttab_insert_retrieve_a_t (ut_test_t *const t)
{
  const int log_size = 16;

  struct ttab_item_s is;
  ttab_t table;
  ttab_item_t item;

  item = &is;
  is.hash = 1ULL;
  is.depth = 3;
  is.lower_bound = -60;
  is.upper_bound = +60;
  is.best_move = A1;
  is.pq_index = -1;

  table = ttab_new(log_size);
  ut_assert(t, table);

  ttab_retrieve(table, &item);
  ut_assert(t, item == NULL);

  item = &is;
  ttab_insert(table, item);

  is.depth = 0;
  is.lower_bound = -66;
  is.upper_bound = +66;
  is.best_move = invalid_move;
  is.pq_index = -1;
  ttab_retrieve(table, &item);
  ut_assert(t, item);
  ut_assert(t, is.lower_bound == -60);
  ut_assert(t, is.upper_bound == +60);
  ut_assert(t, is.best_move == A1);
  ut_assert(t, is.pq_index == 0);

  ttab_free(&table);
  ut_assert(t, table == NULL);
}

static void
ttab_table_to_stream_t (ut_test_t *const t)
{
  const int log_size = 4;

  bool verbose = (ut_run_time_is_verbose(t)) ? true : false;

  ttab_t table;
  FILE *f;

  struct ttab_item_s is;
  ttab_item_t item;

  item = &is;

  f = verbose ? stdout : NULL;

  table = ttab_new(log_size);
  ut_assert(t, table);

  is.hash = 1ULL;
  is.depth = 3;
  is.lower_bound = -60;
  is.upper_bound = +60;
  is.best_move = A1;
  is.pq_index = -1;
  ttab_insert(table, item);

  is.hash = 2ULL;
  is.depth = 5;
  is.lower_bound = -40;
  is.upper_bound = +40;
  is.best_move = H8;
  is.pq_index = -1;
  ttab_insert(table, item);

  is.hash = 3ULL;
  is.depth = 2;
  is.lower_bound = -42;
  is.upper_bound = +42;
  is.best_move = H1;
  is.pq_index = -1;
  ttab_insert(table, item);

  ttab_table_to_stream(table, f);

  ttab_free(&table);
  ut_assert(t, table == NULL);
}

static void
ttab_insert_retrieve_b_t (ut_test_t *const t)
{
  const bool is_very_verbose = false;

  bool verbose = (ut_run_time_is_verbose(t)) ? true : false;

  const int log_size = 8;
  const int n_inserts = 2048;

  struct ttab_item_s is;
  ttab_t table;
  ttab_item_t item;

  item = &is;
  is.hash = 0ULL;
  is.depth = 3;
  is.lower_bound = -60;
  is.upper_bound = +60;
  is.best_move = A1;
  is.pq_index = -1;

  table = ttab_new(log_size);
  ut_assert(t, table);

  if (is_very_verbose) printf("\n");
  for (int i = 0; i < n_inserts; i++) {
    is.hash = (uint64_t) i + 1;
    is.depth = i % 8;
    if (is_very_verbose) printf("---  i=%d, preparing to insert: is.hash = %zu, is.depth = %u\n", i, is.hash, is.depth);
    ttab_insert(table, item);
    if (is_very_verbose) ttab_table_to_stream(table, stdout);
  }

  if (verbose) {
    const size_t stats_size = 8;
    size_t stats[stats_size];
    ttab_bucket_filling_stats(table, stats, stats_size);
    printf("\n");
    printf("TT bucket filling statistics:\n");
    for (size_t i = 0; i < stats_size; i++) {
      printf("  %4zu -> %12zu\n", i, stats[i]);
    }
  }

  ttab_free(&table);
  ut_assert(t, table == NULL);
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

  ut_suite_t *const s = ut_suite_new(&config, "transposition_table");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", dummy_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "tratab_table_create", tratab_table_create_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "tratab_insert_item", tratab_insert_item_t);


  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "ttab_new_free", ttab_new_free_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "ttab_summary_to_stream", ttab_summary_to_stream_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_001,  "ttab_insert_retrieve_a", ttab_insert_retrieve_a_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "ttab_table_to_stream", ttab_table_to_stream_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "ttab_insert_retrieve_b", ttab_insert_retrieve_b_t);


  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

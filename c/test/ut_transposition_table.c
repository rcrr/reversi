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

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}

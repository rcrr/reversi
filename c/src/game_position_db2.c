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

#include "game_position_db2.h"



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

static int
gpdb2_compare_entries (const void *item_a,
                       const void *item_b,
                       void *param);

static void
gpdb2_tree_item_destroy_function (void *item,
                                  void *param);


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
  assert(gpx);
  assert(description);

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



/*
 * Internal functions.
 */

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

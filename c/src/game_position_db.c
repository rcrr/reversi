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

const static char field_separator = ';'; /* Field separator for records in the game position db. */

const gint extract_entry_from_line(gchar *line,
                                   GamePositionDbEntry *entry,
                                   GError **e)
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
  entry = g_malloc0(sizeof(GamePositionDbEntry));

  /* Extracts the key (id field). */
  cp0 = record;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    strncpy(entry->id = g_malloc((cp1 - cp0 + 1) * sizeof(entry->id)), cp0, cp1 - cp0);
    entry->id[cp1 - cp0] = '\0';
  } else {
    g_free(entry->id);
    g_free(entry);
    g_free(record);
    g_set_error(e, 1, 1, "The record is missing the id field.\n");
    return EXIT_FAILURE;
  }

  /* Extracts the board field. */
  cp0 = cp1 + 1;
  if ((cp1 = strchr(cp0, field_separator)) != NULL) {
    if ((cp1 - cp0) != 64) {
      printf("ERROR: board pieces must be 64! Found %ld\n", cp1 - cp0);
      //goto error;
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

/* Comparison function for entries.*/
gint gpdb_compare_entries(gconstpointer pa, gconstpointer pb)
{
  const GamePositionDbEntry *a = pa;
  const GamePositionDbEntry *b = pb;

  return strcmp(a->id, b->id);
}

int gpdb_load(FILE *fp,  GamePositionDb *db, GError **e)
{
  GIOChannel *channel;
  GError     *err;
  GIOStatus   ret;
  gchar      *msg;
  gsize       len;
  GTree      *tree;

  tree = g_tree_new(gpdb_compare_entries);

  channel = g_io_channel_unix_new(fileno(fp));

  do {
    err = NULL;
    ret = g_io_channel_read_line(channel, &msg, &len, NULL, &err);
    if (ret == G_IO_STATUS_ERROR)
      g_error("Error reading: %s\n", err->message);

    err = NULL;
    GamePositionDbEntry *entry = NULL;
    extract_entry_from_line(msg, entry, &err);

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


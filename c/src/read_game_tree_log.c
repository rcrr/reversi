/**
 * @file
 *
 * @brief Reads a game tree log dat file.
 * @details This executable reads a file that contains the
 * game tree log binary dump, then executes the actions dictated by the flags given in the command line.
 *
 * @par read_game_tree_log.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2016, 2017 Roberto Corradini. All rights reserved.
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
#include <inttypes.h>

#include <glib.h>

#include "game_tree_logger.h"



/**
 * @cond
 */

/* Static constants. */

static const gchar *program_documentation_string =
  "Description:\n"
  "Read Game Tree Log dump is a program that loads a binary dump file representation of a game tree log, and output it as a table.\n"
  "\n"
  "Author:\n"
  "   Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2016 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;



/* Static variables. */

static gchar *input_file = NULL;

static const GOptionEntry entries[] =
  {
    { "input-file",        'f', 0, G_OPTION_ARG_FILENAME, &input_file,        "Input file name - Mandatory", NULL },
    { NULL }
  };

/**
 * @endcond
 */



/**
 * @brief Main entry for the Read Game Tree Log dump utility.
 */
int
main (int argc, char *argv[])
{
  LogDataH record;
  char json_doc[game_tree_log_max_json_doc_len];

  /* GLib command line options and argument parsing. */
  GError *error = NULL;
  GOptionGroup *option_group = g_option_group_new("name", "description", "help_description", NULL, NULL);
  GOptionContext *context = g_option_context_new("- Loads a Game Tree Log dump file");
  g_option_context_add_main_entries(context, entries, NULL);
  g_option_context_add_group(context, option_group);
  g_option_context_set_description(context, program_documentation_string);
  if (!g_option_context_parse (context, &argc, &argv, &error)) {
    g_print("Option parsing failed: %s\n", error->message);
    return -1;
  }

  /* Checks command line options for consistency. */
  if (!input_file) {
    g_print("Option -f, --file is mandatory.\n");
    return -2;
  }

  board_module_init();

  /* Opens the binary file for reading. */
  FILE *fp = fopen(input_file, "r");
  g_assert(fp);

  fprintf(stdout, "%s;%s;%s;%s;%s;%s;%s;%s\n",
          "SUB_RUN_ID",
          "CALL_ID",
          "HASH",
          "PARENT_HASH",
          "BLACKS",
          "WHITES",
          "PLAYER",
          "JSON_DOC");

  while (fread(&record, sizeof(LogDataH), 1, fp)) {
    if (!record.json_doc) {
      GamePositionX gpx = { .blacks = record.blacks, .whites = record.whites, .player = record.player };
      const int json_doc_len  = game_tree_log_data_h_json_doc(json_doc, record.call_level, &gpx);
      if (json_doc_len > game_tree_log_max_json_doc_len) abort();
    } else {
      size_t len = fread(json_doc, record.json_doc_len + 1, 1, fp);
      if (len != 1) abort();
    }
    fprintf(stdout, "%6d;%8" PRIu64 ";%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%1d;%s\n",
            record.sub_run_id,
            record.call_id,
            (int64_t) record.hash,
            (int64_t) record.parent_hash,
            (int64_t) record.blacks,
            (int64_t) record.whites,
            record.player,
            json_doc);
  }

  fclose(fp);

  return 0;
}

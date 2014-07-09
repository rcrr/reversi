/**
 * @file
 *
 * @brief Game tree logger module definitions.
 * @details This module defines the functions used to log the game tree.
 *
 * @par game_tree_logger.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#ifndef GAME_TREE_LOGGER_H
#define GAME_TREE_LOGGER_H

#include <glib.h>

#include "board.h"


/**
 * @brief Log data is collecting the info logged into one record.
 */
typedef struct {
  int        sub_run_id;  /**< @brief Sub run id field. */
  uint64     call_id;     /**< @brief Call id. */
  uint64     hash;        /**< @brief Game position hash. */
  uint64     parent_hash; /**< @brief Parent game position hash. */
  SquareSet  blacks;      /**< @brief Blacks field part of the game position. */
  SquareSet  whites;      /**< @brief Whites field part of the game position. */
  Player     player;      /**< @brief Player field part of the game position. */
  gchar     *json_doc;    /**< @brief Json field. */
} LogData;

/**
 * @brief The log file used to record the game DAG traversing.
 */
static FILE *game_tree_log_file = NULL;



/********************************************************/
/* Function implementations for the GameTreeLog entity. */ 
/********************************************************/

extern void
game_tree_log_open (const gchar * const filename);

extern void
game_tree_log_write (const LogData * const log_data);

extern void
game_tree_log_close (void);


#endif /* GAME_TREE_LOGGER_H */

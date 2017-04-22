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
 * @copyright 2014, 2016, 2017 Roberto Corradini. All rights reserved.
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

#include <stdbool.h>

#include "endgame_solver.h"



/**
 * @brief Environment in wich the logger operates.
 */
typedef struct {
  bool       log_is_on;        /**< @brief True when logging is turned on. */
  char      *file_name_prefix; /**< @brief The log file name prefix received by the caller. */
  char      *t_file_name;      /**< @brief The complete name for the tail file. */
  FILE      *t_file;           /**< @brief Tail file. */
  char      *h_file_name;      /**< @brief The complete name for the binary data head file. */
  FILE      *h_file;           /**< @brief Head binary data file. */
} LogEnv;

/**
 * @brief It is collecting the info logged into a record by the head write function.
 */
typedef struct {
  int        sub_run_id;     /**< @brief Sub run id field. */
  uint64_t   call_id;        /**< @brief Call id. */
  uint64_t   hash;           /**< @brief Game position hash. */
  uint64_t   parent_hash;    /**< @brief Parent game position hash. */
  SquareSet  blacks;         /**< @brief Blacks field part of the game position. */
  SquareSet  whites;         /**< @brief Whites field part of the game position. */
  Player     player;         /**< @brief Player field part of the game position. */
  char      *json_doc;       /**< @brief Json field. */
  size_t     json_doc_len;   /**< @brief Json field length. */
  uint8_t    call_level;     /**< @brief Call level, or depth. */
} LogDataH;

/**
 * @brief It is collecting the info logged into a record by the tail write function.
 */
typedef struct {
  int        sub_run_id;  /**< @brief Sub run id field. */
  uint64_t   call_id;     /**< @brief Call id. */
  char      *json_doc;    /**< @brief Json field. */
} LogDataT;



/**********************************************/
/* Global constants.                          */
/**********************************************/

/**
 * @brief The empty square set.
 */
static const size_t game_tree_log_max_json_doc_len = 4096;



/********************************************************/
/* Function implementations for the GameTreeLog entity. */
/********************************************************/

extern void
game_tree_log_open_h (LogEnv *const env);

extern void
game_tree_log_open_t (LogEnv *const env);

extern void
game_tree_log_write_h (const LogEnv *const env,
                       const LogDataH *const data);

extern void
game_tree_log_write_t (const LogEnv *const env,
                       const LogDataT *const data);

extern void
game_tree_log_close (LogEnv *const env);

extern LogEnv *
game_tree_log_init (const char *const file_name_prefix);

extern gchar *
game_tree_log_data_h_json_doc2 (const int call_level,
                                const GamePositionX *const gpx);

extern int
game_tree_log_data_h_json_doc3 (char *const json_doc,
                                const int call_level,
                                const GamePositionX *const gpx);

extern void
do_log (const ExactSolution *const result,
        const GameTreeStack *const stack,
        const unsigned long int sub_run_id,
        const LogEnv *const log_env);



#endif /* GAME_TREE_LOGGER_H */

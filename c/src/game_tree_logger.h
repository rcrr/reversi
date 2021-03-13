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
 * @copyright 2014, 2016, 2017, 2021 Roberto Corradini. All rights reserved.
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

#include "endgame_utils.h"



/**
 * @brief Environment in wich the logger operates.
 */
typedef struct {
  bool       log_is_on;        /**< @brief True when logging is turned on. */
  char      *file_name_prefix; /**< @brief The log file name prefix received by the caller. */
  char      *file_name;        /**< @brief The complete name for the binary data head file. */
  FILE      *file;             /**< @brief Head binary data file. */
} gtl_log_env_t;

/**
 * @brief It is collecting the info logged into a record by the head write function.
 */
typedef struct {
  int        sub_run_id;                /**< @brief Sub run id field. */
  uint64_t   call_id;                   /**< @brief Call id, or cumulated visited positions when entering the node. */
  uint64_t   hash;                      /**< @brief Game position hash. */
  uint64_t   parent_hash;               /**< @brief Parent game position hash. */
  SquareSet  blacks;                    /**< @brief Blacks field part of the game position. */
  SquareSet  whites;                    /**< @brief Whites field part of the game position. */
  Player     player;                    /**< @brief Player field part of the game position. */
  int8_t     alpha;                     /**< @brief Alpha value when entering the node. */
  int8_t     beta;                      /**< @brief Beta value when entering the node. */
  uint8_t    call_level;                /**< @brief Call level, or depth. */
  uint8_t    empty_count;               /**< @brief Empy square count. */
  bool       is_leaf;                   /**< @brief A termination node for the tree. */
  uint8_t    legal_move_count;          /**< @brief Number of legal moves. */
  uint8_t    legal_move_count_adjusted; /**< @brief Same as legal_move_count, zero is turned to one to account for passing. */
  uint8_t    parent_move;               /**< @brief The move played to reach this position. */
  uint8_t    legal_move_array[32];      /**< @brief Legal move array (no more than 25 moves has been found in random generated games). */
} gtl_log_data_head_t;

/**
 * @brief It is collecting the info logged into a record by the tail write function.
 */
typedef struct {
  uint64_t   call_cnt;                 /**< @brief Cumulated visited positions when living the node. */
  int8_t     alpha;                    /**< @brief Alpha value when leaving the node. */
  int8_t     best_move;                /**< @brief Best move found. */
  int8_t     searched_move_cnt;        /**< @brief Count of searched moves. */
  uint8_t    call_level;               /**< @brief Call level, or depth. */
  uint64_t   hash;                     /**< @brief Game position hash. */
  uint8_t    searched_move_array[32];  /**< @brief Searched move move array. */
} gtl_log_data_tail_t;

/**
 * @brief Identifies the head record type in the binary log file.
 */
static const uint8_t gtl_rec_h = 0x01;

/**
 * @brief Identifies the tail record type in the binary log file.
 */
static const uint8_t gtl_rec_t = 0x02;


/********************************************************/
/* Function implementations for the GameTreeLog entity. */
/********************************************************/

extern gtl_log_env_t *
gtl_init (const char *const file_name_prefix);

extern bool
gtl_touch_log_file (const char *const file_name_prefix);

extern void
gtl_open_log (gtl_log_env_t *const env);

extern void
gtl_close_log (gtl_log_env_t *const env);

extern void
gtl_do_log_head (const ExactSolution *const result,
                 const GameTreeStack *const stack,
                 const unsigned long int sub_run_id,
                 const gtl_log_env_t *const log_env);

extern void
gtl_do_log_tail (const ExactSolution *const result,
                 const GameTreeStack *const stack,
                 const unsigned long int sub_run_id,
                 const gtl_log_env_t *const log_env);

extern void
gtl_write_head (const gtl_log_env_t *const env,
                const gtl_log_data_head_t *const data);

extern void
gtl_write_tail (const gtl_log_env_t *const env,
                const gtl_log_data_tail_t *const data);



#endif /* GAME_TREE_LOGGER_H */

/**
 * @file
 *
 * @brief Exact solver module definitions.
 * @details This module defines the #game_position_solve function.
 *
 * @par exact_solver.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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

#ifndef EXACT_SOLVER_H
#define EXACT_SOLVER_H

#include <glib.h>

#include "board.h"

/**
 * @brief An exact solution is an entity that holds the result of a #game_position_solve run.
 *
 * @todo To be detailed ...
 */
typedef struct {
  GamePosition *solved_game_position;        /**< @brief The game position given as input. */
  int           outcome;                     /**< @brief The final endgame score. */
  Square        principal_variation[60];     /**< @brief The sequence of best moves, or principal variation. */
  Board        *final_board;                 /**< @brief The final board state. */
  uint64        leaf_count;
  uint64        node_count;
} ExactSolution;

/**
 * @brief A search node is the structure returned by #game_position_solve function.
 *
 * @todo To be detailed ...
 */
typedef struct {
  Square move;       /**< @brief The move to play. */
  int    value;      /**< @brief The move's value. */
} SearchNode;



/*******************************************************/
/* Function implementations for the SearchNode entity. */ 
/*******************************************************/

extern SearchNode *
search_node_new (const Square move, const int value);

extern SearchNode *
search_node_free (SearchNode *sn);

extern SearchNode *
search_node_negated (SearchNode *sn);



/**********************************************************/
/* Function implementations for the ExactSolution entity. */ 
/**********************************************************/

extern ExactSolution *
exact_solution_new (void);

extern ExactSolution *
exact_solution_free (ExactSolution *es);

extern gchar *
exact_solution_print (const ExactSolution * const es);



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

extern ExactSolution *
game_position_solve (const GamePosition * const root,
                     const gboolean             log_file);


#endif /* EXACT_SOLVER_H */

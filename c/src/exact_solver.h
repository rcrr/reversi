/**
 * @file
 *
 * @brief Exact solver module definitions.
 * @details This module defines the #game_position_es_solve function.
 *
 * @par exact_solver.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2016 Roberto Corradini. All rights reserved.
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

#include "endgame_solver.h"
#include "game_tree_utils.h"
#include "board.h"



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

extern ExactSolution *
game_position_es_solve (const GamePositionX *const root,
                        const endgame_solver_env_t *const env);


#endif /* EXACT_SOLVER_H */

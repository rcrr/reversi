/**
 * @file
 *
 * @brief Rglm solver module definitions.
 * @details This module defines the #game_position_rglm_solve function.
 *
 * @par rglm_solver.h
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

#ifndef RGLM_SOLVER_H
#define RGLM_SOLVER_H

#include "endgame_solver.h"



extern ExactSolution *
game_position_rglm_solve (const GamePositionX *const root,
                          const endgame_solver_env_t *const env);

extern ExactSolution *
game_position_rglm_solve_nlmw (const GamePositionX *const root,
                               const endgame_solver_env_t *const env);

extern int
game_position_rglm_load_model_weights_files (bool verbose);

extern void
game_position_rglm_release_model_weights (void);


#endif /* RGLM_SOLVER_H */

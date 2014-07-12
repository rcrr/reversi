/**
 * @file
 *
 * @brief Random game sampler module definitions.
 * @details This module defines the #game_position_random_sampler function.
 *
 * @par random_game_sampler.h
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

#ifndef RANDOM_GAME_SAMPLER_H
#define RANDOM_GAME_SAMPLER_H

#include "board.h"



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

extern ExactSolution *
game_position_random_sampler (const GamePosition * const root,
                              const gchar        * const log_file,
                              const int                  repeats);


#endif /* RANDOM_GAME_SAMPLER_H */

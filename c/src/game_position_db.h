/**
 * @file
 *
 * @brief GamePositionDb module definitions.
 * @details This module defines the #GamePositionDb, and #GamePositionDbEntry entities,
 * and the function prototypes that operate on them.
 *
 * @par game_position_db.h
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

#ifndef GAME_POSITION_DB_H
#define GAME_POSITION_DB_H

#include "board.h"

/**
 * @brief An Entry collects the GamePosition data with a description and an unique key.
 */
typedef struct {
  char   *id;      /**< @brief id is a string used as key in the dictionary. */
  Board  *board;
  Player  player;
  char   *desc;
} GamePositionDbEntry;

typedef struct {
  GamePositionDbEntry **positions;
  int                   number_of_positions;
  char                 *desc;
} GamePositionDb;



/******************************************************/
/* Function prototypes for the GamePositionDb entity. */ 
/******************************************************/

extern void gpdb_load(void);



/***********************************************************/
/* Function prototypes for the GamePositionDbEntry entity. */ 
/***********************************************************/


#endif /* GAME_POSITION_DB_H */

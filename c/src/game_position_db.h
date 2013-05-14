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
 * @enum GamePositionDbEntrySyntaxErrorType
 * @brief The classification of errors thta can be found parsing a database entry record.
 */
typedef enum {
  GPDB_ENTRY_SYNTAX_ERROR_ON_ID,                  /**< Error on parsing the id field. */
  GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64,   /**< Error on the size of the board field. */
  GPDB_ENTRY_SYNTAX_ERROR_SQUARE_CHAR_IS_INVALID, /**< Error on the board field, one square char is out of range. */
  GPDB_ENTRY_SYNTAX_ERROR_C                       /**< Error C. */
} GamePositionDbEntrySyntaxErrorType;

typedef struct {
  GamePositionDbEntrySyntaxErrorType  error_type;
  char                               *source;
  int                                 line_number;
  char                               *line;
  char                               *error_message;  
} GamePositionDbEntrySyntaxError;

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
  GTree  *tree;
  gchar  *desc;
} GamePositionDb;



/**********************************************************************/
/* Function prototypes for the GamePositionDbEntrySyntaxError entity. */ 
/**********************************************************************/

extern GamePositionDbEntrySyntaxError
*gpdb_entry_syntax_error_new(GamePositionDbEntrySyntaxErrorType  error_type,
                                                           char *source,
                                                            int  line_number,
                                                           char *line,
                                                           char *error_message
                             );

extern GString *gpdb_entry_syntax_error_print(const GamePositionDbEntrySyntaxError const *syntax_error);


/******************************************************/
/* Function prototypes for the GamePositionDb entity. */ 
/******************************************************/

extern int gpdb_load(FILE *fp,
                     GamePositionDb *db,
                     GSList *syntax_error_log,
                     GError **e
                     );

extern GamePositionDb *gpdb_new(char *desc);



/***********************************************************/
/* Function prototypes for the GamePositionDbEntry entity. */ 
/***********************************************************/


#endif /* GAME_POSITION_DB_H */

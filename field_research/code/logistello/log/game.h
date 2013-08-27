// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef GAME_H
#define GAME_H

#include "main.h"
#include "game.h"
#include "sboard.h"

typedef sint1 SMOVE;

/* packed representation:  P F M 
 *                        Player: 0=BLACK
 *                          Flag: 1=true
 *                            Move: 1..60	(without center squares)
 */


#define SMOVE_BIT_PLAYER	0x80
#define SMOVE_BIT_FLAG 		0x40

#define SMOVE_PLAYER(b)	(((b) & SMOVE_BIT_PLAYER) ? WHITE : BLACK)
#define SMOVE_MOVE(b)	\
	Code2Move[((b) & 0xff & ~(SMOVE_BIT_PLAYER | SMOVE_BIT_FLAG))]
#define SMOVE_FLAG(b)   (((b) & SMOVE_BIT_FLAG) != 0)


#define SMOVE_GEN(m,p)	(Move2Code[m] | (p == WHITE ? SMOVE_BIT_PLAYER : 0))


/* flags */

#define GAME_BLACK	 1
#define GAME_WHITE	 2
#define GAME_ALT	 4
#define GAME_PUBLIC_DRAW 8

typedef struct {

  uint1 MoveNum;
  sint1 DiscDiffBW;
  SMOVE	Moves[61];	/* endmarker 0    */
  uint1 Flags;		

} GAME;



extern int Code2Move[], Move2Code[];


extern void	sReadGame(char *s, GAME *pGame);
extern bool	fReadGame(FILE *fp, GAME *pGame);
extern void	sWriteGame(char *s, GAME *pGame);
extern bool	fWriteGame(FILE *fp, GAME *pGame);
extern bool	fReadPackedGame(FILE *fp, GAME *pGame);
extern bool	fReadPackedGameOld(FILE *fp, GAME *pGame);
extern bool	fWritePackedGame(FILE *fp, GAME *pGame);
extern int	PlayGame(int MoveNum, GAME *pGame, SPFELD *pBoard);
extern void 	UniqueGame(GAME *pGame);
extern void	Game2Tab(GAME *pGame, SPFELD *pTab);
extern bool	Tab2Game(SPFELD *pTab, GAME *pGame);

extern void	UpdateGame(GAME *pGame, SPFELD *pNewBoard, PARTEI ToMove);


#endif


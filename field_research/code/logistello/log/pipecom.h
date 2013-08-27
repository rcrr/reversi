// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PIPECOM_H
#define PIPECOM_H

#include "sboard.h"
#include "game.h"

#define MSG_MAX  	500
#define FILE_MAX	100

#define SIG_NUM 	6

#define	SIG_EXIT	0
#define	SIG_BREAK	1
#define SIG_BOARD	2	/* ' ' color ' ' time ' ' last_move ' ' */
				/* board (64) */
#define SIG_MOVE	3	/* ' ' move */

#define SIG_GAME	4	/* [ ('+'|'-')move ]+ diskdiff */

#define SIG_CLEAR	5	/* clear hashtab */

void	PipeSend	(char *ch, char *s);
bool	PipeSyncSend	(char *ch, char *s);

bool	PipeSyncSendEXIT(char *ch);
bool	PipeSyncSendCLEAR(char *ch);
bool	PipeSyncSendBREAK(char *ch);
bool	PipeSyncSendBOARD(char *ch, 
			 PARTEI Partei, int Sek, SFPOS LetzterZug, SPFELD *psf);
bool	PipeSyncSendGAME(char *channel, int player, int time, GAME *pGame, bool to_move);
void	PipeSendMOVE  	(char *ch, SFPOS zug, int MoveTime);

bool	PipeReceive	(char *ch, char *s);
bool	PipeSyncReceive	(char *ch, char *s, int MaxZeit);

#endif

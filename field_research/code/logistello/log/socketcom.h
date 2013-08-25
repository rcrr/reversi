// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef SOCKETCOM_H
#define SOCKETCOM_H

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

void    socket_connect(const String &host, int port, const String &cid);
void    socket_disconnect();

void	SocketSend	(const char *s);
bool	SocketSyncSend	(const char *s);

bool	SocketSyncSendEXIT();
bool	SocketSyncSendCLEAR();
bool	SocketSyncSendBREAK();
bool	SocketSyncSendBOARD(PARTEI Partei, int Sek, SFPOS LetzterZug, SPFELD *psf);
bool	SocketSyncSendGAME(int player, int time, GAME *pGame, bool to_move);
void    SocketSendMOVE(SFPOS move, float move_time=0.0, float value=0.0);

bool	SocketReceive	 (char *s, bool block=false);
bool	SocketSyncReceive(char *s, int MaxZeit);

#endif

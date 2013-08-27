// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Header von int.c */

#ifndef INT_H
#define INT_H


#include "playgm.h"
#include "sboard.h"

/*
#define X0 	0
#define Y0 	0
#define XMAX	360
#define YMAX	360
#define DX 	45
#define DY 	45

#define XM0 	(DX/2)
#define YM0 	(DY/2)
#define DMX 	(DX-6)
#define DMY 	(DY-6)

*/

typedef uint4		GFXFLAGS;

/* Bits 0..7 kodieren Zahl */

#define GFX_WHITE	0		/* Farbe des Steins ist WHITE	*/
#define GFX_NEU		(1<<8)		/* Feld auf jeden Fall neu 	*/
#define GFX_ST		(1<<9)		/* Stein da			*/
#define GFX_LOG_X	10		/* X da				*/
#define GFX_X		(1<<GFX_LOG_X)	
#define GFX_PU		(1<<11		/* Punkt da			*/
#define GFX_ZAHL	(1<<12)		/* Zahl da			*/
#define GFX_LOG_BLACK	13		/* Farbe des Steins ist BLACK	*/
#define GFX_BLACK	(1<<GFX_LOG_BLACK)
#define GFX_REFRESH	(1<<14)		/* alten Inhalt ausgeben	*/
#define GFX_SPIELMODUS	(1<<15)		/* im Spielmodus		*/


void	InitInter	(int *pargc, char **argv, PROPINFO *def);
void	FreeInter	(void);

void	Eigenschaften	(void);
void	PropsOn		(void);
void	PropsOff	(void);

void	Spielbrett	(void);
void	MeldeAmZug	(void);
int	EndeCheck	(void);
void	SpielfeldAus	(SPIELINFO *Spiel, int ZugNr, int MaxNr);
void	ClearText	(void);
void	ClearMeldung	(void);
void	ClearText2	(void);
void	Meldung		(char *s);
void	StatistikAus	(SPIELINFO *);
void	ZugAus		(SFPOS SfPos, PARTEI Partei);
void	MeldeKeineMoegl	(void);
void	MeldeZeitUeber	(void);
void	ErgebnisAus	(SPIELINFO *pSpInfo);
int	ZeitErmitteln	(PARTEI Partei);
void	GfxSfAus	(SPFELD *psf, PARTEI Partei, GFXFLAGS flags);

void	MenuModus	(void);
void	SpielModus	(void);
void	EingabeModus	(void);

void	TabelleAus	(SPIELINFO *Spiel, int maxnr);

void	Notifier	(void);

#endif

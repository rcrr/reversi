// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef BOARD_H
#define BOARD_H

#include "sboard.h"

#define DOUBLE_INDEX    true

#define LAZY_UPDATE     true    /* only bitboard update for surrounding */
                                /* no list                              */

#ifdef ALPHA
typedef int Square;
#else
typedef sint1 Square;
#endif

#if LAZY_UPDATE

#define MAKE_MOVE LazyMove
#define UNDO_MOVE LazyUndo
#define L_PLY_MAKE_MOVE LastPlyLazyMove
#define L_PLY_UNDO_MOVE LastPlyLazyUndo


#else

#define MAKE_MOVE Setzen
#define UNDO_MOVE Zurueck
#define L_PLY_MAKE_MOVE Setzen
#define L_PLY_UNDO_MOVE Zurueck

#endif


#define PATT_ADJ	true	/* beim Setzen Strahlen anpassen? */


#define BR_LEER(pb,i)	(((pb)->p[i]) != BLACK && ((pb)->p[i]) != WHITE)
#define BR_INH(pb,i)  \
  (pb->p[i] == BLACK ? BLACK : (pb->p[i] == WHITE ? WHITE : LEER))



/*  10 Feldnamen:
 *
 *    E C B A A B C E
 *    C X F D D F X C
 *    B F I H H I F B
 *    A D H J J H D A
 *    A D H J J H D A
 *    B F I H H I F B
 *    C X F D D F X C
 *    E C B A A B C E
 */


typedef enum { 

  FE_A=0, FE_B, FE_C, FE_D, FE_E, FE_F,   FE_H, FE_I, FE_J, FE_X

} FELD_ART;



typedef Square UMGEB;


#define NEW_INDICES false


#if NEW_INDICES 
#define DELTA_MAX       5
#else
#define DELTA_MAX       5
#endif


#define STRAHL_ANZ	50
#define ALLPATT_NUM     50

typedef int STRAHLTYP;


typedef struct {

/* Strahlen:	konstant	Strahllängen:		Richtung:
 *
 *   0..7:	y		88888888		-
 *   8..15:	x+8		88888888		|
 *  16..30:     16+7+x-y	123456787654321		\
 *  31..45:	31+x+y		123456787654321		/
 *
 */


/* or new ..... */

  STRAHLTYP st[STRAHL_ANZ];		

} BSTRAHLEN;


#if NEW_INDICES

#define PHV1A    1  /* A1,B1,C1,D1,E1,F1,G1,H1 */
#define PHV1B    2  /* A8,B8,C8,D8,E8,F8,G8,H8 */
#define PHV1C    3  /* A1,A2,A3,A4,A5,A6,A7,A8 */
#define PHV1D    4  /* H1,H2,H3,H4,H5,H6,H7,H8 */
#define PHV2A    5  /* A2,B2,C2,D2,E2,F2,G2,H2 */
#define PHV2B    6  /* A7,B7,C7,D7,E7,F7,G7,H7 */
#define PHV2C    7  /* B1,B2,B3,B4,B5,B6,B7,B8 */
#define PHV2D    8  /* G1,G2,G3,G4,G5,G6,G7,G8 */
#define PHV3A    9  /* A3,B3,C3,D3,E3,F3,G3,H3 */
#define PHV3B   10  /* A6,B6,C6,D6,E6,F6,G6,H6 */
#define PHV3C   11  /* C1,C2,C3,C4,C5,C6,C7,C8 */
#define PHV3D   12  /* F1,F2,F3,F4,F5,F6,F7,F8 */
#define PHV4A   13  /* A4,B4,C4,D4,E4,F4,G4,H4 */
#define PHV4B   14  /* A5,B5,C5,D5,E5,F5,G5,H5 */
#define PHV4C   15  /* D1,D2,D3,D4,D5,D6,D7,D8 */
#define PHV4D   16  /* E1,E2,E3,E4,E5,E6,E7,E8 */
#define PD1A    17  /* A8,B7,C6,D5,E4,F3,G2,H1 */
#define PD1B    18  /* H8,G7,F6,E5,D4,C3,B2,A1 */
#define PD2A    19  /* A7,B6,C5,D4,E3,F2,G1 */
#define PD2B    20  /* H7,G6,F5,E4,D3,C2,B1 */
#define PD2C    21  /* A2,B3,C4,D5,E6,F7,G8 */
#define PD2D    22  /* H2,G3,F4,E5,D6,C7,B8 */
#define PD3A    23  /* A6,B5,C4,D3,E2,F1 */
#define PD3B    24  /* H6,G5,F4,E3,D2,C1 */
#define PD3C    25  /* A3,B4,C5,D6,E7,F8 */
#define PD3D    26  /* H3,G4,F5,E6,D7,C8 */
#define PD4A    27  /* A5,B4,C3,D2,E1 */
#define PD4B    28  /* H5,G4,F3,E2,D1 */
#define PD4C    29  /* A4,B5,C6,D7,E8 */
#define PD4D    30  /* H4,G5,F6,E7,D8 */
#define P3X3A   31  /* A1,B1,C1,A2,B2,C2,A3,B3,C3 */
#define P3X3B   32  /* H1,G1,F1,H2,G2,F2,H3,G3,F3 */
#define P3X3C   33  /* A8,B8,C8,A7,B7,C7,A6,B6,C6 */
#define P3X3D   34  /* H8,G8,F8,H7,G7,F7,H6,G6,F6 */

#define P_NUM   34

#else

#define PHV1A    1  /* A1,B1,C1,D1,E1,F1,G1,H1 */
#define PHV1B    2  /* H1,H2,H3,H4,H5,H6,H7,H8 */
#define PHV1C    3  /* H8,G8,F8,E8,D8,C8,B8,A8 */
#define PHV1D    4  /* A8,A7,A6,A5,A4,A3,A2,A1 */
#define PHV2A    5  /* A2,B2,C2,D2,E2,F2,G2,H2 */
#define PHV2B    6  /* G1,G2,G3,G4,G5,G6,G7,G8 */
#define PHV2C    7  /* H7,G7,F7,E7,D7,C7,B7,A7 */
#define PHV2D    8  /* B8,B7,B6,B5,B4,B3,B2,B1 */
#define PHV3A    9  /* A3,B3,C3,D3,E3,F3,G3,H3 */
#define PHV3B   10  /* F1,F2,F3,F4,F5,F6,F7,F8 */
#define PHV3C   11  /* H6,G6,F6,E6,D6,C6,B6,A6 */
#define PHV3D   12  /* C8,C7,C6,C5,C4,C3,C2,C1 */
#define PHV4A   13  /* A4,B4,C4,D4,E4,F4,G4,H4 */
#define PHV4B   14  /* E1,E2,E3,E4,E5,E6,E7,E8 */
#define PHV4C   15  /* H5,G5,F5,E5,D5,C5,B5,A5 */
#define PHV4D   16  /* D8,D7,D6,D5,D4,D3,D2,D1 */
#define PD1A    17  /* A8,B7,C6,D5,E4,F3,G2,H1 */
#define PD1B    18  /* A1,B2,C3,D4,E5,F6,G7,H8 */
#define PD2A    19  /* A7,B6,C5,D4,E3,F2,G1 */
#define PD2B    20  /* B1,C2,D3,E4,F5,G6,H7 */
#define PD2C    21  /* H2,G3,F4,E5,D6,C7,B8 */
#define PD2D    22  /* G8,F7,E6,D5,C4,B3,A2 */
#define PD3A    23  /* A6,B5,C4,D3,E2,F1 */
#define PD3B    24  /* C1,D2,E3,F4,G5,H6 */
#define PD3C    25  /* H3,G4,F5,E6,D7,C8 */
#define PD3D    26  /* F8,E7,D6,C5,B4,A3 */
#define PD4A    27  /* A5,B4,C3,D2,E1 */
#define PD4B    28  /* D1,E2,F3,G4,H5 */
#define PD4C    29  /* H4,G5,F6,E7,D8 */
#define PD4D    30  /* E8,D7,C6,B5,A4 */
#define PD5A    31  /* A4,B3,C2,D1 */
#define PD5B    32  /* E1,F2,G3,H4 */
#define PD5C    33  /* H5,G6,F7,E8 */
#define PD5D    34  /* D8,C7,B6,A5 */
#define PR1A    35  /* A1,B1,C1,D1,A2,B2,C2,D2 */
#define PR1B    36  /* H1,H2,H3,H4,G1,G2,G3,G4 */
#define PR1C    37  /* H8,G8,F8,E8,H7,G7,F7,E7 */
#define PR1D    38  /* A8,A7,A6,A5,B8,B7,B6,B5 */
#define PR1E    39  /* H1,G1,F1,E1,H2,G2,F2,E2 */
#define PR1F    40  /* A8,B8,C8,D8,A7,B7,C7,D7 */
#define PR1G    41  /* A1,A2,A3,A4,B1,B2,B3,B4 */
#define PR1H    42  /* H8,H7,H6,H5,G8,G7,G6,G5 */

#define P_NUM   42

#endif


typedef struct {

  int pnum, log3, is_line, discs, diff;

} PATTD_INFO;


typedef struct { 

  uint4     ZufBLACK1, ZufBLACK2, ZufWHITE1, ZufWHITE2, ZufBW1, ZufBW2;
  STRAHLTYP *sx, *sy, *sxpy, *sxmy;
  int       dy,  dx,  dxpy,  dxmy;
  int       dy2, dx2, dxpy2, dxmy2;

  STRAHLTYP *s[DELTA_MAX];
  int       d[DELTA_MAX], dh[DELTA_MAX];

  Square    *p;

  // fill to next power of 2

#ifndef ALPHA
  char      dummy[128-4-56-DELTA_MAX*12];
#else
  char      dummy[256-5*8-14*4-DELTA_MAX*16];
#endif

} POSDATEN;


typedef struct { 

  Square p[100];			/* 64 Felder + Rand	   		*/

  uint4 Hash1, Hash2;		/* Hashadresse und Lock			*/

  int SteinAnz, StDiffBW,	/* Gesamtanzahl, #BLACK-#WHITE		*/
      SteinSumme;		/* Summe über alle besetzten Positionen */

  BSTRAHLEN NewPatt;

  Square umg[100];		/* Umgebungsfelder			*/

#if !LAZY_UPDATE
  int   LastIndex;		/* Index des letzen Umgebungsfeldes 	*/
  UMGEB umgl[60];		/* Liste von max. 60 Umgebungsfeldern 	*/
#endif

  BSTRAHLEN Patt;		/* alle Strahlen des Feldes		*/

  POSDATEN daten[100];		/* Strahlenindizes, Hashwerte etc. 	*/

  Square FreeSquares[64];        /* square list for endgame ends with 0  */
  Square FastFreeSquares[64];    // ... for fastendgame

  Square FreeSquaresB[64];
  Square FreeSquaresW[64];

} BRETT;



// board differences

typedef struct {

  int   Pos;		/* Setzposition					*/
  int   Partei;		/* Farbe des gesetzten Steins			*/
  int   AltStDiffBW;	/* alte Steindifferenz				*/
  int	AltLastIndex;	/* letzter Index des alten Brettes		*/
  uint4	AltHash1, AltHash2; /* alte Hashadresse und Lock		*/
  BSTRAHLEN AltNewPatt;

  Square Umgedreht[32];	/* maximal 4 Strahlen a 5 Steine + 1 Ende (-1)	*/
  Square uneu[10];	/* maximal 8 neue Umgebungspos.  + 1 Ende (-1)  */

  POSDATEN *pchanged[32];

#if !LAZY_UPDATE
  uint1 SetzIndex;	/* hier war Setzposition im Umgebungsfeld	*/
#endif

} DELTA;


#define MOB_BLACK	1
#define MOB_WHITE	2

#define STEIN_MAX	8


typedef struct {

  int   MobBLACK,     MobWHITE, 
	PotMob1BLACK, PotMob1WHITE,
	PotMob2BLACK, PotMob2WHITE, 
	PotMob3BLACK, PotMob3WHITE;
  int   FeldMob[10];
  int   SteinMob[STEIN_MAX+1];

  uint1 MobFlags[100];

} MOBALL;





extern Square FeldTab[100];



void	InitSetzen	(void);
void	BrettSf		(BRETT  *pb,	SPFELD *psf);
void	SfBrett		(SPFELD *psf,	BRETT  *pb);

bool	Setzen		(BRETT  *pb,	PARTEI Partei, SFPOS SfPos, DELTA *pd);
void	Zurueck		(BRETT  *pnb,	DELTA  *pd);

bool	LazyMove	(BRETT  *pb,	PARTEI Partei, SFPOS SfPos, DELTA *pd);
void	LazyUndo	(BRETT  *pnb,	DELTA  *pd);

bool	LastPlyLazyMove	(BRETT  *pb,	PARTEI Partei, SFPOS SfPos, DELTA *pd);
void	LastPlyLazyUndo	(BRETT  *pnb,	DELTA  *pd);

bool	MoveFast	(BRETT  *pb,	PARTEI Partei, SFPOS SfPos, DELTA *pd);
void	BackFast	(BRETT  *pnb,	DELTA  *pd);

/*
bool	FastMove	(BRETT  *pb,	PARTEI Partei, SFPOS SfPos, DELTA *pd);
void	FastBack	(BRETT  *pnb,	DELTA  *pd);
*/

bool	NSetzen		(BRETT  *pb,	PARTEI Partei, SFPOS SfPos, DELTA *pd);
void	NZurueck	(BRETT  *pnb,	DELTA  *pd);

int	BrettInh	(BRETT  *pb,	SFPOS  pos);
void	BrettAus	(BRETT  *pb);
int	BrettAnz	(BRETT  *pb,	PARTEI Partei);
int 	BrettAnzBW	(BRETT  *pb);

void	BrettVergl	(BRETT *pb1, BRETT *pb2);
void	BrettVergl2	(BRETT *pb1, BRETT *pb2);
void	BrettVergl0	(BRETT *pb1, BRETT *pb2);
#endif

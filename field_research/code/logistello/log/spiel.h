// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef SPIEL_H
#define SPIEL_H

#include "sboard.h"

#define SZUG_BIT_PARTEI	0x80
#define SZUG_BIT_MARKE	0x40

#define SZUG_PARTEI(b)	(((b) & SZUG_BIT_PARTEI) ? WHITE : BLACK)
#define SZUG_ZUG(b)	\
	CodeZug[((b) & 0xff & ~(SZUG_BIT_PARTEI | SZUG_BIT_MARKE))]
#define SZUG_MARKE(b)   (((b) & SZUG_BIT_MARKE) != 0)


typedef sint1 SZUG;

/* gepackte Darstellung:  P M Z 
 *                        Partei: 0=BLACK
 *                          Marke: 1=true
 *                            Zug: 1..60	ohne Mittelfelder
 */

typedef struct {

  uint1 ZugAnz;
  sint1 StDiffBW;
  SZUG	Zuege[61];	/* mit 0 abgeschlossen */
  sint1 ungl;
  sint1 max;

} SPIEL;


extern PARTEI	NachSpielen	(int ZugAnz, SPIEL *pSpiel, SPFELD *psf);
extern bool	TabToSpiel	(SPFELD *pTab, SPIEL *pSpiel);
extern void	SpielToTab	(SPIEL *pSpiel, SPFELD *pTab);
extern bool	fSpielEin	(FILE *fp, SPIEL *pSpiel);
extern bool	fSpielAus	(FILE *fp, SPIEL *pSpiel);
extern bool	fSpielKlarEin	(FILE *fp, SPIEL *pSpiel);
extern bool	sSpielKlarEin	(char *s, SPIEL *pSpiel);
extern void	fSpielKlarAus	(FILE *fp, SPIEL *pSpiel);
extern void	sSpielKlarAus	(char *s, SPIEL *pSpiel);
extern int	SpieleAnzahl	(char *name);
extern int	SpieleEinlesen	(char *name, SPIEL *Spiele, int AnzMax);
extern bool	SpieleSchreiben (char *name, SPIEL *Spiele, int SpieleAnz);
extern int	compSPIEL	(const void *a, const void *b);
extern SZUG	SZUGgen		(PARTEI Partei, SFPOS Zug, bool Marke);
extern void	SpieleSortieren	(SPIEL *Spiele, int SpieleAnz);
extern int	PraefixeVergl	(SPIEL *Spiele, int SpieleAnz);

extern int ZugCode[100], CodeZug[61];

#endif

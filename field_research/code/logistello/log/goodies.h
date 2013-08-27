// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef O_ALLG
#define O_ALLG

#include "sboard.h"
#include "attr.h"
//#include <stream>
#include <stdarg.h>

/* Ausgabe von LoadGame etc. */

#define LADE_FEHLER	(-1)
#define TAB_KORRUPT	(-2)


typedef struct { uint4 sek; uint2 millisek; } ZEIT;

extern	bool UseCpuTime;	/* Flag für Zeit() */


/* Multiplikativer Kongruenzgenerator langer */

extern  void	sMyRand	(uint4 x);
extern  uint4	MyRand	(void);

inline sint4 my_round(REAL w) { return sint4(rint(w)); }
extern int sgn(int a);

extern ostream &myform(ostream& os, const char* fmt, ... );

extern  void	Zeit		(ZEIT *);
extern  void	RealeZeit	(ZEIT *);
extern  void	CPUZeit		(ZEIT *);
extern  REAL	ZeitDiff	(ZEIT *nach, ZEIT *vor);
extern  void	ZeitAdd		(ZEIT *, REAL Sekunden);
extern  void	BusyWait	(int n);
extern  bool	ParseNumber	(char *s, int *pn, char **psnew);
extern  void	InfoToTab	(SPIELINFO *Spiel, int MaxNr, SPFELD *psf);
extern  int	TabToInfo	(SPFELD *psf, SPIELINFO *Spiel);
extern  void	TabToSf		(SPFELD *psf1, SPFELD *psf2);
extern  bool	SaveGame	(char *name, SPIELINFO *Spiel, int MaxNr);
extern  int	LoadGame	(char *name, SPIELINFO *Spiel);
extern  int	LoadNextGame	(FILE *fp,   SPIELINFO *Spiel);

#endif

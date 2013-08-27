// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ORDER_H
#define ORDER_H

#include "move.h"


extern int ZuegeSortieren(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  PARTEI Partei,	/* Spieler am Zug		*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  int    move1,
  ZUGDAT *ZugDat
);

extern int ZuegeSortierenZM(
  ZUGIO	 *pzio,		/* Zeiger auf globale Variablen	*/
  PARTEI Partei,	/* Spieler am Zug		*/
  int	 RealeHoehe,	/* Abstand zum Blatt		*/
  ZUGMENGE zm,
  ZUGDAT *ZugDat
);

#endif

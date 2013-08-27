// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* test: selective search which uses interior node values */


#ifndef PRECOMP
#include "main.h"
#endif

#include "mid.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"
#include "fpatt.h"
#include "order.h"

#include "crt.h"
#include "crtg.h"



#define TEST		false
#define ALBETEST	false

#define KILLER_HOEHE	5	/* < -> Killertabellen benutzen, sonst	*/
				/* Tiefe 1 oder 2 Suche			*/

#define TIEFE2_HOEHE	7	/* < -> Tiefe 1 Vorsortierung, sonst 2  */

#define ECKEN_ZUERST	true	/* nach Hashzug alle Ecken zuerst? */


#if 0				/* alle Optimierungen aus zum Testen */

#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER  
#define WIEDERHOLUNG 
#define WERTEHASH	false

#else

#define SCHNITT		if (lo_value >= be) goto Ende;	/* beta-Schnitt */
#define NULLFENSTER     hi_value = max + 1
#define WIEDERHOLUNG    \
      if (value > max && value < be && ZugAnz > 1 /* && nRealeHoehe > 0 */)\
\
/* Wiederholungssuche */\
\
        value = -newab(nplayer, -be, -value, move, val0, depth+1, diffsum);
#define WERTEHASH	false
#endif


#ifdef KEIN_HASH
#undef WERTEHASH
#define WERTEHASH false
#endif



#define WERTAUS(s, w) printf("%s w=%d\n", s, w)

#if 1
static int SortDepth[25] = {

/* 0 */   1,1,1,1,1,1,1,
/* 7 */   2,2,2,2,5,6,7,
/*14 */   7,7,7,7,7,6,6,
/*21 */   6,6,6,6

};
#endif


ZUGIO zio;
int bestmove0, maxdepth, mindepth,depthinc;


int sdepth[64] = {

3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
1,1,1,1

};


void _abort(void) { exit(1); }

void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {} /* Signal-Handler */






#define BESSER \
      if (value > lo_value) {\
	if (depth == 0) bestmove0 = move;\
	if (value > al || ZugAnz == 1) BestZug = move;\
        lo_value = value;	/* übernehmen, wenn besser */\
	SCHNITT;\
        if (lo_value > max) max = lo_value;\
        NULLFENSTER;\
      }



#define TRY						\
							\
if (pumg[move] && !ZM_GESETZT(move)) {\
    if (Setzen(pbr, player, move, &delta)) {\
      ZugAnz++; ZM_SETZEN(move);\
\
      value = -newab(nplayer, -hi_value, -max, move, val0, depth+1, diffsum);\
      WIEDERHOLUNG;\
      Zurueck(pbr, &delta);\
      BESSER;\
\
    }\
  }




WERT newab(
  int player, 
  int al, int be, 
  int lastmove, 
  int lastval, 
  int  depth, 
  REAL diffsum
)
{
  int		i, ZugAnz, val0;
  PARTEI	nplayer;
  SFPOS		BestZug, move;
  WERT		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl;
  BRETT		*pbr;
  DELTA		delta;
  uint4		ha;
  HASHEINTRAG	*ph;
  sint1		*pumg;
  static int 	zaehler = 0;
  ZUGMENGE	zm;
  REAL		P;


  if (depth > maxdepth) maxdepth = depth;

  val0 = AlphaBeta1(&zio, sdepth[depth]+depthinc, player, WERTMIN, WERTMAX,
		    ZUG_UNBEKANNT); 

  if (diffsum <= 0) return val0;

  P = EXPWERT(WERT_TO_REAL(val0))-EXPWERT(WERT_TO_REAL(lastval));

  if (P < 0) diffsum += P;

  pbr  = &zio.Brett;
  pumg = pbr->umg;


/* Spielfeld voll */

  if (pbr->SteinAnz == 64) return BewDiff(pbr, player);

#if 0
  {
    int maxval = WERTMIN;

  for (i=0; i <= pbr->LastIndex; i++) 
    if (Setzen(pbr, player, move=pbr->umgl[i].UPos, &delta)) {

      value = -zio.BewMitte(pbr, GEGNER(player));

    if (value > maxval) maxval = value;

      Zurueck(pbr, &delta);
    } 


  val0 = maxval + 10000;


  }
#endif



  ZM_LOESCHEN;

  ZugAnz = 0;

  BestZug  = ZUG_UNBEKANNT; 
  lo_value = WERTMIN; 
  hi_value = be;
  max      = al;


  nplayer = GEGNER(player);



#if HASH

/* war Stellung schon da? */


  ha = pbr->Hash1 & (HASHANZ-1);
  if (player == WHITE) ha = HASH_WHITE(ha);

  ph = &zio.hash.HashTab[ha];

  move = ph->BesterZug;

  if (ZUG(move) && LOCK_EQUAL(ph->Hash2, pbr->Hash2)) { TRY }

#endif


#if ECKEN_ZUERST

/* jetzt alle Eckenzüge */

  move = A1; TRY;
  move = H8; TRY;
  move = A8; TRY;
  move = H1; TRY;

#endif


  if (1) {

#if KILLER
  
/* Rest gemäß Killertabelle oder DefaultKill */

    if (ZUG(lastmove)) {

      if (player == BLACK) Kill = zio.killer.KillWHITE; 
      else 		   Kill = zio.killer.KillBLACK;

      pl = &Kill->Liste[lastmove][0];

    } else {

#endif

      pl = &zio.killer.DefaultKill[0];

#if KILLER

    }

#endif


    for (i=zio.killer.FreiAnz; i > 0; i--) {

      move = *pl++;
      TRY
    }


  } else {


    ZUGDAT ZugDat[65];
    int    Anz;

#if 1
    Anz=Vorsort(
	      &zio, player, 0 < TIEFE2_HOEHE ? 1 : 2, zm, ZugDat
	    ); 
#else
    Anz=ZuegeSortieren(
	      &zio, player, SortDepth[0 >= 0 ? 0 : 0], ZugDat
	    );
#endif


    FOR (i, Anz) {

      move = ZugDat[i].Zug;

      TRY
    }

  }



AllesBesucht:


  if (ZugAnz == 0) {				/* player kann nicht ziehen */

    if (lastmove == ZUG_PASSEN) {		/* keiner kann */

      return BewDiff(pbr, player);

    } else {					/* eins tiefer */

      return -newab(GEGNER(player), -be, -al, ZUG_PASSEN, val0, depth+1, diffsum);
    }
  }




Ende:



/* Hash- und Killer-Eintrag anpassen */


  if (1) {

#if TEST
  if (!ZUG(BestZug)) Error("BestZug außerhalb");
#endif



#if KILLER
    if (ZUG(lastmove)) {

      if (!ZUG(BestZug)) { printf("n:"); KoorAus(BestZug); }

      KillerUpdate(&zio.killer, GEGNER(player), lastmove, BestZug);    
    }
#endif

/*printf("ReahleHoehe=%d Wert=%d zurück\n", RealeHoehe, lo_value);*/

  } else if (0) {

#if HASH

    if (zio.Quiescence) 
      SETHASH(ph, pbr, 0, al, be, BestZug, lo_value)

#endif


  }

  return lo_value;
}




int main(void)
{
  FILE *fp;
  SPFELD sf;
  int i, val, player;
  REAL P[] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, -1, 1.0, -1, 1.5, 2.0, 2.5, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, -1 };
  ZEIT StartZeit, AktuelleZeit;
  GAME game;


  fp = fopen("example.osp", "r");
  if (!fp) Error("file not found");

  fTabEin(fp, &sf);

  Tab2Game(&sf, &game);

  player = PlayGame(game.MoveNum, &game, &sf);


  sf.Marke = player;

  SfAus(&sf, player, -1);

  printf("%d\n", player);

  InitZug(&zio, BewA, Check, HASHBITS);

  
  Zeit(&StartZeit);


  for (i=1;; i++) {

    SfBrett(&sf, &zio.Brett);

    maxdepth = 0;

    depthinc = i/10;

    val = newab(player, WERTMIN, WERTMAX, ZUG_UNBEKANNT, 0, 0, i * 0.04);

    KoorAus(bestmove0); printf(": %d (%d)\n", val, maxdepth);

    Zeit(&AktuelleZeit);

printf("%.2f\n", ZeitDiff(&AktuelleZeit, &StartZeit));


    if (ZeitDiff(&AktuelleZeit, &StartZeit) > 30) break;

  }

  return 0;
}

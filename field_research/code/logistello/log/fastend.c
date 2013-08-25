// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// very fast endgame search for last plys / 24.1.97
 
#include "main.h"
#include "move.h"
#include "board.h"
#include "end.h"
#include "crt.h"
#include "crtg.h"


inline void FindFastFreeSquares(ZUGIO *pzio)
{
  int i, move;
  sint1 *p=pzio->Brett.p, *pf=pzio->Brett.FastFreeSquares;
  SFPOS *pk=pzio->killer.DefaultKill;

  for (i=pzio->killer.FreiAnz; i > 0; i--) {
    move = *pk++;
    if (p[move] == LEER) *pf++ = move;
  }

  *pf = 0;
}


#define MOVE(d) \
\
    if (p[Pos+d] == gegn) {		/* gegnerischer Stein */\
\
      register sint1 *r_p=&p[Pos+d+d];\
\
      FOREVER {					                        \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        if (*r_p != gegn) break;		                        \
        r_p += d; \
        break; \
      }                                                                 \
      if (*r_p == col) {			 /* move possible */	\
        Pos1 = r_p - p - d;						\
        do {						                \
	  *pgedreht++ =  Pos1;						\
	  p[Pos1]     =  col;						\
	  Pos1 -= d;							\
        } while (Pos1 != Pos);						\
      }\
    }


inline bool EndMove(
  BRETT  *pnb,		/* Zeiger auf Brett, auf dem gesetzt wird	*/
  PARTEI eigen,		/* Partei am Zug				*/
  SFPOS	 Pos,		/* Position des zu setzenden Steins		*/
  DELTA  *pd		/* Änderungen an *pnb				*/
)
{
  int   diff, Pos1;
  register sint1 *p, gegn, col=eigen;
  register sint1 *pgedreht;	/* Zeiger auf Liste der umgedrehten Steine in *pd */  

  p        = pnb->p;
  pgedreht = pd->Umgedreht; 
  gegn     = GEGNER(eigen);

  MOVE(1); MOVE(-1); MOVE(10); MOVE(-10);
  MOVE(9); MOVE(-9); MOVE(11); MOVE(-11);


  if ((diff=pgedreht-pd->Umgedreht)) {

    *pgedreht = -1;			/* Endmarkierung */

    pnb->SteinAnz++;			/* ein Stein mehr	*/
    pnb->SteinSumme += Pos;

    pd->AltStDiffBW = pnb->StDiffBW;

    if (eigen == BLACK) pnb->StDiffBW += diff+diff;
    else	        pnb->StDiffBW -= diff+diff;

    pnb->StDiffBW += eigen;		/* auch Diff. anpassen	*/
    p[Pos] = eigen; 

    return true;

  }

  return false;
}



// take back move

inline void EndUndo(BRETT *pnb, PARTEI Partei, SFPOS Pos, DELTA *pd)
{
  SFPOS *p=pnb->p;
  sint1 *pgedreht=pd->Umgedreht;
  int   gegn = GEGNER(Partei);

  pnb->SteinAnz--;
  pnb->SteinSumme -= Pos;
  pnb->StDiffBW = pd->AltStDiffBW;

  while (*pgedreht >= 0) p[*pgedreht++] = gegn;

  p[Pos] = LEER;
}



/* bei 63 Steinen ist keine Anpassung mehr nötig! -> nur Steindifferenz berechnen */


/* f a s t e r */


#define MOVEDIFF(d) \
\
    if (p[Pos+d] == opp) {			        /* gegnerischer Stein */\
\
      register int   di = diff;\
      register sint1 *q = &p[Pos+d+d];\
\
      while (*q == opp) { q += d; di += diff; }        /* Strahl absuchen */\
\
      if (*q == col) StDiffBW += di;			/* Zug möglich */\
\
    }



inline int EndMoveDiff(
  BRETT  *pnb,		/* Zeiger auf Brett, auf dem gesetzt wird	*/
  PARTEI eigen,		/* Partei am Zug				*/
  SFPOS	 Pos		/* Position des zu setzenden Steins		*/
)
{
  register sint1 col=eigen, opp=GEGNER(col), *p = pnb->p;
  register int   diff, StDiffBW;

  diff   = eigen+eigen;		/* Steindifferenz BW bei Wechsel eines Steins */

  StDiffBW = pnb->StDiffBW;

  MOVEDIFF(1); MOVEDIFF(-1); MOVEDIFF(10); MOVEDIFF(-10);
  MOVEDIFF(9); MOVEDIFF(-9); MOVEDIFF(11); MOVEDIFF(-11);

  if (StDiffBW != pnb->StDiffBW) StDiffBW += eigen;

  return StDiffBW;
}


 
inline int ResultIfNotFull(BRETT *pbo, int player)
{
  return (pbo->StDiffBW == 0 ? 0 :					
	  (player == BLACK ?
	   (pbo->StDiffBW > 0 ?   pbo->StDiffBW + (64 - pbo->SteinAnz)  : 	
	    pbo->StDiffBW - (64 - pbo->SteinAnz)) :	
	   (pbo->StDiffBW > 0 ? -(pbo->StDiffBW + (64 - pbo->SteinAnz))  : 	
	    -(pbo->StDiffBW - (64 - pbo->SteinAnz)))));
}


inline int Result(BRETT *pbo, int player)
{
  return player == BLACK ? pbo->StDiffBW : -pbo->StDiffBW;
}


inline int Fast63(BRETT *pbo, int player) 
{
  int move = 3168 - pbo->SteinSumme;
  int value;

  if (pbo->StDiffBW != (value=EndMoveDiff(pbo, player, move))) {

    return player == BLACK ? value : -value;

  } else if (pbo->StDiffBW != (value=EndMoveDiff(pbo, GEGNER(player), move))) {

    return player == BLACK ? value : -value;

  } 

  return ResultIfNotFull(pbo, player);
}



static int RecFastEnd(ZUGIO *pzio, int player, int al, int be, bool no_move=false)
{
  int   move, val;
  DELTA delta;
  BRETT *pbo = &pzio->Brett;
  sint1 *p=pbo->p;
  int num = pbo->SteinAnz;

  if (num >= 62) {

    if (num == 62) {

      int max_val = WERTMIN;

      for (sint1 *pm=pbo->FastFreeSquares; (move=*pm); pm++) { 
	
       if (p[move] == LEER && EndMove(pbo, player, move, &delta)) {

          pzio->cio.BewAnz++;
	  val = -Fast63(pbo, GEGNER(player));

	  EndUndo(pbo, player, move, &delta);
	  if (val >= be) return val;
	  if (val > max_val) max_val = val;
	}
      }

      if (max_val == WERTMIN) {   // no move possible

	if (no_move) return ResultIfNotFull(pbo, player);

	return -RecFastEnd(pzio, GEGNER(player), -be, -al,  true);

      }

      return max_val;


    } else if (num == 63) {

      // one square free -> immediate move

      pzio->cio.BewAnz++;
      return Fast63(pbo, player);


    } else if (num == 64) {

      // no free square => return difference

      return ResultIfNotFull(pbo, player);

    }
  }

  int max_val = WERTMIN;

  for (sint1 *pm=pbo->FastFreeSquares; (move=*pm); pm++) { 
	
    if (p[move] == LEER && EndMove(pbo, player, move, &delta)) {

      pzio->cio.BewAnz++;
      val = -RecFastEnd(pzio, GEGNER(player), -be, -al, false);

      EndUndo(pbo, player, move, &delta);
      if (val >= be) return val;
      if (val > max_val) max_val = val;
    }
  }

  if (max_val == WERTMIN) {   // no move possible

    if (no_move) return ResultIfNotFull(pbo, player);
    
    return -RecFastEnd(pzio, GEGNER(player), -be, -al, true);
    
  }

  return max_val;
}




int FastEnd(
  ZUGIO		*pzio,	
  PARTEI	player,	
  int		al,
  int		be
)
{
  pzio->fertig = false;

  FindFastFreeSquares(pzio);
 
  return RecFastEnd(pzio, player, al, be, false);
}


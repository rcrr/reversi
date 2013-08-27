/* make move, update only bit board of surrounding squares */



inline BOOL ILazyMove(
  BRETT  *pnb,		/* Zeiger auf Brett, auf dem gesetzt wird	*/
  PARTEI col,		/* Partei am Zug				*/
  SFPOS	 Pos0,		/* Position des zu setzenden Steins		*/
  DELTA  *pd		/* Änderungen an *pnb				*/
)
{
  register POSDATEN *pda;
  int	 diff, opp, Pos1, pos=Pos0;
  SBYTE	 *p;
  SBYTE	 *pgedreht;	/* Zeiger auf Liste der umgedrehten Steine in *pd */  
  SBYTE  *pneu;		/* Zeiger auf Liste neuer Umgebungspos.		  */
  SBYTE  *umg;
  ULONG  DHash1, DHash2;

#if LAZYTEST 
DELTA delta;
SPFELD sf, sf0;
BRETT board, board0;
int b;

  {
    board0 = *pnb;
    memcpy(sf0.p, pnb->p, sizeof(sf.p));

    sf = sf0;
    b = SfSetzen(&sf, col, pos);

    if (b) {

      SfBrett(&sf, &board);

    }

  }
#endif


  if (pnb->umg[pos] < UMG) return FALSE; /* no surrounding disc */

  p	   = pnb->p; 
  pgedreht = pd->Umgedreht;
  opp      = GEGNER(col);

  DHash1 = DHash2 = 0;



#define SANP(v)				   \
  { int i; FOR (i, DELTA_MAX) {            \
    *(pda->s[i]) =*(pda->s[i]) v pda->d[i];\
  }}

#define SANP2(v)			    \
  { int i; FOR (i, DELTA_MAX) {             \
    *(pda->s[i]) =*(pda->s[i]) v pda->dh[i];\
  }}
 


/* unrolled (!) */

#define LSETZCHECK(dp,vorz)						\
                                                                        \
    if (p[pos+dp] == opp) {	     /* opponent's disc adj.? */\
      register SBYTE *r_p=&p[pos+dp];\
      register SBYTE r_opp=opp;\
      FOREVER {					                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
        if (*(r_p+=dp) != r_opp) break;		                        \
      }                                                                 \
      if (*r_p == col) {			 /* move possible */	\
        Pos1 = r_p - p - dp;						\
	pda = &pnb->daten[Pos1];					\
        do {						                \
	  *pgedreht++ =  Pos1;						\
	  p[Pos1]     =  col;						\
	  DHash1 ^= pda->ZufBW1;					\
	  DHash2 ^= pda->ZufBW2;					\
	  SANP(vorz);		  			                \
	  pda  -= dp;							\
	  Pos1 -= dp;							\
        } while (Pos1 != pos);						\
      }									\
    }

  if (col == BLACK) {
    LSETZCHECK(1,+); LSETZCHECK(-9,+); LSETZCHECK(-10,+); LSETZCHECK(-11,+);
    LSETZCHECK(-1,+); LSETZCHECK(9,+); LSETZCHECK(10,+); LSETZCHECK(11,+);
  } else {
    LSETZCHECK(1,-); LSETZCHECK(-9,-); LSETZCHECK(-10,-); LSETZCHECK(-11,-);
    LSETZCHECK(-1,-); LSETZCHECK(9,-); LSETZCHECK(10,-); LSETZCHECK(11,-);
  }



  if ((diff=pgedreht-pd->Umgedreht)) {	/* move possible */


/*KoorAus(Pos); printf(" S\n");*/

    *pgedreht = -1;			/* end marker */

    umg  = pnb->umg;

    pd->AltStDiffBW  = pnb->StDiffBW;
    pd->AltHash1     = pnb->Hash1;
    pd->AltHash2     = pnb->Hash2;
    pd->Partei       = col;
    pd->Pos	     = pos;

    pnb->SteinAnz++;
    pnb->SteinSumme += pos;

    p[pos] = col; 
    umg[pos] = 0;


/* hash & pattern updates for move position */

    if (col == BLACK) {

      pnb->Hash1 ^= DHash1 ^ pda->ZufBLACK1; 
      pnb->Hash2 ^= DHash2 ^ pda->ZufBLACK2;
      pnb->StDiffBW += diff+diff+1;

      SANP2(+);

    } else {

      pnb->Hash1 ^= DHash1 ^ pda->ZufWHITE1;
      pnb->Hash2 ^= DHash2 ^ pda->ZufWHITE2;
      pnb->StDiffBW -= diff+diff+1;

      SANP2(-);

    }



/* examine squares that surround move position */

    pneu  = pd->uneu;


    { register SBYTE *umg=&pnb->umg[pos], *p = &pnb->p[pos];


#define UCHECK(dp) \
\
    if (p[dp] == LEER && umg[dp] < UMG) { /* new position */\
      *pneu++ = pos+dp;\
      umg[dp] = UMG;\
    }


    UCHECK(-1); UCHECK(-9); UCHECK(-10); UCHECK(-11);
    UCHECK(+1); UCHECK(+9); UCHECK(+10); UCHECK(+11);

    }


    *pneu = -1;


#if LAZYTEST

    if (!b) Error("setzen geht nicht?");

    BrettVergl2(&board, pnb);

#endif


    return TRUE;
  }


#if LAZYTEST
  if (b) Error("setzen geht?");
#endif

  return FALSE;
}



/* undo move (lazy) */

inline void ILazyUndo(BRETT *pnb, DELTA *pd)
{
  int   pos;
  register SFPOS *p=pnb->p, opp;
  SBYTE *pgedreht, *umg=pnb->umg;
  POSDATEN *pda, *daten=pnb->daten;


  pnb->SteinAnz--;
  pnb->SteinSumme -= (pos=pd->Pos);
  pnb->StDiffBW = pd->AltStDiffBW;
  pnb->Hash1    = pd->AltHash1;
  pnb->Hash2    = pd->AltHash2;

  umg[pos] = UMG;
  p[pos]   = LEER;

  opp = GEGNER(pd->Partei);

  pgedreht = pd->Umgedreht;


  if (opp == WHITE) {

    register SBYTE *r_p = pnb->p;

    pda = &daten[pos];
    SANP2(-);

    while ((pos=*pgedreht++) >= 0) { 

      r_p[pos] = WHITE;
      pda = &daten[pos];
      SANP(-);

    }

  } else {

    register SBYTE *r_p = pnb->p;

    pda = &daten[pos];
    SANP2(+);

    while ((pos=*pgedreht++) >= 0) { 

      r_p[pos] = BLACK;
      pda = &daten[pos];
      SANP(+);

    }
  }

  pgedreht = pd->uneu;
  while ((pos=*pgedreht++) >= 0) { umg[pos] = 0; }
}


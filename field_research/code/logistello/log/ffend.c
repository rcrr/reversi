// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// fastest first search at the bottom of the tree / 7.99
// doesn't work


#include "main.h"
#include "ffend.h"
#include "end.h"

#if !LAZY_UPDATE
#error "LAZY_UPDATE false"
#endif


#define FF_SORT_MAX 58


inline bool move_ok(Square *p, int Pos, PARTEI to_move, PARTEI opp, int d)
{
  if (p[Pos+d] != opp) return false;
  
  register Square *r_p=&p[Pos+d+d];\
    
  FOREVER {                                                         
    if (*r_p != opp) break;                                        
    r_p += d; 
    if (*r_p != opp) break;                                        
    r_p += d; 
    if (*r_p != opp) break;                                        
    r_p += d; 
    if (*r_p != opp) break;                                        
    r_p += d; 
    if (*r_p != opp) break;                                        
    r_p += d; 
    break; 
  }
  
  return *r_p == to_move;
}



inline int mobility(BRETT *pbr, PARTEI Partei, SFPOS *free_list)
{
  int m = 0;
  Square *p=pbr->p;
  int mv;
  
  while ((mv = *free_list++)) {

    if (p[mv] == LEER) {

      if (move_ok(p, mv, Partei, GEGNER(Partei), +1) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), -1) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), +10) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), -10) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), +11) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), -11) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), +9) ||
	  move_ok(p, mv, Partei, GEGNER(Partei), -9))
	m++;
    }
  }
  return m;
}


int FFEndAlphaBeta(
  ZUGIO		*pzio,		// pointer to global variables 
  PARTEI	Partei,		// player to move
  int		al,		// alpha-beta window
  int		be,
  int           last_move,
  SFPOS         *free_list
)
{
  int		i, ZugAnz, KZug; 
  SFPOS		BestZug, AktZug, HashZug;
  BRETT		*pbr;
  DELTA		Delta;
  int		value, lo_value, hi_value, max;
  KILLTAB	*Kill;
  SFPOS		*pl, f_list[65];
  UMGEB		*pu;


  pzio->cio.BewAnz++;
  pbr = &pzio->Brett;

  if (--zaehler < 0) { 
    pzio->Check(&pzio->cio, pzio, false, false); 
    zaehler = CHECK_COUNT; 
  }

  if (pbr->SteinAnz >= 63) {	// 63 -> board almost full

    if (pbr->SteinAnz == 64) ANTI_RET(BEWDIFF);

    else {

      // one square free -> immediate move

      pzio->cio.BewAnz++;
      AktZug = 3168 - pbr->SteinSumme;

      if (pbr->StDiffBW != (value=EndSetzDiff(pbr, Partei, AktZug))) {

	ANTI_RET(Partei == BLACK ? value : -value);

      } else if (pbr->StDiffBW != (value=EndSetzDiff(pbr, GEGNER(Partei), AktZug))) {

	ANTI_RET(Partei == BLACK ? value : -value);

      } 

      BEWTEST
	ANTI_RET(BEWDIFF1);
    }
  }


  if (!free_list) {

    // init free square list if not provided

    Square *p=pzio->Brett.p;
    SFPOS *pk=pzio->killer.DefaultKill, *pf = f_list;

    for (i=pzio->killer.FreiAnz; i > 0; i--) {
      int move = *pk++;
      if (p[move] == LEER) *pf++ = move;
    }

    *pf = 0;
    free_list = f_list;

  }

  
  if (pbr->SteinAnz <= FF_SORT_MAX) {

    // sort according to minimum opponent mob.
    
    ZUGDAT zd[65];
    int k = 0;
    int mv;
    SFPOS *p = free_list;
    
    while ((mv=*p++)) {
      
      if (pbr->p[mv] == LEER) {
	zd[k].Zug = mv;
	if (EndSetzen(pbr, Partei, mv, &Delta)) {
	  zd[k].Wert = - -mobility(pbr, GEGNER(Partei), free_list);
	  EndZurueck(pbr, Partei, mv, &Delta);
	} else {
	  zd[k].Wert = -1000;
	}
	k++;
      }
    }
    
    qsort((char*)zd, (size_t) k, sizeof(ZUGDAT), compZUGDAT);

#if 0
    
    {
      SPFELD sf;
      
      BrettSf(pbr, &sf);
      //SfAus(&sf, 0, 0);
      //printf("%d \n", Partei);
      
      //FOR (i, k) { KoorAus(zd[i].Zug); printf(" %d\n", zd[i].Wert); }
      //puts("");
      
      if (64-SfAnz(&sf) != k) printf("!!!!!!!!!!!!!!!\n");
    }
    
#endif
    
    FOR (i, k) f_list[i] = zd[i].Zug;
    f_list[k] = 0;
    free_list = f_list;
  }
  
  
  ZugAnz = 0;
  lo_value = WERTMIN; max = al;

  for (pu=free_list; (AktZug=*pu); pu++) { 

    if (pbr->p[AktZug] == LEER && EndSetzen(pbr, Partei, AktZug, &Delta)) {

      ZugAnz++; 

      value = - FFEndAlphaBeta(pzio, GEGNER(Partei), -be, -max, AktZug, free_list);

#if 0

      if (pbr->SteinAnz >= 60) {
	int val2 = - EndAlphaBeta1(pzio, GEGNER(Partei), -be, -max, AktZug);
      
	if (value != val2) {

	  SPFELD sf;
	  BrettSf(pbr, &sf);
	  SfAus(&sf, 0, 0);
	  printf("p=%d al=%d be=%d val=%d val2=%d\n\n", GEGNER(Partei), max, be, value, val2);
	}
      }

#endif
      
      
      EndZurueck(pbr, Partei, AktZug, &Delta);

      if (value > lo_value) {
	lo_value = value;			// update if better
	if (lo_value >= be) return lo_value;	// beta cut
	if (lo_value > max) max = lo_value;
      }
    }
  }

  if (ZugAnz == 0) {				// no move

    if (last_move == ZUG_PASSEN) {		// neither player has a move

      BEWTEST
	ANTI_RET(BEWDIFF1);

    } else {					// pass

      return -FFEndAlphaBeta(pzio, GEGNER(Partei), -be, -al, ZUG_PASSEN, free_list);
    }
  }

  return lo_value;
}


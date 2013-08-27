// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* new endgame search 1.95:

  1. evaluate root with depth n heuristically
  2. choose minimax node at depth n-k
  3. evaluate this node exactly
  4. goto 1. until the root value is known

  current move is that with best heuristic evaluation so far

*/

#include "main.h"

#include "mid.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"
#include "fpatt.h"
#include "order.h"
#include "pmove.h"
#include "newend.h"
#include "goodies.h"

#include "crt.h"
#include "crtg.h"

#include "end.h"

#define P_WIN           (0.72)    /* switch to win search when  >= */
#define P_LOSS          (0.40)    /* switch to draw search when <= */

#define HASH2_NUM	(1L<<17)

#define TEST		false


enum { 

  NOT_DEFINED=0, HEURISTIC_EXACT, HEURISTIC_LESS, HEURISTIC_GREATER, OUTCOME 

} HASH2TYPE;


typedef struct {

  uint4 Hash2;		/* lock */
  int   Value;		
  uint1 Type;

} HASH2_ENTRY; 



sint1 MoveLists[64][64];

static ZUGIO *pzio;
static BRETT *pbr;
static Square *pumg;
static HASH2_ENTRY *HashTab2=NULL;
static int   endal, endbe, DrawVal;

static int value, max_, maxbe;
static int upnum;


static int SortDepth[25] = {

/* 0 */   4,3,2,1,1,1,1,
/* 7 */   1,1,1,1,1,1,1,
/*14 */   1,1,1,1,1,1,1,
/*21 */   1,1,1,1

};


void InitIterAlphaBeta(ZUGIO *p)
{
  int i;

  pzio = p;
  pbr  = &pzio->Brett;
  pumg = pbr->umg;

  if (!HashTab2) {

    HashTab2 = (HASH2_ENTRY*) malloc(sizeof(HASH2_ENTRY) * HASH2_NUM);

    if (!HashTab2) Error("InitIterAlphaBeta: no mem\n");

  }

  FOR (i, HASH2_NUM) HashTab2[i].Type = NOT_DEFINED;
}



void PrintIterStat(void)
{
  int i;


  printf("(");
  FOR (i, min(END_CHECK_DEPTH,6)) { 
    KoorAus(MoveLists[0][i]); 
    if (i < min(END_CHECK_DEPTH, 6)-1) printf(" "); 
  }

  printf("%3d%% %3d) (", round(EXPWERT(WERT_TO_REAL(value))*100.0), upnum);

  KoorAus(pzio->Path[0]); 
  printf(" %d%3d%%)", maxbe, round(EXPWERT(WERT_TO_REAL(max_))*100.0));    

  printf("\r"); fflush(stdout);
}



int UpdateWLD(SPFELD *psf, PARTEI colour, int leaf_value)
{
  int   i, value, ha, ha0;
  BRETT Board;
  SPFELD sf;
  DELTA Delta;


  SfBrett(psf, &Board);

  FOR (i, END_CHECK_DEPTH) {

/*
KoorAus(MoveLists[0][i]); printf(" "); fflush(stdout);
*/
    if (MoveLists[0][i] != ZUG_PASSEN && 
        !MAKE_MOVE(&Board, colour, MoveLists[0][i], &Delta)) {

      MoveLists[0][i] = ZUG_PASSEN;
/* Error("UpdateWLD: move?"); Problem if early wipe out occurs */
    }

    colour = GEGNER(colour);
  }


  BrettSf(&Board, &sf);


  SfBrett(&sf, pbr);

  pzio->Selective   = false;
  pzio->Percentile1 = pzio->Percentile2 = END_PERCENTILE;
  pzio->Quiescence  = true;


#if KILLER
  KillerAdjust(&pzio->killer, &pzio->Sf);
#endif


#if 1

  if (leaf_value >= 0) {

    printf("EndAlphaBeta1 problem: check whether NextHashStamp has been called");
    exit(20);
    
    value = EndAlphaBeta1(
              pzio, colour, 0, 1, MoveLists[0][END_CHECK_DEPTH-1]
            );

    if (value <= 0) 

      value = EndAlphaBeta1(
                pzio, colour, -1, 0, MoveLists[0][END_CHECK_DEPTH-1]
              );

  } else {

    printf("EndAlphaBeta1 problem: check whether NextHashStamp has been called");
    exit(20);

    value = EndAlphaBeta1(
              pzio, colour, -1, 0, MoveLists[0][END_CHECK_DEPTH-1]
            );

    if (value >= 0) 

      value = EndAlphaBeta1(
                pzio, colour, 0, 1, MoveLists[0][END_CHECK_DEPTH-1]
              );

  } 
 
#if 0
  if (sgn(EndAlphaBeta1(pzio, colour, -1, 1, MoveLists[0][END_CHECK_DEPTH-1]))
      != sgn(value)) Error("val sgn");
#endif

#else

  printf("EndAlphaBeta1 problem: check whether NextHashStamp has been called");
  exit(20);
  
  value = EndAlphaBeta1(pzio, colour, -1, 1, MoveLists[0][END_CHECK_DEPTH-1]);

#endif


/* search entry (linear probing), don't overwrite outcome entries */

  ha0 = pbr->Hash1 & (HASH2_NUM-1);
  if (colour == WHITE) ha0 = HASH_WHITE(ha0);

  ha = ha0;

  FOREVER {

    if (HashTab2[ha].Type != OUTCOME) break;

    ha++; if (ha >= HASH2_NUM) ha = 0;
    
    if (ha == ha0) Error("UpdateWLD: Hashtable full");

  }

  HashTab2[ha].Type  = OUTCOME;
  HashTab2[ha].Hash2 = Board.Hash2;
  HashTab2[ha].Value = value;

  return pzio->Wert;
}


#if 1 

#define DAUS

#define AUS

#else

#define DAUS printf("d=%d %d\n", depth, colour);

#define AUS   \
{ int i, j;   \
printf("m=%d %d\n", BestMove, depth);\
  FOR (i, END_CHECK_DEPTH+1) {\
    FOR (j, END_CHECK_DEPTH+1) printf("%2d ", MoveLists[i][j]);\
    printf("\n");\
  }\
}

#endif


#define TRY \
if (pumg[CurMove] && !ZM_GESETZT(CurMove))  {\
    if (MAKE_MOVE(pbr, colour, CurMove, &Delta)) {\
      MoveNum++; ZM_SETZEN(CurMove);\
\
      value = -IterAlphaBeta(depth+1, GEGNER(colour), -hi_value, -max, CurMove);\
\
/*KoorAus(CurMove); printf("(%d %d) (%d %d) -> %d\n", al, be, max, hi_value, value);*/\
/*printf(":::%d (%d,%d) ### %d\n", MoveNum, max, hi_value, value);*/\
\
      if (value > lo_value) {\
\
/* store path before search repetition */\
\
	BestMove = CurMove;\
        strcpy((char*)&MoveLists[depth][depth+1], (char*)&MoveLists[depth+1][depth+1]);\
	MoveLists[depth][depth] = BestMove;\
AUS;\
      }\
\
/* search repetition ? */\
\
      if (value > max && value < be && MoveNum > 1)\
\
        value = -IterAlphaBeta(depth+1, GEGNER(colour), -be, -value, CurMove);\
\
      UNDO_MOVE(pbr, &Delta);\
\
      if (value > lo_value) {\
\
	BestMove = CurMove;\
        strcpy((char*)&MoveLists[depth][depth+1], (char*)&MoveLists[depth+1][depth+1]);\
        MoveLists[depth][depth] = BestMove;\
AUS;\
        lo_value = value;\
\
	if (lo_value >= be) goto Ende;	/* beta-cut */\
\
        if (lo_value > max) max = lo_value;\
\
        hi_value = max + 1;\
\
      }\
    }\
  }


WERT IterAlphaBeta(
  int    depth,         /* distance to root             */
  PARTEI colour,	/* colour to move		*/
  WERT	 al,		/* alpha-beta window		*/ 
  WERT	 be,
  int    LastMove
)
{
  int		i, MoveNum;
  SFPOS		BestMove, CurMove, HashMove;
  WERT		value, lo_value, hi_value, max;
  DELTA		Delta;
  uint4		ha, ha0;
  HashEntry	*ph;
  ZUGMENGE	zm;


DAUS;

  if (depth == END_CHECK_DEPTH) {

    MoveLists[depth][depth] = 0;

    ha0 = pbr->Hash1 & (HASH2_NUM-1);
    if (colour == WHITE) ha0 = HASH_WHITE(ha0);

    ha = ha0;


#if 1

/* heuristic value in hashtable? */

    if (HashTab2[ha].Hash2 == pbr->Hash2) {

      if (HashTab2[ha].Type == HEURISTIC_EXACT) return HashTab2[ha].Value;

      if (HashTab2[ha].Type == HEURISTIC_GREATER && 
          HashTab2[ha].Value >= be) return HashTab2[ha].Value;

      if (HashTab2[ha].Type == HEURISTIC_LESS && 
          HashTab2[ha].Value <= al) return HashTab2[ha].Value;
    }

#endif


/* outcome in hashtable? (linear probing) */

    FOREVER {

      if (HashTab2[ha].Type != OUTCOME) break;

      if (HashTab2[ha].Hash2 == pbr->Hash2) {

        int value = HashTab2[ha].Value;

        if      (value == 0) value =  DrawVal;
        else if (value <  0) value = -WERTGEWINN;
        else                 value = +WERTGEWINN;

        return value;
      }

      ha++; if (ha >= HASH2_NUM) ha = 0;
    
      if (ha == ha0) break;

    }


/* not found => evaluate */

    pzio->Selective  = true;
    pzio->Percentile1 =  pzio->Percentile2 = 1.4;
    pzio->Quiescence = true;

/* maximal depth at <= 40 discs */

    { int evaldepth = (40+END_CHECK_DEPTH+END_EVAL_DEPTH) - pbr->SteinAnz;

/* deeper evaluation at positions with less discs => faster in late endgame */

      if (evaldepth < 2)              evaldepth = 2;
      if (evaldepth > END_EVAL_DEPTH) evaldepth = END_EVAL_DEPTH;


#if 1 

/*  large hashtable check, neccessary since otherwise deep pre-searches
 *  are needless
 */

      { uint4 l_ha;
        HashEntry *l_ph;

        l_ha = pbr->Hash1 & pzio->hash.HashMask;
        if (colour == WHITE) l_ha = HASH_WHITE(l_ha);
 
        l_ph = &pzio->hash.HashTab[l_ha];

        if (HashEntry::locks_equal(l_ph->get_lock(), pbr->Hash2) && 
            int(l_ph->get_height()) >= evaldepth &&
	    int(l_ph->get_height()) != HashEntry::END_HEIGHT &&
            ZUG(l_ph->get_best_move()) && pbr->umg[l_ph->get_best_move()] >= UMG &&
            (l_ph->get_value_type() == HashEntry::VALUE_EXACT  ||
           (l_ph->get_value_type() == HashEntry::VALUE_LE_MINIMAX && l_ph->get_value() >= be) ||
           (l_ph->get_value_type() == HashEntry::VALUE_WERT_GE_MINIMAX && l_ph->get_value() <= al))) {

/* no new evaluation! */

#if 0
printf("*"); fflush(stdout);
#endif
          value = l_ph->get_value();

        } else {

          value = AlphaBeta1(pzio, evaldepth, colour, al, be, LastMove);
	}

      }
#else

      value = AlphaBeta1(pzio, evaldepth, colour, al, be, LastMove);

#endif

    }

/* don't overwrite outcome entries */

    if (HashTab2[ha0].Type != OUTCOME) {

      if      (value >= be) HashTab2[ha0].Type  = HEURISTIC_GREATER;
      else if (value <= al) HashTab2[ha0].Type  = HEURISTIC_LESS;
      else                  HashTab2[ha0].Type  = HEURISTIC_EXACT; 
      HashTab2[ha0].Hash2 = pbr->Hash2;
      HashTab2[ha0].Value = value;
    }

    return value;
  }

  ZM_LOESCHEN;
  MoveNum  = 0;
  BestMove = ZUG_UNBEKANNT; 
  lo_value = WERTMIN; 
  hi_value = be;
  max      = al;


#if !HASH 
#error no HASH?
#endif


  ha = pbr->Hash1 & pzio->hash.HashMask;
  if (colour == WHITE) ha = HASH_WHITE(ha);

#if TEST
  if (ha >= pzio->hash.HashMask+1) { printf("%lu", ha); Error("hash"); }
#endif

  ph = &pzio->hash.HashTab[ha];

  CurMove = ph->BesterZug;

#if TEST
  if (CurMove < -2 || CurMove >= 89) {
    printf("%x %d\n", ha, CurMove); Error("hashinhalt1");
  }
#endif
  
/*printf("HashZug "); KoorAus(CurMove); printf("\n");*/


  if (ZUG(CurMove) && LOCK_EQUAL(ph->Hash2, pbr->Hash2)) { 

    HashMove = CurMove;
    TRY 

  } else HashMove = -1;




/* sort moves */

  {
    ZUGDAT ZugDat[65];
    int    Anz;

    Anz = ZuegeSortieren(pzio, colour, SortDepth[0], HashMove, ZugDat);

    FOR (i, Anz) {

      CurMove = ZugDat[i].Zug;

      TRY
    }
  }


  if (MoveNum == 0) {				/* no move? */

    if (LastMove == ZUG_PASSEN) {		/* no remaining move */

      int val = BewDiff(pbr, colour);

      if (val == 0) val = DrawVal;

      return val;

    } else {					/* deeper */

      int val;


      val = -IterAlphaBeta(depth+1, GEGNER(colour), -be, -al, ZUG_PASSEN);

      strcpy((char*)&MoveLists[depth][depth+1], (char*)&MoveLists[depth+1][depth+1]);
      MoveLists[depth][depth] = ZUG_PASSEN;

      return val;
    }
  }


Ende:

#if TEST
  if (!ZUG(BestMove)) Error("BestMove?");
#endif

  return lo_value;
}



int EndIteration(
  ZUGIO *p,		/* pointer to global variables */
  PARTEI colour,	/* player to move              */
  int , int be,	        /* alpha-beta window           */
  int                   /* LastMove */
)
{
  int    outcome, maxseen=false;
  SPFELD sf;
  BRETT  board=p->Brett;
  bool   forced=false;
  int    forced_be=0;

  endbe = be;


  max_ = -(WERTGEWINN+65);
  upnum = 0;

  InitIterAlphaBeta(p);


  BrettSf(pbr, &sf);


  FOREVER {

    endal = endbe - 1;
    pzio->al = endal;
    pzio->be = endbe;

    if (endal == 0) DrawVal = -WERTGEWINN; else DrawVal = +WERTGEWINN;

    pzio->Sf = sf;

    SfBrett(&pzio->Sf, &pzio->Brett);

    pzio->DiscNum0 = SfAnz(&pzio->Sf);

#if KILLER
  KillerAdjust(&pzio->killer, &pzio->Sf);
#endif

    value = IterAlphaBeta(0, colour, -WERTGEWINN, +WERTGEWINN, ZUG_UNBEKANNT);

    if (value >= max_) {      /* >= because winning move must be chosen */

      maxseen = true;
      max_   = value;
      maxbe = pzio->be;

      pzio->PathLen   = 1;
      pzio->PathValue = max_;
      pzio->PathAl    = endal;
      pzio->PathBe    = endbe;
      pzio->PathType  = MIDGAME;

      pzio->Path[0] = MoveLists[0][0];
      pzio->Path[1] = 0;

    }

    StatAus(&pzio->cio, pzio);

    if (abs(value) >= WERTGEWINN) {

      if ((endbe >  0 && value >= +WERTGEWINN) ||               /* win  */
	  (endbe == 0 && value <= -WERTGEWINN) ||               /* loss */
          (forced && forced_be == 0 && value >= +WERTGEWINN) || /* draw */
          (forced && forced_be == 1 && value <= -WERTGEWINN)) { /* draw */

/* restore board and return if true value is known */

        if ((forced && forced_be == 0 && value >= +WERTGEWINN) || /* draw */
            (forced && forced_be == 1 && value <= -WERTGEWINN))   /* draw */
          value = 0;


        { int val = value;

        if (value >= +WERTGEWINN) value = +1;
        if (value <= -WERTGEWINN) value = -1;

        p->Brett = board;
        pzio->cio.IterPathValue = pzio->PathValue = val;
        pzio->cio.IterPathType  = pzio->PathType  = ENDGAME;
 
        }

        pzio->Wert = value;
        return value;

      }

/* sure >= draw? */
      if (value >= +WERTGEWINN) { forced = true; forced_be = 1; }

/* sure <= draw? */
      if (value <= -WERTGEWINN) { forced = true; forced_be = 0; }

printf("forced %d\n", forced_be);    
    }


    if (forced) endbe = forced_be;
    else {
      if      (endbe == 0 && EXPWERT(WERT_TO_REAL(value)) >= P_WIN ) endbe = 1;
      else if (endbe == 1 && EXPWERT(WERT_TO_REAL(value)) <  P_LOSS) endbe = 0;
    }
/*
printf("v=%f be=%d\n", EXPWERT(WERT_TO_REAL(value)), endbe);
*/
    outcome = UpdateWLD(&sf, colour, value);
    upnum++;
  }
}


// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Zug ermitteln  */


#include "main.h"
#include "move.h"
#include "mid.h"
#include "sel.h"
#include "end.h"
#include "board.h"
#include "eval.h"
#include "killer.h"
#include "hash.h"

#include "goodies.h"
#include "crtg.h"
#include "crt.h"

#include "newend.h"

#define ANZ_MAX   24
#define ANZ_MIN_T 4



/* alles initialisieren */

void InitZug(
  ZUGIO *pzio, BEWFKT BewMitte, CHECKFKT *Check, 
  int HashBits, int HashMaxDepth, int HashBits0, int HashMaxDepth0
)
{
  if (Check == NULL) Error("Check-Fkt undef.");

  pzio->Check         = Check;
  pzio->cio.mBewMitte = pzio->BewMitte = BewMitte;
  pzio->ZugVorgabe    = false;
  pzio->Selective     = false;
  pzio->Percentile1 = pzio->Percentile2 = 1.0;
  pzio->Quiescence    = false;
  pzio->EndCutNiveau  = 1.5;
  pzio->Cut           = false;
  pzio->EvalCut       = NULL;

  pzio->Ausgabe	= pzio->cio.Ausgabe = false;

  pzio->cio.Selective  = false;
  pzio->cio.Percentile1 = pzio->cio.Percentile2 = 1.0;
  pzio->cio.Quiescence = false;
  pzio->cio.EndCutNiveau = 1.5;
  pzio->cio.NoAutomaticEndgame = false;
  pzio->cio.EarlyEndN = 0;
  pzio->cio.IterOutcome = false;
  pzio->cio.OnlyKomi = false;
  pzio->cio.t4t = false;
  pzio->cio.p2  = false;  
  pzio->cio.t4t_score = 0.0;
  pzio->cio.t4t_updated = false;
  pzio->cio.t4t_msg[0] = 0;
 
  pzio->cio.ForcedDepth = 0;
  pzio->cio.ForcedTime  = 0;
  pzio->cio.ForcedEndDiscNum = 0;
  pzio->cio.TimeFrac = 1.0;

  pzio->cio.StopM = pzio->cio.StopN = 0;

  pzio->cio.game_komi = 0.0;
  pzio->cio.game_synchro = 0;
  
  pzio->cio.UseTestGameKomi = false;
  pzio->cio.test_game_komi = 0.0;

  pzio->cio.UseTestGameRand = false;
  pzio->cio.test_game_rand  = 0;

  pzio->cio.game_rand = 0;
  
#if HASH
  pzio->hash0.init(HashBits0, HashMaxDepth0);
  pzio->hash.init(HashBits, HashMaxDepth);

  //pzio->endhash0.init(HashBits0, HashMaxDepth0);
  //pzio->endhash.init(HashBits, HashMaxDepth);
  
  //pzio->hash1.init(19, 8);
  //pzio->hash2.init(19, 10);
#endif

#if KILLER
  InitKiller(&pzio->killer);
#endif

  InitBewert();
  InitSetzen();
}


#if 0

void FreeZug(ZUGIO *pzio)
{
  FreeHash(&pzio->hash);
  FreeKiller(&pzio->killer);
} 

#endif


// return interval around game_komi value (0-window if komi != 2k)

void komi_ab(int to_move, float game_komi, int &al, int &be)
{
  if (to_move == WHITE) game_komi = -game_komi;

  int k = (int)floor(game_komi * 0.5) * 2;

  al = k+1;
  be = k+2;

  if (game_komi == k) { al -= 2; be--; }
}


void Zugermittlung(ZUGIO *pzio)
{
  int    ZugAnz, LetzterZug;
  SFPOS	 Zuege[64];


  if (!pzio->BewMitte) Error("BewMitte==NULL\n");
 
  if (pzio->Modus == MODUS_NORMAL && 
	(pzio->MaxTiefe < 0 || pzio->MaxTiefe >= HashEntry::MAX_HEIGHT))
    Error("Maxtiefe");

  if (pzio->Partei != BLACK && pzio->Partei != WHITE) Error("Partei");

  SfBrett(&pzio->Sf, &pzio->Brett);

  pzio->DiscNum0 = SfAnz(&pzio->Sf);

#if KILLER
  KillerAdjust(&pzio->killer, &pzio->Sf);
#endif

#if HASH
  pzio->hash.next_stamp();
  pzio->hash0.next_stamp();  // new
  pzio->endhash.next_stamp();  // new
  pzio->endhash0.next_stamp();  // new
#endif
  

  ZugAnz = SfMoeglZuegeE(&pzio->Sf, pzio->Partei, Zuege);


#ifdef xxx
SfAus(&pzio->Sf, pzio->Partei, 0xffff);
printf("%d\n", pzio->Partei);
FOR (i, ZugAnz) { KoorAus(Zuege[i]); printf(" "); }
printf("\n");
#endif

/* wichtig: LetzterZug darf tatsächlich nicht mehr möglich sein! (Killer) */

if (ZUG(pzio->LetzterZug) && pzio->Sf.p[pzio->LetzterZug] == LEER)
  printf(">>>>>>>> Letzte Position (%d) leer!\n", pzio->LetzterZug);

  LetzterZug = pzio->LetzterZug;

/* nicht wg. 2xPassen aufhören*/

  if (LetzterZug == ZUG_PASSEN) LetzterZug = ZUG_UNBEKANNT;
 

  pzio->PathLen		= 0;
  pzio->PathValue	= 0;
  pzio->MovesTotal	= ZugAnz;
  pzio->fertig   	= false;
  pzio->cio.timeout	= false;

  pzio->Wert	 = 0;
  pzio->BestZug  = ZUG_UNBEKANNT;		/* bisher kein Zug gefunden */
  pzio->ZugAnz   = ZugAnz;

/*
if (pzio->ZugVorgabe) {
  printf("ZUGVORGABE %d von %d: ", pzio->VorgabeAnz, pzio->ZugAnz);
  FOR (i, pzio->VorgabeAnz) { KoorAus(pzio->Zuege[i]); printf(" "); }
  printf("\n");
}
*/

  switch (ZugAnz) {

/* immer rechnen! */
/*
    case 0:

      return;

    case 1:

      pzio->PathLen = 1;
      pzio->Path[0] = pzio->BestZug = Zuege[0];
      pzio->PathValue = 0;

      return;
*/

    default:

      pzio->fertig = true;


      if (SETJMP(pzio->cio.timeout_env)) {

#if TEST
printf("LONGJMP wg. SIG_TIMEOUT\n");
#endif

        goto Ende;
      }


      pzio->cio.count = CHECK_COUNT;

      Zeit(&pzio->cio.LastCheck);

      switch (pzio->Modus) {
 
        case MODUS_NORMAL: 

	  pzio->SearchMode = SM_MIDGAME;

	  pzio->al  = -(WERTGEWINN+64);
	  pzio->be  =   WERTGEWINN+64;

          pzio->Wert = AlphaBeta(pzio, pzio->MaxTiefe, pzio->Partei, 
			  pzio->al, pzio->be, LetzterZug);
	  break;


        case MODUS_ASPIRATION: 

	  pzio->SearchMode = SM_MIDGAME;

          pzio->Wert = AlphaBeta(pzio, pzio->MaxTiefe, pzio->Partei, 
			  pzio->al, pzio->be, LetzterZug);
	  break;


        case MODUS_ITER: 

	  Error("iter endgame not supported anymore");

#if 0
	  pzio->SearchMode = SM_ITERATION;

          pzio->Wert = EndIteration(pzio, pzio->Partei, 
				    pzio->al, pzio->be, LetzterZug);
#endif
	  break;


        case MODUS_GEWINN:

	  pzio->SearchMode = SM_ENDGAME;
	  pzio->Quiescence = false;

	  komi_ab(pzio->cio.Partei, pzio->cio.game_komi, pzio->al, pzio->be);

          pzio->Wert = EndAlphaBeta(pzio, pzio->Partei,
			pzio->al, pzio->be, LetzterZug);
	  break;


        case MODUS_DIFFERENZ:

	  pzio->SearchMode = SM_ENDGAME;

	  pzio->Selective  = false;
	  pzio->Quiescence = false;

	  pzio->al = -64;
	  pzio->be =  64;
	  pzio->Wert = EndAlphaBeta(pzio, pzio->Partei,
			pzio->al, pzio->be, LetzterZug);
	  break;


        case MODUS_NEGAC:

	  pzio->SearchMode = SM_ENDGAME;

          pzio->Wert = EndAlphaBeta(pzio, pzio->Partei, 
			pzio->al, pzio->be, LetzterZug);
	  break;

        default: Error("Unbekannter Modus");
	
      }

Ende:

    if (pzio->PathLen > 0) pzio->BestZug = pzio->Path[0]; 
    else pzio->BestZug = ZUG_UNBEKANNT;

  }
}




/* für Quicksort: maximaler Wert., noch nicht da */

int compZUGDAT(const void *a, const void *b)
{
  return ((ZUGDAT*) b)->Wert - ((ZUGDAT*) a)->Wert;
}



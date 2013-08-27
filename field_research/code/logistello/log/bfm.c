// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* BPIP-like search / 9.95 */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "goodies.h"
#include "eval.h"
#include "featurem.h"
#include "fpatt.h"
/*#include "time.h"*/
#include "mid.h"
#include "filecom.h"
#include "distr.h"

#define DEPTH 1

#define INC_DEPTH     false 

#define BEST_NUM      false
#define EXPAND_NUM    5
#define EXPAND_FACTOR 0.06

#define BREAK_DIFF    1.3
#define COMPRESS_PEAK_NUMBER 30

#define NODE_MAX 10000

#define EVALMID EvalASlow

#define VARIANCE (0.5)

#define SUCC_MAX 40

typedef struct _NODE {

  int          to_move, exact;
  int          move_num, succ_num, exp_childs_num, wait_num;
  sint1        moves[SUCC_MAX];
  struct _NODE *pred, *succ[SUCC_MAX];
  DISTR        distr;
  float        value, score, P_max[SUCC_MAX];

} NODE;


typedef struct {

  float pmax, mean, var;
  int   move;
  NODE  *pl;

} SDIS;


typedef struct {

  NODE *pbest;
  int  value;

} AB_RES;


typedef struct {

  int move_num;
  int moves[64];
  int to_move[64];

} PATH;


extern int num_malloc, num_free;

uint4 ab_leaf_num;

char TO_ME[DATEINAME_MAX], FROM_ME[DATEINAME_MAX];


void _abort(void) { exit(1); }

void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}



#define PMAX_EPS (1e-5)

int comp_SDIS(const void *a, const void *b)
{
  const SDIS *pa=a, *pb=b;
  float r;

  r = pb->pmax - pa->pmax;

  if (r >= +PMAX_EPS) return +1;
  if (r <= -PMAX_EPS) return -1;

  r = pb->mean - pa->mean;

  if (r > 0) return +1;
  if (r < 0) return -1;

  return 0;
}



NODE *NewNode(void)
{
  NODE *pn;

  pn = (NODE*) calloc(sizeof(NODE), 1);

  if (!pn) Error("NewNode: no mem");

  return pn;
}


void FreeTree(NODE *pt)
{
  int i;

  FOR (i, pt->succ_num) FreeTree(pt->succ[i]);

  FreeDistr(&pt->distr);
  free(pt);
}



AB_RES ab(NODE *pn, int alpha, int beta)
{
  int  i, max;
  AB_RES local_res, res;

  if (!pn->succ_num) {

    res.pbest = pn;
    res.value = pn->value;

    return res;
  }

  max = alpha;
  res.value = WERTMIN;

  res.pbest = NULL;

  FOR (i, pn->succ_num) {

    local_res = ab(pn->succ[i], -beta, -max);

    if (-local_res.value > res.value) {

      res.value = -local_res.value;
      res.pbest = local_res.pbest;

      if (res.value > max) max = res.value;
    }
  }

  return res;
}


PATH MirrorBestPath(NODE *pn)
{
  int i;
  PATH path;
  NODE *ppred;


  path.move_num=0;


/* follow path to root and determine mirrored movelist */

  while ((ppred=pn->pred)) {

    FOR (i, ppred->succ_num) if (ppred->succ[i] == pn) break;

    if (i >= ppred->succ_num) Error("BestPath: no path to root");

    path.moves[path.move_num]   = ppred->moves[i];
    path.to_move[path.move_num] = ppred->to_move;

    path.move_num++;

    pn = ppred;
  }

  return path;
}


/* return pointer to last node != root */

NODE *UpdateDistr(NODE *pn)
{
  int  i;
  NODE *plast=NULL;
  DISTR distr[MAX_N];

 
  while (pn) {

    FOR (i, pn->succ_num) distr[i] = pn->succ[i]->distr;

    FreeDistr(&pn->distr);

#if 0
FOR (i, pn->succ_num) printf("%d ", distr[i].n);
printf("\n");
#endif

    pn->distr = PNegaMaxDistr(distr, pn->succ_num, pn->P_max);

    CompressDistr(&pn->distr, COMPRESS_PEAK_NUMBER);

    if (pn->pred) plast = pn;

    pn = pn->pred;

  }


  return plast;
}






/* *pzio must be set */

NODE *CreateNextChild(ZUGIO *pzio, NODE *pn, SFPOS *moves)
{
  int   i, res, exact, BestMove, depth;
  SFPOS *p;
  uint1 m[100];
  SFPOS moves2[64];
  SPFELD sboard;
  float  value;
  NODE   *pnew;


  if (pn->succ_num && pn->move_num == pn->succ_num) return NULL;

  depth = DEPTH;

#if INC_DEPTH
  if (SfAnz(&pzio->Sf) < 18 || SfAnz(&pzio->Sf) >= 36) depth++;
#endif

  pzio->cio.BewAnz = 0;

  if (!*moves) {

/* pass */


    if (!SfMoeglZuege(&pzio->Sf, GEGNER(pn->to_move), moves2)) {

/* game end */
      
      res = SfAnzBLACK(&sboard) - SfAnzWHITE(&sboard);

      if      (res > 0) res += WERTGEWINN;
      else if (res < 0) res -= WERTGEWINN;
      else              res =  0;

      if (pn->to_move == WHITE) res = -res;

      value = WERT_TO_REAL(res);
      exact = true;
      BestMove = ZUG_PASSEN;
    
    } else {

      res = AlphaBeta1(
              pzio, depth, pn->to_move, 
             -WERTGEWINN, +WERTGEWINN, ZUG_UNBEKANNT
            );

      value = WERT_TO_REAL(res);
      exact = abs(res) >= WERTGEWINN;
      BestMove = ZUG_PASSEN;

    }


  } else {

/* find remaining moves */

    for (p=moves; *p; p++) m[*p] = 1;

    FOR (i, pn->succ_num) m[pn->moves[i]] = 0;

    pzio->VorgabeAnz = 0;

    for (p=moves; *p; p++) 
      if (m[*p]) pzio->Zuege[pzio->VorgabeAnz++] = *p;

#if 0
FOR (i, pzio->VorgabeAnz) KoorAus(pzio->Zuege[i]);
printf("\n");
#endif

    pzio->LetzterZug = ZUG_UNBEKANNT;
    pzio->SearchMode = SM_MIDGAME;
    pzio->Quiescence = true;
    pzio->Selective  = true;
    pzio->Percentile = 1.5;

    pzio->fertig     = false;
    pzio->cio.timeout= false;

    pzio->ZugVorgabe = true;

    res = AlphaBeta(
            pzio, depth, pn->to_move, 
            -WERTGEWINN, +WERTGEWINN, ZUG_UNBEKANNT
          );

    value = WERT_TO_REAL(res);
    exact = abs(res) >= WERTGEWINN;
    BestMove = pzio->Path[0];

#if 0
printf(":"); KoorAus(BestMove);
#endif

  }

  ab_leaf_num += pzio->cio.BewAnz;

  value = -value;

/* create new node */

  pnew = NewNode();
  pnew->pred = pn;
  pnew->to_move = GEGNER(pn->to_move);
  pnew->value = value;
  pnew->exact = true;

  if (exact) pnew->distr = OnePeak(value);
  else       pnew->distr = Triangle(7, value, 0.5, 0.1);


  pn->succ[pn->succ_num]  = pnew;
  pn->moves[pn->succ_num] = BestMove;
  pn->succ_num++;

  return pnew;
}


static SPFELD sboard0;

void BoardOfNode(NODE *pn, SPFELD *psf)
{
  int  i;
  PATH path=MirrorBestPath(pn);
  
  *psf = sboard0;  

  for (i=path.move_num-1; i >= 0; i--) {

    if (path.moves[i] != ZUG_PASSEN && 
        !SfSetzen(psf, path.to_move[i], path.moves[i])) 
      Error("EvalNode: no move");

  }
}
 

void ExpandNode(ZUGIO *pzio, NODE *pn)
{
  int i, move_num, res;
  sint1 moves[64], moves2[64];
  PATH path;
  SPFELD sboard, sboard1;
  NODE *pnew, *pl;
  float value, value_max=-1e20;

  if (pn->move_num) Error("ExpandNode: Node already expanded");

/* determine position to expand */

  BoardOfNode(pn, &sboard);
 
  if (DEPTH >= 3) KillerAdjust(&pzio->killer, &sboard);

  move_num = SfMoeglZuege(&sboard, pn->to_move, moves);

  moves[move_num] = 0;
 
  if (!move_num) move_num = 1;  

  pn->move_num = move_num;

  pzio->Sf = sboard;  
  SfBrett(&sboard, &pzio->Brett);


  FOR (i, move_num) {

/* possible improvement: use last value as bound */

    pl = CreateNextChild(pzio, pn, moves);

    if (!pl) Error("no child?");

    value = -pl->value;

#if 0
printf("-> %f\n", value);
#endif

/* partial expansion */

    if (value > value_max) value_max = value;
    if (value_max - value >= BREAK_DIFF) break;

  }


  if (pn->pred) {

    pn->pred->exp_childs_num++;

    if (pn->pred->exp_childs_num == pn->pred->succ_num &&
        pn->pred->succ_num < pn->pred->move_num) {

/* last child expanded => create next */

printf("c"); fflush(stdout);
 
      BoardOfNode(pn->pred, &sboard);
 
      if (DEPTH >= 3) KillerAdjust(&pzio->killer, &sboard);

      move_num = SfMoeglZuege(&sboard, pn->pred->to_move, moves);
      moves[move_num] = 0;

      pzio->Sf = sboard;  
      SfBrett(&sboard, &pzio->Brett);

      CreateNextChild(pzio, pn->pred, moves);

    }
  }

  UpdateDistr(pn);
}


static NODE *Nodes[NODE_MAX];
static NODE **pN;
static int  path_sum, path_len;



void CollectLeaves(NODE *pn)
{
  int i;

  if (pn->succ_num == 0) { 

    path_sum += path_len;
    *pN++ = pn; 
    if (pN-Nodes >= NODE_MAX) Error("too many nodes");

  } else {

    path_len++;
    FOR (i, pn->succ_num) CollectLeaves(pn->succ[i]);
    path_len--;
  }
}



/*  Determine prob. for each move to be the best
 *  sorted result in sdis 
 */

void ProbBest(NODE *proot, SDIS *sdis)
{
  int j;
  DISTR gdistr[64];


  FOR (j, proot->succ_num) { 

    CopyDistr(&gdistr[j], &proot->succ[j]->distr);
    NegateDistr(&gdistr[j], 0.0);
    StripDistr(&gdistr[j]);

  }

  ProbMax(gdistr, proot->succ_num);

  FOR (j, proot->succ_num) { 

    sdis[j].pmax = gdistr[j].pmax; 
    sdis[j].move = proot->moves[j]; 
    MeanVar(&gdistr[j], &sdis[j].mean, &sdis[j].var);
    FreeDistr(&gdistr[j]);
  }
}


#define DEPTH_CONST 1.1


float Score(NODE *pn, float P_max[64][64])
{
  int i, j;
  float score_sum = 0, score, c = 1.0, c_sum=0;

  i = 1;

  while (pn) {

    score = 0;

#if 0
printf("%d: \n", i);
#endif

    FOR (j, pn->succ_num) {
      score += fabs(pn->P_max[j] - P_max[i][j]);
#if 0
KoorAus(pn->moves[j]);
printf(" %.4f %.4f %.4f /", pn->P_max[j], P_max[i][j], fabs(pn->P_max[j] - P_max[i][j]));
#endif

    }

    score /= pn->succ_num;

    score_sum += score * c; c_sum += c;

#if 0
printf(" -> %f %f\n", score, score_sum);
#endif

    c *= DEPTH_CONST;

    pn = pn->pred; i++;

  }

  return score_sum / c_sum;
}


int ChooseLeaf(NODE *proot, SDIS *res)
{
  int i, j, k, num, move_num, scored_num=0;
  DISTR sdistr, distr_path[64];
  float h, hs, max = -1, mean, var, means[100], prev, P_max[64][64];
  NODE  *pl, *pn, *plast=NULL, *pmaxl=NULL;
  SDIS sdis[64];

  pN = Nodes; CollectLeaves(proot);

  num = pN - Nodes;

  move_num = proot->succ_num;


/* determine means for tie-break */

  FOR (j, move_num) { 

    MeanVar(&proot->succ[j]->distr, &mean, &var);

    means[proot->moves[j]] = -mean;

  }


/* find leaf to expand */

  FOR (i, num) {

    float score, score0, score1;

    pl = Nodes[i];



    if (pl->wait_num > 0) {

      pl->wait_num--;
      score = pl->score;

    } else {

/* save distr and P_max values of path to root */

      scored_num++;

      j = 0; pn = pl;

      while (pn) {

        CopyDistr(&distr_path[j], &pn->distr);
        memcpy(&P_max[j][0], pn->P_max, pn->succ_num*sizeof(float));

#if 0
        { int k; FOR (k, pn->succ_num) printf("%.4f ", P_max[i][k]);
          printf("\n"); }
#endif

        pn = pn->pred; j++;

      }

/* win score */

      pl->distr = OnePeak(+100.0);
      UpdateDistr(pl->pred);

      score1 = Score(pl->pred, P_max);


/* loss score */

      FreeDistr(&pl->distr);
      pl->distr = OnePeak(-100.0);
      UpdateDistr(pl->pred);

      score0 = Score(pl->pred, P_max);


/* restore distr and P_max values */

      j = 0;

      pn = pl;

      while (pn) {

        FreeDistr(&pn->distr);
        pn->distr = distr_path[j]; 
        memcpy(pn->P_max, &P_max[j][0], pn->succ_num*sizeof(float));
        pn = pn->pred; j++;

      }
    
      score = EXPWERT(pl->value) * score1 + (1.0-EXPWERT(pl->value)) * score0;

      pl->score = score;
    }

#if 0

printf("sort result:\n");

    FOR (j, move_num) {

      KoorAus(sdis[j].move); 
      printf(" %5.1f   mean=%f sdev=%f\n", sdis[j].pmax * 100, sdis[j].mean, sqrt(sdis[j].var));

    }

#endif


    { int j;
      float su;
      PATH path = MirrorBestPath(pl);

#if 0
FOR (j, path.move_num) KoorAus(path.moves[path.move_num-1-j]);
printf(" : ");
#endif

      res[i].pmax = score;
      res[i].move = path.moves[path.move_num-1];
      res[i].mean = means[res[i].move];
      res[i].pl   = Nodes[i];
    } 

#if 0
printf("score0=%f score1=%f val=%f score=%f\n", score0, score1, pl->value, score);
#endif

  }

  qsort(res, num, sizeof(res[0]), comp_SDIS);

  printf("%d leaves, %d scored ", num, scored_num);
  

#if 0

printf("\n");

FOR (i, min(4,num)) {

  int j;

  printf("%d: score=%f mean=%f ", i, res[i].pmax, res[i].mean);

  {
    PATH path = MirrorBestPath(res[i].pl);

    FOR (j, path.move_num) KoorAus(path.moves[path.move_num-1-j]);
    printf("\n");
  }
}
#else

FOR (i, min(4,num)) printf("%.4f ", res[i].pmax);
printf("\n");

#endif

  return num;

}



int main(int argc, char **argv)
{
  int     i, j, k, num, iter, da, ok, last_move, n, dummy, rem_time, move_num, col, move, expand_num;
  char    *p, msg[NACHRICHT_MAX];
  sint1   moves[64];
  clock_t Start, Ende;
  int    seconds;
  ZUGIO  zio;
  PATH   path;
  SPFELD Sf;
  NODE   *proot, *pl;
  GAME   Game, InputGame;
  SDIS   sdis[64], res[NODE_MAX];
  uint4  search_time;


#if 0 

  DISTR d[2], e;


  d[0] = NormalDistr(0.0, 1.0, 0.8, 1);
  d[1] = NormalDistr(0.8, 1.0, 0.8, 1);

  PrintDistr("d0", &d[0]); PrintDistr("d1", &d[1]);

  e = MaxDistr(d, 2);

  PrintDistr("e", &e);

  StripDistr(&e);

  PrintDistr("se", &e);


  exit(0);


#endif

  InitCrt();

  if (argc != 3) Error("obf channel seconds\n");

  if (sscanf(argv[2], "%d", &seconds) != 1) Error("time?");

  strcpy(ParameterFile, "evala.par");
  strcpy(TableFile,     "oplay.tab");

  InitZug(&zio, EVALMID, Check, HASHBITS, HASHMAXDEPTH);

  zio.cio.BewAnz = 0;
  
  sprintf(TO_ME,   TO_PRAEFIX"%s",   argv[1]);
  sprintf(FROM_ME, FROM_PRAEFIX"%s", argv[1]);

  Empf(TO_ME, msg);		/* cancel old msg */



  FOREVER {

/* wait for board */

    do {

    if ((da=Empf(TO_ME, msg))) {

      if (!ParseNumber(msg, &n, &p) || n < 0 || n >= SIGNAL_ANZ) { 
        printf("'%s' ", msg); 
	Error("unknown signal");
      }

      if (n == SIG_BOARD) {

        ok = ParseNumber(p, &col, &p) && (col == BLACK || col == WHITE);

	ok &= ParseNumber(p, &rem_time, &p) && rem_time > 0;

	ok &= ParseNumber(p, &last_move, &p) && 
		      (last_move == ZUG_UNBEKANNT || 
		       last_move == ZUG_PASSEN || 
		       ZUG(last_move));

	Sf = SfNull;

	FOR_SFPOS10 (i) {

	  int j;

	  ok &= ParseNumber(p, &j, &p) && 
	        (j == LEER || j == BLACK || j == WHITE);
	  Sf.p[i] = j;
	}
 

	if (!ok) {
	  printf("'%s' ", msg); 
	  Error("SIG_BOARD message corrupt");
	}


       } else if (n == SIG_GAME) {

        ok = ParseNumber(p, &col, &p) && 
	     (col == BLACK || col == WHITE);

	ok &= ParseNumber(p, &rem_time, &p) && rem_time > 0;
        ok &= ParseNumber(p, &dummy, &p);    /* my move */

	sReadGame(p, &InputGame);

	last_move = ZUG_UNBEKANNT;

	Game = InputGame;
	PlayGame(Game.MoveNum, &Game, &Sf);

      }

    } else SLEEP(2);

    } while (!da || (n != SIG_GAME && n != SIG_BOARD));


/* board received */

    sboard0 = Sf;
    ab_leaf_num = 0;

    SfAus(&Sf, 0, 0);
    printf("to move: %d\n", col);

    move_num = SfMoeglZuegeE(&Sf, col, moves);

    proot = NULL;

    if      (move_num == 0) move = ZUG_PASSEN;
    else if (move_num == 1) move = moves[0];
    else {

/* more than 1 move */

    proot = NewNode();
    proot->pred = NULL;
    proot->to_move = col;

    ExpandNode(&zio, proot);

    search_time = 0;

    for (iter=1;; iter++) {

/* output */

      ProbBest(proot, sdis);
      qsort(sdis, proot->succ_num, sizeof(sdis[0]), comp_SDIS);

printf("\niter %d\n", iter);

printf("move:   ");
      FOR (j, proot->succ_num) { KoorAus(sdis[j].move); printf("    "); }
printf("\npmax: ");
      FOR (j, proot->succ_num) { printf("%5.1f ", sdis[j].pmax * 100); }
printf("\neval: ");
      FOR (j, proot->succ_num) { 
        printf("%5.1f ", EXPWERT(sdis[j].mean)*100.0);
      }
printf("\nsdev:  ");
      FOR (j, proot->succ_num) { 
        printf("%4.2f  ", sqrt(sdis[j].var));
      }
printf("\n");


      move = sdis[0].move;

#if 0

printf(">>>>>>>>");
FOR (i, proot->succ_num) { 
  KoorAus(proot->moves[i]); printf(" %.1f\n", proot->P_max[i]*100);
}
printf("\n"); 
#endif




      if ((double)search_time/CLOCKS_PER_SEC > seconds) break;

printf("Choose\n");

      Start = clock();
      n = ChooseLeaf(proot, res);
      search_time += clock() - Start;

      if (BEST_NUM) expand_num = min(n,EXPAND_NUM);
      else          expand_num = max(min(5,n),round(EXPAND_FACTOR * n));

printf("Expand: ");

      FOR (j, expand_num) {

        path = MirrorBestPath(res[j].pl);

#if 0
FOR (i, path.move_num) KoorAus(path.moves[path.move_num-1-i]);
printf("\n");
#else
printf("*"); fflush(stdout);
#endif

        Start = clock();

        ExpandNode(&zio, res[j].pl);

        search_time += clock() - Start;
      }

printf("\n");

      if (n >= 20) {

        for (j=expand_num; j < n; j++) {

          if (res[j].pl->wait_num == 0) {

            if      (res[j].pmax < 0.125 * res[0].pmax) 
              res[j].pl->wait_num = 3;
            else if (res[j].pmax < 0.25  * res[0].pmax) 
              res[j].pl->wait_num = 2;
            else if (res[j].pmax < 0.5 * res[0].pmax) 
              res[j].pl->wait_num = 1;

          }
        }
      }
    }
  }

/* send move */

    SendMOVE(FROM_ME, move, round((double)(clock()-Start)/CLOCKS_PER_SEC));


    if (proot) {

      pN = Nodes; path_sum=0; path_len=0; CollectLeaves(proot);
      printf("%d leaves, avg. pathlen %f\n", 
             pN-Nodes, (double)path_sum/(pN-Nodes));

      FreeTree(proot);
    }

printf("%u ab_leaves, malloc=%d free=%d\n", ab_leaf_num, num_malloc, num_free);

  }

  return 0;
}

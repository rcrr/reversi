// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* check library: evaluate every vertex in library tree and save 
 * all vertices (as a not completed game), in which the best move
 * was not made, to correct it using ocor,  1,3.94
 *
 */
 

#ifndef PRECOMP
#include "main.h"
#endif

#include "lib.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "killer.h"
#include "eval.h"
#include "hash.h"
#include "fpatt.h"


#define OUT		false

#define FIRSTNUM	0


#define EVALFUNC	EvalA
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"

#define SELECTIVE	true
#define PERCENTILE	1.5
#define QUIESCENCE	true


#define MIN_DISCS	12
#define LOOSER_CHECK	0.44	/* Prob. for checking deeper 		*/
#define LOOSER_GOOD	0.52	/* Prob. greater or equal => save game 	*/
#define LOOSER_INC	2	/* depth increment for deeper check	*/

#define DELTA	  	0.04  /* range of good moves 		        (-res)*/
#define DELTA_BLUNDER	0.06  /* accepted diff. for "not optimal" moves (-blu)*/

#define P_GOOD	 	1.00  /* return if position is extreme */

#define P_DRAW		0.46  /* value for an even position */


#define DEPTH_INC	3     /* depth increment for searching best move */

ZUGIO zio;

char newfile[200];



void _abort(void) { exit(1); }

void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}	/* Signal-Handler */



/* find value of "best" move in given position */

int Value(SPFELD *pBoard, int Player, int depth, int *pbestmove)
{
  int d;
  static int num=0;


  if (num-- <= 0) { 

    ClearHashTab(&zio.hash); 	/* otherwise problems:                     */
    num = 20; 			/* max. value of re-search can be greater! */
  } 


  zio.Sf         = *pBoard;
  zio.Partei     = Player;
  zio.LetzterZug = ZUG_UNBEKANNT;
  zio.Modus      = MODUS_NORMAL;

  zio.Selective  = SELECTIVE;
  zio.Percentile = PERCENTILE;
  zio.Quiescence = QUIESCENCE;


#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

  for (d=1; d <= depth; d++) {

    zio.MaxTiefe = d; Zugermittlung(&zio);
  }

  *pbestmove = zio.BestZug;

  return zio.Wert;
}




int saved=0, gamenum=0;


void SaveG(GAME *pGame, int move, int player)
{
  FILE *fp;
  SPFELD tab;


  saved++;

  if (ZUG(move)) {

    pGame->Moves[pGame->MoveNum++] = SMOVE_GEN(move, player);
  }


  if (!(fp=fopen(newfile, "a"))) Error("can't append game to file");
  Game2Tab(pGame, &tab);


  fTabAus(fp, &tab);


  fclose(fp);

  if (ZUG(move)) pGame->MoveNum--;

}


/* evaluate vertices recursively */


void CheckVertex(
  LIBRARY  *pT, 
  POSINFO  *pPI,
  GAME     *pGame,
  SPFELD   *psf, 
  int      searchdepth, 
  int      maxnum,
  bool     blunder
)
{
  int    movenum=0, mn, discnum, player, i, j, dummy, value, bestmove;
  int    value0, value1;
  SPFELD sf;
  MOVEDATA MoveDat[65];
  SFPOS  moves[65];
  REAL   P, P0, Pmax, Pmin, seenPmax, p;
  int    depth=searchdepth;


if (!pPI->pNode) printf("!!! pPI->pNode==0\a\a\a\n");

  
  if (!pPI->pNode || (discnum=SfAnz(psf)) > maxnum) { 

    gamenum++;

    if (!((gamenum) % 1)) 
    printf("\n>>> %d games, %d saved\n", gamenum,  saved);

    return; 
  }


  if (discnum >= 30) depth++;

  if (discnum >= 35) depth++;


#if 0

/* no move -> end */

  if (pL->Type == LIBNODE_EXT &&
      pT->Games[pL->GameIndex].MoveNum <= discnum - 4) {

printf("%d %d %d\n", pL->GameIndex, pT->Games[pL->GameIndex].MoveNum, discnum);

fWriteGame(stdout, &pT->Games[pL->GameIndex]);

printf(">>> game end\n");

    return;
  }

#endif


/* evaluate new position: collect made moves in MoveDat */
  
  movenum = 0;

  sf = *psf;

  if (pPI->MoveIndex < 0) {			/* branch */

if (!pPI->pNode->SonNum) Error("no son?");

Error("implementation error");
/* !!!
    FOR (i, pPI->pNode->SonNum) if (pPI->pNode->Sons[i].New) break;
*/



    if (i >= pPI->pNode->SonNum) return;	/* nothing new ... */

    player = 0;

    FOR (i, pPI->pNode->SonNum) {

      MoveDat[movenum++].Move = SMOVE_MOVE(pPI->pNode->Sons[i].Moves[0]);
      player = SMOVE_PLAYER(pPI->pNode->Sons[i].Moves[0]);
    }


  } else {					/* path */

Error("implementation error");
/* !!!

    if (!pPI->pNode->Sons[pPI->MoveIndex].New) return;
*/

    MoveDat[movenum++].Move = 
	SMOVE_MOVE(pPI->pNode->Sons[pPI->MoveIndex].Moves[pPI->PathIndex]);

    player =
      SMOVE_PLAYER(pPI->pNode->Sons[pPI->MoveIndex].Moves[pPI->PathIndex]);
   
  }


#if OUT
{ 
printf("\nPosition:\n");
  SfAus(psf, 0, 0);
  player = PlayGame(pGame->MoveNum, pGame, &sf);
  printf("%d\n", player);

printf("lib moves: [ ");
    FOR (i, movenum) { KoorAus(MoveDat[i].Move); printf(" "); }
printf("]\n");


if (!SfGleich(&sf, psf)) {
fWriteGame(stdout, pGame);
SfAus(&sf, 0, 0);
SfAus(psf, 0, 0);
Error("boards !=");
}
}
#endif



if (gamenum >= FIRSTNUM)

  if (movenum < (mn=SfMoeglZuegeE(psf, player, moves))) {


/* not all moves seen */

    value0 = Value(psf, player, depth-1, &bestmove);

    value1 = Value(psf, player, depth,   &bestmove);

/* average values of two iterations */

    value = (value0 + value1) / 2;

    P0 = EXPWERT(WERT_TO_REAL(value));


#if OUT
printf("search best (d=%d q=%d s=%d): ", depth, zio.Quiescence, zio.Selective);
KoorAus(bestmove); printf(" v0=%.2f v1=%.2f -> P0=%d\n", EXPWERT(WERT_TO_REAL(value0)), EXPWERT(WERT_TO_REAL(value1)), round(100*P0));
#endif



    Pmax = EXPWERT(WERT_TO_REAL(value1));


    if (!blunder) {

      bool new=false;
      REAL delta;


/* find DELTA-best moves and save them if not seen */



/* use DELTA/2 if position is "clear" */

      if (P0 > (1-P_GOOD) && P0 < P_GOOD) delta = DELTA;
      else				  delta = DELTA/2;

#if OUT
printf("delta=%f\n", delta);
#endif


      Pmin = Pmax - delta;


      FOR (i, mn) {

    	FOR (j, movenum) if (moves[i] == MoveDat[j].Move) break;

	if (j >= movenum) {

/* move not seen */

	  sf = *psf;

if (!ZUG(moves[i])) Error("move 1?");

	  if (!SfSetzen(&sf, player, moves[i])) Error("can't move x");


	  P = EXPWERT(WERT_TO_REAL(
		 -Value(&sf, GEGNER(player), depth-1, &dummy)
	      ));


#if OUT
printf("["); KoorAus(moves[i]); printf(":%d] ", round(P*100)); fflush(stdout);
#endif

if (P > Pmax) { printf(">>> greater!\a\a\a\n"); }


	  if (P >= Pmin) { 

/* good move not made -> save game */


#if OUT
printf("<-S ");
#endif

	    new = true;
	    SaveG(pGame, moves[i], player);
	  }
	}
      }

#if OUT
printf("\n");
#endif


    } else {			/* find blunders */


      FOR (i, movenum) if (MoveDat[i].Move == bestmove) break;


      if (i >= movenum)  {	/* best move not made */


/* if best move has value >= P_DRAW and all moves seen have values <P_DRAW
   save best, or when there is no move with value >= Pmax-DELTA_BLUNDER
*/

	seenPmax = -1;

        FOR (i, movenum) {

	  sf = *psf;

if (!ZUG(MoveDat[i].Move)) Error("move 2?");
	  if (!SfSetzen(&sf, player, MoveDat[i].Move)) Error("can't move y");


	  P = EXPWERT(WERT_TO_REAL(
		  -Value(&sf, GEGNER(player), depth-1, &dummy)
	      ));

	  if (P > seenPmax) seenPmax = P;
#if OUT
printf("["); KoorAus(MoveDat[i].Move); 
printf(":%d] ", round(P*100)); fflush(stdout);
#endif

if (P > Pmax) { printf(">>> greater!"); }
 

        }

#if OUT
printf("\n");
#endif

        if (Pmax - seenPmax > DELTA_BLUNDER || 
	    (Pmax >= P_DRAW && seenPmax < P_DRAW)) { 

if (Pmax - seenPmax > DELTA_BLUNDER) printf("DELTA:"); else printf("PDRAW:");
fflush(stdout);

          p = EXPWERT(WERT_TO_REAL(
		Value(psf, player, depth+DEPTH_INC, &bestmove)
	      ));

	  ClearHashTab(&zio.hash);


          FOR (i, movenum) if (MoveDat[i].Move == bestmove) break;

	  if (i >= movenum) {

/* best move not made => save game */


printf("save (d=%d) [", depth+DEPTH_INC);
KoorAus(bestmove); printf(":%d]\n", round(p*100));


	    SaveG(pGame, bestmove, player);

	  } else printf("move made!");

printf("\n");

	}
      }

    }

  } else if (movenum > mn) Error("more moves than possible?");   




/* examine next positions */


  if (pPI->MoveIndex < 0) {			/* branch */

if (!pPI->pNode->SonNum) Error("no son?");


    FOR (i, pPI->pNode->SonNum) {

      SMOVE   smove;
      POSINFO PI = *pPI;


      if (pPI->pNode->Sons[i].MoveNum > 1) {

        PI.MoveIndex = i;
        PI.PathIndex = 1;

      } else {

	PI.pNode = pPI->pNode->Sons[i].Next;
        PI.MoveIndex = -1;

      }

      smove = pPI->pNode->Sons[i].Moves[0];

if (player != SMOVE_PLAYER(smove)) Error("pl1");

      sf = *psf;
      if (!SfSetzen(&sf, player, SMOVE_MOVE(smove))) Error("move1");

      pGame->Moves[pGame->MoveNum++] = smove;
      CheckVertex(pT, &PI, pGame, &sf, searchdepth, maxnum, blunder);
      pGame->MoveNum--;

    }


  } else {					/* path */
    
    POSINFO PI = *pPI;
    SMOVE   smove = pPI->pNode->Sons[pPI->MoveIndex].Moves[pPI->PathIndex];

if (player != SMOVE_PLAYER(smove)) Error("pl2");

    sf = *psf;
    if (!SfSetzen(&sf, player, SMOVE_MOVE(smove))) Error("move2");

    PI.PathIndex = pPI->PathIndex+1;

    if (PI.PathIndex >= pPI->pNode->Sons[pPI->MoveIndex].MoveNum) {

/* path end -> next node */

      PI.pNode = pPI->pNode->Sons[pPI->MoveIndex].Next;

      if (!PI.pNode) { printf("!!! game end\a\a\a\n"); return; }

      PI.MoveIndex = -1;

    }


    pGame->Moves[pGame->MoveNum++] = smove;

    CheckVertex(pT, &PI, pGame, &sf, searchdepth, maxnum, blunder);
    pGame->MoveNum--;
   
  }
}





int main(int argc, char **argv)
{
  char     path[400];
  int      argi, depth, maxnum, newnum, searchdepth, gamenum=0, savenum=0;
  LIBRARY  *pT;
  GAME     game, game1, startgame;
  FILE     *fp, *fpout;
  SPFELD   sf;
  char     *file;
  bool     f_blu=false, f_res=false, f_looser=false, f_p=false;
  POSINFO  PI;

  if (argc < 4) {

error: 

    Error("\n\
ochk [ -blu | -res | -looser ] searchdepth maxnum [-p path] [-o old-lib] new-games\n\n\
      -blu:           find blunders\n\
      -res:           research - find move alternatives\n\
      -looser:        save game if there is a good position for the looser\n\
      searchdepth:    tree search look ahead (>= 2)\n\
      maxnum:         consider boards with at most maxnum discs\n\
      old-lib:        old-library (oko-file)\n\
      new-games:      new games (oko-file) to check\n\n\
 save games to correct in new-games.new[.path]");


  }


  pT = NewLibrary();

  startgame.MoveNum = 0;
  startgame.DiscDiffBW = 0;
  path[0] = 0;

  argi = 1;

  if      (!strcmp(argv[argi], "-blu"))    f_blu = true;
  else if (!strcmp(argv[argi], "-res"))    f_res = true;
  else if (!strcmp(argv[argi], "-looser")) f_looser = true;

  argi++;

  if (sscanf(argv[argi], "%d", &depth) != 1 || depth < 2 || depth > 20) goto error;
  
  argi++;

  if (sscanf(argv[argi], "%d", &maxnum) != 1 || maxnum < 5 || maxnum > 64) goto error;

  argi++;

  if (argv[argi] && !strcmp(argv[argi], "-p")) {

    argi++;

    if (!argv[argi]) goto error;

    strcpy(path, argv[argi]);

    strcat(path, ": +0");

    sReadGame(path, &startgame);

    strcpy(path, ".");
    strcat(path, argv[argi]);


    f_p = true;

    argi++;
  }

  if (argv[argi] && !strcmp(argv[argi], "-o")) {

    argi++;
    file = argv[argi++];

    if (!file) goto error;

    fp = fopen(file, "r");
    if (!fp) Error("can't open old-lib");

/* read old gametree */

    FOREVER {

      if (!fReadPackedGame(fp, &game)) break;

      if (!AppendGameToLibrary(pT, &game)) { printf("."); fflush(stdout); }

    }

    fclose(fp);

    printf("\nOld library: %d game(s)\n", pT->GameNum);

    MarkOld(pT, pT->pRoot);
  }


  file = argv[argi++];
  if (!file) goto error;



  if (f_looser) {

    int i, val, Player, d, CheckPlayer;
    SFPOS moves[65];


    fp = fopen(file, "r");
    if (!fp) Error("can't open new-games");


    sprintf(newfile, "%s.new", file);
 

    fpout = fopen(newfile, "w");
    if (!fpout) Error("can't open new-games.new");


    InitZug(&zio, EVALFUNC, Check, HASHBITS);

    strcpy(ParameterFile, PARFILE);
    strcpy(TableFile, TABFILE);

    FOREVER {


      if (!fReadPackedGame(fp, &game)) break;

/*fWriteGame(stdout, &game);*/





      gamenum++;


      if (!game.DiscDiffBW) continue;

      CheckPlayer = 0;
      if (game.DiscDiffBW > 0) CheckPlayer = WHITE;
      if (game.DiscDiffBW < 0) CheckPlayer = BLACK;

      SfGrund(&sf);


printf("game %-4d (%4d saved):  diff=%+3d ", gamenum, savenum, game.DiscDiffBW);


      FOR (i, game.MoveNum) {

        if (SfAnz(&sf) > maxnum) break;

	Player = SMOVE_PLAYER(game.Moves[i]);

	game1 = game;
	game1.MoveNum = i;
	game1.Moves[i] = 0;	/* prefix */

	if (Player == CheckPlayer 			&& 
	    SfAnz(&sf) >= MIN_DISCS			&& 
	    SfMoeglZuegeE(&sf, Player, moves) > 1	&&
            !SearchPosition(pT, &game1, &PI)) {

/*
SfAus(&sf, 0, 0);
printf("%d\n", Player);
*/

	  zio.Sf         = sf;
	  zio.Partei     = Player;
	  zio.LetzterZug = ZUG_UNBEKANNT;
	  zio.Modus      = MODUS_NORMAL;

	  zio.Selective  = SELECTIVE;
	  zio.Percentile = PERCENTILE;
	  zio.Quiescence = QUIESCENCE;

	  zio.ZugVorgabe = true;

          zio.Zuege[0] = SMOVE_MOVE(game.Moves[i]);
	  zio.VorgabeAnz = 1;

#if KILLER
  KillerAdjust(&zio.killer, &zio.Sf);
#endif

	  if (SfAnz(&sf) > 35 || SfAnz(&sf) < 20) searchdepth = depth+1;
	  else					  searchdepth = depth;

	  for (d=1; d <= searchdepth; d++) {

printf("%2d\b\b", d); fflush(stdout);

	    zio.MaxTiefe = d; Zugermittlung(&zio);

	    if (d == searchdepth-1) val = zio.Wert;

	  }

	  if ((val < 0) ^ (zio.Wert < 0)) { 

/* one more iteration, if signs of values differ */ 

      	    val = zio.Wert;

printf("%2d\b\b", d); fflush(stdout);

            zio.MaxTiefe = searchdepth+1; Zugermittlung(&zio);

	  }

	  val = (val + zio.Wert) / 2;

          val = round(100*EXPWERT(WERT_TO_REAL(val)));

printf("%d", val);

	  if (val >= LOOSER_CHECK*100) {

	    for (d=searchdepth+LOOSER_INC-1; d <= searchdepth+LOOSER_INC; d++) {

printf("%2d\b\b", d); fflush(stdout);

	      zio.MaxTiefe = d; Zugermittlung(&zio);

	      if (d == searchdepth+LOOSER_INC-1) val = zio.Wert;

	    }

	    val = (val + zio.Wert) / 2;

            val = round(100*EXPWERT(WERT_TO_REAL(val)));

printf("[%d]", val);

	    if (val >= LOOSER_GOOD*100) {

printf(" -> good at %d discs!", SfAnz(&sf));

	      savenum++;

	      if (!AppendGameToLibrary(pT, &game)) Error("game exists?");

	      fWritePackedGame(fpout, &game);

	      break;
	    }
	  }

printf(" ");

	}

        if (!SfSetzen(&sf, Player, SMOVE_MOVE(game.Moves[i])))
	  Error("no move?");

      }

printf("\n");

    }


    printf("%d game(s) examined, %d saved\n", gamenum, savenum);

    exit(0);

  }







  fp = fopen(file, "r");
  if (!fp) Error("can't open new-games");

/* append new games */

  newnum = 0;

  FOREVER {

    if (!fReadPackedGame(fp, &game)) break;

    if (AppendGameToLibrary(pT, &game)) newnum++;

  }

  fclose(fp);

  printf("\n%d new game(s) to check\n", newnum);

  { int dummy;
    EvalLibNode(pT, pT->pRoot, &dummy);
  }

  sprintf(newfile, "%s.new%s", file, path);

  fp = fopen(newfile, "w");
  if (!fp) Error("can't open output file");
  fclose(fp);

  InitZug(&zio, EVALFUNC, Check, HASHBITS);

printf("start with: ");
fWriteGame(stdout, &startgame);

  PlayGame(startgame.MoveNum, &startgame, &sf);


  if (!SearchPosition(pT, &startgame, &PI)) Error("path not found");


SfAus(&sf, 0, 0);

  CheckVertex(pT, &PI, &startgame, &sf, depth, maxnum, f_blu);

  return 0;
}

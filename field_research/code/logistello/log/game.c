// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* game stuff */

#ifndef PRECOMP
#include "main.h"
#endif

#include "lib.h"
#include "crt.h"
#include "goodies.h"
#include "trans.h"
#include "killer.h"
#include "playm.h"
#include "pmove.h"
#include "game.h"



/* 10x10 move -> 1..60 */

int Move2Code[100] = {
  0,   0,  0,  0,  0,  0,  0,  0,  0, 0,  
  0,   1,  2,  3,  4,  5,  6,  7,  8, 0,  
  0,   9, 10, 11, 12, 13, 14, 15, 16, 0,  
  0,  17, 18, 19, 20, 21, 22, 23, 24, 0,  
  0,  25, 26, 27, 0,  0,  28, 29, 30, 0,  
  0,  31, 32, 33, 0,  0,  34, 35, 36, 0,  
  0,  37, 38, 39, 40, 41, 42, 43, 44, 0,  
  0,  45, 46, 47, 48, 49, 50, 51, 52, 0,  
  0,  53, 54, 55, 56, 57, 58, 59, 60, 0,  
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0  
};


/* 1..60 -> 10x10 move */

int Code2Move[61] = {
  0,
  11, 12, 13, 14, 15, 16, 17, 18, 
  21, 22, 23, 24, 25, 26, 27, 28, 
  31, 32, 33, 34, 35, 36, 37, 38, 
  41, 42, 43,         46, 47, 48, 
  51, 52, 53,         56, 57, 58, 
  61, 62, 63, 64, 65, 66, 67, 68, 
  71, 72, 73, 74, 75, 76, 77, 78, 
  81, 82, 83, 84, 85, 86, 87, 88 
};



bool fReadPackedGameOld(FILE *fp, GAME *pGame)
{
  pGame->MoveNum = fgetc(fp);

  if (feof(fp)) return false;

  if (pGame->MoveNum > 60) Error("fReadPackedGame: MoveNum > 60");

  pGame->DiscDiffBW = fgetc(fp);

  fgetc(fp);

  if (fread((char*)pGame->Moves, sizeof(pGame->Moves), 1, fp) != 1) 
    return false;

  pGame->Moves[pGame->MoveNum] = 0;

  pGame->Flags = 0;

  return !feof(fp);
}



bool fReadPackedGame(FILE *fp, GAME *pGame)
{
  pGame->MoveNum = fgetc(fp);

  if (feof(fp)) return false;

  if (pGame->MoveNum > 60) Error("fReadPackedGame: MoveNum > 60");

  pGame->DiscDiffBW = fgetc(fp);


  { int i;

    FOR (i, pGame->MoveNum) pGame->Moves[i] = fgetc(fp);

  }

  pGame->Moves[pGame->MoveNum] = 0;

  pGame->Flags = 0;

  return !feof(fp);
}


bool fWritePackedGame(FILE *fp, GAME *pGame)
{
  fputc(pGame->MoveNum, fp);
  fputc(pGame->DiscDiffBW, fp);


#if 0

old

  fputc(0, fp);

  if (fwrite((char*)pGame->Moves, sizeof(pGame->Moves), 1, fp) != 1)
    return false;

#else

  { int i;

    FOR (i, pGame->MoveNum) fputc(pGame->Moves[i], fp);

  }

#endif

  return !ferror(fp);
}



void sWriteGame(char *s, GAME *pGame)
{
  char  s1[500], t[20];
  int   i, player;
  int   move;

  s[0] = 0;

  FOR (i, pGame->MoveNum) {

    move   = SMOVE_MOVE  (pGame->Moves[i]);
    player = SMOVE_PLAYER(pGame->Moves[i]);

    t[0] = player == BLACK ? '+' : '-';
    t[1] = 0;

    sKoorAus(&t[1], move);
    sprintf(s1, "%s%s", s, t); strcpy(s, s1);
  }

  sprintf(s1, "%s: %+03d", s, pGame->DiscDiffBW);

  strcpy(s, s1);

}


bool fWriteGame(FILE *fp, GAME *pGame)
{
  char s[500];

  sWriteGame(s, pGame);
  fprintf(fp, "%s\n", s);

  return !ferror(fp);
}




void sReadGame(char *s, GAME *pGame)
{
  int i=0, x, y, Player;

  pGame->MoveNum = 0;

  while (s[i] == ' ') i++;


  FOREVER {

    if      (s[i] == '+') Player = BLACK;
    else if (s[i] == '-') Player = WHITE;
    else if (s[i] == ':') break;
    else { Error("sReadGame: not +/-/:"); break; }
    
    x = s[i+1] - 'a';  

    if (x < 0 || x > 7) Error("sReadGame: x corrupt");

    y = s[i+2] - '1';

    if (y < 0 || y > 7) Error("sReadGame: y corrupt");

    if (pGame->MoveNum >= 60) Error("sReadGame: MoveNum >= 60");

    pGame->Moves[ pGame->MoveNum++] = SMOVE_GEN(Tab8to10[y*8+x], Player);   

    i += 3;
  }

  if (sscanf(&s[i+1], "%d", &i) != 1) 
    Error("sReadGame: DiscDiffBW?");

  pGame->DiscDiffBW = i;
  pGame->Moves[pGame->MoveNum] = 0;

}


bool fReadGame(FILE *fp, GAME *pGame)
{
  char s[501];
 
  if (!fgets(s, 500, fp)) return false;

  sReadGame(s, pGame);
  return true;
}



#if 0 

wrong ...


void Spiel2Game(SPIELINFO *SpInfo, int MoveNum, GAME *pGame)
{
  int i;


  for (i=1; i <= MoveNum; i++) {

    if (SpInfo[i].AmZug == BLACK) pGame->Moves[i-1] =  SpInfo[i].Zug;
    else			  pGame->Moves[i-1] = -SpInfo[i].Zug;

  }



}
#endif




SMOVE SMOVEgen(int player, int move, bool flag)
{
  if ((player != BLACK && player != WHITE) || 
      (flag  != false && flag  != true)  || 
      !ZUG(move)) { printf("%d %d %d ", player, move, flag); Error("SMOVEgen"); }

/* KoorAus(Zug); printf("-> %d ", ZugCode[Zug]); */

  return (player == WHITE ? SMOVE_BIT_PLAYER : 0) |
         (flag            ? SMOVE_BIT_FLAG   : 0) | 
         Move2Code[move];
}





/* return player to move, return 0 <=> game end */

int PlayGame(int ZugAnz, GAME *pGame, SPFELD *psf)
{
  int	 i, Zug, flag;
  PARTEI Partei=BLACK;
  SFPOS  Zuege[65];

  SfGrund(psf);

/*
fSpielKlarAus(stdout, pGame);
*/

  if (ZugAnz > pGame->MoveNum) { printf(">>> ZugAnz zu groß"); return 0; }

  FOR (i, ZugAnz) {


    Zug = SMOVE_MOVE(pGame->Moves[i]);

/*
KoorAus(Zug);
*/

    if (!SfSetzen(psf, Partei, Zug)) { 

      Partei = GEGNER(Partei);

      if (!SfSetzen(psf, Partei, Zug)) {
	printf(">>> Fehler in Spiel");
	return 0;
      }
 
    }

    flag = SMOVE_FLAG(pGame->Moves[i]);

    pGame->Moves[i] = SMOVEgen(Partei, Zug, flag);

    Partei = GEGNER(Partei);
  }


  if (!SfMoeglZuege(psf, BLACK, Zuege) && !SfMoeglZuege(psf, WHITE, Zuege)) {

/*

nonsense ...

    pGame->DiscDiffBW = SfAnzBLACK(psf) - SfAnzWHITE(psf);
*/

    return 0;

  }


  if (SfMoeglZuege(psf, Partei, Zuege)) 
    return Partei; 
  else 
    return GEGNER(Partei);
}



bool Tab2Game(SPFELD *pTab, GAME *pGame)
{
  int i, SteinAnz;
  SPFELD sf;
  SFPOS	 *p;


  TabToSf(pTab, &sf);

  SteinAnz = SfAnz(&sf);

  if (pTab->Marke != BLACK || SteinAnz != 4 || 
      sf.p[E4] != BLACK || sf.p[D5] != BLACK ||
      sf.p[D4] != WHITE || sf.p[E5] != WHITE)

    return false;


  FOR (i, 61) pGame->Moves[i] = 0;

  p = pTab->p;

  FOR (i, 100) 
    if (p[i] >= NUM_DISP) {
      pGame->Moves[p[i] - NUM_DISP -1] = SMOVEgen(BLACK, i, false);

/*
KoorAus(i);
printf(" %d\n", pGame->Moves[p[i] - NUM_DISP -1]);
*/

    }

  FOR (i, 60) if (pGame->Moves[i] == 0) break;

  pGame->Moves[i] = 0;
  pGame->MoveNum = i;

  PlayGame(i, pGame, &sf);
  pGame->DiscDiffBW = SfAnzBLACK(&sf) - SfAnzWHITE(&sf);

  return true;
}


void Game2Tab(GAME *pGame, SPFELD *pTab)
{
  int i, Zug;


  SfGrund(pTab);

  pTab->Marke = BLACK;

  FOR (i, pGame->MoveNum) {

    Zug = SMOVE_MOVE(pGame->Moves[i]);

    if (!ZUG(Zug)) Error("Game2Tab: game corrupt!");

    pTab->p[Zug] = NUM_DISP + i + 1;
  }
}


#define UMSG(s) printf(">>> update: %s\n", s);

void UpdateGame(GAME *pGame, SPFELD *pNewBoard, PARTEI ToMove)
{
  SPFELD Board, Board1;
  SFPOS  Moves[65], Moves1[65];
  int    i, j, k, MoveNum, Player, DiscNum;
  char   s[100], s1[100];

#if 0
UMSG("board:\n");
SfAus(pNewBoard, 0, 0);
printf("%d to move\n", ToMove);
#endif

  DiscNum = SfAnz(pNewBoard);

  if (DiscNum <= 4 || DiscNum > pGame->MoveNum + 4 + 1) {

clear:

UMSG("clear");

    pGame->MoveNum  = 0;
    pGame->Moves[0] = 0;

    return;
  }

  SfGrund(&Board);

  FOR (i, pGame->MoveNum) {

    if (DiscNum-1 == i+4) break;

    if (!SfSetzen(&Board, SMOVE_PLAYER(pGame->Moves[i]),
			  SMOVE_MOVE(pGame->Moves[i]))) { 

      Error("UpdateGame: illegal move");
    }
  }

  if (DiscNum-1 > i+4) {

UMSG("sequence to short");
    goto clear;	/* move sequence is too short */

  } else {

    Player = (i == 0) ? BLACK : GEGNER(SMOVE_PLAYER(pGame->Moves[i-1]));
        
    if (!SfMoeglZuege(&Board, Player, Moves)) {

      Player = GEGNER(Player);

/* does sequence lead to finished game? */

      if (!SfMoeglZuege(&Board, Player, Moves)) {

UMSG("game end");
	goto clear; 
      }
    }

#if 0
    cout << "UPDATE GAME" << endl;
    SfAus(&Board, BLACK, 0);
    SfAus(pNewBoard, BLACK, 0);
    cout << Player << endl;
    cout << endl;
#endif

    MoveNum = SfMoeglZuege(&Board, Player, Moves);

    if (!MoveNum) return;

    FOR (j, MoveNum) {

#if 0
SfAus(&Board, 0, 0);
printf("%d ", Player); KoorAus(Moves[j]); 
printf("\n");
#endif

      Board1 = Board;

      if (!SfSetzen(&Board1, Player, Moves[j])) Error("Move???");

      FOR (k, 100) if (Board1.p[k] != pNewBoard->p[k]) { 
	break; 
      }
      if (k == 100) break;
    }

    if (j >= MoveNum) { 

UMSG("board not reachable");
      goto clear;
    }

    if (ToMove != GEGNER(Player) && 
	SfMoeglZuege(&Board1, GEGNER(Player), Moves1)){

UMSG("wrong player");
      goto clear;
    }

/* new board and player are OK */

    pGame->Moves[i++] = SMOVE_GEN(Moves[j], Player);
    pGame->Moves[i]   = 0;
    pGame->MoveNum    = i;

sKoorAus(s1, Moves[j]);
sprintf(s, "board/player OK, Move: %s", s1);

UMSG(s);
/*fWriteGame(stdout, pGame);*/

  }
}


void UniqueGame(GAME *pGame)
{
  SPFELD tab;
  GAME   gameu;
 
  Game2Tab(pGame, &tab);
  TabEindeutig(&tab);
  Tab2Game(&tab, &gameu);
  gameu.DiscDiffBW = pGame->DiscDiffBW;  /* OOPS - restore old value!!! */
  *pGame = gameu;
}

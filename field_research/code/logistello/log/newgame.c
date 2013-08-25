// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// new game class 2/97

#include "main.h"
#include "lib.h"
#include "crt.h"
#include "goodies.h"
#include "trans.h"
#include "newgame.h"



/* 10x10 move -> 1..60 */

int PackedMove::Move2Code[100] = {
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

int PackedMove::Code2Move[61] = {
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


#if 0
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

#endif


bool NewGame::f_read_packed(FILE *fp)
{
  set_flags(fgetc(fp));

  if (feof(fp)) return false;

  set_move_num(fgetc(fp));
  set_value(fgetc(fp));

  if (get_move_num() > 60) Error("f_read_packed: MoveNum > 60");

  int i;

  FOR (i, get_move_num()) get_pm(i).set_raw(fgetc(fp));

  get_pm(get_move_num()).set_raw(0);

  return !feof(fp);
}


bool NewGame::f_write_packed(FILE *fp)
{
  fputc(get_flags(), fp);
  fputc(get_move_num(), fp);
  fputc(get_value(), fp);

  int i;

  FOR (i, get_move_num()) fputc(get_pm(i).get_raw(), fp);

  return !ferror(fp);
}



void NewGame::s_write(char *s)
{
  char  s1[500], t[20];
  int   i, player;
  int   move;

  s[0] = 0;

  FOR (i, get_move_num()) {

    move   = get_pm(i).get_move();
    player = get_pm(i).get_player();

    t[0] = player == BLACK ? '+' : '-';
    t[1] = 0;

    sKoorAus(&t[1], move);
    sprintf(s1, "%s%s", s, t); strcpy(s, s1);
  }

  sprintf(s1, "%s: %+03d %2x", s, get_value(), get_flags());

  strcpy(s, s1);

}


bool NewGame::f_write(FILE *fp)
{
  char s[500];

  s_write(s);
  fprintf(fp, "%s\n", s);

  return !ferror(fp);
}




void NewGame::s_read(char *s)
{
  int i=0, x, y, player, move_num=0;
  PackedMove pm;

  while (s[i] == ' ') i++;

  FOREVER {

    if      (s[i] == '+') player = BLACK;
    else if (s[i] == '-') player = WHITE;
    else if (s[i] == ':') break;
    else { Error("s_read: not +/-/:"); break; }
    
    x = s[i+1] - 'a';  

    if (x < 0 || x > 7) Error("s_read: x corrupt");

    y = s[i+2] - '1';

    if (y < 0 || y > 7) Error("s_read: y corrupt");

    if (move_num >= 60) Error("s_read: MoveNum >= 60");

    pm.set(player, Tab8to10[y*8+x]);
    set_pm(move_num++, pm);

    i += 3;
  }

  pm.set_raw(0);
  set_pm(move_num, pm);
  set_move_num(move_num);

  int val1, val2;

  i = sscanf(&s[i+1], "%d %x", &val1, &val2);

  if (i < 1) Error("s_read: no value");

  set_value(val1);

  if (i == 2)
    set_flags(val2);
  else
    set_flags(0);
}


bool NewGame::f_read(FILE *fp)
{
  char s[501];
 
  if (!fgets(s, 500, fp)) return false;

  s_read(s);
  return true;
}


// true <=> ok

bool NewGame::s_read_srv(char *s)
{
  int i=0, x, y, player, move_num=0;
  PackedMove pm;

  while (s[i] == ' ') i++;

  FOREVER {

    if      (s[i] >= 'A' && s[i] <= 'H') player = BLACK;
    else if (s[i] >= 'a' && s[i] <= 'h') player = WHITE;
    else if (s[i] == 0) break;
    else return false;
    
    x = toupper(s[i]) - 'A';  

    if (x < 0 || x > 7) return false;

    y = s[i+1] - '1';

    if (y < 0 || y > 7) return false;

    if (move_num >= 60) return false;

    pm.set(player, Tab8to10[y*8+x]);
    set_pm(move_num++, pm);

    i += 2;
  }

  pm.set_raw(0);
  set_pm(move_num, pm);
  set_move_num(move_num);

  set_flags(0);
  set_value(99);

  return true;
}



// return player to move at the end, return 0 <=> game finished

int NewGame::play(int ZugAnz, SPFELD &sf)
{
  int	 i, Zug;
  PARTEI Partei=BLACK;
  SFPOS  Zuege[65];

  SfGrund(&sf);

  if (ZugAnz > get_move_num()) { printf("play: move_num too large"); return 0; }

  FOR (i, ZugAnz) {

    Zug = get_pm(i).get_move();

    if (!SfSetzen(&sf, Partei, Zug)) { 

      Partei = GEGNER(Partei);

      if (!SfSetzen(&sf, Partei, Zug)) {
	printf(">>> Fehler in Spiel");
	return 0;
      }
    }

    get_pm(i).set(Partei, Zug);  // now with correct color!

    Partei = GEGNER(Partei);
  }

  if (!SfMoeglZuege(&sf, BLACK, Zuege) && !SfMoeglZuege(&sf, WHITE, Zuege)) {
    return 0;
  }

  if (SfMoeglZuege(&sf, Partei, Zuege)) 
    return Partei; 
  else 
    return GEGNER(Partei);
}



// return result if game is complete or 65 if something is wrong

int NewGame::play()
{
  int	 i, Zug;
  PARTEI Partei=BLACK;
  SPFELD sf;
  SFPOS  Zuege[65];

  SfGrund(&sf);

  FOR (i, get_move_num()) {

    Zug = get_pm(i).get_move();

    if (!SfSetzen(&sf, Partei, Zug)) { 

      Partei = GEGNER(Partei);

      if (!SfSetzen(&sf, Partei, Zug)) return 65;  // move not possible
    }

    if (Partei != get_pm(i).get_player()) return 65; // colour doesn't match
    Partei = GEGNER(Partei);
  }

  if (!SfMoeglZuege(&sf, BLACK, Zuege) && !SfMoeglZuege(&sf, WHITE, Zuege)) {
    
    // return result

    int b = SfAnzBLACK(&sf);
    int w = SfAnzWHITE(&sf);

    if (b+w < 64) {

      int d = 64 - b - w;

      if (b > w) b += d;
      else       w += d;
      
    }

    return b - w;
  }

  return 65;  // game not finished
}


bool NewGame::ok()
{
  int	 i, Zug;
  PARTEI Partei=BLACK;
  SPFELD sf;

  SfGrund(&sf);

  FOR (i, get_move_num()) {

    Zug = get_pm(i).get_move();

    if (!SfSetzen(&sf, Partei, Zug)) { 

      Partei = GEGNER(Partei);

      if (!SfSetzen(&sf, Partei, Zug)) { return 0; }
    }

    if (Partei != get_pm(i).get_player()) return false;
    Partei = GEGNER(Partei);
  }

  return true;
}


bool NewGame::from_old(GAME &old_game)
{
  int i;
  SPFELD sf;
  
  set_move_num(old_game.MoveNum);
  set_flags(0);

  FOR (i, 61) get_pm(i).set_raw(old_game.Moves[i]);

  if (play(get_move_num(), sf)) {

    // not finished
    
    is_finished(false);

  } else {

    // finished

    if (old_game.DiscDiffBW != SfAnzBLACK(&sf) - SfAnzWHITE(&sf)) {
      fWriteGame(stdout, &old_game);
      cout << " -> " << SfAnzBLACK(&sf) - SfAnzWHITE(&sf) << endl;
      cout << "*** from_old: game finished but results differ" << endl;
    }

    old_game.DiscDiffBW = SfAnzBLACK(&sf) - SfAnzWHITE(&sf);
    
    is_finished(true);
  }

  set_value(old_game.DiscDiffBW);

  return true; 
}


bool NewGame::from_tab(SPFELD &tab, bool compute_fin)
{
  int i, SteinAnz;
  SPFELD sf;
  SFPOS	 *p;


  TabToSf(&tab, &sf);

  SteinAnz = SfAnz(&sf);

  if (tab.Marke != BLACK || SteinAnz != 4 || 
      sf.p[E4] != BLACK || sf.p[D5] != BLACK ||
      sf.p[D4] != WHITE || sf.p[E5] != WHITE)

    return false;

  FOR (i, 61) get_pm(i).set_raw(0);

  p = tab.p;

  FOR (i, 100) 
    if (p[i] >= NUM_DISP) {
      get_pm(p[i] - NUM_DISP - 1).set(BLACK, i);

/*
KoorAus(i);
printf(" %d\n", pGame->Moves[p[i] - NUM_DISP -1]);
*/

    }

  FOR (i, 60) if (get_pm(i).get_raw() == 0) break;
  get_pm(i).set_raw(0);

  set_move_num(i);

  SPFELD sf1;
  int pl = play(i, sf1);  // important! this sets player-bits!

  if (compute_fin) {

    if (!pl) {
    
      is_finished(true);
      set_value(SfAnzBLACK(&sf1) - SfAnzWHITE(&sf1));
    
    } else {

      is_finished(false);
      set_value(0);
      
    }
  }
  return true;
}


void NewGame::to_tab(SPFELD &tab)
{
  int i, Zug;

  SfGrund(&tab);

  tab.Marke = BLACK;

  FOR (i, get_move_num()) {

    Zug = get_pm(i).get_move();

    if (!ZUG(Zug)) {
      FOR (i, get_move_num()) { printf("%d ", get_pm(i).get_move()); }

      Error("to_tab: game corrupt!");
    }

    tab.p[Zug] = NUM_DISP + i + 1;
  }
}


#define UMSG(s) printf(">>> update: %s\n", s);

void NewGame::update(SPFELD &NewBoard, PARTEI ToMove)
{
  SPFELD Board, Board1;
  SFPOS  Moves[65], Moves1[65];
  int    i, j, k, MoveNum, player, DiscNum;
  char   s[100], s1[100];

#if 0
UMSG("board:\n");
SfAus(pNewBoard, 0, 0);
printf("%d to move\n", ToMove);
#endif

  DiscNum = SfAnz(&NewBoard);

  if (DiscNum <= 4 || DiscNum > get_move_num() + 4 + 1) {

clear:

UMSG("clear");

    set_move_num(0);
    get_pm(0).set_raw(0);

    return;
  }


  SfGrund(&Board);

  FOR (i, get_move_num()) {

    if (DiscNum-1 == i+4) break;

    if (!SfSetzen(&Board, get_pm(i).get_player(), get_pm(i).get_player())) {
      Error("UpdateGame: illegal move");
    }
  }

  if (DiscNum-1 > i+4) {

UMSG("sequence too short");
    goto clear;	/* move sequence is too short */

  } else {

    player = (i == 0) ? BLACK : GEGNER(get_pm(i-1).get_player());
        
    if (!SfMoeglZuege(&Board, player, Moves)) {

      player = GEGNER(player);

      // does sequence lead to finished game?

      if (!SfMoeglZuege(&Board, player, Moves)) {

UMSG("game end");
	goto clear; 
      }
    }

    MoveNum = SfMoeglZuege(&Board, player, Moves);

    if (!MoveNum) return;

    FOR (j, MoveNum) {

#if 0
SfAus(&Board, 0, 0);
printf("%d ", player); KoorAus(Moves[j]); 
printf("\n");
#endif

      Board1 = Board;

      if (!SfSetzen(&Board1, player, Moves[j])) Error("Move???");

      FOR (k, 100) if (Board1.p[k] != NewBoard.p[k]) break;
   
      if (k == 100) break;
    }

    if (j >= MoveNum) { 

UMSG("board not reachable");
      goto clear;
    }

    if (ToMove != GEGNER(player) && 
	SfMoeglZuege(&Board1, GEGNER(player), Moves1)){

UMSG("wrong player");
      goto clear;
    }

    // new board and player are OK

    get_pm(i++).set(player, Moves[j]);
    get_pm(i).set_raw(0);
    set_move_num(i);

sKoorAus(s1, Moves[j]);
sprintf(s, "board/player OK, Move: %s", s1);

UMSG(s);
/*fWriteGame(stdout, pGame);*/

  }
}


void NewGame::unique()
{
  SPFELD tab;
  NewGame gameu;
  int v = get_value(), f = get_flags();

  to_tab(tab);
  TabEindeutig(&tab);
  gameu.from_tab(tab, false);

  copy_info(gameu);  // restore old info.

  *this = gameu;

  if (get_value() != v || get_flags() != f) Error("diff.");
}


bool NewGame::is_proper_prefix(NewGame &prefix, int &next_move)
{
  int i;

  if (prefix.move_num >= move_num) return false;

  FOR (i, prefix.move_num) {
    if (prefix.get_pm(i) != get_pm(i)) break;
  }

  if (i < prefix.move_num) return false;

  next_move = get_pm(i).get_move();
  return true;
}

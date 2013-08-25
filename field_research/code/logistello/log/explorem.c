// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// expand book leaf by leaf / 2.97

#include "main.h"
#include "lib.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "killer.h"
#include "eval.h"
#include "hash.h"
#include "fpatt.h"


#define OUT		false


#define EVALFUNC	EvalA
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"
#define PCSTATFILE      "pcstata"

static bool  selective  = true;
static float percentile = 1.0;
static bool  quiescence = true;

static ZUGIO zio;

static char newfile[200];



void _abort(void) { exit(1); }


void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}	/* Signal-Handler */


int saved=0, gamenum=0;


// evaluate vertices recursively


static void EvalPos(
  ZUGIO  &zio,
  SPFELD &bo,
  int    player,
  int    seconds,
  int    &move1,
  int    &value1,
  int    &move2,
  int    &value2
)
{
  int    movenum=0, mn, player1, i, j, d;
  SPFELD sf;
  MOVEDATA MoveDat[65];
  SFPOS  moves[65];

  int move_num = SfMoeglZuegeE(&bo, player, moves);

  if (!move_num) Error("EvalPos: no move?");

  zio.Sf         = bo;
  zio.Partei     = player;
  zio.LetzterZug = ZUG_UNBEKANNT;
  zio.Modus      = MODUS_NORMAL;
  
  zio.Selective  = selective;
  zio.Percentile = percentile;
  zio.Quiescence = quiescence;
  
  zio.ZugVorgabe = true;
  zio.VorgabeAnz = 0; 
  FOR (i, move_num) zio.Zuege[zio.VorgabeAnz++] = moves[i];

  KillerAdjust(&zio.killer, &zio.Sf);

// ClearHashTab(&zio.hash); ClearHashTab(&zio.hash0);


  ZEIT z0;
  CPUZeit(&z0);

  int values[64];
  int best_moves[64];
  bool extended = false;

  // 1. new variation

  for (d=1;; d++) {

    zio.MaxTiefe = d; Zugermittlung(&zio);

    ZEIT z1;
    CPUZeit(&z1);

    float used = ZeitDiff(&z1, &z0);

    printf("%2d ", d);
    printf("%5.1f ", used); KoorAus(zio.BestZug); 
    printf(" %d%%\n", int(rint(EXPWERT(zio.Wert/WERTFAKTOR)*100)));

    values[d]     = zio.Wert;
    best_moves[d] = zio.BestZug;

    if (extended) break;  // only one extension

    if (d < 2) continue;

    if (used >= 0.75 * seconds) {

      if ((values[d] > 0) ^ (values[d-1] > 0)) { 
	cout << " different sign -> extend" << endl;
	extended = true; continue; 
      }

      if (best_moves[d] != best_moves[d-1]) { 
	cout << " different move -> extend " << endl;
	extended = true; continue; 
      }

      break;
    }
  }

  value1  = (values[d] + values[d-1])/2;
  value1  = int(rint(EXPWERT(value1/WERTFAKTOR)*100 - 50));
  move1 = best_moves[d];

  if (move_num <= 1) {
    value2  = 0;
    move2 = ZUG_UNBEKANNT;
    return;
  }


  // 2. deviation

  zio.ZugVorgabe = true;
  zio.VorgabeAnz = 0; 
  FOR (i, move_num) 
    if (moves[i] != move1) zio.Zuege[zio.VorgabeAnz++] = moves[i];

  printf(" 2nd move:\n");
  
  for (int d2=1; d2 <= d; d2++) {

    zio.MaxTiefe = d2; Zugermittlung(&zio);

    ZEIT z1;
    CPUZeit(&z1);

    float used = ZeitDiff(&z1, &z0);

    printf("%2d ", d2);
    printf("%5.1f ", used); KoorAus(zio.BestZug); 
    printf(" %d%%\n", int(rint(EXPWERT(zio.Wert/WERTFAKTOR)*100)));

    values[d2]     = zio.Wert;
    best_moves[d2] = zio.BestZug;
  }

  value2 = (values[d] + values[d-1])/2;
  value2 = int(rint(EXPWERT(value2/WERTFAKTOR)*100 - 50));

  if (value2 >= value1) value2 = value1 - 1;

  move2 = zio.BestZug;

}



int main(int argc, char **argv)
{
  char     outfile[300];
  int      argi, depth, newnum;
  LIBRARY  *pT=NULL;
  GAME     game;
  FILE     *fp;
  SPFELD   sf;
  char     *file;
  POSINFO  PI;
  FILE     *fpout;
  bool     frand=false;
  int      wld_disc_num=64;
  int      seconds = 60;
  bool     draw_bad_for_black;

  if (argc < 4) {

error: 
    Error(" usage: oexpand [-time secs] (-dbb | -dbw) book-file eval-file \n\n");
  }

  argi = 1;

  if (!strcmp(argv[argi], "-time")) {
    argi++;
    seconds = atoi(argv[argi++]);
    if (seconds < 10) Error("time < 10 secs?");
  }

  if (!strcmp(argv[argi], "-dbb")) {

    draw_bad_for_black = true;

  } else if (!strcmp(argv[argi], "-dbw")) {

    draw_bad_for_black = false;

  } else Error("-dbb/-dbw missing");

  argi++;

  LIBRARY &book = *MakeLibrary(argv[argi], argv[argi+1], 0);

  book.Mode = LIBMODE_DEVFIRST;

  book.path_randomization = false;

  book.DrawBad       = false;
  book.DrawGood      = false;
  book.DrawBadBlack  = draw_bad_for_black;
  book.DrawGoodBlack = !draw_bad_for_black;
  
  book.PublicDrawBad       = false;
  book.PublicDrawGood      = false;
  book.PublicDrawBadBlack  = draw_bad_for_black;
  book.PublicDrawGoodBlack = !draw_bad_for_black;



  InitZug(&zio, EVALFUNC, Check, HASHBITS, HASHMAXDEPTH);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile, TABFILE);

  SPFELD bo;
  NewGame ngame;
  int player;
  int move = D3;
  int val;
  ValType vtype;
  sint1 moves[65];



  // find minimax leaf

  do {
    
    player = ngame.play(ngame.get_move_num(), bo);
    if (player == 0) Error("game finished!?");
    
    ngame.get_pm(ngame.get_move_num()).set(player, move);
    ngame.set_move_num(ngame.get_move_num()+1);
    
    zio.cio.Partei = GEGNER(player);
    move = NewGameTreeMove(&book, ngame, &zio, &val, &vtype);
    
  } while (move != ZUG_UNBEKANNT);


  // compute player to move

  player = ngame.play(ngame.get_move_num(), bo);
  if (player == 0) Error("game finished!?");
    
  if (!SfMoeglZuege(&bo, player, moves)) {
    player = GEGNER(player);
    cout << "pass\a" << endl;
    if (!SfMoeglZuege(&bo, player, moves))
      Error("neither player has move");
  }

  if (player == BLACK) cout << "-> ##"; else cout << "-> ()";
  cout << endl << endl;
  SfAus(&bo, 0, 0);
  
  // evaluate position
  
  int move1, val1, move2, val2;

  EvalPos(zio, bo, player, seconds, move1, val1, move2, val2);
  
  if (move1 == ZUG_UNBEKANNT || move1 == ZUG_PASSEN) Error("move1?");
  
  if (player == WHITE) { 

    // value in game-file is from BLACK's point of view

    val1 = -val1;
    val2 = -val2;
  }

  ngame.set_move_num(ngame.get_move_num()+1);
  ngame.get_pm(ngame.get_move_num()-1).set(player, move1);
  ngame.set_value(val1);

  cout << "new variation: ";
  ngame.f_write(stdout);
  
  if (ZUG(move2)) {
    
    ngame.get_pm(ngame.get_move_num()-1).set(player, move2);
    ngame.set_value(val2);
    
    cout << "new deviation: ";
    ngame.f_write(stdout);
    
  } else {
    
    cout << "no new deviation" << endl;
  }

  return 0;
} 

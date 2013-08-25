// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// othello player / communicates via filesystem or ios
   
#include "main.h"
#include "playm.h"
#include "pmove.h"
#include "move.h"
#include "eval.h"
#include "sboard.h"
#include "pcstat.h"

#include "goodies.h"
#include "filecom.h"
#include "socketcom.h"
#include "crt.h"
#include "playgm.h"
#include "lib.h"
#include "book.h"
#include "rndbook.h"
#include "fpatt.h"
#include "hash.h"
#include "killer.h"
#include "timer.h"
#include "crt.h"

#include <time.h>

#define TUNNEL_SCRIPT "~/scripts/tunnel.sh"

#define NEW_BOOK  1

#define BLOCK 1

#undef ARG_MAX 
#define ARG_MAX 500

#include "OthClient.H"


#define REQUEST_TIME	60	/* time between match requests */
#define REQUEST_SPROB   0.2
#define REQUEST_SHORT	14	/* time of short game */
#define REQUEST_LONG	30	/* time of long game */

#define PING_TIME	900	/* time between pings */

#define USE_COUNTERMOVE 0


#define EXTERNE_ZEIT	0.5	/* Zeit pro Zug, in der nicht gerechnet wird */
#define POLSTER		10


#define LOGSUFFIX	".log"	

#define PLAYER_NUM      100

static float book_komi = -1.0;
static bool
  draw_bad = false,
  draw_good = false,
  draw_bad_black = false,
  draw_good_black = false,
  public_draw_bad = false,
  public_draw_good = false,
  public_draw_bad_black = false,
  public_draw_good_black = false;

const char *SUPERUSERS[] = { "mic", "delorme", "Kostas", "unic", "n2" };
const int SUPER_N = sizeof(SUPERUSERS)/sizeof(char*);

bool LogInOut = false;	/* Ein- /Ausgabe merken */
FILE *LogFP   = 0;

OthClient *pvc = 0;

bool synchro_mode = false;  // true -> no signals handled during search

int VERB = 1;


// match conditions not handled by formulas

bool DateCond = false;
time_t MinDate, MaxDate;

bool AcceptCond = false;
vector<String> AcceptNames;

bool RejectCond = false;
vector<String> RejectNames;



bool UseTestGameKomi = false;
float test_game_komi = 0;

bool UseTestGameRand = false;
int  test_game_rand  = 0;

char TO_ME[DATEINAME_MAX], FROM_ME[DATEINAME_MAX];


LIBRARY *pLibrary;

Book *p_book=0;
Book *p_rnd_book=0;

int StartTime=0;

#define SAVE_GAME       true
#define GAMEFILE	"oplay.osp"

GAME Game, InputGame;

bool using_ios   = false;
bool iosbg       = false;
bool my_socket   = false;
bool small       = false;
bool match       = false;
bool NoVal       = false;
bool OwnTime     = false;
bool OneGame     = false;
bool BadExit     = false;
bool BadExitWait = false;
bool BadGame     = false;	// => exit on GAME_END
bool GamePlayed  = false;
bool AutoAccept  = false;
bool T4T         = false;
bool P2          = false;


int open_val=1;


int  exit_delay  = 0;           // no timed exit
int  exit_rnd_delay = 0;
Timer start_time(Timer::REAL_TIME);

String login("foo");
String passwd("###");

String server("opal.cs.ualberta.ca");
int    port = 5000;

char *RequestPlayers[PLAYER_NUM], 
     *AcceptPlayers[PLAYER_NUM], 
     *RejectPlayers[PLAYER_NUM];

vector<String> newargs;

extern int movenum, moveok;

static String game_id = ".?"; // for kibitz

static bool analyse = false;

void _abort(void) { exit(10); }	// for prof


static void close_socket()
{
  if (my_socket) { printf("close socket\n"); socket_disconnect(); }
  if (using_ios) {
    printf("close socket\n");
    // client_DISCONNECT();
  }
}


 
static void set_draw_modes(Book *p_book)
{
  if (draw_bad)
    p_book->set_draw_mode(Book::DrawMode::BAD);
  else if (draw_good)
    p_book->set_draw_mode(Book::DrawMode::GOOD);
  else if (draw_bad_black)
    p_book->set_draw_mode(Book::DrawMode::BAD_FOR_BLACK);
  else if (draw_good_black)
    p_book->set_draw_mode(Book::DrawMode::GOOD_FOR_BLACK);
  else
    p_book->set_draw_mode(Book::DrawMode::NORMAL);


  if (public_draw_bad)
    p_book->set_public_draw_mode(Book::DrawMode::BAD);
  else if (public_draw_good)
    p_book->set_public_draw_mode(Book::DrawMode::GOOD);
  else if (public_draw_bad_black)
    p_book->set_public_draw_mode(Book::DrawMode::BAD_FOR_BLACK);
  else if (public_draw_good_black)
    p_book->set_public_draw_mode(Book::DrawMode::GOOD_FOR_BLACK);
  else
    p_book->set_public_draw_mode(Book::DrawMode::NORMAL);
}


static void handle_komi(COMZIO *pcio, float game_komi, int game_rand,
			Book *p_book, bool game_synchro
)
{
  pcio->game_komi = game_komi;  // needed in endgame search
  pcio->game_rand = game_rand;  // needed for first move timing
  pcio->game_synchro = game_synchro;
  
  if (p_book) {

    if (game_komi == 0) {

      set_draw_modes(p_book);

    } else if (game_komi < 0) {

      p_book->set_draw_mode(Book::DrawMode::GOOD_FOR_BLACK);
      p_book->set_public_draw_mode(Book::DrawMode::GOOD_FOR_BLACK);
      printf(">>> DRAW_GOOD_FOR_BLACK");

    } else {

      p_book->set_draw_mode(Book::DrawMode::BAD_FOR_BLACK);
      p_book->set_public_draw_mode(Book::DrawMode::BAD_FOR_BLACK);
      printf(">>> DRAW_BAD_FOR_BLACK");
    }
  }
}

ZEIT FeldZeit;


static void LogTimeStamp(FILE *fp)
{
  time_t clock;

  time(&clock);
  fprintf(fp, "::: %s", ctime(&clock));
}

static bool encrypted(String &s) {
  return s.size() >= 3 && s[0] == '@' && s[1] == '@' && s[2] == '@';
}

static void Message(const String &s)
{
  String msg = s; msg += '\n';
  cout << "MESSAGE: " << s << endl;
  if (using_ios) pvc->send(msg);
}

 
static bool DateOK(time_t min_time, time_t max_time)
{
  time_t t;
  time(&t);
  struct tm *loctime = localtime(&t);

  t = mktime(loctime);

  printf("local:%ld\n", t);

  return t >= min_time && t <= max_time;
}


static void ios_init()
{
  Message("tell /os vt100 -");           
  Message("tell /os client +");
  Message("tell /os notify - *");
  Message("tell /os notify +");    
  Message("tell /os rated +");
  Message("tell /os trust +");

  Message("tell /os aform");

  ostringstream os;
  form(os, "tell /os open %d", open_val);
  Message(os);

  String dform = 
    "tell /os dform !saved & (size!=8 | anti | mc!=? | (rand & (discs<14 | discs>20)) | (synchro & !rand) | mt1<60 | mm1!=0 | ml1==F | ot1 > 1800 | stored > 0)"; 

    // accept game colors
    //"tell /os dform !saved & (size!=8 | anti | (rand & (discs<14 | discs>20)) | (!rand && synchro) | mt1<60 | mm1!=0 | ml1==F | ot1 > 1800 | stored > 0)";

  if (login == "ant") 

    Message(dform);

  else if (login == "piglet") {

    Message("tell /os dform !saved & (size!=8 | anti | !synchro | !rand | mc!=? | \
discs<16 | discs>20 | mt1<200 | mm1!=0 | ml1==F | mt1 != ot1 | mt2 != ot2 |\
mt3 != ot3 | stored > 0 | md(+1.0) <= 0.0)");

    Message("tell /os aform T");
      
  } else if (login == "mirror") {

    Message("tell /os dform");
      
  } else if (login == "tit4tat") {
	
    Message(dform + " | rated");
    Message("tell /os rated -");

  } else 

    Message(dform + " | !rated");

  SLEEP(10);

  FORS (i, SUPER_N) pvc->add_super_user(SUPERUSERS[i]);
}


// is server dead?

static void PingServer(void)
{
  int  olduse;
  static int called=false;
  static ZEIT now, last;


  if (!called) { called = true; Zeit(&last); return; }

  olduse = UseCpuTime;
  UseCpuTime = false;

  Zeit(&now);

  UseCpuTime = olduse;

  if (ZeitDiff(&now, &last) >= PING_TIME) {
    last = now;
    Message("ping\n");
  }
}


static void MatchRequest(void)
{
  char s[300];
  int  time, olduse;
  static int i=0, called=false;
  static ZEIT now, last;

  if (!called) { called = true; Zeit(&last); return; }

  if (DateCond && !DateOK(MinDate, MaxDate)) return;

  olduse = UseCpuTime;
  UseCpuTime = false;

  Zeit(&now);

  UseCpuTime = olduse;

  if (ZeitDiff(&now, &last) >= REQUEST_TIME) {

    last = now;

    if (!RequestPlayers[i]) i = 0;

    if ((random() % 1000) < 1000*REQUEST_SPROB) time = REQUEST_SHORT;
    else		                        time = REQUEST_LONG;

    sprintf(s, "match %s %d\n", RequestPlayers[i], time);
    Message(s);

    i++;

  }
}

 

// handle signals

void SignalCheck(COMZIO *pcio, ZUGIO *pzio, bool no_count, bool block)
{
  int	 i, n, dummy;
  bool	 ok;
  ZEIT	 AktuelleZeit;
  char	 Nachricht[NACHRICHT_MAX];
  char   *p;  
  PARTEI Partei;
  SPFELD Sf;
  int    Restzeit;	// int is important
  int    LetzterZug;
  ZEIT   AktZeit;
  static int AnzZ=0;
  bool   da;

  
  // printf("Check\n");

  if (!no_count) {

    Zeit(&AktZeit);

    if (ZeitDiff(&AktZeit, &pcio->LastCheck) < 0.8) return;

    pcio->LastCheck = AktZeit;
  }

  if (exit_delay > 0) {
    Timer current_time(Timer::REAL_TIME);
    
    if (current_time.diff(start_time) >= exit_delay) {
      printf("time elapsed => exit\n");
      exit(0);
    }
  }

  if (synchro_mode && pcio->state == STATE_SEARCH && pcio->selbst)
    goto ZeitPruefen;  // no interrupt in synchro mode

  if (using_ios && !pvc->active_games() && BadGame && BadExitWait) {
    cout << "BadExitWait" << endl;
    exit(0);
  }

  // signals: check communication channel and time
  
  da = false;

  if (using_ios) {
    
    pvc->io();
    sint4 type = pvc->state();

    if (type == Client::SERVICE_ON) {

      ios_init();
      pvc->reset();

    } else if (type == Client::SERVICE_OFF) {
 
      if (BadGame && (BadExit || BadExitWait)) {
	cout << "exit to learn (service gone)" << endl;
	exit(0);
      }

      pvc->delete_games();
      pvc->reset();
    }

    if (type == Client::END) {

      pvc->reset();
      da = true; n = SIG_BREAK;
	
    } else if (type == Client::MATCH_REQ) {
	
      pvc->reset();

      String s;
      ClientMatchReq &req = pvc->m_req;
	
      if (AutoAccept) {
	  
	// default action: accept
	  
	s = "tell /os accept ";
	s += req.id;

	String user = req.p1;
#if 0
	printf("'%s' r=%d %d %d %d %d %d %d (id=%d)\n",
	       v_match.user,
	       v_match.rated,
	       v_match.my_time, v_match.op_time,
	       v_match.my_inc,  v_match.op_inc,
	       v_match.my_def,  v_match.op_def, 
	       v_match.game_id);
#endif
	if (RejectCond) {

	  FOR (i, (int)RejectNames.size()) {
	    if (user == RejectNames[i]) {
	      s = "tell "; s += user; s += " sorry";
	      goto decline;
	    }
	  }
	}

	if (AcceptCond) {
	    
	  FOR (i, (int)AcceptNames.size()) {
	    if (user == AcceptNames[i]) goto checkconds;
	  } 
	  s = "tell "; s += user; s += " sorry, I'm waiting for another player"; 
	  goto decline;
	    
	}
	  
      checkconds:
	  
	if (DateCond && !DateOK(MinDate, MaxDate)){
	    
	  char smin[200], smax[200];
	    
	  strncpy(smin, ctime(&MinDate), 199);
	  strncpy(smax, ctime(&MaxDate), 199);
	    
	  if (!smin[0] || !smax[0]) Error("date string corrupt");
	    
	  smin[strlen(smin)-1] = 0;
	  smax[strlen(smax)-1] = 0;
	    
	  s = "tell "; s += user; s += " sorry, I only accept games from ";
	  s += smin; s += " till "; s += smax; s += " LOG-local time!";
	  goto decline;
	}
	  
	Message(s);
	goto end; // accept all saved and other games
	  
	  
      decline:
	  
	Message(s);  // comment
	s = "tell /os decline "; s += req.id;
	Message(s);
	  
      end: ;
      }

      cout << "ASK?\a" << endl;

    } else if (type == Client::JOIN || type == Client::UPDATE) {

      printf("SIG_JOIN | SIG_UPDATE\n");

      String id;
      
      if (type == Client::JOIN) id = pvc->join.id; else id = pvc->update.id;
      
      const OthClientGame *cgame = (OthClientGame*)pvc->get_game(id);

      if (!cgame) Error("SIG_JOIN: game not found!");

      if (cgame->pinf[0].name != pvc->login() && cgame->pinf[1].name != pvc->login())
	goto reset;

      String msg = "tell /os break ";
      msg += id;

      // not 8x8 or anti -> break
      
      if (cgame->get_type().get_board_type().get_code() != 8 ||
	  cgame->get_type().is_anti_game()) {
	Message(msg);
	goto reset;
      }

      // player on reject list -> break
      
      if (RejectCond) {

	// resist root
	
	FOR (i, (int)RejectNames.size())
	  if (cgame->pinf[0].name == RejectNames[i] ||
	      cgame->pinf[1].name == RejectNames[i]) {
	    Message(msg);
	    goto reset;
	  }
      }

      if (type == Client::JOIN && !cgame->get_type().is_rand_game()) {

	// regular game started -> replay game

	SPFELD tab;
	
	SfGrund(&tab); tab.Marke = BLACK; 

	uint4 i;

	FOR (i, cgame->moves.size()) {
	  const OthClientMove &mv =
	    dynamic_cast<const OthClientMove&>(*(cgame->moves[i]));
	  sint4 x = ClientBoard::ind2x(mv.get_sq());
	  sint4 y = ClientBoard::ind2y(mv.get_sq());	  
	  sint4 ind = x+1 + 10 * (y+1);
	  tab.p[ind] = i+1 + NUM_DISP;
	}

	Tab2Game(&tab, &Game);
	TabAus(&tab);
	fWriteGame(stdout, &Game);
      }

      game_id = id;  // for kibitz

      if (!cgame->moves.size() == 0 ||
	  !cgame->get_type().is_komi_game()) 
	if (cgame->name_to_move() != pvc->login()) goto reset;  // not my turn

      // convert current position

      const OthClientBoard &bo =
	    dynamic_cast<const OthClientBoard&>(*(cgame->curr_pos));
      sint4 x, y;
      
      Sf = SfNull;

      FOR (y, 8) {
	FOR (x, 8) {

	  sint4 ind = ClientBoard::xy2ind(x, y);
	  sint4 i = Tab8to10[x+y*8];
	  
	  switch (bo.get_sq_cont(ind)) {
	  case ClientBoard::BLACK : Sf.p[i] = BLACK; break;
	  case ClientBoard::WHITE : Sf.p[i] = WHITE; break;
	  case ClientBoard::EMPTY : Sf.p[i] = LEER;  break;	    
	  case ClientBoard::BORDER: Sf.p[i] = RAND;  break;	    
	  default: Error("illegal board entry"); break;
	  }
	}
      }

      if (cgame->moves.size() > 0) {

	const OthClientMove &cm =
	  dynamic_cast<const OthClientMove&>(*(cgame->moves.back()));

	if (cm.is_pass()) LetzterZug = ZUG_PASSEN;
	else {
	  sint4 x = ClientBoard::ind2x(cm.get_sq());
	  sint4 y = ClientBoard::ind2y(cm.get_sq());	  
	  LetzterZug = x+1 + 10* (y+1);
	}
	  
      } else {

	LetzterZug = ZUG_UNBEKANNT;

      }
	
      if (bo.get_to_move() == ClientColor::BLACK) Partei = BLACK; else Partei = WHITE;

      printf(">>> board_komi=%f board_rand=%d\n",
	     cgame->komi,
	     ((cgame->get_type().is_rand_game() &&
	       !cgame->get_type().is_synchro_game()) ?
	      cgame->get_type().get_rand_type() : -1) 
	     );

      handle_komi(pcio,
		  cgame->komi,
		  ((cgame->get_type().is_rand_game() &&
		    !cgame->get_type().is_synchro_game()) ?
		  cgame->get_type().get_rand_type() : -1), 
		  p_book,
		  cgame->get_type().is_synchro_game());

      int ind = cgame->name_index(pvc->login());
      Restzeit = cgame->pinf[ind].clock.time_left(false) / uSec;

#if 0
      cgame->pinf[0].clock.print(cout, false); cout << endl;
      cgame->pinf[1].clock.print(cout, false); cout << endl;
      cout << cgame->pinf[ind].clock.time_left(false); << endl;
      cout << cgame->pinf[1-ind].clock.time_left(false) << endl;
#endif

      da = true; n = SIG_BOARD;
      pvc->reset();

    } else if (pvc->state() == Client::REMOTE_CMD) {

      pvc->reset();
      
      cout << "REMOTE command received from '" << pvc->remote.sender
	   << "': " << pvc->remote.msg[0] << endl;

      String msg = "tell ";
      msg += pvc->tell.sender;
      msg += " issued your command";
      Message(msg);

      if (encrypted(pvc->tell.msg[0])) {
	
	int r = ::system((String(TUNNEL_SCRIPT) + " " + pvc->tell.msg[0]).c_str());

	String rs;
	rs.form("%d", r);
	msg = "tell ";
	msg += pvc->tell.sender;
	msg += " called: tunnel " + pvc->tell.msg[0] + "(" + rs + ")";
	Message(msg);
	
      } else {
	
	Message(pvc->tell.msg[0]);
      }
      
      pvc->reset();

    } else if (pvc->state() == Client::INTERNAL_CMD) {

      if (pvc->internal.msg == "analyse") {

	analyse = !analyse;
	cout << "set analyse = " << analyse << endl;
	  
      } else {

	Message(pvc->internal.msg);
      }

      pvc->reset();
      
    } else {

    reset:
      
      pvc->reset();

    }
	  
  } else {

    if (my_socket) 
      da = SocketReceive(Nachricht, block);
    else 
      da = Empf(TO_ME, Nachricht);

    if (da) {

      if (!ParseNumber(Nachricht, &n, &p) || n < 0 || n >= SIGNAL_ANZ) { 
	printf("'%s' ", Nachricht); 
	Error("unknown signal");
      }

      if (n == SIG_BOARD) {

	ok = ParseNumber(p, &Partei, &p) && 
	  (Partei == BLACK || Partei == WHITE);

	ok &= ParseNumber(p, &Restzeit, &p) && Restzeit > 0;

	ok &= ParseNumber(p, &LetzterZug, &p) && 
	  (LetzterZug == ZUG_UNBEKANNT || 
	   LetzterZug == ZUG_PASSEN || 
	   ZUG(LetzterZug));

	Sf = SfNull;

	FOR_SFPOS10 (i) {

	  int j;

	  ok &= ParseNumber(p, &j, &p) && 
	    (j == LEER || j == BLACK || j == WHITE);
	  Sf.p[i] = j;
	}
 

	if (!ok) {
	  printf("'%s' ", Nachricht); 
	  Error("SIG_BOARD message corrupt");
	}


      } else if (n == SIG_GAME) {

	ok = ParseNumber(p, &Partei, &p) && 
	  (Partei == BLACK || Partei == WHITE);

	ok &= ParseNumber(p, &Restzeit, &p);
	ok &= ParseNumber(p, &dummy, &p);    // my move

	if (!ok) {
	  printf("'%s' ", Nachricht); 
	  Error("SIG_GAME message corrupt");
	}

	/*
	  printf("\n%s\n", p);
	*/

	sReadGame(p, &InputGame);

	LetzterZug = ZUG_UNBEKANNT;

	/*
	  printf("SIG_GAME:");

	  fWriteGame(stdout, &InputGame);
	*/

	/*printf("Input-Restzeit:%d\n", Restzeit);*/

	if (Restzeit < 0) {

	  // code: RestZeit = -(100*forcedenddiscnum + forceddepth)

	  pcio->ForcedEndDiscNum = (-Restzeit) / 100;
	  pcio->ForcedDepth      = (-Restzeit) % 100;
	    
	  printf("SIG_GAME: server-mode: fd=%d fedn=%d\n", 
		 pcio->ForcedDepth, pcio->ForcedEndDiscNum);

	  // first three endgame searches only WLD

	  pcio->StopM = pcio->ForcedEndDiscNum;
	  pcio->StopN = pcio->ForcedEndDiscNum+5;

	  Restzeit = 10000;
	}
      }
    }
  } 
  
  
  if (da) {

    switch (n) {

    case SIG_EXIT:

      printf("\nEXIT\n");

      if (LogInOut) { 
	LogTimeStamp(LogFP); fprintf(LogFP, "[SIG_EXIT]\n\n"); fflush(LogFP);
      }

      _abort();
      break;

    case SIG_CLEAR:

      printf("\nCLEAR\n");

      free((char*)pzio->killer.KillBLACK);
      free((char*)pzio->killer.KillWHITE);
      InitKiller(&pzio->killer);
	
      pzio->hash0.clear();
      pzio->hash.clear();
      break;

    case SIG_BREAK:

      printf("\nBREAK\n");

      // reset t4t values

      pzio->cio.t4t_score = 0;

      // reset randomization parameter

#if NEW_BOOK

      if (p_book) {
	int ma = p_book->reset_max();
	cout << ">>> reset max-value to " << ma << endl;
      }

#else

      ResetMax(pLibrary);
      if (pLibrary) printf(">>> Reset max-value to %d\n", pLibrary->curr_max_val);
#endif


      if (LogInOut) { 
	LogTimeStamp(LogFP); 
	fprintf(LogFP, "[SIG_BREAK]\n\n"); fflush(LogFP);
      }

      if (OneGame && GamePlayed) { 
	printf("\n*** game played & SIG_BREAK => bye\n"); 
	exit(0); 
      }

      if (BadExit && BadGame) { 
	printf("*** bad game & SIG_BREAK => bye\n");
	exit(0);
      }

      pcio->state = STATE_WAIT;
      LONGJMP(pcio->states_env);
      break;


    case SIG_BOARD:	

    board_received:


#if 0
    // load random-generators "randomly"

    Zeit(&Z);
    srand(Z.sek);
    sMyRand(Z.sek);
#endif

    // use test_game_komi in test mode

    if (pcio->UseTestGameKomi) {
      printf(">>> use test_game_komi %f\a\n", pcio->test_game_komi);
      pcio->game_komi = pcio->test_game_komi;
    }

    if (pcio->UseTestGameRand) {
      printf(">>> use test_rand %d\a\n", pcio->test_game_rand);
      pcio->game_rand = pcio->test_game_rand;
    }
	
	
    if (SfAnz(&Sf) <= 5) StartTime = (Restzeit+30)/60;


    // subtract communication time

    Restzeit -= my_round(((64 - SfAnz(&Sf))/2 + 1) * EXTERNE_ZEIT + POLSTER);

    if (Restzeit < 3) Restzeit = 3;

    // rand: first move gets more time

    if (pcio->game_rand == SfAnz(&Sf)) {

      printf(">>> rand: (rand=%d #=%d) first move gets more time %d -> %d\a\n",
	     pcio->game_rand, SfAnz(&Sf), Restzeit, 4*Restzeit);
	  
      Restzeit *= 4;
    }


    // update game

    printf("\n");

    UpdateGame(&Game, &Sf, Partei);

    if (Game.MoveNum <= 1) {
      
      // reset randomization parameter when new sequence arrived

#if NEW_BOOK

      if (p_book) {
	int ma = p_book->reset_max();
	cout << ">>> reset max-value to " << ma << endl;
      }
#else

      ResetMax(pLibrary);
      if (pLibrary) printf(">>> Reset max-value to %d\n", pLibrary->curr_max_val);
#endif

    }

    if (LogInOut) {
      LogTimeStamp(LogFP);
      fprintf(LogFP, "[SIG_BOARD]\n"); 
      fSfAus(LogFP, &Sf, 0, 0);
      if (Partei == BLACK) 
	fprintf(LogFP, "BTM"); 
      else 
	fprintf(LogFP, "WTM");
      fprintf(LogFP, ", Time: %d\n\n", Restzeit); fflush(LogFP);
    }
  

    Zeit(&FeldZeit);	// time-stamp of board-message

    if (!pcio->selbst) {

      FOR_SFPOS10(i) if (pcio->Sf.p[i] != Sf.p[i]) break;

#ifdef EIGENE_ZEIT
      i = 0;
#endif

      if (!pcio->vermutet || 
	  i < 89 ||
	  pcio->Partei != Partei) {

	// no or wrong prediction -> new search

	printf("\n>>> wrong or no prediction ...\n");

	// restore old maximum value in case that wrong prediction 
	// caused a max-increase ...

#if NEW_BOOK

	if (p_book) {
	  int ma = p_book->restore_max();
	  cout << ">>> restore max-value to " << ma << endl;
	}
#else
	RestoreMax(pLibrary);  
	if (pLibrary) printf(">>> Restore max-value to %d\n", pLibrary->curr_max_val);
#endif

	pcio->vermutet = false;

	pcio->Sf	 = Sf; 
	pcio->Partei	 = Partei;
	pcio->Restzeit	 = Restzeit;
	pcio->LetzterZug = LetzterZug;
	    
	pcio->selbst = true;
	pcio->state  = STATE_SEARCH;

	LONGJMP(pcio->states_env);

      } else {

	// prediction OK -> continue on own time

	pcio->vermutet = false;

	pcio->selbst = true;

	pcio->Restzeit   = Restzeit;
	pcio->LetzterZug = LetzterZug;

	if (pcio->search_state == S_STATE_LIB)

	  Error("should not reach this statement");

	else {

	  ZugDauer(&pcio->Sf, Restzeit,
		   &pcio->NormDauer, &pcio->MaxDauer);

	  // mirror

	  if (pcio->ForcedTime) 
	    pcio->NormDauer = pcio->MaxDauer = pcio->ForcedTime; 

	}

	if (pcio->search_state == S_STATE_WAIT) {

	  LONGJMP(pcio->swait_env);

	}


	// don't set new starting time -> save time

	{ 
	  ZEIT AktZeit; 

	  Zeit(&AktZeit);


	  printf("Norm=%f Diff=%f Extr=%f\n", 
		 pcio->NormDauer, 
		 ZeitDiff(&AktZeit, &pcio->Startzeit),
		 pcio->DauerExtr);

	  if (pcio->NormDauer + ZeitDiff(&AktZeit, &pcio->Startzeit)
	      < pcio->DauerExtr) { 

	    // printf("\nLOHNT NICHT\n"); fflush(stdout);*/

	    pcio->MaxDauer = 0.0;
	    goto ZeitPruefen;

	  } else {

	    pcio->NormDauer += ZeitDiff(&AktZeit, &pcio->Startzeit);
	    pcio->MaxDauer  += ZeitDiff(&AktZeit, &pcio->Startzeit);

	  } 
	}
      }		  

    } else {

      printf(">>> new board\n");

      pcio->Sf	       = Sf; 	// new task
      pcio->Partei     = Partei;
      pcio->Restzeit   = Restzeit;
      pcio->LetzterZug = LetzterZug;

      if (ZUG(LetzterZug) && pcio->Sf.p[LetzterZug] == LEER) 
	Error("last move corrupt");

      pcio->selbst     = true;
      pcio->state      = STATE_SEARCH;

      LONGJMP(pcio->states_env);
    }
    break;


    case SIG_GAME:

      Game = InputGame;
      PlayGame(Game.MoveNum, &Game, &Sf);
      goto board_received;
      break;

    default: Error("unknown signal");

    }
  }

 ZeitPruefen: ;
  
  if (pcio->state == STATE_SEARCH && 
      (pcio->search_state == S_STATE_ZUGERM || 
       pcio->search_state == S_STATE_LIB)) {

    REAL diff;

    Zeit(&AktuelleZeit);

    diff = ZeitDiff(&AktuelleZeit, &pcio->Startzeit);

    if (pcio->Ausgabe && AnzZ++ >= 3) { 
      AnzZ = 0;
      StatAus(pcio, pzio);
    }

    if (pcio->selbst && pcio->ZugDa && diff >= pcio->MaxDauer) {

      //printf("SIG_TIMEOUT\n");
	
      pcio->timeout = true;
      LONGJMP(pcio->timeout_env);
    }
  }
  
}



static SFPOS Zug, Vermutung;


String LibFile, ParFile, TabFile, StatFile,  
  EvalFile, DrawsFile, SaveFile, rnd_book_file;


int main(int argc, char **argv)
{
  int	 i;
  float  Percentile1, Percentile2, EndCutNiveau;
  char	 s[DATEINAME_MAX+5], Nachricht[NACHRICHT_MAX];
  ZUGIO  zio;
  bool   f_par=false, f_lib=false, f_rnd_book=false;
  bool   f_sel=false, f_qui=false, f_depth=false, f_num=false, f_endniv=false;
  bool   f_tab=false, f_eval=false, f_draws=false, f_iter=false, f_srand=false;
  bool   f_stat=false;
  bool   f_path_rand=false;
  bool   f_priv_disc_max = false;
  bool   f_no_auto_end = false;
  int    earlyend_n = 0;
  int    depth, num, priv_disc_max, HashBits=HASHBITS, HashMaxDepth=HASHMAXDEPTH,
         HashBits0=HASHBITS0, HashMaxDepth0=HASHMAXDEPTH0,
         sdepth=0, stime=0, send=0, StopM=0, StopN=0, max_delta=0;
  float  tfrac = 1.0;

#if NEW_BOOK
  int    bookmode = Book::Mode::RES_FIRST;
#else
  LIBMODE libmode = LIBMODE_RESFIRST;
#endif
  int    argi;
  SPFELD sf;
  BRETT  Brett;


  InitCrt();

  atexit(close_socket);


  if (argc < 2) {

Fehler:

    Error("\
call: oplay\n\nOptions:\n\n\
(id | ios | iosbg | socket) : communication channel or ios/iosbackgr or socket.\n\
[-options file]       : options stored in file\n\n\
search:\n\n\
[-user]               : usertime\n\
[-own]                : use only own time\n\
[-small]              : small but slow\n\
[-sel perc1 [perc2]]  : prob-cut niveau(s)\n\
[-qui]                : quiescence search\n\
[-noautoend]          : disable switch to endgame-search\n\
[-earlyend n]         : start endgame search early by adding n to mid-ply\n\
[-endniv endcutniveau]: endgame cut niveau\n\
[-iter]               : iterative outcome search\n\
[-hash0 bits [maxdepth]]: first levels hashtable-size [maximum depth]\n\
[-hash bits [maxdepth]]: hashtable-size [maximum depth]\n\
[-par parameter-file] : alternative parameters\n\
[-tab table-file]     : alternative table\n\
[-pcstat pcstat-file] : alternative pcstat-file\n\
[-book oko-file \n\
[-alt alternative-file]\n\
[-draws draws-file]\n\
[-bookmode resfirst | devfirst | mix]\n\
[-onegame osp-file]\n\
[-badexit osp-file]\n\
[-badexitwait osp-file]\n\
[-drawbad | -drawgood | -drawbadblack | -drawgoodblack]\n\
[-publicdrawbad | ...]\n\
[-privdiscmax n]\n\
[-depth DepthBound]\n\
[-num GameNumBound]\n\
[-pathrand max-delta]\n\
[-srand ]         : fixed srand\n\
[-rndbook oko-file]   : use random-book\n\
[-sdepth d]           : searchdepth <= d\n\
[-stime  s]           : searchtime  <= s seconds\n\
[-tfrac  f]           : searchtime * f\n\
[-t4t ]               : enable tit-for-tat\n\
[-p2  ]               : enable p2\n\
[-send   n]           : endgame if >= n discs\n\
[-stop m n]           : stop after WLD search (m <= # <= n)\n\
[-exit m n]           : exit after m+rnd(0,1)*n seconds\n\
[-log]                : save messages in id.log\n\n\
ios:\n\n\
[-server s]           : ios hostaddress\n\
[-port   n]           : ios port\n\
[-login  s]           : ios login as s\n\
[-passwd s]           : using password s\n\
[-match [name...]]    : ios match requests\n\
[-noval]              : no evaluation output\n\
[-synchro]            : one request at a time\n\
[-open n]             : n simul games\n\
[-auto]               : auto-accept\n\
[-accept name...]  : only accept these players\n\
[-reject name...]  : reject these players\n\
[-date begin end]  : accept duration (format DD:MM:YYYY:HH)\n\
[-time min max ]   : time range for games\n\
[-test-komi k  ]   : use game-komi k (testing)\n\
[-test-rand k  ]   : use game-rand k (testing)\n");
  }
  
  if (!argv[1] || strlen(argv[1]) > DATEINAME_MAX-5) 
    Error("id not seen or too long");

  if      (!strcmp(argv[1], "ios"))    using_ios = true;
  else if (!strcmp(argv[1], "iosbg"))  using_ios = iosbg = true;
  else if (!strcmp(argv[1], "socket")) my_socket = true;

  // collect options in newars
  
  i = 0;
  
  for (argi=2; argv[argi]; argi++) {
    
    char s[510];
    int j, k;
    FILE *fp;
 
    if (!strcmp(argv[argi], "-options")) {
      
      if (!argv[argi+1]) Error("options file?");
      
      fp = fopen(argv[argi+1], "r");
      if (!fp) Error("can't open options file");
 
      while (!feof(fp)) {

        if (!fgets(s, 500, fp)) break;
        if (s[strlen(s)-1] == '\n') s[strlen(s)-1] = 0;
	
        if (s[0] != '#') {
	  
          if (s[0] != '-') Error("no '-' at beginning of line");
	  
	  // parse line
	  
	  j = 0;
	  
	  while (s[j]) {
	    
            while (s[j] == ' ' || s[j] == '\t') j++;
 
	    if (s[j]) {
	      
	      k = j;
	      while (s[k] && s[k] != ' ' && s[k] != '\t') k++;
 
	      String ns(&s[j], k-j);
	      newargs.push_back(ns);
	      j = k;
            }
          }
        }
      }
      
      fclose(fp);
      argi++;
      continue;
    }
    
    newargs.push_back(String(argv[argi]));
  }

#if 0
  FOR (i, (int)newargs.size()) cout << "'" << newargs[i] << "'" << endl;
#endif

  newargs.push_back(String(""));
  
  FOR (argi, (int)newargs.size()-1) {
    
    String &opt = newargs[argi];
 
#if 0
    cout << "-> " << opt << endl;
#endif
    if (opt == "-login") {
      
      login = newargs[argi+1];
      if (login == "") Error("login?");
      argi++;
 
    } else if (opt == "-passwd") {
      
      passwd = newargs[argi+1];
      if (passwd == "") Error("passwd?");
      argi++;
 
    } else if (opt == "-server") {
      
      server = newargs[argi+1];
      if (server == "") Error("server?");
      argi++;
      
    } else if (opt == "-port") {
      
      if (sscanf(newargs[argi+1].c_str(), "%d", &port) != 1) Error("port?");
      argi++;
      
    } else if (opt == "-open") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &open_val) != 1) Error("open?");
      argi++;

     } else if (opt == "-t4t") {
      
      T4T = true;
      
    } else if (opt == "-p2") {
      
      P2 = true;
      
    } else if (opt == "-auto") {
      
      AutoAccept = true;
      
    } else if (opt == "-accept") {
      
      AcceptCond = true;
      if (newargs[argi+1] == "") Error("accept?");
      
      do {
        argi++;
        AcceptNames.push_back(String(newargs[argi]));
      } while (newargs[argi+1] != "" && newargs[argi+1][0] != '-');
      
      
    } else if (opt == "-reject") {
    
      RejectCond = true;
      if (newargs[argi+1] == "") Error("reject?");
      
      do {
        argi++;
        RejectNames.push_back(String(newargs[argi]));
      } while (newargs[argi+1] != "" && newargs[argi+1][0] != '-');

    } else if (opt == "-date") {

      struct tm tmin, tmax;

      DateCond = true;

      if (newargs[argi+1] == "" || newargs[argi+2] == "" ||
          sscanf(newargs[argi+1].c_str(), "%d:%d:%d:%d", 
		 &tmin.tm_mday, &tmin.tm_mon, &tmin.tm_year, &tmin.tm_hour) != 4 ||
          sscanf(newargs[argi+2].c_str(), "%d:%d:%d:%d",
		 &tmax.tm_mday, &tmax.tm_mon, &tmax.tm_year, &tmax.tm_hour) != 4)
	Error("date?");

      tmin.tm_mon--;
      tmin.tm_year -= 1900;
      tmin.tm_sec = 0;
      tmin.tm_min = 0;
      tmin.tm_isdst = -1;

      tmax.tm_mon--;
      tmax.tm_year -= 1900;
      tmax.tm_sec = 0;
      tmax.tm_min = 0;
      tmax.tm_isdst = -1;

      MinDate = mktime(&tmin);
      MaxDate = mktime(&tmax);

      if (MinDate < 0 || MaxDate < 0) Error("date?");
      if (MinDate > MaxDate) Error("begin > end?");
      argi += 2;

    } else if (opt == "-earlyend") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &earlyend_n) != 1)
        Error("earlyend_n?");
      argi++;

    } else if (opt == "-sdepth") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &sdepth) != 1)
	Error("sdepth?");
      argi++;

    } else if (opt == "-stime") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &stime) != 1)
	Error("stime?");
      argi++;

    } else if (opt == "-tfrac") {

      if (sscanf(newargs[argi+1].c_str(), "%f", &tfrac) != 1 ||
      tfrac <= 0 || tfrac > 1) Error("tfrac?");
      argi++;

    } else if (opt ==  "-send") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &send) != 1) Error("send?");
      argi++;

    } else if (opt == "-stop") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &StopM) != 1) Error("stop m?");
      if (StopM < 20) Error("stop m < 20!");
      argi++;

      if (sscanf(newargs[argi+1].c_str(), "%d", &StopN) != 1) Error("stop n?");
      if (StopN <= StopM) Error("stop m >= n!");
      argi++;

    } else if (opt == "-exit") {

      if (sscanf(newargs[argi+1].c_str(), "%d", &exit_delay) != 1) Error("exit delay?");
      argi++;

      if (sscanf(newargs[argi+1].c_str(), "%d", &exit_rnd_delay) != 1)
	Error("exit rnd delay?");
      argi++;

      if (exit_delay < 0 || exit_delay + exit_rnd_delay <= 0)
	Error("exit_delay < 0 || exit_delay + exit_rnd_delay <= 0");

    } else if (opt == "-book-komi") {

      if (newargs[argi+1] == "" || sscanf(newargs[argi+1].c_str(), "%f", &book_komi) != 1
	  || abs(book_komi) > 1)
	Error("komi?");

      argi++;

    } else if (opt == "-test-komi") {

      if (newargs[argi+1] == "" ||
	  sscanf(newargs[argi+1].c_str(), "%f", &test_game_komi) != 1)
	Error("test_komi?");

      UseTestGameKomi = true;
      argi++;

    } else if (opt == "-test-rand") {

      if (newargs[argi+1] == "" ||
	  sscanf(newargs[argi+1].c_str(), "%d", &test_game_rand) != 1)
	Error("test_rand?");

      UseTestGameRand = true;
      argi++;

    } else if (opt == "-own") {

      OwnTime = true;

    } else if (opt == "-small") {

      small = true;

    } else if (opt == "-noval") {

      NoVal = true;

    } else if (opt == "-synchro") {

      synchro_mode = true;

    } else if (opt == "-user") {

      UseCpuTime = true;

    } else if (opt == "-match") {

      match = true;

    } else if (opt == "-log") {

      if (LogInOut) Error("double -log");

      LogInOut = true;

      if (newargs[argi+1] == "") Error("playm: filename not here");
      String t = newargs[argi+1]; t += LOGSUFFIX;

      if (!(LogFP=fopen(t.c_str(), "a"))) Error("can't open log-file\n");
      argi++;


    } else if (opt == "-sel") {

      f_sel = true;

      if (newargs[argi+1] == "" || sscanf(newargs[argi+1].c_str(), "%f", &Percentile1)
	  != 1) 
        Error("playm: Percentile1 not here");

      argi++;

      if (newargs[argi+1] != "" && sscanf(newargs[argi+1].c_str(), "%f", &Percentile2)
	  == 1) {
	
	argi++;

      } else Percentile2 = Percentile1;

    } else if (opt == "-endniv") {

      f_endniv = true;

      if (newargs[argi+1] == "" || sscanf(newargs[argi+1].c_str(), "%f", &EndCutNiveau)
	  != 1) 
        Error("playm: endcutniveau not here");

      argi++;
 
    } else if (opt == "-hash") {
      
      if (newargs[argi+1] == "" || sscanf(newargs[argi+1].c_str(), "%d", &HashBits) != 1) 
        Error("hashbits?");
	
      argi++;
      
      if (newargs[argi+1] != "" && sscanf(newargs[argi+1].c_str(), "%d", &HashMaxDepth)
	  == 1)
	argi++;
      else
	HashMaxDepth = HASHMAXDEPTH;

    } else if (opt == "-hash0") {

      if (newargs[argi+1] == "" || sscanf(newargs[argi+1].c_str(), "%d", &HashBits0) != 1) 
        Error("hashbits0?");

      argi++;

      if (newargs[argi+1] != "" && sscanf(newargs[argi+1].c_str(), "%d", &HashMaxDepth0)
	  == 1)
	argi++;
      else
	HashMaxDepth0 = HASHMAXDEPTH0;

    } else if (opt == "-qui") {

      f_qui = true;

    } else if (opt == "-noautoend") {

      f_no_auto_end = true;

    } else if (opt == "-iter") {

      f_iter = true;

    } else if (opt == "-par") {

      f_par = true;
      if (newargs[argi+1] == "") Error("playm: -par filename");
      ParFile = newargs[argi+1];
      argi++;

    } else if (opt == "-tab") {

      f_tab = true;
      if (newargs[argi+1] == "") Error("playm: -tab filename");
      TabFile = newargs[argi+1];
      argi++;

    } else if (opt == "-pcstat") {

      f_stat = true;
      if (newargs[argi+1] == "") Error("playm: -pcstat filename");
      StatFile = newargs[argi+1];
      argi++;

    } else if (opt == "-book") {

      f_lib = true;
      if (newargs[argi+1] == "") Error("playm: -book filename");
      LibFile = newargs[argi+1];
      argi++;

    } else if (opt == "-rndbook") {

      f_rnd_book = true;
      if (newargs[argi+1] == "") Error("playm: -rndbook filename");
      rnd_book_file = newargs[argi+1];
      argi++;

    } else if (opt == "-alt") {

      f_eval = true;
      if (newargs[argi+1] == "") Error("playm: -alt filename");
      EvalFile = newargs[argi+1];
      argi++;

    } else if (opt == "-draws") {

      f_draws = true;
      if (newargs[argi+1] == "") Error("playm: -draws filename");
      DrawsFile = newargs[argi+1];
      argi++;

    } else if (opt == "-onegame") {

      OneGame = true;
      if (newargs[argi+1] == "") Error("playm: -onegame filename");
      SaveFile = newargs[argi+1];
      argi++;

    } else if (opt == "-badexit") {

      BadExit = true;
      if (newargs[argi+1] == "") Error("playm: -badexit filename");
      SaveFile = newargs[argi+1];
      argi++;

    } else if (opt == "-badexitwait") {

      BadExitWait = true;
      if (newargs[argi+1] == "") Error("playm: -badexitwait filename");
      SaveFile = newargs[argi+1];
      argi++;

    } else if (opt == "-bookmode") {

      argi++;

      if (newargs[argi] == "") goto Fehler;
      String o = newargs[argi];

#if NEW_BOOK
      if      (o == "resfirst") bookmode = Book::Mode::RES_FIRST;
      else if (o == "devfirst") bookmode = Book::Mode::DEV_FIRST;
      else if (o == "mix")      bookmode = Book::Mode::RES_DEV_MIX;
      else goto Fehler;
#else
      if      (o == "resfirst") libmode = LIBMODE_RESFIRST;
      else if (o == "devfirst") libmode = LIBMODE_DEVFIRST;
      else if (o == "add")      libmode = LIBMODE_ADD;
      else goto Fehler;
#endif

    } else if (opt == "-drawbad") {

      draw_bad = true;

    } else if (opt == "-drawgood") {

      draw_good = true;

    } else if (opt == "-drawbadblack") {

      draw_bad_black = true;

    } else if (opt == "-drawgoodblack") {

      draw_good_black = true;

    } else if (opt == "-publicdrawbad") {

      public_draw_bad = true;

    } else if (opt == "-publicdrawgood") {

      public_draw_good = true;

    } else if (opt == "-publicdrawbadblack") {

      public_draw_bad_black = true;

    } else if (opt == "-publicdrawgoodblack") {

      public_draw_good_black = true;

    } else if (opt == "-privdiscmax") {

      f_priv_disc_max = true;
      argi++;

      if (newargs[argi] == "" ||sscanf(newargs[argi].c_str(), "%d", &priv_disc_max) != 1|| 
	  priv_disc_max < 4 || priv_disc_max > 63) goto Fehler;

    } else if (opt == "-srand") {

      f_srand = true;

    } else if (opt == "-depth") {

      f_depth = true;
      argi++;

      if (newargs[argi] == "" || sscanf(newargs[argi].c_str(), "%d", &depth) != 1 || 
	  depth < 4 || depth > 63) goto Fehler;

    } else if (opt == "-num") {

      f_num = true;
      argi++;

      if (newargs[argi] == "" || sscanf(newargs[argi].c_str(), "%d", &num) != 1 || 
	  num < 1) goto Fehler;

    } else if (opt == "-pathrand") {

      f_path_rand= true;
      argi++;

      if (newargs[argi] == "" || sscanf(newargs[argi].c_str(), "%d", &max_delta) != 1 || 
	  max_delta <= 0 || max_delta > 20) goto Fehler;

    } else goto Fehler;
  }


  if (f_eval  && !f_lib) Error("-alt only with -book");
  if (f_draws && !f_lib) Error("-draws only with -book");


  cout << "\n\nLOGISTELLO 0.99.1 by Michael Buro ["__DATE__"]" << endl << endl;


  if (f_tab) TableFile = TabFile;


#ifdef EVAL_A

  if (small) {

    cout << "[ EVAL_A-slow ]" << endl;
 InitZug(&zio, EvalASlow, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);
 
  } else {
    
    cout << "[ EVAL_A-fast ]" << endl;
 InitZug(&zio, EvalA, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);
  }

#if 0
  if (f_cut) {
    cout << "[ futility cutoffs ]" << endl;
    zio.Cut = true;
    zio.EvalCut = EvalACut;
  }
#endif

  PCStatFile    = "pcstata";
  EndPCStatFile = "endcutstata";

#endif

#ifdef EVAL_D
  cout << "[ EVAL_D ]" << endl;
InitZug(&zio, EvalD, SignalCheck, HashBits, HashMaxDepth);
#endif
 
#ifdef EVAL_L
  if (small) {

    cout << "[ EVAL_L-slow ]" << endl;
    InitZug(&zio, EvalLSlow, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);

  } else {

    printf("[ EVAL_L-fast ]\n");
    InitZug(&zio, EvalL, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);

  }

  PCStatFile = "pcstatl";
  EndPCStatFile = "endcutstatl";

#endif

#ifdef EVAL_B

  printf("[ EVAL_B ]\n");
  InitZug(&zio, EvalB, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);

  strcpy(PCStatFile,    "pcstatb");
  strcpy(EndPCStatFile, "endcutstatb");

#endif


#ifdef EVAL_K

  if (small) {
    
    cout << "[ EVAL_K-slow ]" << endl;
    InitZug(&zio, EvalKSlow, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);

  } else {

    cout << "[ EVAL_K-fast ]" << endl;
    InitZug(&zio, EvalK, SignalCheck, HashBits, HashMaxDepth, HashBits0, HashMaxDepth0);

  }

  PCStatFile    = "pcstatk";
  EndPCStatFile = "endcutstatk";

#endif

#ifdef EVAL_MAX
  cout << "[ EVAL_MAX ]" << endl;
  InitZug(&zio, BewDiff, SignalCheck, HashBits, HashMaxDepth);
#endif

#ifdef EVAL_ANTI
  cout << "[ EVAL_ANTI ]\n" << endl;
  InitZug(&zio, AntiEval, SignalCheck, HashBits, HashMaxDepth);
#endif

  if (f_par) ParameterFile = ParFile;
  if (f_tab) TableFile = TabFile;
  
  if (f_stat) {
    PCStatFile = StatFile;
    cout << "[ pcstat-file: " << PCStatFile << " ]" << endl;
  }

/*
printf("[ parameter-file: '%s' ]\n", ParameterFile);
  printf("[ table-file: '%s' ]\n", TableFile);
*/


  if (using_ios)
    cout << "[ login: " << login << " passwd: " << passwd << " ]" << endl;
  
  cout << "[ hash0: "		    << HashBits0
				    << " (" << HashMaxDepth0
				    << "), hash: " << HashBits << " ("
				    << HashMaxDepth << ") ]"
				    << endl;
  
  if (stime || sdepth || send || tfrac != 1.0) {
    
    zio.cio.ForcedTime = stime; 
    zio.cio.ForcedDepth= sdepth;
    zio.cio.ForcedEndDiscNum = send;
    zio.cio.TimeFrac = tfrac;
    
 form(cout, "[ ForcedTime:%d ForcedDepth:%d ForcedEndDiscNum:%d TimeFrac=%.1f ]\n",
      stime, sdepth, send, tfrac);
  }  

  if (f_no_auto_end) {
    zio.cio.NoAutomaticEndgame = f_no_auto_end;
    printf("[ no automatic endgame-search ]\n"); 
  }

  if (earlyend_n != 0) {
    zio.cio.EarlyEndN = earlyend_n;
    printf("[ early end: depth+%d ]\n", earlyend_n); 
  }

  if (DateCond) {
    time_t t;
    time(&t);
    struct tm *loctime = localtime(&t);
    t = mktime(loctime);
    
    printf("[ begin: %ld end: %ld current: %ld ]\n", MinDate, MaxDate, t);
  }

  if (StopM) {
    zio.cio.StopM = StopM; 
    zio.cio.StopN = StopN;
    printf("[ stop: %d <= # <= %d ]\n", StopM, StopN);
  }


  if (UseCpuTime) printf("[ usertime ]\n");

  if (OwnTime) printf("[ use only own time ]\n");

  if (OneGame) printf("[ only one game ]\n");

  if (BadExit) printf("[ bad game => exit ]\n");

  if (T4T) {
    printf("[ Tit-For-Tat ]\n");
    zio.cio.t4t = true;
    if (!zio.cio.ForcedDepth) Error("no ForcedDepth!");
  }

  if (P2) {
    printf("[ P2 ]\n");
    zio.cio.p2 = true;
    if (!zio.cio.ForcedDepth) Error("no ForcedDepth!");
  }

  if (AutoAccept) {

    if (AcceptCond) {
      cout << "[ accept: ";
      FOR (i, (int)AcceptNames.size()) cout << AcceptNames[i] << " ";
      cout << "]" << endl;
    }

    if (RejectCond) {
      cout << "[ reject: ";
      FOR (i, (int)RejectNames.size()) cout << RejectNames[i] << " ";
      cout << "]" << endl;
    }
  }

  if (LogInOut) cout << "[ logfile: " << s << " ]" << endl;

  if (f_sel) {
    form(cout, "[ prob-cut niveaus %.2f %.2f ]\n", Percentile1, Percentile2);
    zio.cio.Selective   = true;
    zio.cio.Percentile1 = Percentile1;
    zio.cio.Percentile2 = Percentile2;
  }

  if (f_endniv) {
    form(cout, "[ endgame cut niveau %.2f ]\n", EndCutNiveau);
    zio.cio.EndCutNiveau = EndCutNiveau;
  }

  if (f_qui) {
    cout << "[ quiescence search ]" << endl;
    zio.cio.Quiescence = true;
  }

  if (f_iter) {
    cout << "[ iterative outcome search ]" << endl;
    zio.cio.IterOutcome = true;
  }

  form(cout, "[ book-komi %.2f ]\n", book_komi);

  if (UseTestGameKomi) {
    printf("[ test-game-komi %f ]\n", test_game_komi);
    zio.cio.UseTestGameKomi = true;
    zio.cio.test_game_komi = test_game_komi;
  }
        
  if (UseTestGameRand) {
    printf("[ test-game-rand %d ]\n", test_game_rand);
    zio.cio.UseTestGameRand = true;
    zio.cio.test_game_rand = test_game_rand;
  }
        
  InitCompzug();

  if (f_srand) {

    int t= 11;

    printf("[ fixed seed=%d ]\n", t);
    srand(t); sMyRand(t); srandom(t);

  } else {    

    time_t t;

    time(&t);

    srand(t); sMyRand(t); srandom(t);

    printf("[ time seed=%ld ]\n", t);
  }


  if (my_socket) socket_connect(server, port, login);

  if (exit_delay + exit_rnd_delay > 0) {
    exit_delay += my_round(FRAN * exit_rnd_delay);
     if (exit_delay <= 0) exit_delay = 1;
    cout << "[ exit after " << exit_delay << " seconds ]" << endl;
  }


  if (using_ios) {

    // try to connect to IOS before doing lib-stuff

    pvc = new OthClient(server, port, login, passwd);
 
    if (iosbg) pvc->close_input();

    Message("vt100 -");
// Message("verbose -");
    Message("notify - *");
    Message("notify + /os");
    Message("notify +");    
    Message("chann - .chat .help");

  } else {

    sprintf(TO_ME,   "%s"TO_PRAEFIX,   argv[1]);
    sprintf(FROM_ME, "%s"FROM_PRAEFIX, argv[1]);

    zio.cio.id = argv[1];

    // cancel old message

    if (my_socket) {
      // SocketReceive(TO_ME, Nachricht);
    } else {
      Empf(TO_ME, Nachricht);  
    }
  }

  // Init Game

  Game.MoveNum  = 0;
  Game.Moves[0] = 0;


  // force input of parameters and tables 

  SfGrund(&sf);

  // sf.p[A1] = sf.p[A2] = sf.p[A3] = sf.p[B1] = sf.p[B2] = sf.p[B3] = 
  // sf.p[C1] = sf.p[C2] = WHITE;

  SfBrett(&sf, &Brett);
  zio.BewMitte(&Brett, BLACK);


  if (f_lib) { 

#if NEW_BOOK

    p_book = new Book;

    p_book->create_from_files(LibFile, EvalFile, DrawsFile);

    if (f_depth)         p_book->set_depth_bound(depth);
    if (f_num)           p_book->set_game_num_bound(num);
    if (f_priv_disc_max) p_book->set_priv_disc_max(priv_disc_max);

    p_book->set_max_delta(max_delta);
    p_book->set_path_randomization(f_path_rand);

    p_book->set_mode(bookmode);

    set_draw_modes(p_book);
      
    printf("[ book: mode=");

    if      (p_book->get_mode() == Book::Mode::RES_FIRST)   printf("resfirst ");
    else if (p_book->get_mode() == Book::Mode::DEV_FIRST)   printf("devfirst ");
    else if (p_book->get_mode() == Book::Mode::RES_DEV_MIX) printf("mix ");
    else Error("unknown book-mode");

    switch (p_book->get_draw_mode()) {
    case Book::DrawMode::NORMAL:
      break;
    case Book::DrawMode::BAD:
      cout << "drawbad";
      break;
    case Book::DrawMode::GOOD:
      cout << "drawgood";
      break;
    case Book::DrawMode::BAD_FOR_BLACK:
      cout << "drawbadblack";
      break;
    case Book::DrawMode::GOOD_FOR_BLACK:
      cout << "drawgoodblack";
      break;
    default: Error("case1");
    }

    cout << " ";

    switch (p_book->get_public_draw_mode()) {
    case Book::DrawMode::NORMAL:
      break;
    case Book::DrawMode::BAD:
      cout << "publicdrawbad";
      break;
    case Book::DrawMode::GOOD:
      cout << "publicdrawgood";
      break;
    case Book::DrawMode::BAD_FOR_BLACK:
      cout << "publicdrawbadblack";
      break;
    case Book::DrawMode::GOOD_FOR_BLACK:
      cout << "publicdrawgoodblack";
      break;
    default: Error("case2");
    }

    if (p_book->get_path_randomization())
      printf("path-randomization(%d) ", p_book->get_max_delta());

    if (p_book->get_public_draw_mode() != Book::DrawMode::NORMAL)
      cout << "priv-disc-max(" << p_book->get_priv_disc_max() << ") ";

    cout << "]" << endl;

#else 

    pLibrary = MakeLibrary(LibFile, EvalFile, DrawsFile);
    if (f_depth) pLibrary->DepthBound   = depth;
    if (f_num)   pLibrary->GameNumBound = num;
    if (f_priv_disc_max) pLibrary->PrivDiscMax = priv_disc_max;

    pLibrary->max_delta = max_delta;
    pLibrary->path_randomization = f_path_rand;

    pLibrary->Mode           = libmode;
    pLibrary->DrawBad        = draw_bad;
    pLibrary->DrawGood       = draw_good;
    pLibrary->DrawBadBlack   = draw_badblack;
    pLibrary->DrawGoodBlack  = draw_goodblack;

    pLibrary->PublicDrawBad        = public_draw_bad;
    pLibrary->PublicDrawGood       = public_draw_good;
    pLibrary->PublicDrawBadBlack   = public_draw_bad_black;
    pLibrary->PublicDrawGoodBlack  = public_draw_good_black;
     
    printf("[ lib: mode=");

    if      (pLibrary->Mode == LIBMODE_RESFIRST) printf("resfirst ");
    else if (pLibrary->Mode == LIBMODE_DEVFIRST) printf("devfirst ");
    else if (pLibrary->Mode == LIBMODE_ADD)      printf("add ");
    else Error("unknown library mode");
    
    if (pLibrary->DrawBad )           printf("drawbad ");       
    if (pLibrary->DrawGood)           printf("drawgood ");      
    if (pLibrary->DrawBadBlack)       printf("drawbadblack ");  
    if (pLibrary->DrawGoodBlack)      printf("drawgoodblack "); 

    if (pLibrary->PublicDrawBad )     printf("publicdrawbad ");       
    if (pLibrary->PublicDrawGood)     printf("publicdrawgood ");      
    if (pLibrary->PublicDrawBadBlack) printf("publicdrawbadblack ");  
    if (pLibrary->PublicDrawGoodBlack)printf("publicdrawgoodblack "); 

    if (pLibrary->path_randomization)
      printf("path-randomization(%d) ", pLibrary->max_delta);

    if (pLibrary->PublicDrawBad || pLibrary->PublicDrawGood ||
        pLibrary->PublicDrawBadBlack || pLibrary->PublicDrawGoodBlack)
      printf("priv-disc-max(%d) ", pLibrary->PrivDiscMax);


    printf("]\n");

#endif

    if (draw_bad + draw_good + draw_bad_black + draw_good_black > 1)
      Error("more than one draw-option");

    if (public_draw_bad + public_draw_good + 
	public_draw_bad_black + public_draw_good_black > 1)
      Error("more than one publicdraw-option");

  } else { 

#if NEW_BOOK

    p_book = 0;

#else

    pLibrary = 0;

#endif

  }

  if (f_rnd_book) {
    p_rnd_book = new Book();
    p_rnd_book->create_from_files(rnd_book_file, "", "");
    p_rnd_book->set_rnd_book(true);
  }

  cout << endl << "OK" << endl;


#if 0
{ int i, j;
SPFELD sf;
  SFPOS  Zuege[60];
  ZUGDAT1 ZugDat[60];


  SfGrund(&sf);

  SfSetzen(&sf, BLACK, D3); 

  j = ZuegeAusSpielen(WHITE, &sf, Zuege, ZugDat, 0.8);

  printf("--- %d\n", j);

  FOR (i, j) printf("w=%d\n", ZugDat[i].MinMax);

  exit(1);
}
#endif



  zio.cio.state = STATE_WAIT;

  SETJMP(zio.cio.states_env);


  FOREVER {

    switch (zio.cio.state) {


// ***************** wait for signal *********************

      case STATE_WAIT:

        zio.cio.selbst   = false;
        zio.cio.vermutet = false;

	if (my_socket) {

#if BLOCK
	  SignalCheck(&zio.cio, &zio, true, true);
#else
	  SignalCheck(&zio.cio, &zio, true, false);
	  SLEEP(1);
#endif

	} else {

	  SignalCheck(&zio.cio, &zio, true);
	  SLEEP(1);
	  if (/*ios &&*/ match) MatchRequest();

	  if (using_ios) PingServer();
	  
	}

	break;


// ************************* compute move *********************

      case STATE_SEARCH:	

// zio.cio.search_state = S_STATE_ZUGERM;

	Zug = ZUG_UNBEKANNT;


	if (p_rnd_book && Game.MoveNum) {

	  int     Value;
	  ValType ValType;
	  NewGame ngame;

	  ngame.from_old(Game);

	  Zug = p_rnd_book->find_move(ngame, zio.cio.Partei, Value, ValType);

	  if (Zug != ZUG_UNBEKANNT) {

	    printf(">>> rndbook-move: "); KoorAus(Zug); printf("\n");

	    zio.cio.IterPath[0]   = Zug; 
            zio.cio.IterPathLen   = 1;
            zio.cio.IterPathValue = Value;
            zio.cio.IterPathType  = ValType;
	  }
	}


	if (Zug == ZUG_UNBEKANNT && (pLibrary || p_book) && Game.MoveNum) {

	  int     Value;
	  ValType ValType;
	  NewGame ngame;

	  ngame.from_old(Game);

#if NEW_BOOK

	  Zug = p_book->find_move(ngame, zio.cio.Partei, Value, ValType);
#else

	  Zug = 
	    NewGameTreeMove(pLibrary, ngame, &zio, &Value, &ValType);

#endif

	  if (Zug != ZUG_UNBEKANNT) {

	    printf(">>> book-move: "); KoorAus(Zug); printf("\n");

	    zio.cio.IterPath[0]   = Zug; 
            zio.cio.IterPathLen   = 1;
            zio.cio.IterPathValue = Value;
            zio.cio.IterPathType  = ValType;
	  }
	}


	if (Zug == ZUG_UNBEKANNT) {

	  Zug = CompZug(&zio);	// compute move

	}




// printf("Ende selbst=%d\n", zio.cio.selbst);

	if (!zio.cio.selbst) {

	  zio.cio.search_state = S_STATE_WAIT;

	  if (!SETJMP(zio.cio.swait_env)) {

	    printf(">>> wait\n");

	    if (my_socket) {

#if BLOCK
	      SignalCheck(&zio.cio, &zio, true, true);
#else
	      FOREVER {
		SignalCheck(&zio.cio, &zio, true, false);
		SLEEP(1);
	      }
#endif

	    } else {

	      FOREVER {
		SignalCheck(&zio.cio, &zio, true);
		SLEEP(1);
		if (/*ios &&*/ match) MatchRequest();
	      }

	    }
	  }

          // here, if guess was OK

	} 

/*printf("Zug=%d g_it=%d g_da=%d g_v=%d g_fe=%d\n",
        Zug, zio.cio.IterVarPos, zio.cio.ZugDa, zio.cio.vermutet, zio.fertig);*/

	printf("\ncontinuation: "); 
	FOR (i, zio.cio.IterPathLen) { 
	  KoorAus(zio.cio.IterPath[i]); printf(" "); 
	}
	printf("\n");


// send move

#if NEW_BOOK

	if (p_book) {
	  int ma = p_book->set_old_max();  // prevents setting to -infty in case of -own
	  cout << ">>> set old-max to " << ma << endl;
	}

#else
	SetOldMax(pLibrary);  // prevents setting to -infty in case of -own
        if (pLibrary) printf(">>> set old-max to %d\n", pLibrary->old_max_val);
#endif


	printf("val= %d type= %d\n", zio.cio.IterPathValue, zio.cio.IterPathType);

	printf("discs= %d val1= %.3f val2= %.3f\n",
	       SfAnz(&zio.cio.Sf),
	       WERT_TO_REAL(zio.cio.val1) * DISCFAKTOR,
	       WERT_TO_REAL(zio.cio.val2) * DISCFAKTOR
	       );
	
		
	if (using_ios) {

	  if (zio.cio.t4t) sleep(2);  // be nice to human opponents
	  
	  ZEIT AktZeit;
	  String move;
	  real4 eval;
	  real4 time;
	  
	  if (Zug == ZUG_PASSEN)
	    move = "pass";
	  else {
	    char s[100];
	    sKoorAus(s, Zug);
	    move = s;
	  }
	  
	  if (zio.cio.IterPathType == MIDGAME) {
	    
	    if (zio.cio.IterPathValue >= WERTGEWINN)
	      
	      eval = zio.cio.IterPathValue - WERTGEWINN;
	    
	    else if (zio.cio.IterPathValue <= -WERTGEWINN) 
	      
	      eval = zio.cio.IterPathValue + WERTGEWINN;
	    
	    else {

	      if (zio.cio.game_rand > 0 &&
		  zio.cio.game_rand == SfAnz(&zio.cio.Sf)) {

		// first move in rand game:
		// compensate for evaluation error (under/overestimate): add avg*0.2
		// and report nearest 2k

		float val = WERT_TO_REAL(zio.cio.Average)*10.0;

		eval = ADJ_KOMI(val);
		printf(">>> V_ORIG= %f V_ADJ= %f (should be %f)\n", 
		       val,
		       eval,
		       (float)(my_round(val*1.33*0.5)*2));

	      } else if (SfAnz(&zio.cio.Sf) == 4) {

		eval = book_komi;
		
	      } else {

		eval = WERT_TO_REAL(zio.cio.IterPathValue)*10.0;

	      }
	    }

	  } else if (zio.cio.IterPathType == ENDGAME)
	    
	    eval = zio.cio.IterPathValue;
	  
	  else 
	    
	    eval = ((float)zio.cio.IterPathValue)/MIX_CONST;
	  
	  
	  Zeit(&AktZeit);
	  
	  time = ZeitDiff(&AktZeit, &FeldZeit);
	  
	  //  printf("########## value=%f (%d)\n", v_move.value, zio.cio.IterPathValue);
	  
	  
	  // if (NoVal) eval = 0;   // not working because of komi games

	  char s[5000];

	  sprintf(s, "tell /os play %s %s/%.3f/%.3f",
		  game_id.c_str(),
		  move.c_str(),
		  eval,
		  time);
	    
	  Message(s);
	  
	} else {
	  
	  {
	    ZEIT AktZeit;
	    
	    Zeit(&AktZeit);
	    
	    if (my_socket) {
	      SocketSendMOVE(Zug, ZeitDiff(&AktZeit, &FeldZeit), 0);
	    } else {
	      SendMOVE(FROM_ME, Zug, my_round(ZeitDiff(&AktZeit, &FeldZeit)));
	    }
	  }
	}
	

	if ((zio.cio.t4t || zio.cio.p2) && zio.cio.t4t_updated) {
	  
	  String str = "tell /os tell ";
	  str += game_id; str += " "; str += zio.cio.t4t_msg;
	  
	  if (using_ios) Message(str);
	  
	  cout << str << endl;
	  zio.cio.t4t_updated = false;
	  zio.cio.t4t_msg[0] = 0;
	}
	
	
	if (LogInOut) {
	  LogTimeStamp(LogFP);
	  fprintf(LogFP, "[SendMOVE] "); 
	  fKoorAus(LogFP, Zug); fprintf(LogFP, "\n\n");
	  fflush(LogFP);
	}

	if (ZUG(Zug)) {			// make move

	  if (!SfSetzen(&zio.cio.Sf, zio.cio.Partei, Zug)) 
            Error("Zug geht nicht xxx");

	}

	UpdateGame(&Game, &zio.cio.Sf, GEGNER(zio.cio.Partei));


	// save game when it is finished or has one empty square

	{ SPFELD Board;
	  FILE   *fp;
	  SFPOS  Moves[65];
	  int    Player;
	  GAME   GameCpy=Game;

	  Player = PlayGame(GameCpy.MoveNum, &GameCpy, &Board);

	  if (!Player) printf(">>> Game End\n");

	  if (Player && GameCpy.MoveNum == 59) {

	    // complete game: no board expected with one empty and move possible

	    printf(">>> complete game\n");
	    
	    SfAus(&Board, -1, Player);
	    printf("%d\n", Player);
	    
	    if (SfMoeglZuege(&Board, Player, Moves)) {

	      GameCpy.Moves[GameCpy.MoveNum++] = SMOVE_GEN(Moves[0], Player);
	      GameCpy.Moves[GameCpy.MoveNum]   = 0;

	    } else printf(">>> no moves?\n");   

	  }

	  if (!PlayGame(GameCpy.MoveNum, &GameCpy, &Board)) {

	    printf(">>> GAME ENDED ...\n");
	    
	    // game end -> append game to GAMEFILE if it doesn't exist

	    UniqueGame(&GameCpy);

#if 0

	    if (pLibrary && !AppendGameToLibrary(pLibrary, &GameCpy)) 

	      printf(">>> game exists\n");

	    else {

	      if (pLibrary) {

		int dummy; 

	        printf(">>> MiniMax\n");
		EvalLibNode(pLibrary, pLibrary->pRoot, &dummy);

	      }

	      printf(">>> append game\n");

	      fp = fopen(GAMEFILE, "a");

	      if (fp) {

		SPFELD tab;

		tab.Marke = BLACK;
	        Game2Tab(&GameCpy, &tab);
    	        fTabAus(fp, &tab);
	        fprintf(fp, "\n%d min\n\n", StartTime);

	        fclose(fp);

	      } else printf(">>> can't open GAMEFILE\n");
	    }

#else

#if SAVE_GAME
	    printf(">>> append game\n");

	    fp = fopen(GAMEFILE, "a");

	    if (fp) {

	      SPFELD tab;

	      tab.Marke = BLACK;
	      Game2Tab(&GameCpy, &tab);
    	      fTabAus(fp, &tab);
	      fprintf(fp, "\n%d min (%d-%d)\n\n", StartTime, 
			SfAnzBLACK(&Board), SfAnzWHITE(&Board));

	      fclose(fp);

	    } else printf(">>> can't open GAMEFILE\n");
#endif

	    GamePlayed = true;

	    if (BadExit || BadExitWait) {

	      int diff=SfAnzBLACK(&Board)-SfAnzWHITE(&Board);

	      if ((zio.cio.Partei == BLACK && diff <= 0) ||
	          (zio.cio.Partei == WHITE && diff >= 0)) {

		BadGame = true;

		if (BadExitWait && using_ios) {
		  Message("tell /os open 0");  // no new games
		}
	      }
	    }

	    // save game for further processing if it was bad or the only one

	    if (BadGame || OneGame) {

	      SPFELD tab;

	      printf(">>> append game to file '%s'\n", SaveFile.c_str());

	      fp = fopen(SaveFile.c_str(), "a");
	      if (fp) {
		tab.Marke = BLACK;
		Game2Tab(&GameCpy, &tab);
		fTabAus(fp, &tab);
		fprintf(fp, "\n%d min (%d-%d)\n\n", StartTime, 
			SfAnzBLACK(&Board), SfAnzWHITE(&Board));

		fclose(fp);
	      } else printf(">>> can't append to SAVEFILE\n");
	    }
	  }
#endif

	}


        // adjust time

	{ ZEIT AktZeit;
	  Zeit(&AktZeit);
	  printf("%.2f sec used\n", ZeitDiff(&AktZeit, &FeldZeit));

	  zio.cio.Restzeit -= ZeitDiff(&AktZeit, &FeldZeit);
	  Zeit(&zio.cio.Startzeit);
	}


	if (OwnTime) { 

	  zio.cio.vermutet = false; 
	  zio.cio.state    = STATE_WAIT;
	  break;
	}


	if (USE_COUNTERMOVE && 
	    zio.cio.IterPathLen > 1) {	// guessed => move and calculations
					// on opponent's time
	  zio.cio.vermutet = true;		

	  Vermutung = zio.cio.IterPath[1];	// "best" countermove

	} else {			// no countermove -> predicting

	  printf(">>> predicting ...");

	  zio.cio.Partei = GEGNER(zio.cio.Partei);

	  Vermutung = ZUG_UNBEKANNT;	// not from book

	  if (Vermutung == ZUG_UNBEKANNT) {

	    REAL R = zio.cio.Restzeit;

	    if (R > 1.0) {

	      zio.cio.Restzeit /= 10.0;

	    } else {

	      zio.cio.Restzeit = 1.0;

	    }

	    printf(" %.2f\n", zio.cio.Restzeit);

	    Vermutung = CompZug(&zio);		// determine move

	    zio.cio.Restzeit = R;

	  } else { 

	    printf(">>> libmove: "); KoorAus(Vermutung); printf("\n"); 
	  }

	  zio.cio.Partei = GEGNER(zio.cio.Partei);

	}
  
	if (ZUG(Vermutung)) {

	  printf(">>> "); KoorAus(Vermutung); printf(" predicted\n");

	  if (!SfSetzen(&zio.cio.Sf, GEGNER(zio.cio.Partei), Vermutung)) {
		
	    printf("*** illegal move predicted 2\n");

	    zio.cio.vermutet = false;
	    zio.cio.state    = STATE_WAIT;
	    break;
	  }

	  UpdateGame(&Game, &zio.cio.Sf, zio.cio.Partei);

	  ZugDauer(&zio.cio.Sf, zio.cio.Restzeit, &zio.cio.NormDauer,
		     &zio.cio.MaxDauer);

          // mirror

          if (zio.cio.ForcedTime) 
	    zio.cio.NormDauer = zio.cio.MaxDauer = zio.cio.ForcedTime; 

	  zio.cio.LetzterZug = Vermutung;

	  zio.cio.selbst   = false;	// from now on opponent's time
	  zio.cio.vermutet = true;

	} else {

	  printf(">>> no prediction!\n");
	  zio.cio.vermutet = false;
	  zio.cio.state    = STATE_WAIT;
	}
	
        break;


/**************** other states ****************************************/

      default:
	Error("unknown state\n"); break;
    }
  }

  return 1;
} 


#if 0
#define b(t,x,o) (((const t *) _nl_C_LC_CTYPE_##x) + o)
 
const unsigned short int *__ctype_b = b (unsigned short int, class, 128);

//compat_symbol (libc, __ctype_b, __ctype_b, GLIBC_2_0);
#endif

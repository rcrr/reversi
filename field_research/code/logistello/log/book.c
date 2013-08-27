// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// book routines / 12.93, 3.94, 6.96, 2.97

#include "main.h"
#include "book.h"
#include "crt.h"
#include "goodies.h"
#include "trans.h"
#include "killer.h"
#include "playm.h"
#include "pmove.h"
#include "game.h"
#include "newgame.h"


#define TEST		false

const int MAX_LIB_DISCS = 64;
const int MAX_ALT_DISCS	= 38;   // was 39
const int FACTOR  	= 100;  // values are multiplied by FACTOR to allow
                                // small adjustments
const int SURE_VALUE    = 51*FACTOR;    // larger than any heuristic evaluation
const int DIFF_ALT      = -100*FACTOR;  // smaller than any normal result-value

const int GOOD_VAL      = 10*FACTOR;    // threshold for adding constant in mixed mode
const int GOOD_DELTA    = 10*FACTOR;    // additive constant


const int MOVE_POOL_SIZE = 50000;


class MoveData {

public:

  int    move;
  int    h_val;      // heuristic value  [-50..50] = 0..100%   *FACTOR
  int    e_val;      // endgame value    [-64..64]             *FACTOR

  bool   public_draw;
  int    game_num;
  int    path_num;   // for path-randomization


  friend ostream & operator << (ostream &os, MoveData &md) 
  {
    os << "[";
    char s[500];
    sKoorAus(s, md.move);
    os << s;
    os << " e=" << md.e_val
       << " h=" << md.h_val
       << " n=" << md.game_num
       << "] ";
    return os;
  }
};


// result first / deviation has the lowest priority!

int compSGNMoveData(const void *a, const void *b)
{
  int sa, sb, d, ea, eb;

  ea = ((MoveData*) a)->e_val;
  if (ea == DIFF_ALT) sa = -2;
  else if (ea > 0)    sa = 1;
  else if (ea < 0)    sa = -1;
  else                sa = 0;

  eb = ((MoveData*) b)->e_val;
  if (eb == DIFF_ALT) sb = -2;
  else if (eb > 0)    sb = 1;
  else if (eb < 0)    sb = -1;
  else                sb = 0;

  d = sb - sa;

  if (d) return d;

  d = ((MoveData*) b)->h_val - ((MoveData*) a)->h_val; 

  if (d) return d;

  // if signs of modified difference & chances are equal use difference
 
  return eb - ea;
}



// chance first

int compSGNMoveData2(const void *a, const void *b)
{
  int  d;

  d = my_round(((float)((MoveData*) b)->h_val)/FACTOR) - 
    my_round(((float)((MoveData*) a)->h_val)/FACTOR);

  if (d) return d;

  return ((MoveData*) b)->e_val - ((MoveData*) a)->e_val;
}


/***********  book stuff   ****************/


// allocate new node

BookNode::BookNode(int sn)
{
  if (sn < 0) Error("BookNode: sn < 0");

  son_num = sn;

  if (sn == 0) sn = 1;

  // allocate movenodes for sons

  sons = (MoveNode*) malloc(sn * sizeof(MoveNode));

  if (!sons) Error("BookNode: no mem");

  //  FOR (i, son_num) sons[i].is_new(true);
}




void Book::init_move_node(MoveNode &mn, NewGame &game, int move_index)
{
  mn.value = game.get_value();
  mn.set_flags(0);  // LIB_NEW?

  if (game.is_alternative()) mn.is_alternative(true);
  if (game.is_finished())    mn.is_finished(true);

  mn.next = 0;
  mn.move_num = game.get_move_num()-move_index;

  if (move_pool_num + mn.move_num >= MOVE_POOL_SIZE) {

    move_pool = (PackedMove*) malloc(MOVE_POOL_SIZE * sizeof(PackedMove));
    if (!move_pool) Error("init_move_node: no memory");

    move_pool_num = 0;
  }

  mn.moves = &move_pool[move_pool_num];
  move_pool_num += mn.move_num;
  memcpy(mn.moves, &game.get_pm(move_index), mn.move_num);
}




// allocate new book

void Book::init()
{
  p_root = new BookNode(0);

  games = 0;
  game_num = 0;

  move_pool = 0;
  move_pool_num = MOVE_POOL_SIZE;

  depth_bound    = 64;
  game_num_bound = 1;

  set_mode(Mode::RES_FIRST);

  set_draw_mode(DrawMode::NORMAL);
  set_public_draw_mode(DrawMode::NORMAL);

  black_offset = public_black_offset = 0;

  set_priv_disc_max(100);

  set_path_randomization(false);
  set_max_delta(0);

  set_rnd_book(false);
  reset_max();
}


Book::Book() { init(); }


// return false <=> game exists

bool Book::append_game(BookNode &bn, NewGame &game, int move_index)
{
  int i, j;

  if (move_index >= game.get_move_num()) return false;

  FOR (i, bn.son_num) {
    if (bn.sons[i].moves[0] == game.get_pm(move_index)) break;
  }


  if (i >= bn.son_num) {			// new path

#if 1
    if (game.is_alternative()) {
// printf(">>> %d\n", game.get_move_num() - move_index);

      if (game.get_move_num() - move_index != 1) {
	cout << "no game!" << endl;
        return false;
      }
    }
#endif

    if (game.is_public_draw()) {

      // public draws must be in book-skeleton!
      cout << "n" << flush;
      return false;
    }

    bn.sons = (MoveNode*) 
      realloc((char*)bn.sons, (bn.son_num+1) * sizeof(MoveNode));

    if (!bn.sons) Error("realloc?");

    init_move_node(bn.sons[bn.son_num], game, move_index);

    bn.son_num++;

  } else {					// old path


    FOR (j, bn.sons[i].move_num) 
      if (bn.sons[i].moves[j] != game.get_pm(move_index+j)) break;

    if (!game.get_pm(move_index+j).get_raw()) { 

      // path included 

      if (game.is_public_draw()) {  
	if ((bn.sons[i].is_alternative())) {   // obsolete
	  cout << "*** alt -> public draw?" << endl;
	  return false;
	}

        // change flag: public draw

	if (game.get_value() != 0) {
	  cout << "*** public draw  diff != 0" << endl;
	  return false;
	}

	bn.sons[i].is_public_draw(true);
	return true;

      } else {

	// printf("*** path included\n");

	return false;
      }
    }


    if (j >= bn.sons[i].move_num && !bn.sons[i].next) { 

      // book path is included in new game

      if (game.is_public_draw()) {
	cout << "i" << flush;
	return false;
      }

      // extend existing line (for alternatives which lie *on* paths)
      // note: alternatives are generated starting at root => extension is OK
      // printf("*** longer path\n");  

      init_move_node(bn.sons[i], game, move_index);

      return true;
    }


    if (game.is_alternative()) {

      if ((bn.sons[i].is_alternative())) {
	cout << "#" << flush;
        return false;
      }
    }



    if (j >= bn.sons[i].move_num) {		// next node

      if (append_game(*bn.sons[i].next, game, move_index+j)) {

        // bn.sons[i].Flags |= LIB_NEW;
	return true;
	
      } else return false;


    } else {					// split move sequence


      if (game.is_public_draw()) {

	// public draws must be in book-skeleton!

	cout << "p" << flush;
	return false;
      }


      if (game.is_alternative()) {

	// split only at the end of alternative-path (remove error in book)

        if (move_index + j != game.get_move_num()-1) {
	  cout << "%" << flush;
          return false;
        }
      }

      BookNode &nbn = *new BookNode(2);

      // old part starting from difference point

      nbn.sons[0].move_num = bn.sons[i].move_num - j;  // old part
      nbn.sons[0].moves   = bn.sons[i].moves+j;
      nbn.sons[0].set_flags(bn.sons[i].get_flags()); // flags of old path!

      nbn.sons[0].value   = bn.sons[i].value;
      nbn.sons[0].next    = bn.sons[i].next;

      // new sequence starting from difference point

      init_move_node(nbn.sons[1], game, move_index+j); // new part

      // old sequence is now shorter and new

      bn.sons[i].next    = &nbn;
      bn.sons[i].move_num = j;
      //  bn.sonsy[i].Flags  |= LIB_NEW;
    }
  }

  return true;
}




// append game to game tree, return false <=> game exists

bool Book::append_game(NewGame &game)
{
  if (game.is_alternative() && game.get_move_num()+4 > MAX_ALT_DISCS+1) {
    return false;  // discard obsolete alternatives beyond endgame horizon
  }

  if (append_game(*p_root, game, 0)) {
    game_num++;
    return true;
  }

  return false;
}



void Book::create_from_files(const String &book_file,
			     const String &alt_file,
			     const String &draws_file)
{
  FILE *fp;
  NewGame game;
  int num = 0;

  init();

  fp = fopen(book_file.c_str(), "r");

  if (fp) {

    cout << "[ reading book '" << book_file << "' ..." << flush;

    FOREVER {

      if (!game.f_read_packed(fp)) break;

      if (!append_game(game)) { cout << "." << flush; }
      else num++;

    }

    cout << "OK - " << num << " game(s) ]" << endl;

    fclose(fp);

  } else cout << "[ can't read book!\a ]" << endl;


  if (num) {
  
    int value;
    bool is_pu_dr, is_finished;

    num = 0;

    if (alt_file != "") { 

      fp = fopen(alt_file.c_str(), "r");

      if (fp) {

	cout << "[ reading alt-file '" << alt_file << "' ..." << flush;

	FOREVER {

	  if (!game.f_read_packed(fp)) break;
	  
	  if (game.is_finished())
	    cout << "finished game as alternative!";

	  game.is_alternative(true);

	  if (!append_game(game)) { cout << "." << flush; }
	  else num++;
	}

	fclose(fp);

      } else cout << "[ can't read alt-file! ]\a" << endl;

      cout << "OK - " << num << " alternative(s) ]" << endl;

    }


    cout << "[ MiniMax ..." << flush;

    rec_visit(*p_root, value, is_pu_dr, is_finished);

    cout << "OK - root value " << value << " ]" << endl;

    if (draws_file != "") {

      // mark public draws

      fp = fopen(draws_file.c_str(), "r");

      num = 0;

      if (fp) {

	cout << "[ reading draws from '" << draws_file << "' ..."<< flush;

	FOREVER {
	  
	  if (!game.f_read_packed(fp)) break;

	  game.is_public_draw(true);

	  if (!append_game(game)) { cout << "." << flush; }
	  else num++;
	}

	cout << "OK - " << num << "draws(s) ]" << endl;

	fclose(fp);

      } else cout << "[ can't read draws-file!\a ]\n" << endl;
    }
  }
}

# if 0

// mark all subsequent nodes as old

void MarkOld(Book *pT, BookNode *pL)
{
  int i;

  
  if (!pL) return;

  FOR (i, pL->SonNum) {

    pL->sons[i].Flags &= ~LIB_NEW;

    MarkOld(pT, pL->sons[i].next);

  }
}

#endif



#if 0
int CollectMoves(Book *pT, BookNode *pL, bool Alt, MoveData *MoveData)
{
  int i, move_num=0;


  FOR (i, pL->SonNum) {

    if (((pL->sons[i].is_alternative()) != 0) == (Alt != 0)) {

      move_data[move_num].move    = SMOVE_MOVE(pL->sons[i].moves[0]);
      move_data[move_num].value   = pL->sons[i].value;
      move_data[move_num].game_num = pL->sons[i].game_num;

      if (SMOVE_PLAYER(pL->sons[i].moves[0]) == WHITE)

	move_data[move_num].value = - move_data[move_num].value; 

      move_num++;

    }

  }


  return move_num;
}
#endif





// visit nodes recursively and compute
//   minimax game result, game number, finished-flag, and public draws

void Book::rec_visit()
{
  int value;
  bool is_public_draw, is_finished;

  rec_visit(*p_root, value, is_public_draw, is_finished);
}

int Book::rec_visit(BookNode &bn, int &value, bool &is_public_draw, bool &is_finished)
{
  int     i, val, maxval=-100, player=0, g_n=0;
  static  int discs=3;
  bool    is_pu_dr, father_is_pu_dr = true;
  bool    is_fi, father_is_fi = false;

  discs++;  // !!! upon return, discs must be decremented !!!

  if (bn.son_num == 0) Error("no son?");

  player = bn.sons[0].moves[0].get_player();

  FOR (i, bn.son_num) {

    if (player != bn.sons[i].moves[0].get_player()) Error("player?");

    if (bn.sons[i].next) {

      bn.sons[i].game_num = 
	rec_visit(*bn.sons[i].next, val, is_pu_dr, is_fi);

      bn.sons[i].value = val;
      g_n += bn.sons[i].game_num;

    } else {		// leaf sequence

      if (!bn.sons[i].is_alternative()) {
        bn.sons[i].game_num = 1;
	g_n++;
      } else {
        bn.sons[i].game_num = 0;
      }

      is_pu_dr = bn.sons[i].is_public_draw();
      is_fi    = bn.sons[i].is_finished();

      if ((bn.sons[i].is_finished())) {

	// finished => endgame value

        val = bn.sons[i].value;

      } else {

	// not finished => public_draw & finished are not affected

        continue;
      }
    }

    // finished flag true iff there is a finished son

    bn.sons[i].is_finished(is_fi);
    father_is_fi |= is_fi;


    // handle public draw recursion

    if (val == 0 && is_pu_dr) bn.sons[i].is_public_draw(true);
    else                      bn.sons[i].is_public_draw(false);

    if (val == 0 && !is_pu_dr) {

      // there is a private drawing successor

// cout << "ptdiscmax=" << pT->PrivDiscMax << endl;

      if (discs <= priv_disc_max) 

	// only private if #discs is not too large
	// (this prevents getting into analyzed drawing lines)
	
	father_is_pu_dr = false;
    }


    // negamax value

    if (player == WHITE) val = -val;

    if (val > maxval) maxval = val;
  }

  if (!g_n) { 
    cout << discs << " " << bn.son_num << endl;
    Error("no normal game!\a\n");
  }

  // if there is no finished game beneath, maxval = +-100
  // => is_public_draw = false, is_finished = false

  if (player == WHITE) maxval = -maxval;

  value = maxval;

  // no public draw if max. value is != 0

  if (maxval != 0) father_is_pu_dr = false;

  is_public_draw = father_is_pu_dr;
  is_finished    = father_is_fi;

  discs--;

  // printf("%d: %d %d\n", discs, maxval, g_n);

  return g_n;
}





// search move alternatives recursively and save paths

void Book::f_write_alternatives(FILE *fp)
{
  NewGame game;

  f_write_alternatives(fp, *p_root, game, 0);
}


void Book::f_write_alternatives(FILE *fp, BookNode &bn, NewGame &game, int movenum)
{
  int i, j;

  if (movenum > MAX_ALT_DISCS-4) return;

  FOR (i, bn.son_num) 
    if (!bn.sons[i].next && (bn.sons[i].is_alternative())) break;


  if (i >= bn.son_num) {	// no alternative in BookNode => save new

    game.set_move_num(movenum);
    game.f_write_packed(fp);
  }


  FOR (i, bn.son_num) {

    if (!(bn.sons[i].is_alternative())) {	// normal path

      FOR (j, bn.sons[i].move_num) {

        game.get_pm(movenum+j) = bn.sons[i].moves[j];
        game.set_move_num(movenum+j+1);

        if (movenum+j+1+4 > MAX_ALT_DISCS) break;

        if (j < bn.sons[i].move_num-1) game.f_write_packed(fp);

      }

      if (j >= bn.sons[i].move_num && bn.sons[i].next) 

	f_write_alternatives(fp, *bn.sons[i].next, game, movenum+bn.sons[i].move_num);

    }
  }
}


// write prefixes that occur >= n times up to disc-number m

void Book::f_write_prefixes(FILE *fp, int n, int m)
{
  NewGame game;

  f_write_prefixes(fp, *p_root, game, 0, n, m);
}


void Book::f_write_prefixes(FILE *fp, BookNode &bn, NewGame &game, int movenum, int n, int m)
{
  int i, j;
  bool good_son = false;

  if (movenum >= m-4) {
    game.set_move_num(movenum);
    game.f_write_packed(fp);
    return;
  }

  FOR (i, bn.son_num) {

    if (!(bn.sons[i].is_alternative()) && 
	bn.sons[i].next && 
	bn.sons[i].game_num >= n) {  // normal path

      good_son = true;

      FOR (j, bn.sons[i].move_num) {

        game.get_pm(movenum+j) = bn.sons[i].moves[j];
        game.set_move_num(movenum+j+1);

        if (movenum+j+1+4 >= m) break;
      }

      if (j < bn.sons[i].move_num) {
	game.f_write_packed(fp);
	continue;
      }

      f_write_prefixes(fp, *bn.sons[i].next, game, movenum+bn.sons[i].move_num, n, m);
    }
  }

  if (!good_son) {
    game.set_move_num(movenum);
    game.f_write_packed(fp);
  }
}



/* search good move alternatives in lost positions recursively and save paths 
 * with values 
 *
 */


void Book::f_write_good(FILE *fp)
{
  NewGame game;

  f_write_good(fp, *p_root, game, 0);
}


void Book::f_write_good(FILE *fp, BookNode &bn, NewGame &game, int movenum)
{
  int i, j, player=0, altval=0, altmove=0, val=0, maxval=-100;
  bool alt=false;


  if (movenum > MAX_ALT_DISCS-4) return;

  FOR (i, bn.son_num) {

    if (!player) player = bn.sons[i].moves[0].get_player();
    else if (player != bn.sons[i].moves[0].get_player()) Error("player?");


    if (!(bn.sons[i].is_alternative())) {

      val =  bn.sons[i].value;
 
    } else {

      alt = true; 

      altmove = bn.sons[i].moves[0].get_move();
      altval  = bn.sons[i].value;

      if (player == WHITE) altval = -altval;

      continue; 
    }

    if (player == WHITE) val = -val;

    if (val > maxval) maxval = val;
  }

  if (val <= 0 && alt) {

    game.get_pm(movenum).set(player, altmove);
    game.set_value(altval);
    game.set_move_num(movenum+1);
    game.f_write_packed(fp);

  }

  FOR (i, bn.son_num) {

    if (!(bn.sons[i].is_alternative())) {	// normal path

      FOR (j, bn.sons[i].move_num) {

        game.get_pm(movenum+j) = bn.sons[i].moves[j];
        game.set_move_num(movenum+j+1);
        if (movenum+j+1+4 > MAX_ALT_DISCS) break;

      }

      if (j >= bn.sons[i].move_num && bn.sons[i].next) 
	f_write_good(fp, *bn.sons[i].next, game, movenum+bn.sons[i].move_num);
    }
  }
}


#if 0
#define EO(x) x;
#else
#define EO(x) 
#endif


// evaluate chances recursively, NegaMax with different strategies


// root information 

static MoveData global_move_data[65];
static int      global_move_num;



#define OUT false


int Book::rec_eval(
  BookNode &bn, int discs, int depth, RandPathInfo &rpi
)
{
  int  i, player, move_num, offset, public_offset;
  MoveData move_data[65];
  RandPathInfo lrpi = rpi;  // copy info

  if ((move_num=bn.son_num) < 1) Error("<1 sons?");

  player = bn.sons[0].moves[0].get_player();

  offset = black_offset;
  if (player == WHITE) offset = -offset;

  public_offset = public_black_offset;
  if (player == WHITE) public_offset = -public_offset;


  EO(printf("depth=%d discs=%d %d\n", depth, discs, player))


  // compute info for each move

  FOR (i, move_num) {

    MoveNode &mn = bn.sons[i];

    move_data[i].move = mn.moves[0].get_move();
    move_data[i].public_draw = mn.is_public_draw();
    move_data[i].game_num = mn.game_num;

    int ev, hv;          // endgame & heuristic value
    int val = mn.value;  // tree-value

    // nega...

    if (mn.moves[0].get_player() == WHITE) val = -val;

    if (mn.next) {	 // branch follows

      hv = rec_eval(*mn.next, discs+mn.move_num, depth+1, lrpi);

      if (mn.next->sons[0].moves[0].get_player() != player) hv = -hv;

      ev = val * FACTOR;
      move_data[i].path_num = lrpi.path_num;

    } else { 

      if (!mn.is_finished()) {

	// not finished => heuristic evaluation

	hv = val * FACTOR;
	ev = DIFF_ALT;

	// only count prefixes in book (not alternatives)

      } else {

	// finished => game result

	ev = val * FACTOR;

	if (mn.is_alternative()) 
	  cout << "finishing alternative" << endl;

	if      (val > 0) hv = +SURE_VALUE;
	else if (val < 0) hv = -SURE_VALUE;
	else 		  hv =  0;
	
	if ((mn.is_public_draw()) && val == 0 && public_black_offset) {

//KoorAus(move_data[i].move);
//printf("\a (%d) public draw reached\n", depth);

	  cout << "p"; 

	  // drawing moves are good or bad

	  if (public_offset > 0) hv = +(SURE_VALUE-1);//-1=>choose winning move if ex.
	  if (public_offset < 0) hv = -(SURE_VALUE-1);//+1=>choose drawing move if ex.

	} else {

	  if (val == 0 && black_offset) {

	    // drawing moves are good or bad
	    
	    if (offset > 0) hv = +(SURE_VALUE-1);//-1=>choose winning move if ex.
	    if (offset < 0) hv = -(SURE_VALUE-1);//+1=>choose drawing move if ex.
	  }
	}
      }

      if (rpi.determine_path_num) {

	// determine path-number for leaves/alternatives

	int root_val = hv;

	if (player != rpi.root_player) root_val = -root_val;

	// root_val = chance from point of view of root player
	// root_val in [alpha,beta] => #=1, otherwise #=0
	
	if (rpi.alpha <= root_val && root_val <= rpi.beta)
	  move_data[i].path_num = 1;
	else 
	  move_data[i].path_num = 0;
      }
    }

    // endgame & heuristic value

    move_data[i].e_val = ev;
    move_data[i].h_val = hv;
  }


  if (offset || public_offset) {

    // change draw values according to offset

    FOR (i, move_num) 
      if (move_data[i].e_val == 0) {

	if (bn.sons[i].is_public_draw() && public_offset) {

//	  printf("::: %d ", depth); KoorAus(move_data[i].move); printf("\n");
 	  move_data[i].e_val = public_offset;

	} else if (offset)
	  move_data[i].e_val = offset;

      }
  }


  int num_path_sons = 0;  // number of sons which path_num counts

#if 0
  FOR (i, move_num) {
    MoveData &md = move_data[i];

    cout << "[" << flush;
    KoorAus(md.move); fflush(stdout);
    cout << " e=" << md.e_val << " h=" << md.h_val << "] " 
         << " n=" << md.game_num << flush;

  }
  cout << endl;
#endif

  if (mode == Mode::DEV_FIRST) {

    // second approach: deviation value first 
    // find move with best chance (result is tiebreaker)

    qsort(move_data, (size_t) move_num, sizeof(MoveData), compSGNMoveData2);
    num_path_sons = move_num;

  } else if (mode == Mode::RES_FIRST) {

    // first approach: game result first 
    // find move with maximum value and chance

    qsort(move_data, (size_t) move_num, sizeof(MoveData), compSGNMoveData);

if (move_data[0].e_val == DIFF_ALT) {

#if 0
  FOR (i, move_num) {
    MoveData &md = move_data[i];

    cout << "[" << flush;
    KoorAus(md.move); fflush(stdout);
    cout << " e=" << md.e_val << " h=" << md.h_val << "] " 
         << " n=" << md.game_num << flush;

  }
  cout << endl;
#endif

  Error("no result!?");  // has to be fixed
}

    if (move_data[0].e_val <= 0) {

      // look also at move-alternative in position is <= draw

      FOR (i, move_num)
	if (move_data[i].e_val == DIFF_ALT)
	  move_data[i].e_val = move_data[0].e_val;
    }

    // find move with maximum value and chance according to offset & alternative

    qsort(move_data, (size_t) move_num, sizeof(MoveData), compSGNMoveData);


    // determine number of sons whose path_num must be considered
    // = # of relevant sons

    FOR (i, move_num) 
      if (sgn(move_data[0].e_val) != sgn(move_data[i].e_val)) break;

    num_path_sons = i;

    if (path_randomization) 
      Error("randomization doesn't work with resfirst");

    // since deviation value is not monotone! (e.g.  0 60 -> +2 40)
    // this means some work for max-update ... 

  } else if (mode == Mode::RES_DEV_MIX) {

    if (path_randomization) 
      Error("randomization doesn't work with res_dev_mix yet");

    num_path_sons = 0; // ???

    // e_val sort

    qsort(move_data, (size_t) move_num, sizeof(MoveData), compSGNMoveData);

    if (move_data[0].e_val < 0) {

      // loss => look at all possibilities

      // h_val sort

      qsort(move_data, (size_t) move_num, sizeof(MoveData), compSGNMoveData2);

    } else {

      // there is a draw or win => 
      // add GOOD_DELTA to all "res-best" move h-values that are >= GOOD_VAL

#if 0
      cout << "A  ";
      FOR (i, move_num) cout << move_data[i];
      cout << endl;
#endif

      int old_val[64], old_move[64], change_num=0;

      FOR (i, move_num) {
	if (sgn(move_data[0].e_val) != sgn(move_data[i].e_val)) break;
	
	if (move_data[i].h_val >= GOOD_VAL) {
	  old_val[change_num]  = move_data[i].h_val;
	  old_move[change_num] = move_data[i].move;
	  move_data[i].h_val += GOOD_DELTA;
	  change_num++;
	}
      }


      // h_val sort

      qsort(move_data, (size_t) move_num, sizeof(MoveData), compSGNMoveData2);

#if 0
      cout << "B  ";
      FOR (i, move_num) cout << move_data[i];
      cout << endl;
#endif
      // restore old h-values

      while (--change_num >= 0) {

	int i;

	FOR (i, move_num) 
	  if (old_move[change_num] == move_data[i].move) 
	    move_data[i].h_val = old_val[change_num];
      }

#if 0
      cout << "C  ";
      FOR (i, move_num) cout << move_data[i];
      cout << endl;
#endif

    } 

  } else Error("unknown book-mode");


  if (depth == 0) {

    // copy information of all root-successors for further processing
 
    FOR (i, move_num) global_move_data[i] = move_data[i];
    global_move_num = move_num;

  }

EO(printf("depth=%d: ", depth))
EO(FOR(i, move_num) { KoorAus(move_data[i].move); printf(":%d/%d/%d ", move_data[i].e_val, move_data[i].h_val, move_data[i].path_num); })

EO(printf("\n(%d) max-return %d\n", depth, move_data[0].h_val))


  
  if (rpi.determine_path_num) {

    int p_num = 0;

#if OUT

   cout << "depth=" << depth << " " << move_num << " sons " << 
       " same players:" <<  (player == rpi.root_player) << endl;

FOR (i, num_path_sons) {
  cout << "[" << move_data[i].h_val << " " << move_data[i].path_num << "] ";
}
cout << endl;
#endif


    if (player == rpi.root_player) {

      // root player to move => 
      //   # = sum of all path_nums for sons with values in [alpha,beta] 

      FOR (i, num_path_sons) {

	if (rpi.alpha <= move_data[i].h_val && move_data[i].h_val <= rpi.beta)
	  p_num += move_data[i].path_num;
      }

    } else {

      // root player not to move => 
      // # = min of path_nums for sons with maximum deviation 
      //     value (in [alpha,beta]) 
      
      p_num = move_data[0].path_num;

      for (i=1; i < num_path_sons; i++) {

	if (move_data[0].h_val != move_data[i].h_val) break;

	p_num = min(p_num, move_data[i].path_num);
      }
    }

#if OUT
cout << "-->" << p_num << endl;
#endif

    rpi.path_num = p_num;

  }

  return move_data[0].h_val;
}



// ********** search stuff *********** 



#define BMSG(s) cout << ">>> book: " << s << endl;


// return true <=> found

bool Book::find_position(NewGame &game, PosInfo &PosI)
{
  int     i, tr, move_index, path_index, j;
  SPFELD  sf, sf1, sf2, sfelder[8];
  NewGame gameu;
  BookNode *pN;

  if (!game_num) return false;

  gameu = game;
  gameu.unique();

  // compute transformation

  game.play(game.get_move_num(), sf1);
  gameu.play(gameu.get_move_num(), sf2);

  Transform(&sf2, sfelder);

  FOR (tr, 8) {

    FOR (i, 100) if (sf1.p[i] != sfelder[tr].p[i]) break;
    if (i >= 100) break;
  }

  if (tr >= 8) Error("find_position: no transformation?");

  //sprintf(s, "Trans: %d", tr); BMSG(s);

  PosI.trans = tr;

  SfGrund(&sf);

  pN = p_root;
  move_index = -1;
  path_index = -1;

  for (i=0; gameu.get_pm(i).get_raw(); i++) {

    if (move_index < 0) {			// search move

      FOR (j, pN->son_num)
        if (pN->sons[j].moves[0] == gameu.get_pm(i)) break;

      if (j >= pN->son_num) return false;	// not found

      move_index = j;
      path_index = 0;
    }

    // compare with path move

    if (gameu.get_pm(i) != pN->sons[move_index].moves[path_index]) 
      return false;

    // next tree position

    path_index++;

    if (path_index >= pN->sons[move_index].move_num) {

      pN = pN->sons[move_index].next;
      move_index = -1;

      if (!pN) return false;	// no more moves, e.g. alternative

      if (!pN) Error("pN=0");
    }
  }

  PosI.p_node     = pN;
  PosI.move_index = move_index;
  PosI.path_index = path_index;

  return true;
}



// delete alt_leaf at the end of game-path

void Book::delete_alt_leaf(NewGame &ngame)
{
  PosInfo PI;
  NewGame game = ngame;

  // delete last move
  
  PackedMove pm; // endmarker

  game.set_pm(game.get_move_num()-1, pm);
  game.set_move_num(game.get_move_num()-1);

  if (find_position(game, PI)) {

    if (!PI.is_branch()) Error("delete_alt_leaf: no branch!");

    int i;

    FOR (i, PI.p_node->son_num) {

      if (PI.p_node->sons[i].is_alternative() && 
          !PI.p_node->sons[i].is_finished()) {

	// alternative found

	break;
      }
    }

    if (i >= PI.p_node->son_num) { cout << "no alt?" << endl; return; }

    // delete alternative

    PI.p_node->sons[i] = PI.p_node->sons[PI.p_node->son_num-1];
    PI.p_node->son_num--;    

  } else Error("delete_alt_leaf: path not found");
}




int Book::find_move(
  NewGame  &game,
  int      to_play,
  int      &value,
  ValType  &vtype
)
{
  char    s[400], s1[400];
  int     i, move_num, player, discs, best_move;
  int     val;
  SPFELD  board;
  SFPOS   moves[65];
  PosInfo PI;

  game.set_value(0);
  game.s_write(s1); 
  sprintf(s, "looking for %s", s1);
  BMSG(s);

  if (path_randomization) {
    old_max_val = curr_max_val;	
    cout << "old curr_max=" << old_max_val << endl;
  }

  if (game.get_move_num() > depth_bound) {
    BMSG("depth > depth_bound");
    return ZUG_UNBEKANNT;
  }

  if (!(player=game.play(game.get_move_num(), board))) {
    BMSG("GameEnd");
    return ZUG_UNBEKANNT;
  }

  if (to_play != player) {

    BMSG("different players");
    return ZUG_UNBEKANNT;
  }

  if (SfAnz(&board) > MAX_LIB_DISCS) {
    BMSG("too many discs");
    return ZUG_UNBEKANNT;
  }

  move_num = SfMoeglZuegeE(&board, player, moves);

  if (!move_num) Error("No Move - Can not be!\a");

  if (move_num == 1) { 

    BMSG("One Move");

    value = 0;
    vtype = ENDGAME;
    return moves[0]; 
  }

  switch (draw_mode) {
  case DrawMode::NORMAL:
    black_offset = 0;    
    break;
  case DrawMode::BAD:
    black_offset = -player;
    break;
  case DrawMode::GOOD:
    black_offset = player;
    break;
  case DrawMode::BAD_FOR_BLACK:
    black_offset =  -1;
    break;
  case DrawMode::GOOD_FOR_BLACK:
    black_offset =  1;
    break;
  default:
    Error("find_move: here?");
  }

  switch (public_draw_mode) {
  case DrawMode::NORMAL:
    public_black_offset = 0;    
    break;
  case DrawMode::BAD:
    public_black_offset = -player;
    break;
  case DrawMode::GOOD:
    public_black_offset = player;
    break;
  case DrawMode::BAD_FOR_BLACK:
    public_black_offset =  -1;
    break;
  case DrawMode::GOOD_FOR_BLACK:
    public_black_offset =  1;
    break;
  default:
    Error("find_move: here2?");
  }


  // search game in GameTree and collect game moves and values in MoveData

  if (find_position(game, PI)) {

    if (PI.move_index < 0) {		// branch

      RandPathInfo rpi;

      discs = 4 + game.get_move_num();

      rpi.determine_path_num = false;
      
      val = rec_eval(*PI.p_node, discs, 0, rpi);

      if (global_move_num < 1) Error("no moves?");


      if (path_randomization) {

	// determine new maximum

        if (val > curr_max_val) {

	  curr_max_val = val;
	  printf(">>> new curr_max=%d\n", val);
        }

	rpi.determine_path_num = true;
	rpi.root_player = player;

#if 1
	rpi.alpha = curr_max_val - max_delta * FACTOR;
	rpi.beta  = val+1;
#else

// test

	rpi.alpha = -10000000;
	rpi.beta  = +10000000;
#endif

	printf("[ pathrand: val=%d player=%d al=%d be=%d ]\n",
	  val, rpi.root_player, rpi.alpha, rpi.beta
	);

        rec_eval(*PI.p_node, discs, 0, rpi);

	if (mode == Mode::RES_FIRST) {

	  // clear path_nums of irrelevant moves

	  FOR (i, global_move_num) {
	    if (sgn(global_move_data[i].e_val) != sgn(global_move_data[0].e_val)) 
	      global_move_data[i].path_num = 0;
	  }
	}
      }

      // determine transpositions

      FOR (i, global_move_num) 
	global_move_data[i].move = Trans[PI.trans](global_move_data[i].move);  

      // print move-data

      printf("\n");

      FOR (i, global_move_num) {

	printf("["); KoorAus(global_move_data[i].move);

	printf(": vt=%.1f ch=%d gn=%d", 
			(REAL)global_move_data[i].e_val/FACTOR,
			my_round((REAL)global_move_data[i].h_val/FACTOR)+50,
			global_move_data[i].game_num);

	if (global_move_data[i].public_draw)
	  printf(" pud");

	if (path_randomization) {

	  printf(" pa=%d", global_move_data[i].path_num);

	}

	printf("] ");
      }

      printf("\n");


      if (rnd_book) {

	// choose move randomly according to game_num

	int game_num = 0;

	FOR (i, global_move_num) game_num += global_move_data[i].game_num;

	if (game_num <= 0) Error("game_num <= 0");

	int rnd_num = (IRAN % game_num) + 1;

        int sum = 0;

        FOR (i, global_move_num) {
	  sum += global_move_data[i].game_num;
	  if (sum >= rnd_num) break;
	}

	if (i >= global_move_num) Error("no move found");

	// swap contents of 0 and i

	{ 
	  MoveData t = global_move_data[0];
	  global_move_data[0] = global_move_data[i];
	  global_move_data[i] = t;
	}

        cout << "randomly chosen move: " << flush;
        KoorAus(global_move_data[0].move); fflush(stdout);
	cout << endl;

	value = 0;
	vtype = ENDGAME;
	return global_move_data[0].move;
      }

      if (path_randomization) {

	// choose move randomly according to path_num

	int num_paths = 0;

	FOR (i, global_move_num) num_paths += global_move_data[i].path_num;

	if (num_paths <= 0) Error("num_paths <= 0");

	int num_rand = (IRAN % num_paths) + 1;

        int sum = 0;

        FOR (i, global_move_num) {
	  sum += global_move_data[i].path_num;
	  if (sum >= num_rand) break;
	}

	if (i >= global_move_num) Error("no move found");

	// swap contents of 0 and i

	{ 
	  MoveData t = global_move_data[0];
	  global_move_data[0] = global_move_data[i];
	  global_move_data[i] = t;
	}

        cout << "randomly chosen move: " << flush;
        KoorAus(global_move_data[0].move); fflush(stdout);
	cout << endl;
      }

      // compute output-values

      { int chance, diff;
        double val;

        chance = my_round(global_move_data[0].h_val / FACTOR) + 50;
        if (chance == 0)   chance = 1;
        if (chance == 100) chance = 99;

        diff = my_round(global_move_data[0].e_val/FACTOR);

        if (diff < -64) diff = 0;  // alternative chosen

        if (diff < 0) val = diff - chance * 0.01; 
	else          val = diff + chance * 0.01;

	printf("val= %+.2f\n", val); 

        value = my_round(val*MIX_CONST);
        vtype = MIX;

      }


      if (global_move_data[0].e_val >= 0) {
 
	BMSG("good position");

      } else {

	BMSG("bad position");

      }

      return global_move_data[0].move;

    } else {				// path

      if (PI.p_node->sons[PI.move_index].is_alternative()) 
	Error("**** ALT???\a\a\a");

      BMSG("path");

      best_move = Trans[PI.trans](
        PI.p_node->sons[PI.move_index].moves[PI.path_index].get_move());

      val = PI.p_node->sons[PI.move_index].value;

      if (rnd_book) val = 0;

      if (PI.p_node->sons[PI.move_index].moves[PI.path_index].get_player() == WHITE)

	val = -val;

      if (val >= 0) {

	BMSG("good position => play move");

        value = val;
        vtype = ENDGAME;
	return best_move;

      } else {

	BMSG("bad position => search");
	return ZUG_UNBEKANNT;
      }
    }

  } else {

    BMSG("game not found");

    return ZUG_UNBEKANNT;
  }

  Error("NewGameTreeMove: what?");

  return ZUG_UNBEKANNT;
}


int Book::reset_max()
{
  curr_max_val = old_max_val = min_val = -(SURE_VALUE+1);
  return curr_max_val;
}


int Book::restore_max()
{
  curr_max_val = old_max_val;
  return curr_max_val;
}

int Book::set_old_max()
{
  old_max_val = curr_max_val;
  return old_max_val;
}




void Book::show_book_node(BookNode &bn, int depth, int maxdepth, char *out)
{
  int i, j, len;

  if (depth >= maxdepth) { cout << "\n"; return; }

  len = strlen(out);

  FOR (i, bn.son_num) {

    MoveNode &mn = bn.sons[i];

    cout << out << "+->";

    if (i < bn.son_num-1) strcat(out, "|  "); else strcat(out, "   ");

    for (j=0; j < mn.move_num && j + depth < maxdepth; j++) {

      if (mn.moves[j].get_player() == BLACK) 
        KoorAus(mn.moves[j].get_move());
      else
        GKoorAus(mn.moves[j].get_move());
 
      cout << " ";
      strcat(out, "   ");
    }


    if ((!mn.is_finished())) {

      cout << "(" << int(mn.value) << ")" << endl;
      
    } else {

      cout << "[" <<  int(mn.value) << " " << mn.game_num;
      if ((mn.is_public_draw())) cout << " p";
      cout << "]" << endl;
    }

    out[strlen(out)-3] = 0;
    
    if (j + depth < maxdepth && mn.next) 
      show_book_node(*mn.next, depth+j, maxdepth, out);
    
    out[len] = 0;
  }
}





bool Book::show_sub_tree(NewGame &game, int depth)
{
  PosInfo pi;
  char out[500];

  out[0] = 0;

  if (!find_position(game, pi)) return true;
  if (!pi.is_branch()) return true;

  show_book_node(*pi.p_node, 0, depth, out);
  return false;
}


void Book::save_state(BookState &bs)
{
  bs.curr_max_val = curr_max_val;
  bs.old_max_val  = old_max_val;
}

void Book::load_state(BookState &bs)
{
  curr_max_val = bs.curr_max_val;
  old_max_val  = bs.old_max_val;
}



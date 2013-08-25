// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <vector>
#include <algorithm>

#include "ClientTypes.H"
#include "ClientGameType.H"
#include "ClientGame.H"

#define DBG  0
#define TEST 1


// ggf functions

static bool skip_until(istream &is, char u)
{
  char c;

  do { is >> c; } while (is && c != u);
  return is;
}


static bool read_until(istream &is, char u, String &s)
{
  char c;

  s = "";
  
  do { 
    c = is.get();
    s += c; 
  } while (is && c != u);

  if (is) s.erase(s.length()-1);
  
#if 0  
  // delete trailing ' ' 

  while (i >= 0)
    if (s[i] == ' ') s[i--] = 0; else break;

  // delete preceeding ' ' 

  i = 0;
  while (s[i] == ' ') i++;

  int j = 0;

  while (s[i]) s[j++] = s[i++];
  s[j] = 0;

#if DBG
  strerr << "read: " << s << endl;
#endif

#endif
  
  return is;
}


static bool next_token(istream &is, String &s)
{
  char c;

  // skip white space and ';'

  do { c = is.get(); } while (is && (c == ' ' || c == '\n' || c == ';'));

  if (!is) return false;
  
  if (c == ')') {  // end of game
    s += c;
    return true;
  }

  is.putback(c);                         

  do { 

    c = is.get();

#if DBG
    strerr << "<" << c << ">" << endl;
#endif
    
    s += toupper(c);
    if (s.size() > 3) return false;
    
  } while (is && c != '[');

  if (is) s.erase(s.length()-1);
  return is;
}


// black - index 0

String ClientGame::read_ggf(istream &is)
{
  char c;
  bool type_read = false;
  
  // search "(;"

  do {
    if (!skip_until(is, '(')) return "no ( found";
    is >> c;
    if (c != ';') is.putback(c);
  } while (c != ';');

  reset();

  pinf[0].color = ClientColor::BLACK;
  pinf[1].color = ClientColor::WHITE;

  FOREVER {

    String s;
    float f;

    if (!is) return "read error";
    if (!next_token(is, s)) return "no more tokens";

#if DBG
    strerr << "token: " << s << endl;
#endif

    if      (s == ")") break; // end of game
    else if (s == "GM" ||     // ZAP
	     s == "US" || 
	     s == "CP" || 
	     s == "COPYRIGHT" ||
	     s == "GN" || 
	     s == "C"  || 
	     s == "BL" || 
	     s == "WL" || 
	     s == "NB" || 
	     s == "NW" || 
	     s == "EV" || 
	     s == "LT") { skip_until(is, ']'); } 

    else if (s == "PB") {           // black name

      if (!read_until(is, ']', s)) return "] not found after PB";
      pinf[0].name = s;

    } else if (s == "PW") {         // white name

      if (!read_until(is, ']', s)) return "] not found after PW";
      pinf[1].name = s;

    } else if (s == "RB") {         // black rating

      if (!read_until(is, ']', s)) return "] not found after RB";
      pinf[0].rating = atof(s.c_str());

    } else if (s == "RW") {         // white rating

      if (!read_until(is, ']', s)) return "] not found after RW";
      pinf[1].rating = atof(s.c_str());
      
    } else if (s == "PC") {         // place

      if (!read_until(is, ']', s)) return "] not found after PC";
      place = s;
      
    } else if (s == "DT") {         // date

      if (!read_until(is, ']', s)) return "] not found after DT";
      date = s;

    } else if (s == "TY") {         // type

      if (!read_until(is, ']', s)) return "] not found after TY";

      istringstream iss(s.c_str());

      String es = type->parse(iss);
      if (!es.empty()) return es;
      type_read = true;

    } else if (s == "TI") {         // both times

      if (!read_until(is, ']', s)) return "no ] after TI";
      if (!pinf[0].clock.parse(s)) return "TI: corrupt clock";

      pinf[1].clock = pinf[0].clock;  // equal times
      
    } else if (s == "TB") {         // b time
      
      if (!read_until(is, ']', s)) return "no ] after TB";
      
      if (!pinf[0].clock.parse(s)) return "TB: corrupt clock";

    } else if (s == "TW") {         // w time
      
      if (!read_until(is, ']', s)) return "no ] after TW";
      if (!pinf[1].clock.parse(s)) return "TW: corrupt clock";

    } else if (s == "KB") {         // black komi move
      
      if (!read_until(is, ']', s)) return "no ] after KB";

      pinf[0].komi_move = new_ClientMove();
      String es = pinf[0].komi_move->from_string(s);
      if (!es.empty()) return "KB move corrupt: " + es;

    } else if (s == "KW") {         // white komi move
      
      if (!read_until(is, ']', s)) return "no ] after KW";

      pinf[1].komi_move = new_ClientMove();      
      String es = pinf[1].komi_move->from_string(s);
      if (!es.empty()) return "KW move corrupt: " + es;

    } else if (s == "KM") {         // komi

      if (!read_until(is, ']', s)) return "no ] after KM";
      if (sscanf(s.c_str(), "%f", &f) == 1) {
        komi = f;
	komi_defined = true;
      } else {
        return "syntax error: KM";
      }

    } else if (s == "RE") {         // result

      if (!read_until(is, ']', s)) return "no ] after RE";

      // ... ignored
      
    } else if (s == "BO") {         // starting position

      if (!read_until(is, ']', s)) return "no ] after BO";

      istringstream is(s);

      // errstr << "bo=" << s << " " << s.length() << endl;

      if (!start_pos->read_ggf(is).empty()) return "start_pos corrupt";
      
    } else if (s == "B" || s == "W") {         // move

      if (!read_until(is, ']', s)) return "no ] after B/W";

      ClientPTR<ClientMove> tmp_move = new_ClientMove();
      
      if (!tmp_move->from_string(s).empty()) return "B/W move corrupt";
      moves.push_back(tmp_move);

    } else {
      
      ERR2("unknown GGF command: \a", s);
      return "unknown GGF command";
    }
  }
    
  if (!type_read) return "no type found";

  if (!(type->get_board_type() == start_pos->get_type())) {
    return "incompatible board types";
  }

  if (type->is_komi_game() && !komi_defined) {
    pinf[0].color = ClientColor::UNDEF;
    pinf[1].color = ClientColor::UNDEF;
  }

#if 0
  cout << "COL0= " << pinf[0].color << " COL1= " << pinf[1].color << endl;

  cout << 
    pinf[0].komi_move->to_string() << " " <<
    pinf[1].komi_move->to_string() << " " << endl;    
#endif
  
  
#if 0
  if (pinf[0].name != pinf[0].name) {

    // wrong player order
    
    ClientPlayerInfo pi;
    pi = pinf[0]; pinf[0] = pinf[1]; pinf[1] = pi;
  }
#endif
  
  replay();
  return "";
}



void ClientGame::write_ggf(ostream &os, bool one_line) const
{
  sint4 indb = 0;
  sint4 indw;
  
  if (!type->is_komi_game() || komi_defined) {
    indb = color_index(ClientColor::BLACK);
  }

  indw = 1-indb;
  String my_eol("");

  if (!one_line) my_eol += '\n';
    
  os << "(;GM[Client]PC[VC/OS]DT[" << time(0) << "]" << my_eol;
  os << "PB[" << pinf[indb].name << "]";
  os << "PW[" << pinf[indw].name << "]" << my_eol;
  os << "RB[" << pinf[indb].rating << "]";
  os << "RW[" << pinf[indw].rating << "]" << my_eol;

  ostringstream os1, os2;
  
  pinf[indb].clock.print(os1, true);
  pinf[indw].clock.print(os2, true);

  String s1(os1), s2(os2);

  if (s1 != s2) {
    os << "TB[" << s1 << "]";
    os << "TW[" << s2 << "]" << my_eol;
  } else {
    os << "TI[" << s1 << "]" << my_eol;
  }
  
  os << "TY[" << type->to_string() << "]";

  if (pinf[indb].komi_move.get()) {
    os << "KB[" + pinf[indb].komi_move->to_string() + "]";
  }
  if (pinf[indw].komi_move.get()) {
    os << "KW[" + pinf[indw].komi_move->to_string() + "]";
  }
  if (komi_defined) {
    os << "KM[" << komi << "]";
  }
  os << "RE[";
  if (!is_finished()) os << "?";
  os << "]" << my_eol;

  // starting position

  os << "BO[";
  start_pos->write_ggf(os, one_line);
  os << "]" << my_eol;

  int n = 0, color = start_pos->get_to_move();

  FORU (i, moves.size()) {
    (color == ClientColor::BLACK) ? os << "B" : os << "W";
    os << "[" + moves[i]->to_string() + "]";
    color = ClientColor::opponent(color);
    ++n;
    if (n >= 4) { n = 0; if (i < moves.size()-1) os << my_eol; }
  }
  if (n) os << my_eol;
  os << ";)";
}
    

sint4 ClientGame::index_to_move() const
{
  return color_index(curr_pos->get_to_move());
}


const String &ClientGame::name_to_move() const
{
  return pinf[index_to_move()].name;
}

sint4 ClientGame::color_index(sint4 color) const
{
  if      (pinf[0].color == color) return 0;
  else if (pinf[1].color == color) return 1;

  ERR("undef color");
  return 0;
}

sint4 ClientGame::name_index(const String &pl) const
{
  if      (pinf[0].name == pl) return 0;
  else if (pinf[1].name == pl) return 1;

  ERR("undef player");
  return 0;  
}


void ClientGame::reset_offers_and_requests()
{
  pinf[0].reset_offers_and_requests();
  pinf[1].reset_offers_and_requests();
}

bool ClientGame::matching_offers() const
{
  if (!pinf[0].offered || !pinf[1].offered) return false;

  return pinf[0].offered_value <= - pinf[1].offered_value;
}

bool ClientGame::aborted() const
{
  return pinf[0].aborted && pinf[1].aborted;
}

bool ClientGame::adjourned() const
{
  return pinf[0].adjourned || pinf[1].adjourned;
}
 
bool ClientGame::resigned() const
{
  return pinf[0].resigned || pinf[1].resigned;
}
 


bool ClientGame::is_finished() const
{
  return curr_pos->is_finished();
}


bool ClientGame::play(ostream &os, const String &pl, const ClientMove &mv0)
{
  bool komi_game = type->is_komi_game();
  bool prev_komi_defined = komi_defined;
  int pi=-1, oi=-1;
  ClientPTR<ClientMove> tmp_move = mv0.clone();
  
  reset_offers_and_requests();
  
  if (komi_game && !komi_defined) {

    // in initial position => collect komi moves

    ClientPTR<ClientBoard> tmp_board = curr_pos->clone();

    if (!tmp_board->make_move(*tmp_move)) {
      os << "illegal komi move " + tmp_move->to_string();
      return false;
    }

    if (pl == pinf[0].name && pl == pinf[1].name) {

      // selfplay => fill komi move slots
	
      if (pinf[0].komi_move.get()) pi = 1; else pi = 0;
      pinf[pi].komi_move->copy_of(*tmp_move);
	
    } else {
      
      if (pl == pinf[0].name) {
	if (pinf[0].komi_move.get()) {
	  cerr << ">>> first move already sent";
	  return false;
	}
	pinf[0].komi_move = new_ClientMove();
	pinf[0].komi_move->copy_of(*tmp_move);
	pi = 0;

      } else if (pl == pinf[1].name) {

	if (pinf[1].komi_move.get()) {
	  cerr << ">>> first move already sent";
	  return false;
	}
	pinf[1].komi_move = new_ClientMove();
	pinf[1].komi_move->copy_of(*tmp_move);
	pi = 1;

      } else {

	os << pl + " not playing";
	ERR(pl + " not playing");
	return false;

      }
    }

    if (pinf[0].komi_move.get() && pinf[1].komi_move.get()) {

      // received both komi moves => compute komi value and assign colors

      komi = (pinf[0].komi_move->get_eval() + pinf[1].komi_move->get_eval()) * 0.5;

      if (pinf[0].komi_move->get_eval() > komi) {

	pinf[0].color = curr_pos->get_to_move();

      } else if (pinf[0].komi_move->get_eval() < komi) {

	pinf[0].color = ClientColor::opponent(curr_pos->get_to_move());
	  
      } else {

	if (pinf[0].color == ClientColor::UNDEF) {

	  // assign random colors in non-replay mode
	  // should never get here in client

	  cout << ">>> ASSIGNING RAND COLORS IN CLIENT\a" << endl;
	  exit(20);
	  
// 	  pinf[0].color = (ClientBoard::rand_int(2) > 0) ?
// 	    ClientColor::BLACK :
// 	    ClientColor::WHITE;
	}

      }

      pinf[1].color = ClientColor::opponent(pinf[0].color);

      cout << pinf[0].color << " " << pinf[0].komi_move->get_eval() << endl;
      cout << pinf[1].color << " " << pinf[1].komi_move->get_eval() << endl;
      cout << komi << endl;
    
      if (curr_pos->get_to_move() == ClientColor::WHITE) komi = -komi;
      komi_defined = true;
    }
  }

  bool regular_move = !komi_game || prev_komi_defined;
  
  if (regular_move) {
  
    // check whether "name" is to move
  
    if (name_to_move() != pl) {
      os << "not your turn";
      return false;
    }

    // check whether move is legal

    pi = index_to_move();
    oi = 1 - pi;
  
    if (!curr_pos->make_move(*tmp_move)) {
      os << "illegal move";
      return false;
    }

  } else {

    // one of the first two moves in komi games

    oi = 1-pi;

  }

  if (pi < 0 || oi < 0 || oi != 1-pi) {
    ERR("pi not set");
  }

  // adjust player clock

#if 1
  pinf[pi].clock.print(cout); cout << endl;
  int r = pinf[pi].clock.stop(tmp_move->get_time());
  pinf[pi].clock.print(cout); cout << endl;
  cout << "########## ADJ TIME " << pi << " " << tmp_move->get_time()
       << " " << r << endl;
#endif
  
  // don't start clock if in first komi game move or in synchro games
  
  if ((!komi_game || komi_defined)) {
    pinf[oi].clock.start();
  }
  
  if (!regular_move) {

    // overwrite komi move (now having correct time)

    pinf[pi].komi_move->copy_of(*tmp_move);
  }

  if (!komi_game || komi_defined) {

    // update board with actual move

    // in case that komi just got defined take
    // the according first move

    if (!prev_komi_defined && komi_defined) {

      cout << "MOVE0=" << pinf[0].komi_move->to_string() << endl;
      cout << "MOVE1=" << pinf[1].komi_move->to_string() << endl;      

      tmp_move->copy_of(*pinf[index_to_move()].komi_move);

      cout << index_to_move() << " FIRST_MOVE=" << tmp_move->to_string() << endl;
      
      if (!curr_pos->make_move(*tmp_move)) {
	os << "illegal komi move"; return false;
      }
    }

    // append move to list
  
    moves.push_back(tmp_move);
  }
  
  if (komi_game && !komi_defined && pinf[0].name == pinf[1].name) {
    return play(os, pl, *tmp_move);
  }
  
  return true;
}


#if 0
bool ClientGame::play(ostream &os, const String &pl, const String &mv_info_time)
{
  ClientMove mv;
  ostringstream oss;

  // mv syntactically correct?

  if (!mv.parse(os, mv_info_time)) {
    os << err_string();
    mv.parse(os, mv_info_time);
    return false;
  }
  
  return play(os, pl, mv);
}
#endif


// assumes names,colors,clocks and komi-moves set

void ClientGame::replay()
{
  // save relevant info before resetting

  ClientPTR<ClientMove> km0 = pinf[0].komi_move;
  ClientPTR<ClientMove> km1 = pinf[1].komi_move;
  int c0 = pinf[0].color;
  int c1 = pinf[1].color;
  
  // save move list and erase original
  
  vector< ClientPTR<ClientMove> > mvs;

  mvs.insert(mvs.begin(), moves.begin(), moves.end());
  moves.erase(moves.begin(), moves.end());

  reset();

  pinf[0].color = c0;
  pinf[1].color = c1;
  curr_pos->copy_of(*start_pos);

  pinf[0].clock.reset();
  pinf[1].clock.reset();

#if 1
  if (type->is_komi_game()) {

    pinf[0].clock.start();
    pinf[1].clock.start();
  
  } else {

    pinf[name_index(name_to_move())].clock.start();
    
  }    
#endif

  // replay all komi moves

  if (km0.get()) {
    ostringstream os;
    os << "km0=" + km0->to_string() + " ";
    if (!play(os, pinf[0].name, *km0)) {
      ERR2("illegal komi move in replay 0", String(os));    
    }
  }

  if (km1.get()) {
    ostringstream os;
    os << "km1=" + km1->to_string() + " ";
    if (!play(os, pinf[1].name, *km1)) {
      ERR2("illegal komi move in replay 1", String(os));    
    }
  }

  // replay remaining moves

  FORU (i, mvs.size()) {

    if (i == 0 && type->is_komi_game()) {
      // first move in list is komi move
      continue;
    }
    
    String pl = name_to_move();
    ostringstream os;
    os << mvs[i]->to_string() + " ";
    if (!play(os, pl, *mvs[i])) {

      //      curr_pos->write(cerr, 0);
      ERR2("illegal move in replay", String(os));
    }
  }
}


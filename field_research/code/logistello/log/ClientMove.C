// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "ClientMove.H"
#include "ClientBoard.H"

// parse ascii-move [e.g. "E13/10/23.5" ]
// return true iff syntax correct

String ClientMove::from_string(const String &args)
{
  String s, rest, rest2;

  eval = 0;
  time = 0;

  String::parse(args, s, rest, '/');
  
  if (s == "") return "missing move";

  String es;
  
  if ((es=str2coords(s)) != "") return es;

  if (rest != "") {

    // read eval
    
    String::parse(rest, s, rest2, '/');
    if (! s.empty() ) {
      istringstream is(s);
      is >> eval;
      if (!is) { 
	return "move-eval expected";
      }
    }
    if (rest2 != "") {

      // read time
      
      istringstream is(rest2.c_str());
      real4 time_sec;
      is >> time_sec;
      if (!is || time_sec < 0) { 
	return "move-time >= 0 expected";
      }

      time = sint8(time_sec * uSec);
    }
  }
  
  return "";
}

// bool ClientMove::is_pass() const { return sq_index == PASS; }

// bool ClientMove::is_valid() const
// {
//   return is_pass() || (sq_index >= 0 && sq_index < ClientBoardType::MAX_BOARD_SIZE);
// }


String ClientMove::to_string() const
{
  assert(is_valid());

  String s = coords2str(), t;

  if (eval != 0 || time >= 5000) {
    s += "/";
    if (eval != 0) { t.form("%.2f", eval); s += t; }
    if (time >= 5000) { t.form("/%.2f", time*1E-6); s += t; }
  }

  return s;
}

String ClientMove::coord2str(int c)
{
  String s;
  s.form("%c%d", 'A'+ClientBoard::ind2x(c), ClientBoard::ind2y(c)+1);
  return s;
}



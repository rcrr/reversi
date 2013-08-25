#include "ClientBoard.H"
#include <vector>
#include <algorithm> 

const sint4 ClientBoard::d[8] =
{
  +1, 
  DX,
  DX+1,
  DX-1,

  -1,
  -DX,
  -(DX+1),
  -(DX-1)
};


void ClientBoard::clear()
{
  int w = width();

  FORS (i, (sint4)MAX_BOARD_SIZE) sq[i] = BORDER;

  FORS (y, w) {
    FORS (x, w) {
      sq[xy2ind(x, y)] = EMPTY;
    }
  }
}


String ClientBoard::read_sgf(istream &is)
{
  char c;
  String s;
  ostrstream os;

  String es = type->parse(is);

  if (!es.empty() || !is) return "illegal board type";

  setup();
   
  sint4 w = width();

  FORS (y, w) {
    FORS (x, w) {
      sint4 ind = ClientBoard::xy2ind(x,y);
      if (sq[ind] != BORDER) {
	is >> c;
	//      errstr << "square " << c << endl << flush;
	if (!is) return "read error";
	
	sq[ind] = char2cont(c);
      }
    }
  }

  is >> c;

  if (c == '*') turn_color = ClientColor::BLACK;
  else if (c == 'O') turn_color = ClientColor::WHITE;
  else return "illegal color to move";

  return "";
}

void ClientBoard::write_sgf(ostream &os, bool one_line) const
{
  sint4 w = width();

  os << type->to_string();
  if (!one_line) os << endl; else os << " ";

  FORS (y, w) {
    FORS (x, w) {
      os << cont2char(sq[xy2ind(x,y)]);
    }

    if (one_line) os << " "; else os << endl;
  }

  if (get_to_move() == ClientColor::BLACK) os << "*"; else os << "O";
}


void ClientBoard::toggle_to_move()
{
  turn_color = ClientColor::opponent(turn_color);
} 

#if 0
sint4 ClientBoard::get_square(sint4 ind) const
{
  return squares[ind];
}

sint4 ClientBoard::get_type() const
{
  return type;
}


bool ClientBoard::no_moves(bool opponent) const
{
  ClientBoard bo = *this;
  uint4 i;
  
  if (opponent) bo.toggle_to_move();

  FOR (i, ClientBoardType::MAX_BOARD_SIZE) {
    ClientMove mv(i);
    if (bo.make_move(mv)) return false;
  }

  return true;
}


bool ClientBoard::is_finished() const
{
  if (!no_moves(false)) return false;
  return no_moves(true);
}


int ClientBoard::disc_num(sint4 cont) const
{
  int n = 0;
  uint4 i;
  
  FOR (i, ClientBoardType::MAX_BOARD_SIZE) {
    if (squares[i] == cont) n++;
  }

  return n;
}

int ClientBoard::disc_num() const
{
  int n = 0;
  uint4 i;
  
  FOR (i, ClientBoardType::MAX_BOARD_SIZE) {
    if (squares[i] == BLACK || squares[i] == WHITE) n++;
  }

  return n;
}

sint4 ClientBoard::result() const
{
  assert(is_finished());

  int b_num = disc_num(BLACK);
  int w_num = disc_num(WHITE);
  int e_num = disc_num(EMPTY);

  int res = b_num - w_num;

  if (e_num)
    if      (res > 0) res += e_num;
    else if (res < 0) res -= e_num;

  return res;
}

#endif


String ClientBoard::to_string() const
{
  String s = "", t;
  sint4 w = width();
  
  s += "  ";
  FORS (x, w) { t.form(" %c", 'A'+x); s += t; }
  s += "\n";
      
  FORS (y, w) {
    
    t.form("%2d", y+1); s += t;
    
    FORS (x, w) {
      s += " "; s += (char)cont2char(sq[xy2ind(x, y)]);
    }

    t.form(" %-2d", y+1); s += t + "\n";
  }
  
  s += "  ";
  FORS (x, w) { t.form(" %c", 'A'+x); s += t; }
  s += "\n\n";
  if (turn_color == ClientColor::BLACK) s += "* to move"; else s += "O to move";
  s += "\n";
  return s;
}

ClientBoard::~ClientBoard() { } 



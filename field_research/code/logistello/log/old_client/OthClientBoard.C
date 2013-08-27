/*
    (c) Michael Buro, mic@research.nj.nec.com
    NEC Research Institute
    4 Independence Way
    Princeton, NJ 08540, USA

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


#include "OthClientBoard.H"
#include "OthClientMove.H"
#include <vector>
#include <algorithm> 

// void ClientBoard::setup()
// {
//   int width = type().board_width();
  
//   assert((width & 1) == 0);
//   int ul = width/2-1;

//   sq[ClientBoard::xy2ind(ul, ul)]     = WHITE;
//   sq[ClientBoard::xy2ind(ul+1, ul+1)] = WHITE;
//   sq[ClientBoard::xy2ind(ul, ul+1)]   = BLACK;
//   sq[ClientBoard::xy2ind(ul+1, ul)]   = BLACK;

//   turn_color = ClientColor::BLACK;
// }
 

void OthClientBoard::setup()
{
  clear();

  if (type->get_code() == 88) {

    sq[ClientBoard::xy2ind(0,0)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(0,1)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(1,0)] = ClientBoard::BORDER;
      
    sq[ClientBoard::xy2ind(9,9)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(9,8)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(8,9)] = ClientBoard::BORDER;
      
    sq[ClientBoard::xy2ind(0,9)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(0,8)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(1,9)] = ClientBoard::BORDER;
      
    sq[ClientBoard::xy2ind(9,0)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(8,0)] = ClientBoard::BORDER;
    sq[ClientBoard::xy2ind(9,1)] = ClientBoard::BORDER;
  }

}

bool OthClientBoard::is_finished() const
{
  if (!no_moves(false)) return false;
  return no_moves(true);
}


sint4 OthClientBoard::char2cont(char c) const
{
  switch (c) {
  case '*': return BLACK;
  case 'O': return WHITE;
  case '-': return EMPTY;
  default:
    ERR("illegal char in board.sgf");
  }
  return EMPTY;
}

sint1 OthClientBoard::cont2char(sint4 co) const
{
  switch (co) {
  case BLACK:  return '*';
  case WHITE:  return 'O';
  case EMPTY:  return '-';
  case BORDER: return '#';
  default:
    ERR("illegal cont");
  }
  return '?';
}

bool OthClientBoard::no_moves(bool opponent) const
{
  OthClientBoard bo = *this;
  
  if (opponent) bo.toggle_to_move();

  FORS (i, ClientBoard::MAX_BOARD_SIZE) {
    OthClientMove mv(i);
    if (bo.make_move(mv)) return false;
  }

  return true;
}


int OthClientBoard::disc_num(sint4 cont) const
{
  int n = 0;
  
  FORS (i, ClientBoard::MAX_BOARD_SIZE) {
    if (sq[i] == cont) n++;
  }

  return n;
}

int OthClientBoard::disc_num() const
{
  int n = 0;
  
  FORS (i, ClientBoard::MAX_BOARD_SIZE) {
    if (sq[i] == BLACK || sq[i] == WHITE) n++;
  }

  return n;
}


// sint4 OthClientBoard::result() const
// {
//   assert(is_finished());

//   int b_num = disc_num(BLACK);
//   int w_num = disc_num(WHITE);
//   int e_num = disc_num(EMPTY);

//   int res = b_num - w_num;

//   if (e_num)
//     if      (res > 0) res += e_num;
//     else if (res < 0) res -= e_num;

//   return res;
// }


// true iff move ok

bool OthClientBoard::make_move(const ClientMove &mv0)
{
  bool move_ok = false;

  const OthClientMove &mv = dynamic_cast<const OthClientMove&>(mv0);

  assert(mv.is_valid());

  if (mv.is_pass()) {
    if (no_moves()) {
      // pass OK, toggle color
      toggle_to_move();
      return true;
    }
    return false;
  }
  
  if (sq[mv.sq] != ClientBoard::EMPTY) return false;

  sint4 turn_cont, opp_cont;
  
  if (turn_color == ClientColor::BLACK) { turn_cont = BLACK; opp_cont = WHITE; }
  else                                  { turn_cont = WHITE; opp_cont = BLACK; }

  FORS (i, 8) {

    if (sq[mv.sq + d[i]] == opp_cont) {

      // scan line

      int pos = mv.sq + d[i];

      while (sq[pos] == opp_cont) pos += d[i];

      if (sq[pos] == turn_cont) {

	move_ok = true;

	// flip discs

	while (sq[pos] != EMPTY) {
	  sq[pos] = turn_cont;
	  pos -= d[i];
	}
      }
    }
  }

  if (move_ok) {

    // place new disc and toggle color
      
    sq[mv.sq] = turn_cont;
    toggle_to_move();
  }
  
  return move_ok;
}





#if 0
void OthClientBoard::write(ostream &os, sint4 protocol) const
{
  sint4 w = type.board_width(), x, y;
  
  switch (protocol) {

  case 0:
    //os << "type: ";
    //type.write(os);
    //os << EOL;
    os << "  ";
    FOR (x, w) os.form(" %c", 'A'+x);
    os << endl;
      
    FOR (y, w) {
      os.form("%2d", y+1);
      FOR (x, w) {
	ClientBoard::SqCont co = squares[ClientBoard::xy2ind(x, y)];
	if      (co == BLACK) os << " *";
	else if (co == WHITE) os << " O";
	else if (co == EMPTY) os << " -";
	else                  os << "  ";
      }
      os.form(" %-2d", y+1);
      os << endl;
    }

    os << "  ";
    FOR (x, w) os.form(" %c", 'A'+x);
    os << endl << endl;
    if (get_to_move() == ClientColor::BLACK) os << "* to move"; else os << "O to move";
    os << endl;
    break;
  
  case 1:
    FOR (y, w) {
      FOR (x, w) {
	ClientBoard::SqCont co = squares[ClientBoard::xy2ind(x, y)];
	if      (co == BLACK) os << "*";
	else if (co == WHITE) os << "O";
	else if (co == EMPTY) os << "-";
	else                                os << " ";
      }
      os << endl;
    }
    if (get_to_move() == ClientColor::BLACK) os << "* to move"; else os << "O to move";
    os << endl;
    break;

  default: ERR("unknown protocol");
  }

}
#endif


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


#include "OthClientMove.H"
#include "ClientBoard.H"

// parse ascii-move [e.g. "E13" ]
// return true iff syntax correct

String OthClientMove::str2coords(String &s)
{
  if (s == "") return "empty move string";
  
  if (s == "pa" || s == "PA" || s == "pass" || s == "PASS") {

    // pass move

    sq = PASS;

  } else {

    int x = toupper(s[0]) - 'A';
    if (x < 0 || x >= ClientBoard::MAX_BOARD_WIDTH) {
      return "move-x out of range";
    }

    const char *p = s.c_str();
    int y;
  
    if (sscanf(&p[1], "%d", &y) != 1) {
      return "missing move-y";
    }

    --y;
    
    if (y < 0 || y >= ClientBoard::MAX_BOARD_WIDTH) {
      return "move-y out of range";
    }
 
    sq = ClientBoard::xy2ind(x, y);
  }

  return "";
}


bool OthClientMove::is_valid() const
{
  return sq == PASS || (sq >= 0 && sq < ClientBoard::MAX_BOARD_SIZE);
}

String OthClientMove::coords2str() const
{
  return coord2str(sq);
}

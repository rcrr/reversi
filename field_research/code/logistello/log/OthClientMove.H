// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

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


#ifndef OthClientMove_H
#define OthClientMove_H

#include "ClientMove.H"

class OthClientMove : public ClientMove
{

private:
  
  sint4 sq;    // base Client::BoardType::DX

  OthClientMove &operator=(const OthClientMove &x) {
    ERR("private operator=");
    return *this;
  }

public:

  OthClientMove(sint4 s = PASS) { sq = s; }

  OthClientMove(const OthClientMove &x) { copy_of(x); }

  void copy_of(const ClientMove &x) {
    ClientMove::copy_of(x);
    const OthClientMove &y = dynamic_cast<const OthClientMove&>(x);
    sq = y.sq;
  }

  ClientMove *clone() const { return new OthClientMove(*this); }

  sint4 get_sq() const { return sq; } 
  
  bool   is_pass() const { return sq == PASS; }
  
  void   init(int c1, int c2, int c3) { sq = c1; }; 
  String coords2str() const;
  String str2coords(String &s);
  bool   is_valid() const;

  friend class OthClientBoard;
};


#endif


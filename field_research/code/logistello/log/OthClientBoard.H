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


#ifndef ClientOthBoard_H
#define ClientOthBoard_H

#include "ClientBoard.H"
#include "OthClientBoardType.H"

class OthClientBoard : public ClientBoard
{ 

private:

  sint4 disc_num(sint4 cont) const;
  sint4 disc_num() const;
  bool  no_moves(bool opponent = false) const;

  OthClientBoard &operator=(const OthClientBoard &x) {
    ERR("private operator=");
    return *this;
  }

public:

  OthClientBoard() {
    type = new OthClientBoardType();
  }

  OthClientBoard(const OthClientBoard &x) { copy_of(x); }

  void copy_of(const ClientBoard &x) {
    ClientBoard::copy_of(x);
  }
  
  OthClientBoard *clone() const { return new OthClientBoard(*this); }

  void  setup();
  bool  make_move(const ClientMove &mv);
  bool  is_finished() const;
  sint4 char2cont(char c) const;
  sint1 cont2char(sint4 cont) const;  

};

#endif


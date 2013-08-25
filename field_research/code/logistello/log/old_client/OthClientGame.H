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


#ifndef OthClientGame_H
#define OthClientGame_H

#include "ClientGame.H"
#include "OthClientMove.H"
#include "OthClientGameType.H"
#include "OthClientBoard.H"
 
class OthClientGame : public ClientGame {

public:

  OthClientGame() {

    // init base class

    type      = new OthClientGameType();
    start_pos = new OthClientBoard();
    curr_pos  = new OthClientBoard();    
  }

  ClientMove *new_ClientMove() const { return new OthClientMove(); }

  ~OthClientGame() {}
};


#endif

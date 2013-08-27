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


#ifndef OthClientBoardType_H
#define OthClientBoardType_H

#include "ClientIncludes.H"
#include "ClientBoardType.H"

class OthClientBoardType : public ClientBoardType
{

public:

  OthClientBoardType() {

    types.erase(types.begin(), types.end());
    names.erase(names.begin(), names.end());

    TYPE_DEFAULT = 8;
    
    FORS (i, 6) {
      types.push_back(4 + 2*i);
      String s; s.form("%dx%d", types[i], types[i]);
      names.push_back(s);
    }

    types.push_back(88);
    names.push_back("88oct");
      
    bt = TYPE_DEFAULT;
    if (get_type_index() < 0) ERR("illegal default board type");
  }

  OthClientBoardType(const OthClientBoardType &x) { *this = x; }

  OthClientBoardType &operator=(const OthClientBoardType &x) {
    ((ClientBoardType*)this)->operator=((ClientBoardType&)x);
    return *this; 
  }
  
  ClientBoardType *clone() const { return new OthClientBoardType(*this); }
  

  sint4 board_width() const {
    if (bt == 88) return 10;
    else return bt;
  }
  
  sint4 board_squares() const {
    if (bt == 88) return 88;
    return bt*bt;
  }
 

};


#endif


// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "psbinfo.h"

ostream &operator << (ostream &os, BoardInfo &info)
{
  int x, y;
  ULL mask = 1LL;

  FOR (y, 8) {
    FOR (x, 8) {
      if (info.black.bits & mask) 
	os << "x";
      else if (info.white.bits & mask) 
	os << "o";
      else
	os << "-";
      mask <<= 1;
    }
    os << endl;
  }

  os << endl << "res= " << int(info.result) << endl;

  return os;
}


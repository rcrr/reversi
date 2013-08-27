// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef BITINFO_H
#define BITINFO_H

class BitInfo {

public:

  vector<uint4> bits[3*64];
  int index;
  uint4 mask;

  BitInfo() { 
    int i;
    index = 0; mask = 1; 
    FOR (i, 192) bits[i].push_back(0);
  }

  void set(int i, bool bit) 
  {
    if (bit) bits[i][index] |=  mask;
    else     bits[i][index] &= ~mask;
  }

  void next()
  {
    mask <<= 1;
    if (!mask) {
      mask = 1;
      index++;
      int i;
      FOR (i, 192) bits[i].push_back(0);
    }
  }

};

#endif

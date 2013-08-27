// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef RNDBOOK_H
#define RNDBOOK_H

#include "main.h"
#include "newgame.h"
#include <vector>

class RndBook {

public:

  vector<NewGame> prefixes;

  bool read(char *file_name);
  int find_rnd_move(NewGame &prefix);
};


#endif

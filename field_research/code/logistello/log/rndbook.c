// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "rndbook.h"


// return true <=> error

bool RndBook::read(char *file_name)
{
  FILE *fp;
  NewGame game;

  fp = fopen(file_name, "r");
  if (!fp) Error("can't open rnd-book file");

  FOREVER {
    if (!game.f_read(fp)) break;
    prefixes.push_back(game);
  }

  fclose(fp);

  cout << "rndbook: " << prefixes.size() << " prefixes read" << endl;
  return false;
}


// return ZUG_UNBEKANNT <=> no move found

int RndBook::find_rnd_move(NewGame &prefix)
{
  uint4 i;
  int hits[100];
  int total_hits=0;
  int next_move = ZUG_UNBEKANNT;

  FOR (i, 100) hits[i] = 0;

  FOR (i, prefixes.size()) {
    if (prefixes[i].is_proper_prefix(prefix, next_move)) {

      if (next_move <= 0 || next_move >= 100) Error("next-move out of range");
      hits[next_move]++;
      total_hits++;
    }
  }

  if (total_hits <= 1) return next_move;

  int sum = 0;
  int t = (random() % total_hits) + 1;

  FOR (i, 100) {
    if (hits[i]) {
      sum += hits[i];
      if (sum >= t) return i;
    }
  }

  Error("shouldn't get here");
  return ZUG_UNBEKANNT;
}


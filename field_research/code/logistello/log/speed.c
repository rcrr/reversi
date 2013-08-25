// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// return CPU speed compared to a PP200

#include "main.h"
#include "goodies.h"

static int a;

float speed() 
{
  int i;

  ZEIT z0, z1;
  CPUZeit(&z0);

  FOR (i, 150000000) a += a ^ 0x13456713;

  CPUZeit(&z1);
  
  float used = ZeitDiff(&z1, &z0);
  
  cout << used << " sec" << endl;

  return used;
}


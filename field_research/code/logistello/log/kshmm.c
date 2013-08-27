// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "shm.h"

void _abort() {}


int main()
{
  shm_kill();

  return 0;
}

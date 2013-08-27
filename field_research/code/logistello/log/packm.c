// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// pack configurations

#include "main.h"
#include "sparse.h"
#include "psconf.h"
#include "tab.h"

const float GROW_FACTOR = 1.05;


void _abort() {}


int main(int argc, char **argv)
{
  SparsePattern sp;

  if (argc != 2) {

  error:;

    Error("call: opack config.file(asc)");
    exit(20);
  }

  sp.read(argv[1]);
  sp.construct_hash_tab();
  
  return 0;
}


// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


#ifndef PRECOMP
#include "main.h"
#endif


#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "trans.h"


void _abort(void) {}

int main(int argc, char **argv)
{
  int i, j, k, l;
  SPFELD sf[1000], sft[8];
  FILE *fp;


  fp = fopen("xxx", "r");
  if (!fp) Error("xxx?");



  for (i=0;; i++) {

    if (!fSfRead(fp, &sf[i], 1)) break;

  }


printf("%d boards\n", i);


  FOR (j, i) {

    Transform(&sf[j], sft);

    FOR (k, j) {

      FOR (l, 8) {

        if (SfGleich(&sf[k], &sft[l])) printf("%d=%d\n", k, j);

      }

    }

  }

  return 0;
}


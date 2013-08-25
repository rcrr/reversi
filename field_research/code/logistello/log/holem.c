// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PRECOMP
#include "main.h"
#endif

#include "mid.h"
#include "move.h"
#include "eval.h"
#include "hash.h"
#include "killer.h"
#include "fpatt.h"
#include "order.h"

#include "crt.h"
#include "crtg.h"

void _abort(void) { exit(10); }

int main(void)
{
  int i;
  FILE *fp;
  SPFELD sf;

  fp = fopen("xxx.sfk", "r");

  if (!fp) exit(10);

  FOREVER {


    if (!fSfRead(fp, &sf, 1)) break;


    FOR (i, 100) 
      if (sf.p[i] == LEER) {

	int a;

	a = sf.p[i-1];

	if (a != LEER && sf.p[i+1] == a && sf.p[i+10] == a && sf.p[i-10] == a && 
	   sf.p[i+9] == a && sf.p[i-9] == a && sf.p[i+11] == a && sf.p[i-11] == a) {

	  SfAus(&sf, BLACK, 0); break;
        }
      }
  }

  fclose(fp);

  return 0;
}

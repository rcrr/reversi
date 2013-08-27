// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* compute new ProbCut table, 6'95*/

#ifndef PRECOMP
#include "main.h"
#endif

 
#include "sboard.h"
#include "move.h"
#include "board.h"
#include "crt.h"


void _abort(void) { exit(1); }

char *str[16] = {

/*  0 */ "copy ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 12 */ "     ",
/* 13 */ "copy ",
/* 14 */ "copy ",
/* 15 */ "copy "

};

struct {

  double a, b, s, r;

} tab[16];




void main(int argc, char **argv)
{
  int i;


  for (i=1; i <= 12; i++) {

    if (scanf("%lf %lf %lf %lf", 
        &tab[i].a, &tab[i].b, &tab[i].s, &tab[i].r) != 4)
      break;
  }

/* copy */

  tab[15] = tab[14] = tab[13] = tab[12];
  tab[ 0] = tab[ 1];

  printf(
    "/* discs          1/a        b         s             r^2          */\n"
  );

  FOR (i, 16)

    printf("/* %2d-%2d */  { %8.5f, %8.5f, %8.5f }, /* %8.5f  %s */\n",
      2+i*4, 5+i*4, 1.0/tab[i].a, tab[i].b, tab[i].s, tab[i].r, str[i]);

}


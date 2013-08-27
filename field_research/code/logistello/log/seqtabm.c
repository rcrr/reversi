// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* compute ProbCut table, 1'95*/

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
/*  1 */ "copy ",
/*  2 */ " 6+6 ", /*0*/
/*  3 */ "inter",
/*  4 */ "14+6 ", /*1*/
/*  5 */ "inter",
/*  6 */ "22+6 ", /*2*/
/*  7 */ "inter",
/*  8 */ "30+6 ", /*3*/
/*  9 */ "inter",
/* 10 */ "38+6 ", /*4*/
/* 11 */ "copy ",
/* 12 */ "copy ",
/* 13 */ "copy ",
/* 14 */ "copy ",
/* 15 */ "copy "

};

struct {

  double a, b, s, r;

} tab[16];


void inter(int i)
{
  tab[i].a = 0.5 * (tab[i-1].a + tab[i+1].a);
  tab[i].b = 0.5 * (tab[i-1].b + tab[i+1].b);
  tab[i].s = 0.5 * (tab[i-1].s + tab[i+1].s);
  tab[i].r = 0.5 * (tab[i-1].r + tab[i+1].r);
}




void main(int argc, char **argv)
{
  int i;


  for (i=0; i < 5; i++) {

    if (scanf("%lf %lf %lf %lf", 
        &tab[i].a, &tab[i].b, &tab[i].s, &tab[i].r) != 4)
      break;
  }

/* copy */

  tab[15] = tab[14] = tab[13] = tab[12] = tab[11] = tab[10] = tab[4];
  tab[ 8] = tab[ 3];
  tab[ 6] = tab[ 2];
  tab[ 4] = tab[ 1];
  tab[ 2] = tab[ 1] = tab[ 0];

/* inter */

  inter(3); inter(5); inter(7); inter(9);

  printf(
    "/* discs          1/a        b         s             r^2          */\n"
  );

  FOR (i, 16)

    printf("/* %2d-%2d */  { %8.5f, %8.5f, %8.5f }, /* %8.5f  %s */\n",
      2+i*4, 5+i*4, 1.0/tab[i].a, tab[i].b, tab[i].s, tab[i].r, str[i]);

}


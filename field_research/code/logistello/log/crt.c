// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Textausgabe */

#include "main.h"
#include "crt.h"
#include "attr.h"
#include "sboard.h"


#ifdef AMIGA

extern Enable_Abort;

#else

int Enable_Abort=1;

void Chk_Abort(void)
{}

#endif



void _check(int)
{
  if (Enable_Abort) _abort();
}


void InitCrt(void)
{
  Enable_Abort = 1;

#ifndef AMIGA
  signal(SIGINT, _check);
#endif

}

void GotoXY(int x, int y)
{
  printf("\x1b[%d;%dH", y, x);
}



void sKoorAus(char *s, SFPOS Pos)
{
  if	  (Pos == ZUG_PASSEN)    sprintf(s, "pa");
  else if (Pos == ZUG_UNBEKANNT) sprintf(s, "??");
  else {

/*    if (!ZUG(Pos)) Error("kein Zug"); */

    sprintf(s, "%c%c", 'a'+Pos%10-1, '1'+Pos/10-1);
  }
}




void fKoorAus(FILE *fp, SFPOS Pos)
{
  char s[10];

  sKoorAus(s, Pos);
  fprintf(fp, s);
}



void KoorAus(SFPOS Pos)
{
  fKoorAus(stdout, Pos);
}



void sGKoorAus(char *s, SFPOS Pos)
{
  if	  (Pos == ZUG_PASSEN)    sprintf(s, "pa");
  else if (Pos == ZUG_UNBEKANNT) sprintf(s, "  ");
  else {

/*    if (!ZUG(Pos)) Error("kein Zug"); */

    sprintf(s, "%c%c", 'A'+Pos%10-1, '1'+Pos/10-1);
  }
}




void fGKoorAus(FILE *fp, SFPOS Pos)
{
  char s[10];

  sGKoorAus(s, Pos);
  fprintf(fp, s);
}



void GKoorAus(SFPOS Pos)
{
  fGKoorAus(stdout, Pos);
}



void Error(char *s)
{
  fprintf(stderr, "*** %s\n", s); 
  exit(1);
}

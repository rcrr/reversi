// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "sboard.h"

void _abort() {} 

int main()
{
  int i, j;
  SPFELD sf;

  for (i=0; i<8; i++) {

    char s[1000];

    if (!fgets(s, 999, stdin)) break;

    int pos = 11+i*10;

    for (j=0; j<8; j++) {
      switch(s[j]) {
      case '.' : sf.p[pos+j] = LEER; break;
      case 'b' : sf.p[pos+j] = BLACK; break;
      case 'w' : sf.p[pos+j] = WHITE; break;
      }
    }
    //    printf("%s\n", s);

  }

  int c = fgetc(stdin);

  if (feof(stdin) || ferror(stdin)) Error("read error");
  
  if (c == 'w') { SfInvert(&sf); }

  printf("\n-> ##\n");

  SfAus(&sf, BLACK, 0);
    

}

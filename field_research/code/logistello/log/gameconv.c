// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <stdio.h>

main()
{
  int i;
  char s[1000];

  for (;;) {

    if (!scanf("%s", s)) exit(0);

    if (feof(stdin)) exit(0);

    for (i=0;; i+=3) {

      if (!s[i]) break;

      if (s[i] == '+' || s[i] == '-') printf("%c", s[i]); 
      else { fprintf(stderr, "no +-"); exit(20); }

      printf("%c%c", 'a'+s[i+1]-'1', '1'+s[i+2]-'1');
    }    

    printf(": +0\n");
  }
  return 0;
}

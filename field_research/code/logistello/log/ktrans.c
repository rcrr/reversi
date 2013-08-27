// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// transform Keyano format into L's / 7.97

#include <stdio.h>

#define N 100

main()
{
  int i;
  char line[N+1];

  for (;;) {
    
    if (!fgets(line, N, stdin)) break;

    if (strlen(line) != 75) {
      fprintf(stdout, "error\n");
      exit(20);
    }

    int player = 1;

    for (i=0; i < 36; i++) {
      int x = (line[i]-48) % 8;
      int y = (line[i]-48) / 8;

      if (line[i] < 0) {
	printf("pass?");
	break;
      }
      if (player > 0) printf("+"); else printf("-"); 
      printf("%c%c", y+'a', x+'1');

      player = -player;
    }

    printf(": +00\n");

  }
}


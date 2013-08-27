// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <stdio.h>

#define N 1000


main()
{
  char line[N+1], line1[N+1], line2[N+1];
  int a, b;
  int t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11,  t12, t13;

  while (!feof(stdin)) {

    if (fscanf(stdin, "%s %d %d %d %d %d %d %d %d %d %d %d %d %d", 
	 line1, &t1, &t2, &t3, &t4, &t5, &t6, &a, &t8, &t9, &t10, &t11, &t12, &t13) != 14) exit(20);

    fscanf(stdin, "%s %d %d %d %d %d %d %d %d %d %d %d %d %d", 
	 line2, &t1, &t2, &t3, &t4, &t5, &t6, &b, &t8, &t9, &t10, &t11,  &t12, &t13);

    if (strcmp(line1, line2)) { fprintf(stderr, "different patterns\n"); exit(20); }

    printf("%s %d %d %d %c\n", line1, abs(a-b), a, b, (a < 0) ^ (b < 0) ? '!' : ' ');
  }
}

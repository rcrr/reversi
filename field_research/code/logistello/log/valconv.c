// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <stdio.h>

int main()
{
  char line[1000];
  float v[7];
  FILE *fpout;

  fgets(line, 999, stdin);
  fgets(line, 999, stdin);
  fgets(line, 999, stdin);
  fgets(line, 999, stdin);
  fgets(line, 999, stdin);

  fpout = fopen("out.bin", "w");

  while (!feof(stdin)) {

    fgets(line, 999, stdin);

    if (feof(stdin)) break;

    if (sscanf(line, "%f %f %f %f %f %f %f", 
               &v[0], &v[1], &v[2], &v[3], &v[4], &v[5], &v[6]) == 7) {

      fwrite(v, sizeof(v), 1, fpout);

    } else {

      printf(line);

    }

  }

  fclose(fpout);

  return 0;

}

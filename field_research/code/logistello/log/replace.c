// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <stdio.h>
#include <string.h>

#define N 200000

struct Info {

  char str1[200], str2[10], str3[10];
  int  len;
  int  replaced;

};


void Error(char *s)
{
  fprintf(stderr, "*** %s\n", s);
  exit(20);
}


main()
{
  Info *infos = new Info[N];
  FILE *fp;
  int i, n;

  fp = fopen("gam1", "r");
  if (!fp) Error("gam1?");

  for (n=0; n < N; n++) {

    Info &inf = infos[n];

    int ret = fscanf(fp, "%s %s %s", inf.str1, inf.str2, inf.str3);
    if (ret == EOF) break;
    if (ret != 3) Error("!=3");

    inf.replaced = 0;
    inf.len = strlen(inf.str1);
  }

  fclose(fp);

  printf("%d line(s) read\n", n);


  int n_rep = 0;

  fp = fopen("gam2", "r");
  if (!fp) Error("gam2?");

  for (;;) {

    Info inf;

    int ret = fscanf(fp, "%s %s %s", inf.str1, inf.str2, inf.str3);
    if (ret == EOF) break;
    if (ret != 3) Error("!=3");

    inf.len = strlen(inf.str1);

    for (i=0; i < n; i++) {
      if (inf.len == infos[i].len &&
          !strncmp(inf.str1, infos[i].str1, inf.len-4)) {
	if (infos[i].replaced) {
	  if (strcmp(infos[i].str1, inf.str1) || 
	      strcmp(infos[i].str2, inf.str2) || 
	      strcmp(infos[i].str3, inf.str3)) {
	    printf("%s %s %s\n%s %s %s\n", 
		   infos[i].str1, infos[i].str2, infos[i].str3,
		   inf.str1, inf.str2, inf.str3);
	    printf("->replaced twice with different value!\n");
	  }
	}

	strcpy(infos[i].str1, inf.str1);
        strcpy(infos[i].str2, inf.str2);
        strcpy(infos[i].str3, inf.str3);
	infos[i].replaced++;
	n_rep++;
      }
    }
  }

  fclose(fp);

  printf("%d line(s) replaced\n", n_rep);

  fp = fopen("gam3", "w");
  if (!fp) Error("gam3?");

  for (i=0; i < n; i++) {
    if (infos[i].replaced < 2) 
      fprintf(fp, "%s %s  %s\n", infos[i].str1, infos[i].str2, infos[i].str3);
  }
  
  fclose(fp);

}

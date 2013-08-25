// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "pcstat.h"
#include "eval.h"

String PCStatFile    = "pcstata";
String EndPCStatFile = "endpcstata";


void PCStat::init()
{
  int i, j, k;

  // clear all entries 

  FOR (k, NUM)
    FOR (i, MAX_PC_DEPTH+1)
      FOR (j, MAX_PC_DEPTH+1)
        App[k][i][j].defined = false;
  
}


void PCStat::read(const String &filename)
{
  FILE *fp;
  char line[1000];
  int disc_num = 0;

  cout << "[ loading pcstat-file '" << filename << "' ..." << flush;

  fp = fopen(filename.c_str(), "r");
  if (!fp) Error("PCStat::Read: file not found");
		 
  init();

  while (!feof(fp)) {
    
    int d1, d2, diff;
    float a, b, s, r;    

    if (!fgets(line, 999, fp)) break;

    if (line[0] == '#' || line[0] == '\n') continue;  // skip remarks

    //printf("line= %s\n", line);

    if (sscanf(line, "::: %d", &disc_num) == 1) {

      // new disc-num

      //printf("discnum=%d\n", disc_num);

      if (disc_num < MIN_NUM || disc_num > MAX_NUM) {

	fprintf(stderr, "\n>>> %s\n", line);
	Error("PCStat::Read: disc-num out of range");
      }

    } else {

      if (sscanf(line, "%d %d ( %d ): ### Y = %f * X + %f s = %f rxy^2 = %f",
		 &d1, &d2, &diff, &a, &b, &s, &r) != 7) {

        corrupt:

	fprintf(stderr, "\n>>> %s\n", line);
        Error("PCStat::Read: corrupt line");
      }

      if (d1 < 0 || d1 > MAX_PC_DEPTH) goto corrupt;
      if (d2 < 0 || d2 > MAX_PC_DEPTH) goto corrupt;
      if (d2 <= d1) goto corrupt;
      // if ((d2-d1) & 1) goto corrupt;
      if (diff != d2 - d1) goto corrupt;
      if (s < 0) goto corrupt;
      if (r < 0 || r > 1) goto corrupt;

      if (disc_num < MIN_NUM || disc_num > MAX_NUM) {

        printf("!!!discnum=%d\n", disc_num);
	fprintf(stderr, "\n>>> %s\n", line);
        Error("PCStat::Read: disc_num out of range");
	
      }

      AppInfo &inf = App[disc_num-MIN_NUM][d1][d2];

      if (inf.defined) {

	fprintf(stderr, "\n>>> %s\n", line);
        Error("PCStat::Read: entry already defined");	
      }

      inf.defined = true;
      inf.inva    = 1.0/a;
      inf.b       = b;
      inf.s       = s;
    }
  }

  fclose(fp);

  loaded = true;

  printf("OK ]\n");

#if 0

  // check completeness

  int i, j, k;

  FOR (k, NUM)
    FOR (i, MAX_PC_DEPTH+1)
      for (j=i+2; j <= MAX_PC_DEPTH; j+=2)
	if (!App[k][i][j].defined) { 
	  fprintf(stderr, "*** PCStat::Read: (%d %d %d) entry not defined\n", k, i, j);
	  exit(20);
	}
#endif


}


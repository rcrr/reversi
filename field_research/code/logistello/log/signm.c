// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* print features with regard to signs  */
 
#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h" 

#define FLAG_EQ	1
#define FLAG_GR	2

#define MAXLEN	200
#define MAXVAR	50



void _abort(void) { exit(1); }


int main(int argc, char **argv)
{
  char	    c, s[MAXLEN+1], Features[MAXVAR+1][MAXLEN+1],
	    FeatureNames[MAXVAR+1][MAXLEN+1];
  FILE	    *fpin1, *fpin2;
  int	    argi, i, j, fnum, Flags[MAXVAR];
  float	    r;
  bool	    f_padd = false;


  if (argc < 2 || argc > 4) {

Error("call: osign [-padd] mfl-file [par-file]");
  }

  argi = 1;

  if (argv[argi] && !strcmp(argv[argi], "-padd")) {
    f_padd = true;
    argi++;
  }

  fpin1 = fopen(argv[argi++], "r");
  if (!fpin1) Error("osign: can't read mfl-file");

  if (argv[argi]) {
    fpin2 = fopen(argv[argi], "r");
    if (!fpin2) Error("osign: can't read par-file");
  } else 
    fpin2 = NULL;
    

  if (f_padd && !fpin2) Error("-padd only with par-file");

  fnum = 0;

  FOREVER {

    if (!fgets(&Features[fnum][0], MAXLEN, fpin1)) break;

    if (Features[fnum][0] == '\n') break;

  
    fnum++;
  }

/*fprintf(stderr, "%d features\n", fnum);*/

  FOR (i, fnum) {


    if ((j=sscanf(Features[i], "%s", FeatureNames[i])) != 1) {
      Error("featurename corrupt");
    }

    Flags[i] = 0;
 
    FOR (j, strlen(Features[i])) {

      c = Features[i][j];

      if      (c == '>') { Flags[i] = FLAG_GR; break; }
      else if (c == '=') { Flags[i] = FLAG_EQ; break; }

    }
  }

  fclose(fpin1);

/* skip intercept */

  if (fpin2) {
    if (!fgets(s, MAXLEN, fpin2) || sscanf(s, "%f", &r) != 1) 
	Error("intercept missing in par-file");

    if (f_padd) printf("%.4e,\n", r);
  }


  FOR (i, fnum) {

    if (Flags[i] != FLAG_EQ && fpin2) { 

      if (!fgets(s, MAXLEN, fpin2) || sscanf(s, "%f", &r) != 1) 
	Error("missing value in par-file");
    }

    if (fpin2) {

      if (!f_padd) {

        if (!Flags[i]) 		    
  	  printf("%s\n", FeatureNames[i]);
        else if (Flags[i] == FLAG_GR && r > 0)
	  printf("%s\t>\n", FeatureNames[i]);
	else 
	  printf("%s\t=\n", FeatureNames[i]);

      } else {

        if (!Flags[i]) 		    
  	  printf("%.4e,\n", r);
        else if (Flags[i] == FLAG_GR && r > 0)
	  printf("%.4e,\n", r);
	else if (Flags[i] == FLAG_EQ)
	  printf("0,\n");
	else Error("value < 0 for >feature");

      }


    } else {

      if (Flags[i] != FLAG_EQ) printf("%s\n", FeatureNames[i]);

    }


  }

  if (f_padd) printf("\n");

  if (fpin2 && fgets(s, MAXLEN, fpin2) && sscanf(s, "%f", &r) == 1) {
	Error("to many values in par-file");
  }

  fclose(fpin1); fclose(fpin2);
  return 0;
}


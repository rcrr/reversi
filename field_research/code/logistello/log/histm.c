// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "logmat.h"
#include "logwls.h"


#define CELLMAX	1000


enum { OPTMIN, OPTMAX, OPTMEAN, OPTSTD, OPTMSTD, OPTPSTD, OPTDIFF, OPTNO }
 OPTIONS;

#define OPTNUM 7

char Opt[][20] = {

  "-min",
  "-max", 
  "-mean", 
  "-std",
  "--std",
  "-+std",
  "-diff"

};


void _abort(void) { exit(20); }


 
int main(int argc, char **argv)
{
  int   argi, i, ind, n, num[CELLMAX], zuerst, Zellen;
  double r, meany=0, sumyy=0, sumyeye=0, su=0, sq=0;
  double *a, 
    h[CELLMAX], min[CELLMAX], max[CELLMAX], mean[CELLMAX], pstd[CELLMAX],
    mstd[CELLMAX], std[CELLMAX], sum[CELLMAX], square[CELLMAX], diff[CELLMAX];
  FILE *fp;
  double gmin, gmax, gsum=0, gsquare=0;
  char s[300];
  bool optf[OPTNUM+1], f_int=false, f_intb=false, f_reg=false, first[CELLMAX];
  bool f_yeqx=false;
  MATRIX *pX, *pn, *py, *pbeta;

  if (argc == 1) { 

error:

fprintf(stderr, "\
*** call: ohist (OUT | [OUT] -reg) [-y=x] [INT] #cells data-file\n\
      OUT: -min | -max | -mean | -std | --std | -+std | -diff: mode\n\
      INT: -int xmin xmax | -intb xmin xmax (use borders): intervall\n\n");
exit(20); 

  } 
 
  
  FOR (i, OPTNUM+1) optf[i] = false;

  argi = 1;

  if (!argv[argi]) goto error;

  FOR (i, OPTNUM)
    if (!strcmp(argv[argi], Opt[i])) { argi++; break; }

  optf[i] = true;


  if (!argv[argi]) goto error;

  if (!strcmp(argv[argi], "-reg")) { f_reg = true; argi++; }

  if (!strcmp(argv[argi], "-y=x")) { f_yeqx = true; argi++; }

  if (!strcmp(argv[argi], "-int") || !strcmp(argv[argi], "-intb")) { 

    if (!strcmp(argv[argi], "-int")) f_int = true; else f_intb = true;

    argi++;

    if (!argv[argi] || sscanf(argv[argi], "%lf", &gmin) != 1) goto error;

    argi++;

    if (!argv[argi] || sscanf(argv[argi], "%lf", &gmax) != 1) goto error;

    argi++;

    if (gmin >= gmax) goto error;

  }

  if (!argv[argi]) goto error;



  if (sscanf(argv[argi], "%d", &Zellen) != 1 || 
      Zellen <= 0 || Zellen > CELLMAX)
  {
    fprintf(stderr, "*** #cells?\n"); exit(20); 
  } 
 
  argi++;

  if (!argv[argi]) goto error;

  int no_int = !(f_int || f_intb);

  if (no_int || f_reg) {


    fp = fopen(argv[argi], "r");
    if (!fp) { fprintf(stderr, "*** can't open file\n"); exit(20); } 

    if (!f_int) gmin = gmax = 0;
    zuerst = 1;

    n = 0;

    while (!feof(fp)) {

      if (!fgets(s, 255, fp)) break;

      if (s[0] == '#') continue;

      if ((i=sscanf(s, "%lf", &r)) != 1) {

        if (i < 0) break;
        else { fprintf(stderr, "*** no values\n"); exit(20); }
      }

      if (no_int || (f_reg && r >= gmin && r <= gmax)) n++;

      if (no_int) {

        if (zuerst) { zuerst = 0; gmin = gmax = r; }
        else {

          if (r < gmin) gmin = r;
          if (r > gmax) gmax = r;
        }
      }
    }
 
    fclose(fp);
 


  }

  if (f_reg) {

printf("# %d pairs\n", n);

    pX    = MatMake(n, 2);
    py    = MatMake(1, n);
    pn    = MatMake(n, 1);
    pbeta = MatMake(2, 1);

    if (!pX || !py || !pn || !pbeta) Error("mem");

  }



  fp = fopen(argv[argi], "r");
  if (!fp) { fprintf(stderr, "*** file not found\n"); exit(20); } 

  n = 0;


  FOR (i, CELLMAX) { 

    h[i] = sum[i] = square[i] = 0.0;
    num[i] = 0; 
    first[i] = true; 
  }



  while (!feof(fp)) {

    double x, y;


    if (!fgets(s, 255, fp)) break;

    if (s[0] == '#') continue;

    if (f_yeqx) { 

      if (sscanf(s, "%lf", &x) != 1) break;

      y = x;

    } else {

      if (sscanf(s, "%lf %lf", &x, &y) != 2) break;

    }

    if (f_intb || (x >= gmin && x <= gmax)) {

      if (f_intb) {

	if (x < gmin) x = gmin;
        if (x > gmax) x = gmax;
      }

      if (f_reg) {

        MAT(pX, n, 0) = 1.0;
        MAT(pX, n, 1) = x;
        VEK(py, n)    = y;
        VEK(pn, n)    = 1;
      }

      if (gmax == gmin) ind=0;
      else 	        ind = (int)(((x-gmin)*Zellen)/(gmax-gmin));


      if (ind < 0)       ind = 0;
      if (ind >= Zellen) ind = Zellen-1;

      num[ind]++;
      n++;

      if (first[ind]) {

        first[ind] = false;
        min[ind] = max[ind] = y;

      } else {

        if (y > max[ind]) max[ind] = y;
        if (y < min[ind]) min[ind] = y;
      }

      sum   [ind] += y;
      square[ind] += y*y;


      meany += y;

      //printf("%d %f %f\n", f_intb, x, y);

    }
  }

  fclose(fp);

  if (!n) { fprintf(stderr, "*** no data\n"); exit(20); } 


  meany /= n;


  if (f_reg) {


    if (Regression(pX, pn, py, pbeta)) Error("Regression error");

/*
    VEK(pbeta, 0) += 50;
*/

    printf("# Y = %.5f * X + %.5f\n", VEK(pbeta, 1), VEK(pbeta, 0));


/* compute error statistics */


    FOR (i, CELLMAX) { 

      h[i] = sum[i] = square[i] = 0.0;
      num[i] = 0; 
      first[i] = true; 
    }

    fp = fopen(argv[argi], "r");
    if (!fp) { fprintf(stderr, "*** can't open file\n"); exit(20); } 

    n = 0;

    while (!feof(fp)) {

      double x, y;

      if (!fgets(s, 255, fp)) break;

      if (s[0] == '#') continue;

      if ((i=sscanf(s, "%lf %lf", &x, &y)) != 2) {

        if (i < 0) break;
        else { fprintf(stderr, "*** no values\n"); exit(20); }
      }

      if (f_yeqx) y = x;

      if (x >= gmin && x <= gmax) {

/* printf("%.4f %.4f -> %.4f\n", x, y, VEK(pbeta, 1) * x + VEK(pbeta, 0)); */

	sumyy   += (y-meany)*(y-meany);
        sumyeye += (VEK(pbeta, 1) * x + VEK(pbeta, 0) - meany) *
		   (VEK(pbeta, 1) * x + VEK(pbeta, 0) - meany);

	y = VEK(pbeta, 1) * x + VEK(pbeta, 0) - y;

        if (gmax == gmin) ind=0;
        else 	          ind = (int)(((x-gmin)*Zellen)/(gmax-gmin));

        if (ind < 0)       ind = 0;
        if (ind >= Zellen) ind = Zellen-1;

        num[ind]++;
        n++;

        if (first[ind]) {

          first[ind] = false;
          min[ind] = max[ind] = y;

        } else {

          if (y > max[ind]) max[ind] = y;
          if (y < min[ind]) min[ind] = y;
        }

        sum   [ind] += y;
        square[ind] += y*y;

        su += y;
        sq += y*y;

      }

    }

    fclose(fp);

    if (sumyy != 0.0) {

      printf("# rxy^2 = %f\n", sumyeye/sumyy);

      printf("### Y = %8.5f * X + %8.5f  s = %f  rxy^2 = %f\n", 
        VEK(pbeta, 1), VEK(pbeta, 0), sqrt(sq/n), sumyeye/sumyy);
    }

  }


  FOR (i, Zellen) {

    double u;


    h[i] = ((double)num[i]) / n;

    if (num[i]) { 

      mean[i] = sum[i] / num[i];

      diff[i] = mean[i] - (gmin + (i * (gmax-gmin)) / Zellen);

#if 0
printf("%f %f %f %d\n", mean[i], sum[i], square[i], num[i]);
#endif
      u = square[i]/num[i]-(mean[i]*mean[i]);
      if (u < 0) u = 0;
      std [i] = sqrt(u);

      pstd[i] = mean[i]+std[i];
      mstd[i] = mean[i]-std[i];
    
    }

  }

  a = NULL;

  if (optf[OPTNO])   a = h;
  if (optf[OPTMIN])  a = min;
  if (optf[OPTMAX])  a = max;
  if (optf[OPTMEAN]) a = mean;
  if (optf[OPTSTD])  a = std;
  if (optf[OPTPSTD]) a = pstd;
  if (optf[OPTMSTD]) a = mstd;
  if (optf[OPTDIFF]) a = diff;

  FOR (i, Zellen) {
    printf("%f %f (%d)\n", 
      gmin + ((i+0.5) * (gmax-gmin)) / Zellen, a[i], num[i]);
  }

  printf("\n");

  return 0;
}


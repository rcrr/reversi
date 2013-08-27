// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "crt.h"
#include "tab.h"


#define ASCFILE false

void _abort(void) { exit(10); }

typedef struct {
  int n;
  float y, yy;
} Entry;

#define DISCS 10
#define N     59049

#define CLASS_EPS   0.15

#define PATT8(p1,p2,p3,p4,p5,p6,p7,p8) \
(3280+(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8]))

#define PATT10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
(29524+(3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10]))


static Entry patt[N], test_patt[N];  

static int map[5][5] = {

   4, 3, 2, 1, 0,
   8, 7, 6, 5,-1,
  11,10, 9,-1,-1,
  13,12,-1,-1,-1,
  14,-1,-1,-1,-1

};

static void Update(int index, float y, int n)
{
  patt[index].n  += n;
  patt[index].y  += y;
  patt[index].yy += y*y;
}


double Mean(double sum, double, int num)
{
  if (num <= 0) Error("Mean: num <= 0");

  return sum/(num+1);
}

double Mean(Entry &entr)
{
  if (!entr.n) return 0;
  return Mean(entr.y, entr.yy, entr.n);
}


double Var(double sum, double sum_sq, int num)
{
  if (num <= 0) Error("Mean: num <= 0");

  double var = sum_sq/(num+1) - (sum/(num+1))*(sum/(num+1));

  if (var < 0) return 0;
  return var;
}

double Var(Entry &entr)
{
  if (!entr.n) return 0;
  return Var(entr.y, entr.yy, entr.n);
}

static void WriteDisc(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else                      fprintf(fp, "O");
}


void WritePatt(FILE *fp, int n, int discnum)
{
  int i, r, Cont[20];

  if (discnum > 20) Error("PattOut: too many discs");

  FOR (i, discnum) {

    r = n % 3;
    
    if      (r == 0) Cont[i] = WHITE;
    else if (r == 1) Cont[i] = LEER;
    else             Cont[i] = BLACK;

    n /= 3;
  }

  FOR (i, discnum) SteinEinfAus(fp, Cont[discnum-1-i]);

}


static void SavePatt(Entry *patt, int disc_num, char *filename, double overall_mean)
{
  FILE *fp;
  int i;

  fp = fopen(filename, "w");
  if (!fp) Error("can't open patt-file");

  FOR (i, Pot3[disc_num]) {

    WritePatt(fp, i, disc_num);
    fprintf(fp, " %6d %7f -> %.3f %.3f ", 
	    patt[i].n, patt[i].y, Mean(patt[i]), sqrt(Var(patt[i])));

    if (patt[i].n > 0) fprintf(fp, "%.3f\n", abs(overall_mean - Mean(patt[i])));
    else               fprintf(fp, "%.3f\n", -1.0);

  }

  fclose(fp);
}


static void LoadPatt(Entry *patt, int disc_num, char *filename)
{
  FILE *fp;
  int i;
  char str[1000];
  int n;
  float y, q;
  
  fp = fopen(filename, "r");
  if (!fp) Error("can't open patt-file");

  FOR (i, Pot3[disc_num]) {

    if (fscanf(fp, "%s %d %f -> %f", str, &n, &y, &q) != 4) Error("patt-file corrupt");
    patt[i].n = n;
    patt[i].y = y;

  }

  fclose(fp);
}








int main(int argc, char **argv)
{
  int i, num = 0;
  double sum=0, sumsq=0, new_sum=0, new_sumsq=0;
  double cl_sum[15], cl_sumsq[15], all_sum=0, all_sumsq=0;
  int cl_num[15];
  double diff[15] = {
-0.402824,
-0.436765,
-0.409485,
-0.276368,
 0.016814,
-0.214648,
-0.161798,
-0.002205,
 0.345162,
 0.004723,
 0.177180,
 0.553983,
 0.335173,
 0.754056,
 0.968952
  };


  FOR (i, 15) { cl_sum[i] = cl_sumsq[i] = 0.0; cl_num[i] = 0; }

  if (argc == 1) Error("call: odelta [-read patt-file] sfk-files");

  int argi = 1;

  if (!strcmp(argv[argi], "-read")) {

    printf("loading pattern-file %s\n", argv[argi+1]);
    LoadPatt(test_patt, DISCS, argv[argi+1]);
    argi += 2;

  }

  for (; argv[argi]; argi++) {

    FILE *fp_boards, *fp_values;
    char *file = argv[argi], line[10000];

    fprintf(stderr, "%s ", file);

    fp_boards = fopen(file, "r");
    if (!fp_boards) { fprintf(stderr, "sfk-file not found!\n"); continue; }

#if ASCFILE

    sprintf(line, "%s.out", file);

#else

    sprintf(line, "%s.out.bin", file);

#endif

    fp_values = fopen(line, "r");
    if (!fp_values) { fprintf(stderr, "value-file not found\n"); continue; }

#if ASCFILE
    fgets(line, 999, fp_values);
    fgets(line, 999, fp_values);
    fgets(line, 999, fp_values);
    fgets(line, 999, fp_values);
    fgets(line, 999, fp_values);
#endif

    FOREVER {

      SPFELD sf;

      if (!fSfRead(fp_boards, &sf, 1)) break;
      

#define U(index) Update(index, delta, 1);



      sint1 *p = sf.p;
      float eval, v0, v1, v2, v3, v4, v5;

#if ASCFILE

      fgets(line, 999, fp_values);
      
      if (feof(fp_values)) { fprintf(stderr, "too few values!\n"); break; }

      if (sscanf(line, "%f %f %f %f %f %f %f", &eval, &v0, &v1, &v2, &v3, &v4, &v5) != 7) {
	fprintf(stderr, "corrupt values");
	break;
      }

#else

      short vals[7];

      if (fread(vals, sizeof(vals), 1, fp_values) != 1) {
        fprintf(stderr, "corrupt values");
        break;        
      }

      static float factor = 1.0/1000;

      eval = vals[0] * factor;
      v0 = vals[1] * factor;
      v1 = vals[2] * factor;
      v2 = vals[3] * factor;
      v3 = vals[4] * factor;
      v4 = vals[5] * factor;
      v5 = vals[6] * factor;

#endif

      float b = v5;
      float a = eval;

      if (abs(b) > 13) { /* fprintf(stderr, "*"); fflush(stderr); */ continue; }

      float delta = b - a;

      sum += delta;
      sumsq += delta*delta;
      num++;

      float e[4];

      e[0] = Mean(test_patt[PATT10(A1,B1,C1,D1,E1,F1,G1,H1,B2,G2)]);
      e[1] = Mean(test_patt[PATT10(A8,B8,C8,D8,E8,F8,G8,H8,B7,G7)]);
      e[2] = Mean(test_patt[PATT10(A1,A2,A3,A4,A5,A6,A7,A8,B2,B7)]);
      e[3] = Mean(test_patt[PATT10(H1,H2,H3,H4,H5,H6,H7,H8,G2,G7)]);

      // sort e[]

      FOR (i, 3) {

	for (int j=i+1; j < 4; j++) {

	  if (e[i] < e[j]) {

	    float t=e[i]; e[i] = e[j]; e[j] = t;

	  }
	}
      }

#if 0      
      SFPOS moves[100];
      int move_diff = SfMoeglZuege(&sf, BLACK, moves) -
                  SfMoeglZuege(&sf, WHITE, moves);

      printf("%f  100 %f %f %f %f %d\n", delta, e[0], e[1], e[2], e[3], move_diff);
#endif

#if 0
      float est_delta = 0.25 * (e[0] + e[1] + e[2] + e[3]);
#else
      float est_delta = 
// 0->2
	1.1044-1.371 +
	1.144e+00 * e[0] +
	4.101e-01 * e[1] + 
	5.694e-01 * e[2] + 
	1.134e-01 * e[3];
#endif

      float new_delta = b - (a + est_delta);
      new_sum += new_delta;
      new_sumsq += new_delta*new_delta;

      int n_good=0, n_bad=0;

      FOR (i, 4) {
	if (e[i] >  CLASS_EPS) n_good++;
	if (e[i] < -CLASS_EPS) n_bad++;
      }

      int n = map[n_good][n_bad];

      if (n < 0 || n > 14) Error("n?");

      cl_sum[n]   += delta;
      cl_sumsq[n] += delta * delta;
      cl_num[n]   ++;

      float all_delta = delta - diff[n];

      all_sum += all_delta;
      all_sumsq += all_delta*all_delta;

#if 0
      SfAus(&sf, 0, 0);
      WritePatt(stdout, PATT10(A1,B1,C1,D1,E1,F1,G1,H1,B2,G2), DISCS); 
      printf(" %f\n",Val(test_patt[PATT10(A1,B1,C1,D1,E1,F1,G1,H1,B2,G2)]));
      WritePatt(stdout, PATT10(A8,B8,C8,D8,E8,F8,G8,H8,B7,G7), DISCS);
      printf(" %f\n",Val(test_patt[PATT10(A8,B8,C8,D8,E8,F8,G8,H8,B7,G7)]));
      WritePatt(stdout, PATT10(A1,A2,A3,A4,A5,A6,A7,A8,B2,B7), DISCS);
      printf(" %f\n",Val(test_patt[PATT10(A1,A2,A3,A4,A5,A6,A7,A8,B2,B7)]));
      WritePatt(stdout, PATT10(H1,H2,H3,H4,H5,H6,H7,H8,G2,G7), DISCS);
      printf(" %f\n",Val(test_patt[PATT10(H1,H2,H3,H4,H5,H6,H7,H8,G2,G7)]));
#endif

#if 0
      printf("%f 100 %f\n", delta, est_delta);
#endif

      // compute all edge indices and update values

#if 1

      // edge+2X
      U(PATT10(A1,B1,C1,D1,E1,F1,G1,H1,B2,G2));
      U(PATT10(H1,G1,F1,E1,D1,C1,B1,A1,G2,B2));
      U(PATT10(A8,B8,C8,D8,E8,F8,G8,H8,B7,G7));
      U(PATT10(H8,G8,F8,E8,D8,C8,B8,A8,G7,B7));
      U(PATT10(A1,A2,A3,A4,A5,A6,A7,A8,B2,B7));
      U(PATT10(H1,H2,H3,H4,H5,H6,H7,H8,G2,G7));
      U(PATT10(A8,A7,A6,A5,A4,A3,A2,A1,B7,B2));
      U(PATT10(H8,H7,H6,H5,H4,H3,H2,H1,G7,G2));

#else

      // edge

      U(PATT8(A1,B1,C1,D1,E1,F1,G1,H1));
      U(PATT8(H1,G1,F1,E1,D1,C1,B1,A1));
      U(PATT8(A8,B8,C8,D8,E8,F8,G8,H8));
      U(PATT8(H8,G8,F8,E8,D8,C8,B8,A8));
      U(PATT8(A1,A2,A3,A4,A5,A6,A7,A8));
      U(PATT8(H1,H2,H3,H4,H5,H6,H7,H8));
      U(PATT8(A8,A7,A6,A5,A4,A3,A2,A1));
      U(PATT8(H8,H7,H6,H5,H4,H3,H2,H1));

#endif

    }

    fgets(line, 999, fp_values);
    if (!feof(fp_values)) { fprintf(stderr, "%s lengths differ!\n", line); }

    fclose(fp_boards); fclose(fp_values);

    fprintf(stderr, "\n");
  }

  printf("%d boards read\n", num);

  SavePatt(patt, DISCS, "patt.stat", Mean(sum, sumsq, num));


  printf("MEAN(delta)=%f STDD(delta)=%f\n",         
	 Mean(sum, sumsq, num), sqrt(Var(sum, sumsq, num)));

  printf("MEAN(new_delta)=%f STDD(new_delta)=%f\n", 
	 Mean(new_sum, new_sumsq, num), sqrt(Var(new_sum, new_sumsq, num)));


  printf("MEAN(all_delta)=%f STDD(all_delta)=%f\n",
         Mean(all_sum, all_sumsq, num), sqrt(Var(all_sum, all_sumsq, num)));

  printf("CLASS_EPS=%f\n", CLASS_EPS);

  FOR (i, 15) {

    printf("class %d: NUM=%d MEAN=%f STDD=%f\n",
         i, cl_num[i], Mean(cl_sum[i], cl_sumsq[i], cl_num[i]), 
         sqrt(Var(cl_sum[i], cl_sumsq[i], cl_num[i])));
  }

  return 0;
}


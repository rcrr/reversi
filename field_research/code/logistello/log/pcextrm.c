// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// extract positions with a given property (for probcut estimation improvement)
//   27.11.96

#include "main.h"
#include "crt.h" 
#include "sboard.h"
#include "tab.h"

void _abort(void) { exit(1); }

#define S8(p1,p2,p3,p4,p5,p6,p7,p8) \
 (3280+(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8]))

struct {

  float diff_sum, abs_diff_sum;
  int   n;

} edge_stat[6561];



void upd(int n, float diff)
{
  edge_stat[n].diff_sum += diff;
  edge_stat[n].abs_diff_sum += abs(diff);
  edge_stat[n].n++;
}


void update(SPFELD &sf, float diff)
{
  sint1 *p = sf.p;

  upd(S8(A1,B1,C1,D1,E1,F1,G1,H1), diff);
  upd(S8(H1,G1,F1,E1,D1,C1,B1,A1), diff);
  upd(S8(A8,B8,C8,D8,E8,F8,G8,H8), diff);
  upd(S8(H8,G8,F8,E8,D8,C8,B8,A8), diff);
  upd(S8(A1,A2,A3,A4,A5,A6,A7,A8), diff);
  upd(S8(H1,H2,H3,H4,H5,H6,H7,H8), diff);
  upd(S8(A8,A7,A6,A5,A4,A3,A2,A1), diff);
  upd(S8(H8,H7,H6,H5,H4,H3,H2,H1), diff);
}


int main(int argc, char **argv)
{
  int	 a, i, startanz, endanz;
  SPFELD sf;
  FILE   *fpsfk, *fpdiff;
  int    num=0, true_num=0;
  float  diff;

  if (argc != 3) { 

error:

    Error("usage: opcextr sfk-file diff-file\n");
  }

  fpsfk = fopen(argv[1], "r");
  if (!fpsfk) Error("sfk-file not found");

  fpdiff = fopen(argv[2], "r");
  if (!fpdiff) Error("diff-file not found");

int sum = 0;
float pts=0;

  FOREVER {

    if (!fSfRead(fpsfk, &sf, 1)) break;
    num++;

    /*
    if (fscanf(fpdiff, "%f", &diff) != 1) Error("missing diff-value");

    update(sf, diff);
    */

    pts += (sf.Marke - 100)/100.0;

    sint1 *p = sf.p;
    int c = +1;

if ((p[A1] == 0 && p[B1] == c && p[C1] == c && p[D1] == c && p[E1] == c && p[F1] == c && p[G1] == c && p[H1] == 0 && p[B2] == c && p[G2] == c)
||  (p[A8] == 0 && p[B8] == c && p[C8] == c && p[D8] == c && p[E8] == c && p[F8] == c && p[G8] == c && p[H8] == 0 && p[B7] == c && p[G7] == c)
||  (p[A1] == 0 && p[A2] == c && p[A3] == c && p[A4] == c && p[A5] == c && p[A6] == c && p[A7] == c && p[A8] == 0 && p[B2] == c && p[B7] == c)
||  (p[H1] == 0 && p[H2] == c && p[H3] == c && p[H4] == c && p[H5] == c && p[H6] == c && p[H7] == c && p[H8] == 0 && p[G2] == c && p[G7] == c)
|| (p[H1] == 0 && p[G1] == c && p[F1] == c && p[E1] == c && p[D1] == c && p[C1] == c && p[B1] == c && p[A1] == 0 && p[G2] == c && p[B2] == c)
||  (p[H8] == 0 && p[G8] == c && p[F8] == c && p[E8] == c && p[D8] == c && p[C8] == c && p[B8] == c && p[A8] == 0 && p[G7] == c && p[B7] == c)
||  (p[A8] == 0 && p[A7] == c && p[A6] == c && p[A5] == c && p[A4] == c && p[A3] == c && p[A2] == c && p[A1] == 0 && p[B7] == c && p[B2] == c)
||  (p[H8] == 0 && p[H7] == c && p[H6] == c && p[H5] == c && p[H4] == c && p[H3] == c && p[H2] == c && p[H1] == 0 && p[G7] == c && p[G3] == c)) {

  true_num++;
  sum += sf.Marke - 100;

  if (sf.Marke == 150) {

    true_num++;
    sum += sf.Marke - 100;

  }

  

  /*
  SfAus(&sf, BLACK, 0);
  printf("%d\n", sf.Marke);
  */

  //  printf("%d\n", sf.Marke-100);


}
  }
  if (ferror(fpsfk) || ferror(fpdiff)) Error("read-error");
  fclose(fpsfk);
  fclose(fpdiff);

printf("%d %d %f  pts=%f\n", true_num, sum, float(sum)/true_num, pts/num);
exit(10);


  FOR (i, 6561) {

    PattOut(stdout, i, 8);

    printf(" %6d %f %f\n", 
	   edge_stat[i].n, 
	   (edge_stat[i].diff_sum) / (edge_stat[i].n + 0.01),
	   (edge_stat[i].abs_diff_sum) / (edge_stat[i].n + 0.01)
	  );
  }

  return 0;
}

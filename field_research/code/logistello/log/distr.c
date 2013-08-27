// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "distr.h"
#include "crt.h"

static float macheps = -1;


/* compute Phi at x */

/* Hartung p. 890 */

float Phi(float x)
{
  float t, t0, y;
  int neg;

  neg = x < 0;

  x = fabs(x);

  t0 = t = 1.0/(1.0 + 0.2316419 * x);

  y = exp(-(x*x)*0.5);

  x = 0.1274147  * t;
  t *= t0;
  x -= 0.1422483 * t;
  t *= t0;
  x += 0.7107068 * t;
  t *= t0;
  x -= 0.7265759 * t;
  t *= t0;
  x += 0.5307027 * t;

  if (!neg) return 1-0-x*y; else return x*y;

}


/* compute mean and variance */

void MeanVar(DISTR *pdis, float *pmean, float *pvar)
{
  int i;
  float sum = 0.0, sumq = 0.0, last_y = 0.0;

  FOR (i, pdis->n) {

    sum  += pdis->s[i].x * (pdis->s[i].y - last_y);
    sumq += pdis->s[i].x * pdis->s[i].x * (pdis->s[i].y - last_y); 
    last_y = pdis->s[i].y;
  }

  *pmean = sum;

  *pvar = sumq - sum * sum;
} 


float GetMachineEps(void) 
{
  float result=1.0;

  do { result *= 0.5; } while (1.0 + result > 1.0);

  return result+result;
}


int num_malloc = 0, num_free = 0;

DISTR NewDistr(int n)
{
  DISTR distr;

  if (macheps < 0) macheps = GetMachineEps();

  distr.s = (STAIR*) malloc(n * sizeof(STAIR));

num_malloc += n * sizeof(STAIR);

  if (!distr.s) Error("NewDistr: no mem");

  distr.n = n;

  return distr;
}



void FreeDistr(DISTR *pdistr)
{
num_free += pdistr->n * sizeof(STAIR);
  pdistr->n = 0;
  if (pdistr->s) free(pdistr->s);
  pdistr->s = NULL;
}


void CopyDistr(DISTR *dest, DISTR *src)
{
  *dest = NewDistr(src->n);

  if (!dest->s) Error("CopyDistr: no mem");

  memcpy(dest->s, src->s, sizeof(STAIR)*src->n);
}


void PrintDistr(char *s, DISTR *pdistr)
{
  int i;


  printf("\ndistribution %s (n=%d):\n", s, pdistr->n);

  FOR (i, pdistr->n) printf("(%.3f %.3f) ", pdistr->s[i].x, pdistr->s[i].y);
 
  printf("\n");
}


DISTR OnePeak(float x)
{
  DISTR op;

  op = NewDistr(1);

  op.s[0].x = x;
  op.s[0].y = 1.0;

  return op;
}


DISTR Triangle(int n, float mean, float step, float p0)
{
  int   i;
  float x, y, delta;
  DISTR op;
  
  if (n < 3 || (n & 1) == 0) Error("Triangle: n < 3 || n even");

  if (step <= 0) Error("Triangle: step <= 0");

  if (p0 <= 0.0 || n*p0 > 1.0) Error("Triangle: p0");
 
  delta = 4.0 * (1.0 - n*p0) / ((n-1)*(n-1));

  op = NewDistr(n);

  x = mean - n/2 * step;

  FOR (i, n) { op.s[i].x = x; x += step; }

  y = p0;

  FOR (i, n/2+1) { op.s[i].y = op.s[n-1-i].y = y; y += delta; }

  Dens2Distr(&op);

  op.s[n-1].y = 1.0;

  return op;
}




/*  return approximation of N(mean,var) distribution with 2*num+1 
 *  jumps of distance step 
 */
 
DISTR NormalDistr(float mean, float var, float step, int num)
{
  int i;
  float x, siginv, sum_y;
  DISTR distr;


  x = mean - step * num;

  distr = NewDistr(2*num+1);

  siginv = 1/sqrt(var);

  sum_y = 0.0;

  FOR (i, 2*num+1) {

    distr.s[i].x = x;
    sum_y += distr.s[i].y = exp(-(x - mean)*siginv*(x - mean)*siginv);
    x += step;

  }

  sum_y = 1.0/sum_y;

  FOR (i, 2*num+1) distr.s[i].y *= sum_y;

  Dens2Distr(&distr);

  return distr;
}



void Dens2Distr(DISTR *pdistr) 
{
  int i, n=pdistr->n;
  STAIR *s=pdistr->s;

  for (i=1; i < n; i++) s[i].y += s[i-1].y;
}


void Distr2Dens(DISTR *pdistr) 
{
  int i, n=pdistr->n;
  STAIR *s=pdistr->s;

  for (i=n-1; i > 0; i--) s[i].y -= s[i-1].y;
}



void NegateDistr(DISTR *pdistr, float zero) 
{
  int i, n=pdistr->n;
  STAIR *s=pdistr->s, *s1=&s[0], *s2=&s[n-1];
  float x, y;

/* get raw probabilities, not cdf */

  Distr2Dens(pdistr);            

/* swap values from opposite ends of the density */

  for (i=0; i < n/2; i++) {
    x = s1->x; s1->x = s2->x; s2->x = x;
    y = s1->y; s1->y = s2->y; s2->y = y;
    s1++; s2--;
  }

  zero *= 2.0;

/* reverse the abscissa values */

  for (i=n-1; i >= 0; i--) s[i].x = zero - s[i].x;

/* turn back into cdf */

  Dens2Distr(pdistr);
}

 


float F(DISTR *pdistr, float x)
{
  int   n=pdistr->n, i, l, r;
  STAIR *s=pdistr->s;
 
  if (x <  s[0].x)   return 0.0;
  if (x >= s[n-1].x) return 1.0;

  l = 0; r = n-1;

/* s[l].x <= x <= s[r].x */

  while (l < r-1) {

    i = (l+r)/2;

    if (s[i].x == x) return s[i].y;

    if (s[i].x < x) l = i; else r = i;  

/* s[l].x <= x <= s[r].x */

  }

/* l == r-1 ^ s[l].x <= x <= s[r].x */

  if (s[l+1].y == x) return s[l+1].y;
  
  return s[l].y;
}



DISTR MaxDistr(DISTR *pdistr, int n)
{
  STAIR *start[MAX_N];
  STAIR *stop[MAX_N];
  DISTR res;
  int   i, result_len=0;
  float prod=1.0;     /* Product of all child cdf's at the current jump */

  assert(n <= MAX_N);

  for (i=n-1; i>=0; i--) {

 /* Total length of new distribution */
    result_len += pdistr[i].n; 

/* pointer to last element of ith cdf */
    start[i] = &pdistr[i].s[pdistr[i].n - 1];

/* pointer to first element of ith cdf */
    stop[i]  = &pdistr[i].s[0];   
  }

#if 0
  { static int sum_len=0, sum_num=0;

sum_len += result_len;
sum_num++;
if ((sum_num & 255) == 0) printf("%f\n", (double)sum_len/sum_num);
  }
#endif

  res = NewDistr(result_len);

  for (i=result_len-1; i >= 0; i--) {
 
    int j, which=-1;
    float max = -1e20, tmp;

    /* Find the maximum abscissa value in a child, breaking ties 
       in favor of the higher numbered child. */

    for (j=n-1; j>=0; j--) {
      if (start[j] >= stop[j]) {
        tmp = start[j]->x;
        if (tmp > max) {
          max = tmp; which = j;
        }
      }
    }

    res.s[i].x = start[which]->x;
    res.s[i].y = prod;

    start[which]->pstair = &res.s[i];  /* save results jump address */

    assert(start[which] >= stop[which]);

    start[which]--;    /* This jump is finished, point to next in line */

    if (start[which] >= stop[which]) {

      prod *= start[which]->y / (start[which] + 1)->y;

    } else {

      prod = 0.0;

    }

    assert(prod >= 0.0 && prod <= 1.0);

  }

/*
printf("%d %d\n", result_len, i);
*/

  for (; i > 0; i--) res.s[i-1].x = res.s[i-1].y = 0;

  ResolveEqualAbs(&res);

#if 0
  StripDistr(&res);
  assert(res.s[0].y > 0.0);
#endif

  return res;
}



/* The Negamax combination code for distributions works by first negating
   all of the child distributions about the zero point and then calling
   Max to do the actual propagation. */

DISTR NegaMaxDistr(DISTR *pdistr, int n)
{
  int i;
  DISTR res, distr[MAX_N];  

  if (n > MAX_N) Error("NegaMaxDistr: n > MAX_N");

  FOR (i, n) {
    CopyDistr(&distr[i], &pdistr[i]);
    NegateDistr(&distr[i], 0.0);
    StripDistr(&distr[i]);
  }

  res = MaxDistr(distr, n);

  StripDistr(&res);

  FOR (i, n) FreeDistr(&distr[i]);

  return res;
}


void PMax(DISTR *pdistr, int n)
{
  int i, j;
  float prev_y, sum;

  FOR (i, n) {  

    prev_y = sum = 0;

/*
if (pdistr[i].s[0].y == 0) Error("=0");
*/
    FOR (j, pdistr[i].n) {

      sum += pdistr[i].s[j].pstair->y * (1.0 - prev_y / pdistr[i].s[j].y);
      prev_y = pdistr[i].s[j].y; 

    }

    pdistr[i].pmax = sum;
  }
}

/* compute prob. to be max. of n variables */

void ProbMax(DISTR *pdistr, int n)
{
  DISTR maxdistr;
 
  maxdistr = MaxDistr(pdistr, n);
  PMax(pdistr, n);
  FreeDistr(&maxdistr);

}


DISTR PNegaMaxDistr(DISTR *pdistr, int n, float *pmax)
{
  int i;
  DISTR res, distr[MAX_N];  

  if (n > MAX_N) Error("NegaMaxDistr: n > MAX_N");

  FOR (i, n) {
    CopyDistr(&distr[i], &pdistr[i]);
    NegateDistr(&distr[i], 0.0);
    StripDistr(&distr[i]);
  }

  res = MaxDistr(distr, n);

  PMax(distr, n);

  StripDistr(&res);

  FOR (i, n) {
    pmax[i] = distr[i].pmax;
    FreeDistr(&distr[i]);
  }

  return res;
}




void StripDistr(DISTR *pdistr)
{
  int i, j, bytes;
  STAIR *s;

#if 0
PrintDistr("tostr", pdistr);
#endif

  if (macheps < 0) macheps = GetMachineEps();

  for (i=0; i < pdistr->n-1; i++)
    if (pdistr->s[i].y >= macheps) break;

  for (j=pdistr->n-2; j > 0; j--) {
    if (pdistr->s[j].y <= 1.0-macheps) break;
  }

  j++;

  if (i == 0 && j == pdistr->n-1) return;

#if 0
printf("%d %d\n", i, j);
#endif

  bytes = (j + 1 - i) * sizeof(STAIR);

  s = (STAIR*) malloc(bytes);

num_malloc += bytes;

  if (!s) Error("StripDistr: no mem");

  memcpy(s, &pdistr->s[i], bytes);
      
  free(pdistr->s);
num_free += pdistr->n * sizeof(STAIR);

  pdistr->n = j+1-i;
  pdistr->s = s;

#if 0
PrintDistr("str", pdistr);
#endif
}


void ResolveEqualAbs(DISTR *pdistr)
{
  int i, j, n=pdistr->n;
  STAIR *s=pdistr->s;


  FOR (i, n-1) {

    j = i;

    while (j < n-1 && s[j].x == s[j+1].x) j++;

    for (; i < j; i++) s[i].y = s[j].y;

  }
}




/* compress distribution with at most k peaks:
 *
 * new x'[i] are the at most k i/k+1 quantiles (i>0)
 * 
 *
 * interpolation between x[i] and x[i+1]:
 *
 *                                + y[k]
 *                                |
 *             *******************| y'[i] = 0.5*(y[j]+last_y[i+1])
 *                        --------|<--------------------+
 *        y[j] +-------....       |
 *             |                  |
 *     --------|                  |
 *             |                  |
 *            x'[i]             x'[i+1]
 *
 *  ensures maximum error 1/k (has to be proved)
 */ 

void CompressDistr(DISTR *pdistr, int k)
{
  int   i, n=pdistr->n, new_n;
  float quant, dquant, *x, *y, *last_y; 

#if 0
DISTR dcopy;  

CopyDistr(&dcopy, pdistr);
PrintDistr("tocompr", pdistr);
#endif

  if (n <= k) return;

  quant = dquant = 1.0/(k+1);

  x = (float*) malloc(n*sizeof(float));
  if (!x) Error("CompressDistr: no mem");

  y = (float*) malloc(n*sizeof(float));
  if (!y) Error("CompressDistr: no mem");

  last_y = (float*) malloc(n*sizeof(float));
  if (!last_y) Error("CompressDistr: no mem");

  new_n = 0;
  
  FOR (i, n) {

    if (pdistr->s[i].y >= quant) {

      while (pdistr->s[i].y >= quant) quant += dquant;

      x[new_n] = pdistr->s[i].x;
      y[new_n] = pdistr->s[i].y;

      if (i == 0) last_y[new_n] = pdistr->s[i].y;
      else        last_y[new_n] = pdistr->s[i-1].y;

#if 0
printf("x=%f last_y=%f y=%f\n", x[new_n], last_y[new_n], y[new_n]);
#endif

      new_n++;

      if (new_n >= k) break;
    }
  }

  FreeDistr(pdistr);

  *pdistr = NewDistr(new_n);

  FOR (i, new_n-1) {

    pdistr->s[i].x = x[i];
    pdistr->s[i].y = (y[i]+last_y[i+1])*0.5;

  }

  pdistr->s[new_n-1].x = x[new_n-1];
  pdistr->s[new_n-1].y = 1.0;


  free(last_y); free(y); free(x); 


#if 0

  PrintDistr("compr", pdistr);

  { float diff = NormDiff(pdistr, &dcopy);
    static float max = -1;

    if (diff > max) { max = diff; printf("*** %f\n", max); }
  }


  FreeDistr(&dcopy);
#endif


  return;
}


float NormDiff(DISTR *pda, DISTR *pdb)
{
  int i, j;
  float max = -1.0, y;

  j = 0;

  FOR (i, pda->n) {

    while (j < pdb->n && pdb->s[j].x <= pda->s[i].x) j++; 

    if      (j >= pdb->n) y = 1.0; 
    else if (j > 0)       y = pdb->s[j-1].y;
    else                  y = 0.0;

    if (fabs(pda->s[i].y - y) > max) max = fabs(pda->s[i].y - y);

  }

  return max;
}


// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Eigenwerte und -vektoren einer symmetrischen Matrix bestimmen 
 * für Hauptkomponenten 
 * NUMERICAL RECIPES IN C, pp. 474
 *
 * 8.93 
 */



#ifndef PRECOMP
#include "main.h"
#endif


#include "logm.h"
#include "logmat.h"
#include "logeig.h"



#include "crt.h"

#define VEK1(m, y)    VEK(m, (y)-1)
#define MAT1(m, y, x) MAT(m, (y)-1, (x)-1)



void tred2(MATRIX *pa, MATRIX *pd, MATRIX *pe)
{
  int  l, k, j, i, n;
  REAL scale, hh, h, g, f;

  n = pa->dimx;

  if (pa->dimy != n || 
      pd->dimy != n || pd->dimx != 1 || 
      pe->dimy != n || pe->dimx != 1)

    Error("tred2: Dimension stimmt nicht");


  for (i=n; i >= 2; i--) {

    l = i-1;
    h = scale = 0.0;
  
    if (l > 1) {

      for (k=1; k <= l; k++) scale += fabs(MAT1(pa, i, k));

      if (scale == 0.0) 

	VEK1(pe, i) = MAT1(pa, i, l);

      else {
	
	for (k=1; k <= l; k++) {
	  MAT1(pa, i, k) /= scale;
	  h += MAT1(pa, i, k) * MAT1(pa, i, k);
	}

	f = MAT1(pa, i, l);

	g = f > 0.0 ? -sqrt(h) : sqrt(h);

	VEK1(pe, i) = scale * g;

	h -= f * g;

	MAT1(pa, i, l) = f - g;

	f = 0.0;

	for (j=1; j <= l; j++) {

	  MAT1(pa, j, i) = MAT1(pa, i, j) / h;

	  g = 0.0;

	  for (k=1; k <= j; k++)   g += MAT1(pa, j, k) * MAT1(pa, i, k);
	  for (k=j+1; k <= l; k++) g += MAT1(pa, k, j) * MAT1(pa, i, k);

	  VEK1(pe, j) = g / h;

	  f += VEK1(pe, j) * MAT1(pa, i, j);

	} 

	hh = f / (h + h);

	for (j=1; j <= l; j++) {

	  f = MAT1(pa, i, j);

	  VEK1(pe, j) = g = VEK1(pe, j) - hh * f;

	  for (k=1; k <= j; k++) 
	    MAT1(pa, j, k) -= (f * VEK1(pe, k) + g * MAT1(pa, i, k));

	}
      }	

    } else VEK1(pe, i) = MAT1(pa, i, l);

    VEK1(pd, i) = h;

  }


  VEK1(pe, 1) = 0;
  VEK1(pd, 1) = 0;


  for (i=1; i <= n; i++) {

    l = i - 1;

    if (VEK1(pd, i) != 0.0) {

      for (j=1; j <= l; j++) {

	g = 0;

	for (k=1; k <= l; k++) g += MAT1(pa, i, k) * MAT1(pa, k, j);
	for (k=1; k <= l; k++) MAT1(pa, k, j) -= g * MAT1(pa, k, i);

      }
    }

    VEK1(pd, i) = MAT1(pa, i, i);
    MAT1(pa, i, i) = 1.0;
   
    for (j=1; j <= l; j++) MAT1(pa, j, i) = MAT1(pa, i, j) = 0.0;   
  }
}


#define SQR(a)		((a)*(a))
#define SIGN(a,b)	((b) > 0 ? fabs(a) : -fabs(a))


REAL pythag(REAL a, REAL b)
{
  REAL absa=fabs(a), absb=fabs(b);

  if (absa > absb) return absa * sqrt(1.0 + SQR(absb / absa));
  else return absb == 0.0 ? 0.0 : absb * sqrt(1.0 + SQR(absa / absb));
}



/* Eingabe:
 *
 *   d - Vektor der Diagonalelemente der Dreiecksmatrix
 *   e - 1. Nebendiagonale, e[0] egal
 *
 */


void tqli(MATRIX *pd, MATRIX *pe, MATRIX *pz)
{
  int  m, l, iter, i, k, n;
  REAL s, r, p, g, f, dd, c, b;

  n = pz->dimx;

  if (pz->dimy != n || 
      pd->dimy != n || pd->dimx != 1 || 
      pe->dimy != n || pe->dimx != 1)

    Error("tqli: Dimension stimmt nicht");


  for (i=2; i <= n; i++) VEK1(pe, i-1) = VEK1(pe, i);

  VEK1(pe, n) = 0;


  for (l=1; l <= n; l++) {

    iter = 0;

    do {


      for (m=l; m <= n-1; m++) {

	dd = fabs(VEK1(pd, m)) + fabs(VEK1(pd, m+1));

	if ((fabs(VEK1(pe, m)) + dd) == dd) break;

      }

      if (m != l) {

	if (iter++ == 30) Error("tqli: zu viele Iterationen");

	g = (VEK1(pd, l+1) - VEK1(pd, l)) / (2.0 * VEK1(pe, l));
	r = pythag(g, 1.0);
	g = VEK1(pd, m) - VEK1(pd, l) + VEK1(pe, l) / (g + SIGN(r,g));
	s = c = 1.0;
	p = 0.0;

	for (i=m-1; i >= l; i--) {

	  f = s * VEK1(pe, i);
	  b = c * VEK1(pe, i);

	  VEK1(pe, i+1) = (r = pythag(f,g));

	  if (r == 0.0) {

	    VEK1(pd, i+1) -= p;
	    VEK1(pe, m) = 0.0;
	    break;
	  }

	  s = f / r;
	  c = g / r;

	  g = VEK1(pd, i+1) - p;
	  r = (VEK1(pd, i) - g) * s + 2.0 * c * b;

	  VEK1(pd, i+1) = g + (p=s*r);

	  g = c * r - b;

	  for (k=1; k <= n; k++) {

	    f = MAT1(pz, k, i+1);
	    MAT1(pz, k, i+1) = s * MAT1(pz, k, i) + c * f;
	    MAT1(pz, k, i)   = c * MAT1(pz, k, i) - s * f;
	 
	  }
	}

	if (r == 0.0 && i) continue;

	VEK1(pd, l) -= p;
	VEK1(pe, l) = g;
	VEK1(pe, m) = 0.0;

      }

    } while (m != l);
  }
}


void eigsrt(MATRIX *pd, MATRIX *pa)
{
  int  i, j, k, n=pa->dimx;
  REAL p;

  if (pa->dimy != n || pd->dimy != n || pd->dimx != 1)

    Error("eigsrt: Dimension stimmt nicht");


  for (i=1; i < n; i++) {

    p = VEK1(pd, k=i);

    for (j=i+1; j <= n; j++)
      if (VEK1(pd, j) >= p) p = VEK1(pd, k=j);

    if (k != i) {

      VEK1(pd, k) = VEK1(pd, i);
      VEK1(pd, i) = p;
      
      for (j=1; j <= n; j++) {

	p = MAT1(pa, j, i);
	MAT1(pa, j, i) = MAT1(pa, j, k);
	MAT1(pa, j, k) = p;	

      }	
    }
  }
}

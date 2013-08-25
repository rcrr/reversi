// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Allerlei */

#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"

#include "logm.h"
#include "logmat.h"
#include "logall.h"


#define	WERTMAX 100
#define WERTEANZ (2*WERTMAX+1)

#define ADJ	0		/* wird bei 100% von y abgezogen */





void Indikator(MATRIX *pX, MATRIX **ppI)
{
  int i, j, k, l, indanz, messanz, merkmanz;
  MATRIX *pMin, *pMax, *pDa, *pI;
  REAL	 r;


  if (!ppI) Error("ppI = 0");

  merkmanz = pX->dimx;
  messanz  = pX->dimy;


/* 1. Minimum und Maximum der einzelnen Merkmale feststellen */
  
  pMin = MatMake(1, merkmanz);  
  pMax = MatMake(1, merkmanz);  
  pDa  = MatMake(merkmanz, WERTEANZ);

  if (!pMin || !pMax || !pDa) Error("Speicher3");

  FOR (i, merkmanz) { VEK(pMin, i) = REAL_MAX; VEK(pMax, i) = -REAL_MAX; }


  FOR (i, messanz)
    FOR (j, merkmanz) {

      r = MAT(pX, i, j);

      if	(r > VEK(pMax, j)) VEK(pMax, j) = r;
      else if	(r < VEK(pMin, j)) VEK(pMin, j) = r;

      MAT(pDa, j, (int)r + WERTMAX) = 1; 
    }
 
  indanz = 0;

  FOR (i, merkmanz) {

    indanz += VEK(pMax, i) - VEK(pMin, i) + 1;

    printf("Merkmal %d:  %f ... %f \n", 
	i+1, VEK(pMin, i), VEK(pMax, i));
  }

printf("%d\n", indanz);

/* Indikator-Merkmale erstellen */

  pI = MatMake(messanz, indanz);

  if (!pI) Error("Speicher4");


  FOR (i, messanz) {

 
    k = 0;

    FOR (j, merkmanz) {

      r = MAT(pX, i, j);

      for (l=VEK(pMin, j); l < r; l++, k++) MAT(pI, i, k) = 0;

      MAT(pI, i, k) = 1; k++;

      for (l++; l <= VEK(pMax, j); l++, k++) MAT(pI, i, k) = 0;
    }
  }

  *ppI = pI;
}






/* verschiedene Merkmalsvektoren ermitteln und Anzahlvektoren anpassen */

void Compress(MATRIX **ppX, MATRIX **ppn, MATRIX **ppy)
{
  int i, j, k, messanz=0;
  MATRIX *pX, *pn, *pn0, *py, *py0, *psp;
  REAL   ry, rn;



  if (!ppX || !ppn || !ppy) Error("Zeiger Null");

  pn0 = MatMake((*ppX)->dimy, 1);
  py0 = MatMake((*ppX)->dimy, 1);
  psp = MatMake((*ppX)->dimy, 1);

  if (!pn0 || !py0 || !psp) Error("Speicher2");

  FOR (i, psp->dimy) VEK(psp, i) = 0;


/* Anzahl der verschiedenen Messungen und Klassensumme feststellen */


  FOR (i, (*ppX)->dimy)

    if (!VEK(psp, i)) {


      messanz++;

      VEK(pn0, i) = VEK(*ppn, i); 
      VEK(py0, i) = VEK(*ppy, i);

      for (j=i+1; j < (*ppX)->dimy; j++) {

        if (!VEK(psp, j)) {

          FOR (k, (*ppX)->dimx)
	    if (MAT(*ppX, i, k) != MAT(*ppX, j, k)) break;

/* kommt Messung nochmal vor? */

          if (k >= (*ppX)->dimx) {

	    VEK(py0, i) += VEK(*ppy, j);
	    VEK(pn0, i) += VEK(*ppn, j);

	    VEK(psp, j) = 1;
          }
        }
      }
    }


  pX = MatMake(messanz, (*ppX)->dimx);
  pn = MatMake(messanz, 1);
  py = MatMake(messanz, 1);

  if (!pX || !pn || !py) Error("Speicher1");
  

/* verschiedene Messungen in X sammeln */


  j = 0;

  FOR (i, (*ppX)->dimy) {

    if (!VEK(psp, i)) {

      FOR (k, (*ppX)->dimx) MAT(pX, j, k) = MAT(*ppX, i, k);

      rn = VEK(pn, j) = VEK(pn0, i);
      ry = VEK(py0, i);

      if (RGLEICH(rn, ry)) ry -= ADJ;

      VEK(py, j) = ry;

      j++;
    }
  }




  MatDel(pn0); MatDel(py0); MatDel(*ppX); MatDel(psp);

  *ppX = pX; *ppn = pn; *ppy = py;
}

 

int compREAL(const void *a, const void *b)
{
  REAL r;

  r = *(REAL*)a - *(REAL*)b;

  if       (r < 0.0) return -1;
  else if  (r > 0.0) return  1;
  else               return  0;
}



REAL logdis(MATRIX *pX, MATRIX *pbeta, int z)
{
  int j;
  REAL su;


  su = 0.0;

  FOR (j, pX->dimx) su += VEK(pbeta, j) * MAT(pX, z, j);

  su = exp(su);

  return su / (1.0 + su) - 0.5;
}



#ifdef xxx

/* alte Version */

void Fehler(
  MATRIX *pX, 
  MATRIX *pn, 
  MATRIX *py, 
  MATRIX *pbeta, 
  REAL   *pFehler01,
  REAL   *pFehler10,
  REAL	 *pDisk
)
{
  int    i, j, Klasse, n=0, falsch, falsch01=0, falsch10=0, zK0, zK1;
  REAL   su;
  MATRIX *pK0, *pK1;



  FOR (i, pX->dimy) {

    n +=  VEK(pn, i);

    su = diskr(pX, pbeta, i);

    if (su >= 0.0) Klasse = 1; else Klasse = 0;

    falsch += (int)(VEK(pn, i) * Klasse - VEK(py, i));

    if (falsch > 0) falsch01 +=  falsch;
    else	    falsch10 += -falsch;

  }

  if (!n) Error("Fehler()");


  *pFehler01 = (REAL) falsch01 / n;
  *pFehler10 = (REAL) falsch10 / n;



/* Diskordanz bestimmen */

  pK0 = MatMake(pX->dimy, 1);
  pK1 = MatMake(pX->dimy, 1);

  if (!pK0 || !pK1) Error("Speicherxyz");

  zK0 = zK1 = 0;

  FOR (i, pX->dimy) {

    su = diskr(pX, pbeta, i);

    if (VEK(py, i) >= 1.0) { VEK(pK1, zK1) = su; zK1++; }
    else 	           { VEK(pK0, zK0) = su; zK0++; }

  }

  if (zK0 && zK1) {

/* Werte sortieren */

    qsort(pK0->e, (size_t) zK0, sizeof(REAL), compREAL);
    qsort(pK1->e, (size_t) zK1, sizeof(REAL), compREAL);

    su = 0; i = 0;

    FOR (j, zK0) {

      while (i < zK1 && VEK(pK0, j) >= VEK(pK1, i)) i++;

      su += i;
    }

    *pDisk = (REAL) su / ((REAL) zK0 * (REAL) zK1);

  } else *pDisk = 1.0;

  return; 
}

#endif



static REAL linkomb(MATRIX *pX, MATRIX *pbeta, int z)
{
  int  i, dim=pbeta->dimy;
  REAL su=0.0;


  FOR (i, dim) su += MAT(pX, z, i) * VEK(pbeta, i);

  return su;
}
  


void Fehler(
  MATRIX   *pX,
  MATRIX   *pn,
  MATRIX   *py,
  MATRIX   *pbeta,
  REAL     *pFehler01,
  REAL     *pFehler10,
  REAL	   *pDisk
)
{
  int       i, j, n, falsch01=0, falsch10=0, zK0, zK1, zK01;
  long long su;
  MATRIX    *pK0, *pK1, *pK01;



  pK0  = MatMake(pX->dimy, 1);
  pK1  = MatMake(pX->dimy, 1);
  pK01 = MatMake(pX->dimy, 1);

  if (!pK0 || !pK1 || !pK01) Error("Speicher? in Fehler()");


  n = zK0 = zK1 = 0;


  FOR (i, pX->dimy) {

    VEK(pK01, i) = linkomb(pX, pbeta, i);

    if (VEK(py, i) < 0.5 * VEK(pn,i)) {		/* grobe Trennung */

      VEK(pK0, zK0) = VEK(pK01, i); zK0++;

      if (VEK(pK01, i) >= 0) falsch01 += VEK(pn, i);

      n += VEK(pn, i);

    } else if (VEK(py, i) > 0.5 * VEK(pn,i)) {

      VEK(pK1, zK1) = VEK(pK01, i); zK1++;

      if (VEK(pK01, i) < 0) falsch10 += VEK(pn, i);

      n += VEK(pn, i);

    }

  }

  if (!n) Error("n == 0 in Fehler()");

  *pFehler01 = ((REAL) falsch01) / n;
  *pFehler10 = ((REAL) falsch10) / n;



/* Diskordanz bestimmen */


  if (zK0 && zK1) {

    zK01 = zK0 + zK1;

/* Werte sortieren */

    qsort(pK0->e,  (size_t) zK0,  sizeof(REAL), compREAL);
    qsort(pK1->e,  (size_t) zK1,  sizeof(REAL), compREAL);
    qsort(pK01->e, (size_t) zK01, sizeof(REAL), compREAL);

    su = 0; i = 0;

    FOR (j, zK0) {

      while (i < zK1 && VEK(pK0, j) >= VEK(pK1, i)) i++;

      su += i;
    }


    *pDisk = ((((REAL) su) / zK01) / (zK01-1)) * 2;

/*printf("n=%d su=%d anz=%d *=%d\n", n, su, anz, zK0*zK1);*/


  } else *pDisk = 1.0;


  MatDel(pK0);
  MatDel(pK1);
  MatDel(pK01);

  return; 
}

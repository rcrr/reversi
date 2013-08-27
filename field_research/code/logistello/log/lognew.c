// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* log-Likelihood-Funktion mit Newton-Raphson maximieren */


#ifndef PRECOMP
#include "main.h"
#endif

#include "logmat.h"
#include "logall.h"
#include "logio.h"
#include "lognew.h"


#define ENDREL	(5e-5) 
#define MAX_EPS 0.01

#define ADJ	0


#define MAXTRY	5

#define E_TAU	0.3
#define LAMBDA  (1.0/E_TAU)

#define TAUMAX	4.0		/* maximale Schrittweite fuer Gradient 	*/
#define RANDMIN	(-1.0)		/* Suchraum				*/
#define RANDMAX (1.0)

#define TRIALS  1
#define SUCHANZ 5		/* Versuche im Suchraum	*/

#define LOKTRIALS 40
#define LOKANZ	40		/* Gradientenversuche	*/



/* log-Likelihood-Funktion auswerten */

REAL L(MATRIX *pX, MATRIX *py, MATRIX *pn, MATRIX *pbeta)
{
  int i, j;
  REAL su, wert = 0.0;


  FOR (j, pX->dimx) {

    su = 0.0;
 
    FOR (i, pX->dimy) su += VEK(py, i) * MAT(pX, i, j);

    su *= VEK(pbeta, j);
    wert += su;
  }


  FOR (i, pX->dimy) {

    su = 0.0;

    FOR (j, pX->dimx) su += VEK(pbeta, j) * MAT(pX, i, j);

    wert -= VEK(pn, i) * log(1.0 + exp(su));

  }

  return wert;
}


  
 
/* Normierten Gradienten bestimmen, Norm vor Normierung zurueck */

REAL Gradient(MATRIX *pX, MATRIX *pn, MATRIX *py, MATRIX *pbeta, MATRIX *pg)
{
  int  a, i;
  REAL wert, Norm=0.0;




  FOR (a, pX->dimx) {

    wert = 0.0;
   
    FOR (i, pX->dimy) {

      wert += (VEK(py, i) - VEK(pn, i) * (logdis(pX, pbeta, i) + 0.5)) * 
	      MAT(pX, i, a);
    }

    VEK(pg, a) = wert; Norm += wert * wert;
  }

  
  Norm = sqrt(Norm);

  if (Norm < EPS) return EPS;

  FOR (a, pX->dimx) VEK(pg, a) /= Norm;

  return Norm;
  
}
 

 
int Iter(MATRIX *pX, MATRIX *pn, MATRIX *py, MATRIX *pbeta, REAL *pL)
{
  int    i, j, t, messanz, varanz, ind, a, b;
  MATRIX *pH, *ppi, *pq, *pdelta, *pneu;
  REAL  su, hilf;


  MATCHECK(pX, "NRX: X");  
  MATCHECK(pn, "NRX: n");  
  MATCHECK(py, "NRX: y"); 

  varanz  = pX->dimx;
  messanz = pX->dimy;

  ppi = MatMake(1, messanz);

  pq     = MatMake(varanz, 1);
  pdelta = MatMake(varanz, 1);
  pneu   = MatMake(varanz, 1);

  pH = MatMake(varanz, varanz);


  if (!ppi || !pq  || !pdelta || !pneu || !pH || !pbeta) Error("Speicher");



if (f_aus) {
  VekAus("beta", pbeta);
  printf("START L=%" R_F "\n", L(pX, py, pn, pbeta));
}

  *pL = 0.0;

  t = 0;


/* ITERATION */


  do {

if (f_aus) printf("t=%d\n", t);

    t++;


/* aus Beta(t) pi(t) bestimmen 				   */
/* (approximierte rel. Haeufigkeit bei Vorliegen von X[i]) */

    ind = 0; ppi->dimx = messanz; ppi->dimy = 1;

    FOR (i, messanz) VEK(ppi, i) = logdis(pX, pbeta, i) + 0.5;


/* aus pi(t) Beta(t+1) bestimmen 					*/

/* dazu zunächst die aktuelle Hesse-Matrix bestimmen und invertieren	*/
/* H(t)[a,b] = - summme[i] X[i,a] X[i,b] n[i] pi(t)[i] (1 - pi(t)[i])	*/


    
    FOR (a, varanz) {

      FOR (b, varanz) {

        su = 0.0;

	FOR (i, messanz) 

	  su += MAT(pX, i, a) * MAT(pX, i, b) *
		VEK(pn, i) * VEK(ppi, i) * (1.0 - VEK(ppi, i));

	MAT(pH, a, b) = -su;
      }
    }


/* H(t) invertieren */


    i = MatInv(pH);

    if (i) { 
      if (f_aus) printf("*** Matrix H ist singulär\n");
      return 1; 
    }


/* Vektor q bestimmen:					 */
/* q(t)[j] = summe[i] (y[i] - n[i] * pi(t)[i]) * x[i][j] */

    FOR (j, varanz) {

      su = 0.0;

      FOR (i, messanz) {

        su += (VEK(py, i) - VEK(pn, i) * VEK(ppi, i)) * MAT(pX, i, j);
      }

      VEK(pq, j) = su;        

    }
 


/* Iterationsschritt: beta(t+1) = beta(t) - H(t)^(-1) * q(t) */


    MatMult(pH, pq, pdelta);

    MatSub(pbeta, pdelta, pneu);

    hilf = MatMaxRel(pbeta, pneu);

    MatCopy(pneu, pbeta);

if (f_aus) printf(">>> %" R_F "\n", hilf);

  } while (hilf > ENDREL);


  hilf = L(pX, py, pn, pbeta);

  printf("L= %f\n", hilf);
  
  VekAus("beta", pbeta);


  MatDel(ppi);
  MatDel(pq);
  MatDel(pdelta);
  MatDel(pneu);
  MatDel(pH);

  *pL = hilf;

  return 0;
}



/* log-Likelihood-Funktion maximieren		 */
/*						 */
/*   Eingabe: Zeiger auf MATRIX mit Zeilenaufbau */
/*						 */
/*	0/1	je nach Klassenzugehoerigkeit	 */
/*	0/1+	fuer jedes Indikator-Merkmal	 */
/*						 */
/*   Ausgabe: Zeiger auf Maximal-Vektor		 */



/* Startwert für Newton-Raphson bestimmen (Monte-Carlo Hillclimbing) */

int NR(MATRIX *pX, MATRIX *pn, MATRIX *py, MATRIX **ppmax, REAL *pL)
{
  int i, j, ok=0, trial, maxanz;
  REAL hilf, max, maxalt, glmax, tau;
  MATRIX *pbeta, *pbeta1, *pg, *pmax;


  pbeta  = MatMake(pX->dimx, 1);
  pbeta1 = MatMake(pX->dimx, 1);
  pg     = MatMake(pX->dimx, 1);
  pmax   = MatMake(pX->dimx, 1);
  
  if (!pbeta || !pbeta1 || !pg | !pmax) Error("Mem64");
 

  glmax = - REAL_MAX;

  FOR (trial, TRIALS) {

if (f_aus) printf("Versuch %d\n", trial+1);

    max = - REAL_MAX;

    FOR (i, SUCHANZ) {

Chk_Abort();
if (f_aus) { printf("."); fflush(stdout); }

/* Versuche im grossen Raum */

      FOR (j, pbeta->dimy)
        VEK(pbeta, j) = FRAN * (RANDMAX-RANDMIN) + RANDMIN;

      if ((hilf=L(pX, py, pn, pbeta)) > max) {

	max = hilf;
	MatCopy(pbeta, pmax);
      }
    }

     
if (f_aus) P_REAL(max);


  maxanz = 0;
  maxalt = 0.0;

/*
if (f_aus) { printf("."); fflush(stdout); }
*/

    FOREVER {

nochmal:
      hilf = Gradient(pX, pn, py, pmax, pg);
	

      FOREVER {

Chk_Abort();


/* Gradient * kleiner Wert + alten Vektor testen */

/*
        tau = FRAN * TAUMAX;
*/
	tau = FRAN; if (tau == 0.0) tau = 1e-6;

	tau = -log(tau) / LAMBDA;

if (f_aus) { printf("tau="); P_REAL(tau); }

        FOR (j, pbeta->dimy) VEK(pbeta, j) = VEK(pmax, j) + tau * VEK(pg, j);

if (f_aus) { printf("*"); fflush(stdout); }

        if ((hilf=L(pX, py, pn, pbeta)) > max) {


/* wenn besser, übernehmen und nochmal */

	  maxalt = max;
	  max = hilf; 

if (f_aus) P_REAL(max);

	  MatCopy(pbeta, pmax);

          maxanz++;

if (maxalt != 0.0) P_REAL(max/maxalt);

	  if ((maxalt != 0.0 && max/maxalt >= 1.0 - MAX_EPS) || 
	      (maxalt == 0.0 && maxanz >= MAXTRY)) { 

	    maxanz = 0; 

	    if (!Iter(pX, pn, py, pbeta, pL)) {
	      MatCopy(pbeta, pmax);
	      goto Ende;
	    }
	  }

	  goto nochmal;
        }
      }
    } 
  }

Ende:

  MatDel(pg); MatDel(pbeta); MatDel(pbeta1);

  *ppmax = pmax;

  return 0;
}

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Agresti Seite 117	*/
/* und			*/
/* Diskriminanzanalyse  */

#ifndef PRECOMP
#include "main.h"
#endif


#include "logm.h"
#include "logmat.h"
#include "logall.h"
#include "logio.h"
#include "logwls.h"

#include "crt.h"


#define SMOOTH	false		/* for Diskrim */


/* log-Likelihood-Funktionswert bestimmen */

REAL L(MATRIX *pX, MATRIX *py, MATRIX *pn, MATRIX *pbeta)
{
  int i, j;
  REAL su, wert = 0.0;

/*
MatAus("X", pX);
VekAus("y", py);
VekAus("n", pn);
VekAus("b", pbeta);
*/

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

 

/* Iterative Lösung des nichtlinearen Gleichungssystems für logit-fit */

 
int logitfit(
  MATRIX *pX,		/* Merkmalsausprägungen 	*/
  MATRIX *pn,		/* Gesamtanzahl			*/
  MATRIX *py,		/* Anzahl der positiven Bsp.	*/
  MATRIX *pbeta,	/* geschätzter Parametervektor	*/
  REAL   *plogL,	/* -2 log L			*/
  MATRIX *pH,		/* geschätzte Kovarianzmatrix	*/
  int	 IterMax,	/* maximale Iterationsanzahl	*/
  REAL   EndRel		/* Abbruchfehler		*/
)
{
  int    i, j, messanz, varanz, a, b, ret, IterNum;
  MATRIX *ppi, *pq, *pneu, *pz, *pfy;
  REAL   su, hilf, Lalt, Lneu;


  MATCHECK(pX,    "NRX: X");  
  MATCHECK(pn,    "NRX: n");  
  MATCHECK(py,    "NRX: y"); 
  MATCHECK(pbeta, "NRX: beta"); 
  MATCHECK(pH,    "NRX: pH"); 

  varanz  = pX->dimx;
  messanz = pX->dimy;


  if (pH->dimy    != varanz || pH->dimx    != varanz)  Error("NRX: H Dim?");
  if (pbeta->dimy != varanz || pbeta->dimx != 1)       Error("NRX: beta Dim?");
  if (pn->dimy    != 1      || pn->dimx    != messanz) Error("NRX: n Dim?");
  if (py->dimy    != 1      || py->dimx    != messanz) Error("NRX: y Dim?");


  ppi    = MatMake(1, messanz);
  pz     = MatMake(1, messanz);
  pfy    = MatMake(1, messanz);		/* fitted y */

  pq     = MatMake(varanz, 1);
  pneu   = MatMake(varanz, 1);

  if (!ppi || !pz || !pfy || !pq || !pneu) Error("Speicher");



/* Newton-Raphson-Iteration */


  FOR (IterNum, IterMax) {





/* aus Beta(t) pi(t) bestimmen 				   */
/* (approximierte rel. Häufigkeit bei Vorliegen von X[i]) */


    if (IterNum > 0)

      FOR (i, messanz) VEK(ppi, i) = logdis(pX, pbeta, i) + 0.5;

    else {						/* Startwert */

      FOR (i, messanz) {

	if (VEK(py, i) <  0.0) Error("py[i] < 0");
	if (VEK(pn, i) <= 0.0) Error("pn[i] <= 0");

	if      (VEK(py, i) == 0.0)        VEK(ppi, i) = 0.01; 
        else if (VEK(py, i) == VEK(pn, i)) VEK(ppi, i) = 0.99;
	else {
				   VEK(ppi, i) = VEK(py, i) / 
					 	 VEK(pn, i);
/*printf("%"R_F" %"R_F"\n", VEK(py, i), VEK(pn, i)); */
	}
      }
    }

/* VekAus("pi", ppi); */


/* z(t) aus pi(t) bestimmen */

    FOR (i, messanz) {

      VEK(pfy, i) = VEK(pn, i) * VEK(ppi, i) * (1.0 - VEK(ppi, i));

      if (VEK(ppi, i) == 0.0) VEK(ppi, i) = 0.01;
      if (VEK(ppi, i) == 1.0) VEK(ppi, i) = 0.99;


      if (VEK(ppi, i) == 0.0 || VEK(ppi, i) == 1.0) {
        printf("*** logit undefiniert\n");
        ret = 1;
        goto RetWert;
      }

      VEK(pz, i) = 
        log(VEK(ppi,i) / (1.0-VEK(ppi,i))) + 
        (VEK(py, i) - VEK(pn, i) * VEK(ppi, i)) / VEK(pfy, i);
    }

/*VekAus("z", pz);*/

/* aus z(t) Beta(t+1) bestimmen 					*/

/* dazu zunächst die aktuelle Hesse-Matrix bestimmen und invertieren	*/
/* H(t)[a,b] = summe[i] X[i,a] X[i,b] n[i] pi(t)[i] (1 - pi(t)[i])	*/


    FOR (a, varanz) {

      FOR (b, a+1) {			/* Matrix ist symmetrisch! */

        su = 0.0;

	FOR (i, messanz) su += MAT(pX, i, a) * MAT(pX, i, b) * VEK(pfy, i);

	MAT(pH, a, b) = MAT(pH, b, a) = su;
      }
    }


/* H(t) invertieren */


    i = MatInv(pH);

    if (i) {
      printf("*** Matrix H ist singulär\n");
      ret = 1;
      goto RetWert;
    }

/*MatAus("H", pH);*/


/* Vektor q bestimmen: */
/* q(t)[j] = summe[i] x[i][j] * z[i] * n[i] * pi(t)[i] * (1 - pi(t)[i]) */

    FOR (j, varanz) {

      su = 0.0;

      FOR (i, messanz) su += MAT(pX, i, j) * VEK(pz, i) * VEK(pfy, i); 

      VEK(pq, j) = su;     

    }
 


/* Iterationsschritt: beta(t+1) = H(t)^(-1) * q(t) */


    MatMult(pH, pq, pneu);

    Lneu = L(pX, py, pn, pneu);
/*printf("-> %"R_F" %d %d\n", Lneu, IterNum, IterMax);*/

    if (IterNum > 0) {

      hilf = MatMaxRel(pbeta, pneu);

/*printf("hilf1=%"R_F"\n", hilf);*/

      if (hilf <= EndRel) break;

      if (Lalt != 0.0) {

        hilf = fabs((Lneu - Lalt) / Lneu);
/*printf("hilf2=%"R_F"\n", hilf);*/
 
        if (hilf <= EndRel) break;
      }
    } 

    MatCopy(pneu, pbeta);
    Lalt = Lneu;

/*
VekAus("beta", pbeta);
printf("%"R_F" %d %d\n", Lneu, IterNum, IterMax);
*/


  }

  MatCopy(pneu, pbeta);

  Lalt = Lneu;



  if (IterNum >= IterMax) { 
    printf("*** keine Konvergenz!\n"); ret = 1; goto RetWert; 
  }


/* Kovarianzmatrix schätzen */
/* H(t)[a,b] = summe[i] X[i,a] X[i,b] n[i] pi(t)[i] (1 - pi(t)[i])	*/


  FOR (a, varanz) {

    FOR (b, a+1) {			/* Matrix ist symmetrisch! */

      su = 0.0;

      FOR (i, messanz) 
        su += MAT(pX, i, a) * MAT(pX, i, b) * 
              VEK(pn,i) * VEK(ppi,i) * (1.0 - VEK(ppi,i));

      MAT(pH, a, b) = MAT(pH, b, a) = su;
    }
  }


/* H(t) invertieren */


  i = MatInv(pH);

  if (i) {
    printf("*** H singulär bei Bestimmung der Kovarianzmatrix\n");
    ret = 1;
    goto RetWert;
  }


  ret = 0;

RetWert:

  MatDel(ppi);
  MatDel(pz);
  MatDel(pfy);
  MatDel(pq);
  MatDel(pneu);

  *plogL = -2 * Lalt;

  return ret;
}









/* normal linear regression ... */


int Regression(
  MATRIX *pX,		/* Merkmalsausprägungen */
  MATRIX *pn,		/* Gesamtanzahl		*/
  MATRIX *py,		/* Klassenzugehörigkeit	*/
  MATRIX *pbeta
)
{
  int    i, j, k, ret=0, messanz, varanz;
  MATRIX *pZ, *pm, *pbeta0;


  messanz = pX->dimy;
  varanz  = pX->dimx;

  pZ = MatMake(varanz, varanz);
  pm = MatMake(1, varanz);
  pbeta0 = MatMake(1, varanz);

  if (!pZ || !pm || !pbeta0) Error("Regression: no memory");

/* ß0 = y X (X'X)^-1 */

/* Z = X'X */

  FOR (i, varanz) {
    FOR (j, i+1) {

      REAL su=0;


      FOR (k, messanz) su += MAT(pX, k, i) * MAT(pX, k, j);

      MAT(pZ, i, j) = MAT(pZ, j, i) = su;

    }
  }

  i = MatInv(pZ);

  if (i) {
    printf("*** X'X is singular\n");
    ret = 1;
    goto RetWert;
  }


/* m = yX */

  MatMult(py, pX, pm);


/* ß = m Z */

  MatMult(pm, pZ, pbeta0);

  FOR (i, varanz) VEK(pbeta, i) = VEK(pbeta0, i);

/*
  VEK(pbeta, 0) -= 50;
*/

RetWert:

  MatDel(pm); MatDel(pZ); MatDel(pbeta0);

  return ret;
}






/* univariate linear regression ... */


int UniRegression(
  MATRIX *pX,		/* Merkmalsausprägungen */
  MATRIX *pn,		/* Gesamtanzahl		*/
  MATRIX *py,		/* Klassenzugehörigkeit	*/
  MATRIX *pbeta
)
{
  int    i, j, k, v, ret, messanz, varanz;
  MATRIX *pZ, *pm, *pbeta0;


  messanz = pX->dimy;
  varanz  = pX->dimx;

  pZ     = MatMake(2, 2);
  pm     = MatMake(1, varanz);
  pbeta0 = MatMake(1, 2);

  if (!pZ || !pm || !pbeta0) Error("Regression: no memory");


  VEK(pbeta, 0) = 0;


/* m = yX */

  MatMult(py, pX, pm);


  for (v=1; v < varanz; v++) {

/* ß = y X (X'X)^-1 */

/* Z = X'X */

    for (i=0; i <= v; i += v)

      for (j=0; j <= i; j += v) {

        REAL su=0;

        FOR (k, messanz) su += MAT(pX, k, i) * MAT(pX, k, j);

        MAT(pZ, i/v, j/v) = MAT(pZ, j/v, i/v) = su;

      }

    i = MatInv(pZ);

    if (i) {
      printf("*** X'X is singular\n");
      ret = 1;
      goto RetWert;
    }


/* ß0 = m[0,v] Z */

    FOR (j, 2) {   

      REAL su=0;


      FOR (i, 2) su += VEK(pm, i * v) * MAT(pZ, i, j);

      VEK(pbeta0, j) = su; 

    }


    VEK(pbeta, v) = VEK(pbeta0, 1);

/*printf("%f %f\n", VEK(pbeta0, 0), VEK(pbeta0, 1));*/

    VEK(pbeta, 0) += VEK(pbeta0, 0);


  }

  FOR (i, varanz) VEK(pbeta, i) /= varanz-1;

  VEK(pbeta, 0) -= 50;

  ret = 0;

RetWert:

  MatDel(pm); MatDel(pZ); MatDel(pbeta0);

  return ret;
}










int Diskrim(
  MATRIX *pX,		/* Merkmalsausprägungen */
  MATRIX *pn,		/* Gesamtanzahl		*/
  MATRIX *py,		/* Klassenzugehörigkeit	*/
  bool   SameCov,
  MATRIX *pbeta
)
{
  int    i, j, k, ret, messanz, varanz, n_v, n_g;
  MATRIX *psu_v, *psu_g, *pH_v, *pH_g, *pH_a, *pbeta_v, *pbeta_g,
	 *perg_v, *perg_g;
  REAL r, sum;


  messanz = pX->dimy;
  varanz  = pX->dimx-1;


  psu_v = MatMake(varanz, 1);
  psu_g = MatMake(varanz, 1);
  perg_v = MatMake(varanz, 1);
  perg_g = MatMake(varanz, 1);
  pbeta_v = MatMake(varanz, 1);
  pbeta_g = MatMake(varanz, 1);
  pH_v  = MatMake(varanz, varanz);
  pH_g  = MatMake(varanz, varanz);
  pH_a  = MatMake(varanz, varanz);


  if (!psu_v || !psu_g || !pH_v || !pH_a || !pH_g || !pbeta_v || !pbeta_g)
    Error("Diskrim: Speicher");


/* estimate feature means */

  FOR (i, varanz) VEK(psu_v, i) = VEK(psu_g, i) = 0.0;

  n_v = n_g = 0;

  FOR (i, varanz)
    FOR (j, varanz) 
      MAT(pH_v, i, j) = MAT(pH_g, i, j) = 0.0;


#if SMOOTH

  FOR (j, messanz) { 

    FOR (i, varanz) VEK(psu_g, i) += VEK(py, j) * MAT(pX, j, i+1);
    n_g += VEK(py, j);

    FOR (i, varanz) VEK(psu_v, i) += (VEK(pn, j) - VEK(py, j))* MAT(pX, j, i+1);
    n_v += VEK(pn, j) - VEK(py, j);

  }

  if (n_v <= 1 || n_g <= 1) Error("Diskrim: zu wenige Beispiele");

  FOR (i, varanz) { VEK(psu_v, i) /= n_v; VEK(psu_g, i) /= n_g; }


/* estimate covariance matrices */

  FOR (k, messanz) {

    FOR (i, varanz)
      FOR (j, varanz) 
        MAT(pH_g, i, j) += VEK(py, k) * 
			   ((MAT(pX, k, i+1) - VEK(psu_g, i)) * 
	  		    (MAT(pX, k, j+1) - VEK(psu_g, j)));
    FOR (i, varanz)
      FOR (j, varanz) 
        MAT(pH_v, i, j) += (VEK(pn, k) - VEK(py, k)) * 
			   ((MAT(pX, k, i+1) - VEK(psu_v, i)) * 
	  		    (MAT(pX, k, j+1) - VEK(psu_v, j)));

  }

#else


  FOR (j, messanz) { 

    if (VEK(py, j) >= 0.5 * VEK(pn, j)) {

      FOR (i, varanz) VEK(psu_g, i) += MAT(pX, j, i+1);
      n_g++;

    } else {

      FOR (i, varanz) VEK(psu_v, i) += MAT(pX, j, i+1);
      n_v++;

    }
  }

  if (n_v <= 1 || n_g <= 1) Error("Diskrim: too few examples");

  FOR (i, varanz) { VEK(psu_v, i) /= n_v; VEK(psu_g, i) /= n_g; }





/* estimate covariance matrices */

  FOR (i, varanz)
    FOR (j, varanz) 
      MAT(pH_v, i, j) = MAT(pH_g, i, j) = MAT(pH_a, i, j) = 0.0;

  FOR (k, messanz) {

    REAL r;

    if (VEK(py, k) >= 0.5 * VEK(pn, k))

      FOR (i, varanz)
        FOR (j, varanz) {

          r = (MAT(pX, k, i+1) - VEK(psu_g, i)) * 
	      (MAT(pX, k, j+1) - VEK(psu_g, j));
	  MAT(pH_g, i, j) += r;
	  MAT(pH_a, i, j) += r;

	}
 
    else

      FOR (i, varanz)
        FOR (j, varanz) {
          r = (MAT(pX, k, i+1) - VEK(psu_v, i)) * 
	      (MAT(pX, k, j+1) - VEK(psu_v, j));
	  MAT(pH_v, i, j) += r;
	  MAT(pH_a, i, j) += r;

	}

  }

#endif




  FOR (i, varanz)
    FOR (j, varanz) {
      MAT(pH_v, i, j) /= (n_v);
      MAT(pH_g, i, j) /= (n_g);
      MAT(pH_a, i, j) /= (n_g+n_v);
    }


/*
printf("1.\n");

VekAus("G", psu_g);
VekAus("V", psu_v);

MatAus("G", pH_g);
MatAus("V", pH_v);
*/


  if (SameCov) {

    REAL su;


    if (pbeta->dimy != varanz+1 || pbeta->dimx != 1)
      Error("NRX: beta Dim?");

#if 0

/* average the covariance estimates of both classes */

    FOR (i, varanz)
      FOR (j, varanz) 
        MAT(pH_v, i, j) = (MAT(pH_g, i, j) + MAT(pH_v, i, j)) * 0.5;

/*
MatAus("mittel:", pH_v);
MatAus("eine:", pH_a);
*/

#endif


    i = MatInv(pH_a);

    if (i) {
      printf("*** Diskrim: Covarianzmatrix is singular");
      ret = 1;
      goto RetWert;
    }


    FOR (i, varanz) {

      su = 0;

      FOR (j, varanz) 
	su += VEK(psu_g, j) * MAT(pH_a, i, j);

      VEK(pbeta_g, i) = su;

      su = 0;

      FOR (j, varanz) 
	su += VEK(psu_v, j) * MAT(pH_a, i, j);

      VEK(pbeta_v, i) = su;

      VEK(pbeta, i+1) = VEK(pbeta_g, i) - VEK(pbeta_v, i);

    }

#if 1

/* geht auch ... */

    su = 0;

    FOR (i, varanz) su += VEK(pbeta, i+1) * (VEK(psu_v, i) + VEK(psu_g, i));

    VEK(pbeta, 0) = - su * 0.5;

#else

    su = 0;

    FOR (i, varanz) su += VEK(pbeta_g, i) * VEK(psu_g, i) - 
			  VEK(pbeta_v, i) * VEK(psu_v, i);

    VEK(pbeta, 0) = - su * 0.5;

#endif


    ret = 0;

    goto RetWert;

  }



#if 0
MatAus("G", pH_g);

  r = MatDet(pH_g);
  if (r <= 0) Error("#### H_g singular");

printf("!!! %e %e\n", r, log(r));
#endif


  i = MatInv(pH_v);

  if (i) {
    printf("*** Diskrim: Kovarianzmatrix V ist singulär");
    ret = 1;
    goto RetWert;
  }

  i = MatInv(pH_g);

  if (i) {
    printf("*** Diskrim: Kovarianzmatrix G ist singulär");
    ret = 1;
    goto RetWert;
  }


  ret = 0;

/*

printf("2.\n");

MatAus("G", pH_g);
MatAus("V", pH_v);

*/


printf(">>>\n");


  FOR (i, varanz) {

    FOR (j, varanz) printf("%+10.3e, ", 0.5*(MAT(pH_g, i, j) - MAT(pH_v, i, j)));
    printf("\n");

  }

  printf("\n");



  MatMult(pH_v, psu_v, perg_v);
  MatMult(pH_g, psu_g, perg_g);

  FOR (i, varanz) {

    printf("%+10.3e, ", VEK(perg_v, i) - VEK(perg_g, i));

  }

  printf("\n\n");

  sum = 0;

  r = MatDet(pH_g);
  if (r <= 0) Error("H_g singular");
  sum += log(r);

#if 0
printf("!!! %e %e\n", r, log(r));
#endif

  r = MatDet(pH_v);
  if (r <= 0) Error("H_v singular");
  sum -= log(r);

#if 0
printf("!!! %e %e\n", r, log(r));
#endif


  FOR (i, varanz) {

    sum += VEK(psu_v, i) * VEK(perg_v, i) - VEK(psu_v, i) * VEK(perg_v, i);

  }

  sum *= 0.5;


  printf("%+10.3e, \n", sum);

/*

VekAus("G", psu_g);
VekAus("V", psu_v);

MatAus("G", pH_g);
MatAus("V", pH_v);

*/

printf("<<<\n");



/* estimate misclassification rate */

  {
    int    falsch=0;
    REAL   r;
    MATRIX *pm, *pmt, *pz, *perg;


    pm   = MatMake(varanz, 1);
    pmt  = MatMake(1, varanz);
    pz   = MatMake(varanz, 1);
    perg = MatMake(1, 1);


  FOR (k, messanz) {

    FOR (i, varanz) VEK(pm, i) = VEK(pmt, i) = MAT(pX, k, i+1) - VEK(psu_g, i);
    MatMult(pH_g, pm, pz); MatMult(pmt, pz, perg);
/*     VekAus("erg_g", perg); */

    r = VEK(perg, 0);

    FOR (i, varanz) VEK(pm, i) = VEK(pmt, i) = MAT(pX, k, i+1) - VEK(psu_v, i);
    MatMult(pH_v, pm, pz); MatMult(pmt, pz, perg);
/*    VekAus("erg_v", perg); */

    r = VEK(perg, 0) - r;

    falsch += (r >= 0.0) ^ (VEK(py, k) >= 0.5 * VEK(pn, k));
 
  }

printf("Fehlerw'keit: %f\n\n", (REAL)falsch/messanz);

    MatDel(pm); MatDel(pmt); MatDel(pz); MatDel(perg);
  }

  
RetWert:

  MatDel(pbeta_v); MatDel(pbeta_g); 
  MatDel(psu_v); MatDel(psu_g); MatDel(pH_v); MatDel(pH_g); MatDel(pH_a);
  MatDel(perg_v); MatDel(perg_g);  
 
  return ret;
}


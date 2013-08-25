// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Maxtrix-Allerlei */

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h"

#include "logm.h"
#include "logmat.h"


int checkMAT(MATRIX *pm, int y, int x, char *datei, int line)
{
  if (!pm) {
    fprintf(stderr, "Datei: %s Zeile %d ", datei, line);
    Error("checkMAT: NULL Pointer");
  }

  if (y < 0 || y >= pm->dimy) {
    fprintf(stderr, "Datei: %s Zeile %d ", datei, line);
    Error("checkMAT: y auﬂerhalb");
  }
  if (x < 0 || x >= pm->dimx) {
    fprintf(stderr, "Datei: %s Zeile %d ", datei, line);
    Error("checkMAT: x auﬂerhalb");
  }
  return 0;
}


int checkVEK(MATRIX *pm, int i, char *datei, int line)
{
  int max;

  if (pm->dimx != 1 && pm->dimy != 1) {
    fprintf(stderr, "Datei: %s Zeile %d ", datei, line);
    Error("checkVEK: kein Vektor");
  }

  if (pm->dimx > pm->dimy) max = pm->dimx; else max = pm->dimy;

  if (i < 0 || i >= max) {
    fprintf(stderr, "Datei: %s Zeile %d ", datei, line);
    Error("checkVEK: i auﬂerhalb");
  }
  return 0;
}





/* Matrix vorgegebener Groesse erstellen */

MATRIX *MatMake(int dimy, int dimx)
{
  MATRIX *pM;


  if (dimx <= 0 || dimy <= 0) Error("MatMakeI");

  pM = (MATRIX *) calloc(sizeof(MATRIX), 1);

  if (!pM) return 0;


  pM->e = (REAL *) calloc(sizeof(REAL), dimx * dimy);

  if (!pM->e) { free(pM); return 0; }

  pM->dimx = dimx;
  pM->dimy = dimy;

  return pM;
}






/* Matrix lˆschen */

void MatDel(MATRIX *pM)
{
  if (pM) {

    if (pM->e) free(pM->e);

    free(pM);
  }
}







/* Matrix ausgeben */

#define FAUS "%6.5"

void MatAus(char *Name, MATRIX *pM)
{
  int x, y;


  printf("%s: dimx=%d dimy=%d\n", Name, pM->dimx, pM->dimy);

  FOR (y, pM->dimy) {

    FOR (x, pM->dimx) printf(FAUS R_F " ", MAT(pM, y, x));

    printf("\n");
  }
}




/* Vektor ausgeben */


void VekAus(char *Name, MATRIX *pM)
{
  int i, dim;


  if (pM->dimx != 1 && pM->dimy != 1) Error("VekAus: kein Vektor");

  printf("%s: ", Name);

  if (pM->dimx == 1) dim = pM->dimy; else dim = pM->dimx;



  FOR (i, dim-1) {

    printf(FAUS R_F ", ", VEK(pM, i));

  }

  printf(FAUS R_F, VEK(pM, i));


  printf("\n");
}








 

   

/* reele nxn-Matrix mit dem Austauschverfahren invertieren		    */
/*   aus:								    */
/*     Engeln-Muellges/Reutter: Formelsammlung zur numerischen Mathematik   */
/*     BI (1986)							    */


int MatInv(MATRIX *pM)
{
  int i, ix, iy, j, k, n, nx, ny;
  int mx[NMAX], my[NMAX];
  REAL emachn, hilf, faktor, pivo;


  if (pM->dimx != pM->dimy || pM->dimx <= 0 || pM->dimy <= 0)
	Error("keine nxn-Matrix in invert");


/* Berechnung von Epsilon */

  emachn = 1.0;

  do emachn *= 0.5; while (1.0 < 1.0 + emachn);
  emachn *= 2.0;


/* Vorbesetzen der Pivotvektoren mx und my mit -1 */

  n = pM->dimx;
  FOR (i, n) mx[i] = my[i] = -1;


/* Hauptschleife */


  FOR (i, n) {


/* Pivotelement bestimmen */

    pivo = 0.0;

    FOR (ix, n)
 
      if (mx[ix] < 0) {

        FOR (iy, n) 

	  if (my[iy] < 0) {

  	    if (fabs(hilf=MAT(pM, iy, ix)) > fabs(pivo)) {
						
	      pivo = hilf;
   	      nx = ix;
	      ny = iy;
	    }
	  }
      }   




/* falls das Pivot-Element 0 ist, ist die Matrix singul‰r */
 	      
   if (fabs(pivo) < 4 * emachn) return 1;


	
/* Indizes merken */

    mx[nx] = ny;
    my[ny] = nx;


/* Berechnung der Matrixelemente */

    hilf = 1.0 / pivo;


    FOR (j, n) {

      if (j != nx) {

	faktor = MAT(pM, ny, j) * hilf;

	FOR (k, n) {
			
	  MAT(pM, k, j) -= MAT(pM, k, nx) * faktor;
	  MAT(pM, ny, j) = -faktor;

	}
      }
    }

    FOR (k, n) {

      MAT(pM, k, nx) *= hilf;
      MAT(pM, ny, nx) = hilf;

    }
  }
  

/* Zeilen- und Spaltenvertauschungen r¸ckg‰ngig machen */

  FOR (i, n) {

    j = mx[i];

    if (j != i) {

      FOR (k, n) {

	hilf = MAT(pM, k, i); 
	MAT(pM, k, i) = MAT(pM, k, j);
	MAT(pM, k, j) = hilf;

      }
      mx[i] = mx[j];
      mx[j] = j;
    }


    j = my[i];

    if (j != i) {

      FOR (k, n) {

	hilf = MAT(pM, i, k); 
	MAT(pM, i, k) = MAT(pM, j, k);
	MAT(pM, j, k) = hilf;
 
      }
      my[i] = my[j];
      my[j] = j;
    }
  }

  return 0;		/* alles klar */
}



REAL MatDet(MATRIX *pM0)
{
  int    i, j, k, n, sign=1;
  REAL r, pivo;
  MATRIX *pM;


  MATCHECK(pM0, "M");

  if (pM0->dimx != pM0->dimy) Error("dims !=");

  n = pM0->dimx;


  
  pM = MatMake(n, n);

  if (!pM) Error("MatDet: no mem");

  MatCopy(pM0, pM);


  FOR (i, n) {

/* Suche maximalen Eintrag */

    pivo = 0.0; k = 0;

    for (j=i; j < n; j++) 
      if (fabs(MAT(pM, j, i)) > fabs(pivo)) { k = j; pivo = MAT(pM, j, i); }

    if (pivo == 0.0) {

/* keinen Eintrag !=0 gefunden! => det==0! */

      r = 0;
      goto ret;
    }


    j = k;


/* Zeilen i,j vertauschen */
    
    if (i != j) {

      sign = -sign;

      for (k=i; k < n; k++) {
        r = MAT(pM, i, k); MAT(pM, i, k) = MAT(pM, j, k); MAT(pM, j, k) = r;
      }       
    }
    
/* mat[i][i] !=0; es darf geteilt werden */      

    pivo = 1/pivo;

    for (j=i+1; j < n; j++) {
      r = MAT(pM, j, i) * pivo;
      for (k=i; k < n; k++) {
        MAT(pM, j, k) -= r * MAT(pM, i, k);
      }
    }    
  }


  r = sign;

  FOR (i, n) r *= MAT(pM, i, i);


ret:

  MatDel(pM);

  return r;

}






void MatSub(MATRIX *p1, MATRIX *p2, MATRIX *pe)
{
  int anz, i;


  MATCHECK(p1, "p1"); MATCHECK(p2, "p2"); MATCHECK(pe, "pe");

/*
printf("%d %d %d %d %d %d\n", p1->dimx,p1->dimy,p2->dimx,p2->dimy,pe->dimx,pe->dimy);
*/




  if (p1->dimx != p2->dimx || 
      p1->dimy != p2->dimy ||
      p1->dimx != pe->dimx || 
      p1->dimy != pe->dimy) Error("MatSub");


  anz = p1->dimx * p1->dimy;

  FOR (i, anz) pe->e[i] = p1->e[i] - p2->e[i];

}




void MatAdd(MATRIX *p1, MATRIX *p2, MATRIX *pe)
{
  int anz, i;


  MATCHECK(p1, "p1"); MATCHECK(p2, "p2"); MATCHECK(pe, "pe");

  if (p1->dimx != p2->dimx || 
      p1->dimy != p2->dimy ||
      p1->dimx != pe->dimx || 
      p1->dimy != pe->dimy) Error("MatSub");


  anz = p1->dimx * p1->dimy;

  FOR (i, anz) pe->e[i] = p1->e[i] + p2->e[i];

}










void MatMult(MATRIX *p1, MATRIX *p2, MATRIX *pe)
{
  int i, j, k;
  REAL su;


  MATCHECK(p1, "mp1"); MATCHECK(p2, "mp2"); MATCHECK(pe, "mpe");

/*printf("%d %d %d %d %d %d\n", p1->dimx,p1->dimy,p2->dimx,p2->dimy,pe->dimx,pe->dimy);
*/




  if (p1->dimx != p2->dimy || 
      p1->dimy != pe->dimy ||
      p2->dimx != pe->dimx) Error("MatMult: Dimensionen stimmen nicht");


  FOR (i, pe->dimy)
    FOR (j, pe->dimx) {

      su = 0.0;

      FOR (k, p1->dimx) 
	su += MAT(p1, i, k) * MAT(p2, k, j);

      MAT(pe, i, j) = su;
    }
} 


 





REAL MatMaxRel(MATRIX *p1, MATRIX *p2)
{
  int i, anz;
  REAL max = 0.0, hilf;


  MATCHECK(p1, "p1R"); MATCHECK(p2, "p2R");

  if (p1->dimx != p2->dimx || 
      p1->dimy != p2->dimy) Error("MatMaxRel");

  anz = p1->dimx * p1->dimy;

  FOR (i, anz) {

    hilf = p1->e[i];

    if (hilf == 0.0) hilf += EPS;

    hilf = fabs((p2->e[i] - hilf) / hilf);

    if (hilf > max) max = hilf;
  }

  return max;
}



 
void MatCopy (MATRIX *p1, MATRIX *p2)
{
  int i, anz;


  MATCHECK(p1, "p1C"); MATCHECK(p2, "p2C");

  if (p1->dimx != p2->dimx || 
      p1->dimy != p2->dimy) Error("MatMaxRel");

  anz = p1->dimx * p1->dimy;
 
  FOR (i, anz) p2->e[i] = p1->e[i];
}

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Logistische Regression / 28.10.91, 14.8.92 */

#ifndef PRECOMP
#include "main.h"
#endif


#include "crt.h"

#include "logmat.h"
#include "logio.h"
#include "logwls.h"
#include "logall.h"
#include "logeig.h"
#include "goodies.h"

 
#define KORR_GROSS	(0.75)		/* hier wird "*" ausgegeben */
#define KORR_KLEIN	(0.05)		/* hier wird "!" ausgegeben */

#define ITERMAX		25
#define ENDREL		(2e-6)

#define KLEIN		(1e-6)		/* kleinster Standabw.wert */

#define STACONST 	0.551328895	/* Wurzel(3) / Pi für standard. Schätzung */



void _abort(void)
{
  exit(1);
}

 



/* untere Schranke für p-Wert zurück für Chi-Quadrat-Verteilung */

REAL ChiQuadrat(REAL x)
{
  int  i;

/* Tabelle aus Knuth 2, Seite 41, von Karen und Handbook of math. function */

  REAL points[26] = { 
    0.00016, 0.001, 0.002, 0.003, 0.00393, 0.1015, 0.4549, 1.0, 
    1.312,     1.5, 2.0,   2.2,   2.6,     2.706,  2.8,    3.0,
    3.2,       3.4, 3.6,   3.841, 4.0,     5.024,  6.0,    6.635,
    7.879,    10.5
  };
  
  REAL p[27] = {
    0.01, 0.02523, 0.03567, 0.04368, 0.05,    0.25, 0.50,    0.68269,  
    0.75, 0.77933, 0.8427,  0.86199, 0.89314, 0.9,  0.90574, 0.91673,
    0.92636, 0.9348, 0.94222, 0.95, 0.9545, 0.975, 0.98569,  0.99,
    0.995,  0.99881,
    1.0
  };


  i = 0;

  while (i < sizeof(points)/sizeof(REAL) && x > points[i]) i++;


  return 1.0 - p[i];
}




int main(int argc, char **argv)
{
  int    i, j, k, j_v, j_g, BeispAnz, varanz, messanz, MerkmAnz;
  int    f_a, f_e, f_k, f_K, f_p, f_d, f_h, f_n, f_r, f_u, f_equal, 
	 ok=false, lauf;
  MATRIX *pbeta, 
	 *psu,   *psu_v,  *psu_g, 
         *pm,    *pm_v,   *pm_g, 
         *ps,    *ps_v,   *ps_g, 
         *pmin,  *pmin_v, *pmin_g, 
         *pmax,  *pmax_v, *pmax_g, 
         *pwald, *perr, *pp, *psta, *pH, *pX, *pK, *pn, *py,
	 *pd, *pe, *pE, *pf;
  REAL   r, x, xi, xk, logL, Fehlklass01, Fehlklass10, Diskordanz;
  char   Merkmale[VARMAX][LENMAX], Input[DATMAX], Name[DATMAX],
         *InterDatei, *TestDatei;
  FILE   *fp;


#if 0

MATRIX *pa;

#define N	2

  pa = MatMake(N, N);

  MAT(pa, 0, 0) = 2;  MAT(pa, 0, 1) = 3; 

  MAT(pa, 1, 0) = 4;  MAT(pa, 1, 1) = 5;


  printf("%f\n", MatDet(pa));


  exit(0);

#endif



  if (argc < 2) {

Fehler:

    Error("\
Aufruf: olog OPTIONS mrk-Datei\n\
             -a: Ausgabe in Dateien ablegen\n\
             -d: Diskriminanzanalyse\n\
	     -r: Lineare Regression\n\
	     -u: Univariate Lineare Regression\n\
             -e: Einfache Statistiken\n\
             -equal: force equal number of examples in both classes\n\
             -h: Hauptkomponenten\n\
             -k: Korrelationsmatrix bestimmen\n\
             -K: Kovarianzmatrix bestimmen\n\
             -n: Merkmale auf Varianz 1 normieren\n\
             -p: Logit-Parameter nicht bestimmen\n\
	     -t Datei: Parameter mit Datei testen\n\
             -i Datei: Interaktions-Datei benutzen zur Angabe von Produktmerkmalen\n\n");

  }

  f_a = f_e = f_k = f_K = f_p = f_d = f_h = f_n = f_r = f_u = f_equal = false; 
  InterDatei = TestDatei = NULL;

  for (i=1; i < argc-1; i++) {

    if (strcmp(argv[i], "-a") == 0)      f_a = true;

    else if (strcmp(argv[i], "-d") == 0) { f_d = true; f_p = true; }

    else if (strcmp(argv[i], "-r") == 0) { f_r = true; f_p = true; }

    else if (strcmp(argv[i], "-u") == 0) { f_u = true; f_p = true; }

    else if (strcmp(argv[i], "-e") == 0) f_e = true;

    else if (strcmp(argv[i], "-equal") == 0) f_equal = true;

/*    else if (strcmp(argv[i], "-h") == 0) f_h = true;*/

    else if (strcmp(argv[i], "-k") == 0) f_k = true;

    else if (strcmp(argv[i], "-K") == 0) f_K = true;

/*    else if (strcmp(argv[i], "-n") == 0) f_n = true;*/

    else if (strcmp(argv[i], "-p") == 0) f_p = true;

    else if (!strcmp(argv[i], "-t")) {

      TestDatei = argv[i+1]; i++; 

      if (!(fp = fopen(TestDatei, "r"))) Error("Test-Datei nicht zu öffnen");
      fclose(fp);

    } else if (!strcmp(argv[i], "-i")) {

      InterDatei = argv[i+1]; i++;

      if (!(fp = fopen(InterDatei, "r"))) Error("Interaktions-Datei nicht zu öffnen");
      fclose(fp);

    } else goto Fehler;
  }


  if ((BeispAnz = BspInput(argv[argc-1], InterDatei, 
		      &pX, &pn, &py, &MerkmAnz, Merkmale, Input)) <= 1)
    Error("keine Daten");

  printf("Datei: %s   (%d Beispiele)\n\n", Input, pn->dimx);

  varanz  = pX->dimx;
  messanz = pX->dimy;

  pbeta = MatMake(varanz, 1);
  psu   = MatMake(varanz, 1);
  psu_v = MatMake(varanz, 1);
  psu_g = MatMake(varanz, 1);
  pm    = MatMake(varanz, 1);
  pm_v  = MatMake(varanz, 1);
  pm_g  = MatMake(varanz, 1);
  ps    = MatMake(varanz, 1);
  ps_v  = MatMake(varanz, 1);
  ps_g  = MatMake(varanz, 1);
  pmin  = MatMake(varanz, 1);
  pmin_v= MatMake(varanz, 1);
  pmin_g= MatMake(varanz, 1);
  pmax  = MatMake(varanz, 1);
  pmax_v= MatMake(varanz, 1);
  pmax_g= MatMake(varanz, 1);
  pwald = MatMake(varanz, 1);
  perr  = MatMake(varanz, 1);
  pp    = MatMake(varanz, 1);
  psta  = MatMake(varanz, 1);

  pH    = MatMake(varanz, varanz);
  pK    = MatMake(varanz, varanz);

  pd    = MatMake(varanz-1, 1);
  pe    = MatMake(varanz-1, 1);
  pE    = MatMake(varanz-1, varanz-1);

  pf    = MatMake(varanz, 1);

  if (!pbeta || !psu   || !psu_v || !psu_g || !pm     || !pm_v   || !pm_g || 
      !ps    || !ps_v  || !ps_g  || !pmin  || !pmin_v || !pmin_g || !pmax || 
      !pmax_v || !pmax_g|| !pwald || !perr  || !pp    || !psta   || !pH   ||
      !pK || !pd || !pe || !pE || !pf)

    Error("olog: Speicher?");



  if (f_equal) {

    int  nl=0, nw=0;
    REAL factor;


    FOR (j, messanz)
      if (VEK(py, j) < 0.5 * VEK(pn, j)) 
	nl += VEK(pn, j); 
      else 
	nw += VEK(pn, j);
    
    printf("equal: win:%d loss:%d\n\n", nw, nl);

    if (!nw || !nl) Error("too few examples");

    factor = ((REAL)nw)/nl;

    FOR (j, messanz)

      if (VEK(py, j) < 0.5 * VEK(pn, j)) {

        VEK(py, j) *= factor;
	VEK(pn, j) *= factor;
   
      }

  }





/* einfache Statistiken berechnen (auch in einzelnen Klassen)	*/
/* Standardabweichung nach Knuth 2, Seite 216			*/

  lauf = 0;

stat:

  FOR (i, varanz) {

    VEK(psu, i) = VEK(psu_v,i) = VEK(psu_g, i) = 
    VEK(pm,  i) = VEK(pm_v, i) = VEK(pm_g,  i) =
    VEK(ps,  i) = VEK(ps_v, i) = VEK(ps_g,  i) = 0.0;

    VEK(pmin, i) = VEK(pmin_v, i) = VEK(pmin_g, i) = 
    VEK(pmax, i) = VEK(pmax_v, i) = VEK(pmax_g, i) = MAT(pX, 0, i);

  }


  j_g = j_v = 0;		/* Anzahlen in den Klassen */

  FOR (j, messanz) {

    FOR (i, varanz) {

      x = MAT(pX, j, i);

      if (x > VEK(pmax, i)) VEK(pmax, i) = x;
      if (x < VEK(pmin, i)) VEK(pmin, i) = x;
      VEK(psu, i) += x;
      r = VEK(pm, i) + (x - VEK(pm, i)) / (j+1);
      VEK(ps, i) += (x - VEK(pm, i)) * (x - r);
      VEK(pm, i) = r;

      if (VEK(py, j) < 0.5 * VEK(pn, j)) {

        if (x > VEK(pmax_v, i)) VEK(pmax_v, i) = x;
        if (x < VEK(pmin_v, i)) VEK(pmin_v, i) = x;
        VEK(psu_v, i) += x;
        r = VEK(pm_v, i) + (x - VEK(pm_v, i)) / (j_v+1);
        VEK(ps_v, i) += (x - VEK(pm_v, i)) * (x - r);
        VEK(pm_v, i) = r;

      } else {

        if (x > VEK(pmax_g, i)) VEK(pmax_g, i) = x;
        if (x < VEK(pmin_g, i)) VEK(pmin_g, i) = x;
        VEK(psu_g, i) += x;
        r = VEK(pm_g, i) + (x - VEK(pm_g, i)) / (j_g+1);
        VEK(ps_g, i) += (x - VEK(pm_g, i)) * (x - r);
        VEK(pm_g, i) = r;

      }
   
    }

    if (VEK(py, j) < 0.5 * VEK(pn, j)) j_v++; else j_g++;

  }

  if (!j_v) j_v = 1;	/* /0 verhindern */
  if (!j_g) j_g = 1;


  FOR (i, varanz) {

    VEK(psu, i) /= messanz;
    VEK(ps,  i) = sqrt(VEK(ps, i) / (messanz-0));
    if (VEK(ps, i) == 0.0) VEK(ps, i) = KLEIN;

    VEK(psu_v, i) /= j_v;
    VEK(ps_v,  i) =  sqrt(VEK(ps_v, i) / (j_v-0));
    if (VEK(ps_v, i) == 0.0) VEK(ps_v, i) = KLEIN;

    VEK(psu_g, i) /= j_g;
    VEK(ps_g,  i) =  sqrt(VEK(ps_g, i) / (j_g-0));
    if (VEK(ps_g, i) == 0.0) VEK(ps_g, i) = KLEIN;

  }


  if (!lauf && f_n) {

/* Merkmale auf Varianz 1 normieren */

    printf("Normieren - Faktoren: ");

    FOR (j, varanz) {
      if (VEK(ps, j) > KLEIN) x = 1.0 / VEK(ps, j); else x = 1.0;
      VEK(pf, j) = x;
      printf("%.4f ", x);
      FOR (i, messanz) MAT(pX, i, j) *= x;
    }
    printf("\n"); 

    lauf = 1; goto stat;
  }




  if (f_e) {


/* Einzelstatistiken */


printf("Klasse 0: (%d)\n\n", j_v);
printf("                        Standard-                     \n");
printf("Merkmal     Mittelwert  abweichung   Minimum   Maximum\n\n");    


  for (i=1; i < varanz; i++) {

    printf("%-10s   ",        Merkmale[i]);

    printf("%8.4f   ",  VEK(psu_v, i));

    printf("%8.4f    ", VEK(ps_v, i));
    
    printf("%8.4f  ",   VEK(pmin_v, i));

    printf("%8.4f   ",  VEK(pmax_v, i));

    printf("\n");
  }

  printf("\n\n\n");


printf("Klasse 1: (%d)\n\n", j_g);
printf("                        Standard-                     \n");
printf("Merkmal     Mittelwert  abweichung   Minimum   Maximum\n\n");    


  for (i=1; i < varanz; i++) {

    printf("%-10s   ",        Merkmale[i]);

    printf("%8.4f   ",  VEK(psu_g, i));

    printf("%8.4f    ", VEK(ps_g, i));
    
    printf("%8.4f  ",   VEK(pmin_g, i));

    printf("%8.4f   ",  VEK(pmax_g, i));

    printf("\n");
  }

  printf("\n\n\n");




/* Gesamtstatistik */


printf("Klassen 0,1: (%d)\n\n", messanz);
printf("                        Standard-                        Standard.\n");
printf("Merkmal     Mittelwert  abweichung   Minimum   Maximum   Differenz\n\n");    


  for (i=1; i < varanz; i++) {

    printf("%-10s   ", Merkmale[i]);

    printf("%8.4f   ",  VEK(psu, i));

    printf("%8.4f    ", VEK(ps, i));
    
    printf("%8.4f  ",   VEK(pmin, i));

    printf("%8.4f   ",  VEK(pmax, i));

    printf("%8.4f   ",  (VEK(pm_g, i) - VEK(pm_v, i)) *
				2 / (VEK(ps_v, i) + VEK(ps_g, i)));
 
    printf("\n");
  }

  printf("\n\n\n");

  }


  if (f_k || f_K) {

    for (i=1; i < varanz; i++) 
      for (k=i; k < varanz; k++) {

        x = 0; xi = VEK(psu, i); xk = VEK(psu, k);

        FOR (j, messanz)
	  x += (MAT(pX, j, i) - xi) * (MAT(pX, j, k) - xk);

	if (f_k) 

	  MAT(pK, i, k) = 
	  MAT(pK, k, i) = x / (VEK(ps, i) * VEK(ps, k) * messanz);

	else

	  MAT(pK, i, k) = 
	  MAT(pK, k, i) = x / messanz;

      }


    if (f_k) printf("Korrelationsmatrix:\n\n\n");
    else     printf("Kovarianzmatrix:\n\n\n");

    printf("                 ");

    for (i=1; i < varanz; i++) printf(" (%2d) ", i);

    printf("\n\n");

    for (i=1; i < varanz; i++) {

      printf("%-10s (%2d)  ", Merkmale[i], i);

      FOR (k, i-1) printf("      ");

      for (k=i; k < varanz; k++) {

        printf("%5.2f", x=MAT(pK, i, k));

	if (f_k) {

	  if      (x >=  KORR_GROSS || x <= -KORR_GROSS) printf("*");
	  else if (x >= -KORR_KLEIN && x <=  KORR_KLEIN) printf("!");
          else					         printf(" ");

	} else printf(" ");
      }

      printf("\n");
    }	  

    printf("\n\n\n");


    if ((f_k || f_K) && f_h) {

      REAL su, su0;

      printf("Eigenwerte:\n");

      FOR (i, varanz-1) 
        FOR (j, varanz-1)
	  MAT(pE, i, j) = MAT(pK, i+1, j+1);

/*MatAus("XXX", pE);*/

      tred2(pE, pd, pe);
      tqli(pd, pe, pE);
      eigsrt(pd, pE);

su = su0 = 0;
FOR (i, varanz-1) su0 += VEK(pd, i);

FOR (i, varanz-1) { 
  su += VEK(pd, i); printf("%8.4f(%d%%) ", VEK(pd, i), round(100*su/su0)); 
}
printf("\n\n");

printf("Eigenvektoren zeilenweise:\n");
FOR (i, varanz-1) {
  FOR (j, varanz-1) printf("%8.4f ", MAT(pE, j, i));
  printf("\n");
}
printf("\n\n");


/* Datenmatrix gemäß der Eigenvektoren transformieren */

      FOR (i, messanz) {

	for (j=1; j < varanz; j++) {

	  x = 0;
	  FOR (k, varanz-1) { 
	    x += MAT(pE, j-1, k) * MAT(pX, i, k+1);
/*printf("%.2f * %.2f\n", MAT(pE, j-1, k), MAT(pX, i, k+1));
*/	    
	  }

	  MAT(pX, i, j) = x;
/*printf("-> %f\n", MAT(pX, i, j));
*/	}
/*printf("\n");
*/
      }
    }
  }
        

  if (f_d) Diskrim(pX, pn, py, false, pbeta); 
  if (f_r) Regression(pX, pn, py, pbeta);
  if (f_u) UniRegression(pX, pn, py, pbeta);
  
  if (f_d || f_r || f_u) {

    printf("            Parameter-\n");
    printf("Merkmal     schätzung\n\n");    

    FOR (i, varanz) {
 
      printf("%-10s ", Merkmale[i]);

      printf("%10.3e  ", VEK(pbeta, i));

      printf("\n");
    }

    Fehler(pX, pn, py, pbeta, &Fehlklass01, &Fehlklass10, &Diskordanz);

    printf(
"\n\n  Fehler= %.2f %% (0f= %.2f %%, 1f= %.2f %%)  Diskordanz= %.2f %%\n\n\n\n\n",
    (Fehlklass01+Fehlklass10)*100, 
    Fehlklass01*100,
    Fehlklass10*100,
    Diskordanz*100);

  }

  if (!f_p)

  if (logitfit(pX, pn, py, pbeta, &logL, pH, ITERMAX, ENDREL)) {

    printf("\n\n\n*** PROBLEME\n\n\n\n");

    ok = false;


  } else {


    ok = true;


    FOR (i, varanz) {

      VEK(perr, i)   = sqrt(MAT(pH, i, i));
      VEK(pwald, i)  = VEK(pbeta,  i)  * VEK(pbeta,  i) / 
		     VEK(perr, i) / VEK(perr, i);
      VEK(pp, i)     = ChiQuadrat(VEK(pwald, i));
      VEK(psta, i)    = VEK(pbeta, i) * VEK(ps, i) * STACONST;
    }




    printf(
"            Parameter-  Standard-      Chi-           Standard.\n");
    printf(
"Merkmal     schätzung    fehler      Quadrat    p     Schätzung\n\n");    

    FOR (i, varanz) {
 
      printf("%-10s ", Merkmale[i]);

      printf("%10.3e  ", VEK(pbeta, i));

      printf("%10.3e   ",  VEK(perr, i));
    
      printf("%8.2f   ", VEK(pwald, i));

      printf("%4.2f   ", VEK(pp, i));

      if (i != 0) printf("%7.4f  ", VEK(psta, i));
      else        printf("  .");
  
      printf("\n");
    }

    Fehler(pX, pn, py, pbeta, &Fehlklass01, &Fehlklass10, &Diskordanz);

    printf("\n\n  -2logL= %.2f\n", logL);

    printf("\n  Fehler= %.2f %% (0f= %.2f %%, 1f= %.2f %%)  Diskordanz= %.2f %%\n\n\n\n\n",
    (Fehlklass01+Fehlklass10)*100, 
    Fehlklass01*100,
    Fehlklass10*100,
    Diskordanz*100);

  }


  if (f_a) {

    VEK(psu,  0) = 1.0;
    VEK(ps,   0) = 0.0;
    VEK(pmin, 0) = 1.0;
    VEK(pmax, 0) = 1.0;

    FOR (i, varanz) {

      sprintf(Name, "out.%s", Merkmale[i]);

      fp = fopen(Name, "w");

      if (!fp) Error("kann Datei nicht öffnen");

      fprintf(fp, "%f  ", VEK(psu, i));
      fprintf(fp, "%f  ", VEK(ps, i));
      fprintf(fp, "%f  ", VEK(pmin, i));
      fprintf(fp, "%f  ", VEK(pmax, i));
 

      if (ok) {

        fprintf(fp, "%f  ", VEK(pbeta, i));
        fprintf(fp, "%f  ", VEK(perr, i));
        fprintf(fp, "%f  ", VEK(pwald, i));
        fprintf(fp, "%f  ", VEK(pp, i));
        fprintf(fp, "%f  ", VEK(psta, i));
 
      } else fprintf(fp, "* * * * *");

      if (ferror(fp)) Error("Schreibfehler");
      fclose(fp);

    }


    fp = fopen("out.glob", "w");
    if (!fp) Error("kann Datei nicht öffnen");

    if (ok) 
      fprintf(fp, "%f  %f  %f  %f  %f", 
	  logL, (Fehlklass01+Fehlklass10)*100, Fehlklass01*100,
	  Fehlklass10*100, Diskordanz*100);
    else
      fprintf(fp, "* * *");
      

    if (ferror(fp)) Error("Schreibfehler");
    fclose(fp);
 

    fp = fopen("out.Merkmale", "w");
    if (!fp) Error("kann Datei nicht öffnen");

    FOR (i, varanz) fprintf(fp, "%s  ", Merkmale[i]);

    if (ferror(fp)) Error("Schreibfehler");
    fclose(fp);
 

  } 





  if (TestDatei) {

    int MerkmAnzT;


    if ((BeispAnz = BspInput(TestDatei, InterDatei, 
		      &pX, &pn, &py, &MerkmAnzT, Merkmale, Input)) <= 1)
      Error("keine Testdaten");


    if (MerkmAnz != MerkmAnzT) Error("andere Merkmalsanzahl");

    printf("Testdatei: %s   (%d Beispiele)\n\n", Input, pn->dimx);
 
    
    Fehler(pX, pn, py, pbeta, &Fehlklass01, &Fehlklass10, &Diskordanz);

    printf(
"\n\n  Fehler= %.2f %% (0f= %.2f %%, 1f= %.2f %%)  Diskordanz= %.2f %%\n\n\n\n\n",
    (Fehlklass01+Fehlklass10)*100, 
    Fehlklass01*100,
    Fehlklass10*100,
    Diskordanz*100);


  }


  return 0;
}

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* .OSP korrigieren, 3.93 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "sboard.h"
#include "goodies.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "fpatt.h"
#include "pcstat.h"


#define ITERANZ		42		/* SteinAnz >= -> Voriteration	*/
#define ITERTIEFE	10		/* Tiefe hierfür		*/

#define EVALFUNC	EvalL		/* Bewertungsfkt. hierfür	*/
#define PARFILE         "evala.par"
#define TABFILE         "oplay.tab"
#define PCSTAT          "pcstatl"
#define ENDCUTSTAT      "endcutstatl"

#define QUIESCENCE	true
#define SELECTIVE	true
#define PERCENTILE1	1.1
#define PERCENTILE2	1.5

#define SCHRITT		4		/* Schrittweite für Differenz */

int z0;


SPIELINFO *Sp=NULL;


void _abort(void) { exit(1); }


void Check(COMZIO *pcio, ZUGIO *pzio, bool no_count, bool block=false) {}	/* Signal-Handler */

#if 0
void SfValue(ZUGIO *pzio, SPFELD *psf, PARTEI Partei)
{
  SFPOS Zuege[64];


  if (!SfMoeglZuege(psf, Partei, Zuege)) {

    if (!SfMoeglZuege(psf, GEGNER(Partei), Zuege)) { 

      pzio->Wert = Partei * (SfAnzBLACK(psf) - SfAnzWHITE(psf));
      return;
    } 

    pzio->Sf = *psf;
    pzio->Partei = GEGNER(Partei);
    pzio->LetzterZug = ZUG_UNBEKANNT;
    Zugermittlung(pzio);
    pzio->Wert = -pzio->Wert;

  } else {

    pzio->Sf = *psf;
    pzio->Partei = Partei;
    pzio->LetzterZug = ZUG_UNBEKANNT;
    Zugermittlung(pzio);
  }
}
#endif
 


int main(int argc, char **argv)
{
  int    anz, tab_korrupt, datei_korrupt, i, j, r, nr, DiffAlt, DiffNeu;
  int    KorrTiefe, KorrAnz, EndAnz, DiffAnz, SteinAnz, ZugAnz, 
	 Wert, al, be, EntscheidZug, Tiefe;
  PARTEI Partei;
  FILE   *fpin, *fpout;
  ZUGIO  zio;
  SFPOS  Zuege[64], Zug, IterZug;
  SPFELD sf, sf1;
  char   Ausgabe[100];
  int    Fehler=0, AbsDiff=0, firstnum=0;
  bool	 gewonnen, neuerZug, zuende;
  int    argi;

  if (argc < 6 || argc > 8) {

error:

    Error("call: ocor [-first num] corrdepth corrnum endnum diffnum osp-file\n"); 
  }

  argi = 1;


  if (argv[argi] && !strcmp(argv[argi], "-first")) {

    if (!argv[argi+1] || sscanf(argv[argi+1], "%d", &firstnum) != 1) goto error;
    if (firstnum <= 0) goto error;

    argi += 2;

  }


  if (!argv[argi]) goto error;
  KorrTiefe = atoi(argv[argi++]);
  if (!argv[argi]) goto error;
  KorrAnz   = atoi(argv[argi++]);
  if (!argv[argi]) goto error;
  EndAnz    = atoi(argv[argi++]);
  if (!argv[argi]) goto error;
  DiffAnz   = atoi(argv[argi++]);

  if (!argv[argi]) goto error;
  if (!(fpin=fopen(argv[argi], "r"))) Error("kann OSP-Eingabe nicht öffnen");


  if (argv[argi+1]) goto error;

  strcpy(PCStatFile,    PCSTAT);
  strcpy(EndPCStatFile, ENDCUTSTAT);

  printf("[ hash=%d ]\n", HASHBITS);


  if (KorrTiefe < 1 || KorrTiefe > 30 || KorrAnz < 4 || KorrAnz > EndAnz) 
    Error("corrdepth or corrnum wrong");

  if (EndAnz < 35 || EndAnz > 63 || EndAnz > DiffAnz || DiffAnz > 63) 
    Error("endnum or diffnum wrong");

  sprintf(Ausgabe, "%s.cor", argv[argi]);

  if (!(fpout=fopen(Ausgabe, "w"))) Error("can't open .cor-file");



  Sp = (SPIELINFO*) malloc (120 * sizeof(SPIELINFO));
  if (!Sp) Error("mem");


  anz = tab_korrupt = datei_korrupt = 0;

  InitZug(&zio, EVALFUNC, Check, HASHBITS, HASHMAXDEPTH, HASHBITS0, HASHMAXDEPTH0);

  strcpy(ParameterFile, PARFILE);
  strcpy(TableFile, TABFILE);


  nr = 1;

  fprintf(fpout, 
	  "Datei: %s, KORRTIEFE=%d KORRANZ=%d ENDANZ=%d DIFFANZ=%d\n\n", 
	  argv[argi], KorrTiefe, KorrAnz, EndAnz, DiffAnz);

  fclose(fpout);


  do { 


    if ((r=LoadNextGame(fpin, Sp)) != LADE_FEHLER) {

      if (r >= 0) {

	printf("Spiel %4d: ", nr); fflush(stdout);

	nr++;

	if (nr-1 < firstnum) continue;

	zuende = !SfMoeglZuege(&Sp[r].Sf, BLACK, Zuege) && 
		 !SfMoeglZuege(&Sp[r].Sf, WHITE, Zuege);

{


	  DiffAlt = SfAnzBLACK(&Sp[r].Sf) - SfAnzWHITE(&Sp[r].Sf);

	  for (i=1; i < r && SfAnz(&Sp[i].Sf) < KorrAnz; i++); 

	  neuerZug = i >= r;

	  if (1 || i < r) {

/* Spiel zuende führen */

	    Partei = Sp[i].AmZug;
	    sf     = Sp[i].Sf;

	    FOREVER {

	      printf("."); fflush(stdout);

	      SteinAnz = SfAnz(&sf);

	      ZugAnz   = SfMoeglZuege(&sf, Partei, Zuege);

	      if (!ZugAnz) {

	        Zug = ZUG_PASSEN;

		if (!SfMoeglZuege(&sf, GEGNER(Partei), Zuege)) break;

		printf("-"); fflush(stdout);

	      } else if (ZugAnz == 1) {

	        Zug = Zuege[0];
	        printf("|"); fflush(stdout);

	      } else {

		zio.Modus = MODUS_NORMAL;

	        zio.Sf		= sf;
	        zio.Partei	= Partei;
	        zio.LetzterZug	= ZUG_UNBEKANNT;

		zio.Selective  = SELECTIVE;
		zio.Percentile1 = PERCENTILE1;
		zio.Percentile2 = PERCENTILE2;
		zio.Quiescence = QUIESCENCE;


#if KILLER
		KillerAdjust(&zio.killer, &zio.Sf);
#endif

		Tiefe = KorrTiefe;

                if (SteinAnz <  25) Tiefe += 2;
		if (SteinAnz >= 32) Tiefe ++;
		if (SteinAnz >= 38) Tiefe ++;

/* if (SteinAnz >= 52) Tiefe = 5; */

	        if (SteinAnz + Tiefe >= 64) Tiefe = 64-SteinAnz;

/*
printf("(%d:%d)", SteinAnz, Tiefe);
fflush(stdout);
*/

		for (j=1; j <= Tiefe; j++) {	
		  zio.MaxTiefe = j;
		  Zugermittlung(&zio);
	        }


		if (SteinAnz >= EndAnz) {	

/* Endberechnung */

		  zio.Selective = false;

		  IterZug = zio.BestZug;

/* 1. Gewinner ermitteln */

		  if (SteinAnz < DiffAnz) printf("?"); 
		  else			  printf("!");
		  fflush(stdout);

/*
SfAus(&zio.Sf, BLACK, 0);
printf("%d\n", zio.Partei);
*/
		  zio.Modus = MODUS_GEWINN;
		  Zugermittlung(&zio);
		  Wert = zio.Wert;
		  EntscheidZug = zio.BestZug;

		  if (!neuerZug && Wert && SteinAnz < DiffAnz) {

/* tut's alter Zug auch? */

		    sf1 = sf;

		    if (!SfSetzen(&sf1, Partei, Sp[i].Zug)) goto iter;
		    
		    zio.Sf         = sf1;
		    zio.Partei	   = GEGNER(Partei);
	            zio.LetzterZug = ZUG_UNBEKANNT;

		    Zugermittlung(&zio);


		    if (sgn(Wert) == sgn(-zio.Wert)) {

/* ja! */
		      zio.BestZug = Sp[i].Zug;

		    } else {

iter:
		      if (Wert < 0) 

/* bei verloren IterZug nehmen */

		        zio.BestZug = IterZug;

		      else 

/* sonst gefundenen entscheidenden Zug */

			zio.BestZug = EntscheidZug;

		    }
	          } 

	          zio.Sf	 = sf;
	          zio.Partei	 = Partei;
	          zio.LetzterZug = ZUG_UNBEKANNT;

	          if (Wert && SteinAnz >= DiffAnz) { 

/* 2. Differenz maximieren in festen Schritten */

		    zio.Modus = MODUS_NEGAC;

		    gewonnen = Wert > 0;

		    al = -1; be = 1;

	            FOREVER {			

		      if (gewonnen) { al = Wert-1; be = Wert+SCHRITT; }
		      else	    { al = Wert-SCHRITT; be = Wert+1; }

	              zio.al = al; zio.be = be;

		      Zugermittlung(&zio);

	              Wert = zio.Wert;

		      if (Wert > al && Wert < be) break;
		    }
		  }

	        }

		Zug = zio.BestZug;

	      } 


/*
KoorAus(Zug); printf(":%d\n", round(100*EXPWERT(WERT_TO_REAL(zio.Wert))));
*/

	      if (ZUG(Zug)) SfSetzen(&sf, Partei, Zug);

	      if (i >= r || Zug != Sp[i].Zug) { 

	        neuerZug = true;
	      }

	      Sp[i].Zug = Zug;

	      KoorAus(Zug);
	      i++;
	      Partei = GEGNER(Partei);

	      Sp[i].Sf    = sf;
	      Sp[i].AmZug = Partei;
	    }

/* korrigiertes Spiel ausgeben */

	    InfoToTab(Sp, i, &sf);

	    TabEindeutig(&sf);

	    if (!(fpout=fopen(Ausgabe, "a"))) 
	      Error("kann Ausgabe-Datei nicht öffnen");

	    fTabAus(fpout, &sf); 
	    fclose(fpout);

	    if (zuende) {

	      DiffNeu = SfAnzBLACK(&Sp[i].Sf) - SfAnzWHITE(&Sp[i].Sf);

	      printf("\t  Alt:%3d Neu:%3d", DiffAlt, DiffNeu);

	      if ((DiffAlt <= 0 && DiffNeu >  0) ||
	          (DiffAlt >= 0 && DiffNeu <  0) ||
	  	  (DiffAlt != 0 && DiffNeu == 0)) { 
	        Fehler++; printf(" !!!"); 
	      }

	      AbsDiff += abs(DiffNeu-DiffAlt);

	      printf("  (%d Fehler, %.2f Steine)\n", 
			  Fehler, (REAL)AbsDiff/(nr-1));
	    }

          }
 	}

      } else tab_korrupt++;

    } else datei_korrupt++;

    anz++;

  } while (!feof(fpin));

  fclose(fpin); fclose(fpout);

  printf("%d Spiele untersucht\n", anz-1);
  if (datei_korrupt-1) printf("%d Eintrag/äge korrupt\n", datei_korrupt-1); 
  if (tab_korrupt)     printf("%d Tabelle(n) korrupt\n",  tab_korrupt); 

  return 0;
}

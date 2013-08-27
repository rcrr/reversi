// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// tournament director

#include "main.h"
#include "crt.h" 
#include "move.h"
#include "eval.h"
#include "sboard.h"
#include "order.h"
#include "playgm.h"
#include "trans.h"
#include "goodies.h"
#include "filecom.h"

#define TEST		false

#define SEND_CLEAR	true

#define L	0
#define D	1
#define W	2


bool f_aus=false, f_e=false, f_single=false;

int spielanz=0, spielnr=0;

bool is_sfk=false;


#define LAENGE	200

char   ZU_1[LAENGE], VON_1[LAENGE], ZU_2[LAENGE], VON_2[LAENGE];



void _abort(void)
{

  Enable_Abort = 0;


  printf("*** abort\a\n");



#if !TEST

/* Spieler killen */

  if (!SyncSendEXIT(ZU_1)) { SLEEP(5); SyncSendEXIT(ZU_1); }
  if (!SyncSendEXIT(ZU_2)) { SLEEP(5); SyncSendEXIT(ZU_2); }

  SLEEP(5);

  KillChannel(ZU_1);  KillChannel(ZU_2);
  KillChannel(VON_1); KillChannel(VON_2);

#endif

  exit(1);
}




int AutoSpiel(
  SPFELD    *psf,
  PARTEI    Beginner,
  int	    feldnr,
  SPIELINFO *pinfo,

  char      *ZU_BLACK, 
  char	    *VON_BLACK, 
  REAL	    RestBLACK,

  char	    *ZU_WHITE, 
  char	    *VON_WHITE,
  REAL	    RestWHITE,

  int       outcome
)
{
  int		n;
  int		zug, movetime, letzter;
  SFPOS		Zuege[64];
  SPFELD	sf;
  REAL		Rest;
  PARTEI	Partei;
  char		*ZU, *VON;
  ZEIT		StartZeit, EndZeit;
  char		Nachricht[NACHRICHT_MAX], *p;


  sf = *psf; sf.Marke = MA_WEISS_NICHT;

  letzter = ZUG_UNBEKANNT;
  Partei = Beginner;

  FOREVER {

    Chk_Abort();

    pinfo[feldnr].Sf 	= sf;
    pinfo[feldnr].AmZug = Partei;

#if TEST
    return 1;
#endif

#if 0

    if (!SfMoeglZuege(&sf, BLACK, Zuege) && !SfMoeglZuege(&sf, WHITE, Zuege))
      return feldnr;

#else 


    if (feldnr >= 3 && 
        pinfo[feldnr-1].Zug == ZUG_PASSEN && pinfo[feldnr-2].Zug == ZUG_PASSEN)
      return feldnr;

#endif


    if (Partei == BLACK) { Rest = RestBLACK; ZU = ZU_BLACK; VON = VON_BLACK; }
    else		 { Rest = RestWHITE; ZU = ZU_WHITE; VON = VON_WHITE; }


    if (SfAnz(&sf) >= outcome) Rest = 1e6;


#if 0

    if (SfAnz(&sf) == 63) {

/* finish game */

      if (SfMoeglZuege(&sf, Partei, Zuege)) {

        zug = Zuege[0];

      } else {

        zug = ZUG_PASSEN;

      }

      goto next;
    }


#endif


    FOREVER {

      SPFELD sf1;


      SfGrund(&sf1);

      if (!is_sfk && SfGleich(&sf1, &pinfo[1].Sf)) {

	GAME Game;
	SPFELD tab;

        InfoToTab(pinfo, feldnr, &tab);
        Tab2Game(&tab, &Game);

        if (SyncSendGAME(ZU, Partei, (int)Rest, &Game, true)) break;
        else { SLEEP(5); /* printf("w"); */ }

      } else {

        if (SyncSendBOARD(ZU, Partei, (int)Rest, letzter, &sf)) break;
        else { SLEEP(5); /* printf("w"); */}
      }
    }

    if (f_aus) {
      printf("\n"); 
      SfAus(&sf, Partei, 0);

      SteinAus(Partei); printf(" (%s)  Restzeit: %.2f sek ", VON, Rest);
      fflush(stdout);
    }
 
    Zeit(&StartZeit);

    FOREVER {

      if (Empf(VON, Nachricht)) {

	if (!ParseNumber(Nachricht, &n, &p) || n != SIG_MOVE ||
	    !ParseNumber(p, &zug, &p)       || (!ZUG(zug) && zug != ZUG_PASSEN)){
	  
	  Error("no move?");
        }

        if (!ParseNumber(p, &movetime, &p)) movetime = -1;
        else if (movetime < 0) Error("movetime < 0");

        break;

      } else SLEEP(1);
    }


next:


    if (f_aus) { printf(" -> Zug: "); KoorAus(zug); printf("\n\n"); }
    else printf("."); fflush(stdout);

    Zeit(&EndZeit);

    if (ZUG(zug)) if (!SfSetzen(&sf, Partei, zug)) {
      KoorAus(zug); Error("Zug falsch");
    }


    if (SfAnz(&sf) < outcome) {

      if (movetime < 0) /* no time sent - use wall clock */

	Rest -= ZeitDiff(&EndZeit, &StartZeit);

      else 

	Rest -= movetime;

      if (Rest < 0) printf(">>> Zeitüberschreitung\n");

      if (Partei == BLACK) RestBLACK = Rest; else RestWHITE = Rest;
    }

    pinfo[feldnr].Zug = letzter = zug;

    Partei = GEGNER(Partei); 
    feldnr++;

  }
}



int main(int argc, char **argv)
{
  int	    i, i1, j, j1, k, ZugAnz, Partei, argi, ZufZugAnz, SteinAnz, 
	    StartNr, EndNr, Beginner, Zug, firstnum, outcome;
  SPIELINFO Spiel[120];
  SPFELD    sf, Tab;
  float	    ZeitA, ZeitB;
  char	    Nachricht[NACHRICHT_MAX];
/*  SPIEL	    Sp; */
  bool	    f_z=false;
  char	    *idA, *idB;
  SFPOS	    Zuege[65];
  ZUGDAT    ZugDat[65];
  char      *datei=NULL;
  FILE      *fp;
  int	    AnzB[2], AnzW[2],
  	    ret1AB, ret2AB, 
	    sumA =0, sumB =0,
	    sumQA=0, sumQB=0,
	    anz1A, anz1B, 
	    anz2A, anz2B,
	    gewA, unent, gewB,
	    erg[3][3], sum[3][3];

  InitCrt();

  if (argc == 1) {

  Fehler: 
    Error("call: oauto [-out] [-single] [-num num] [-first num] [-outcome num] sfk/osp-file idA idB min1 min2\n");

  }

  argi = 1; ZufZugAnz = 0;

  f_aus = false;

  if (argv[argi] && !strcmp(argv[argi], "-out")) { f_aus = true; argi++; } 

  if (argv[argi] && !strcmp(argv[argi], "-single")) { f_single = true; argi++; } 

  if (argv[argi] && !strcmp(argv[argi], "-num")) { 

    argi++;

    if (!argv[argi] || sscanf(argv[argi], "%d", &spielanz) != 1 ||
	spielanz < 1) goto Fehler;

    argi++;

  } else spielanz = 10000000;


  if (argv[argi] && !strcmp(argv[argi], "-first")) { 

    argi++;

    if (!argv[argi] || sscanf(argv[argi], "%d", &firstnum) != 1 ||
	firstnum < 1) goto Fehler;

    argi++;

  } else firstnum = 1;


  if (argv[argi] && !strcmp(argv[argi], "-outcome")) { 

    argi++;

    if (!argv[argi] || sscanf(argv[argi], "%d", &outcome) != 1 ||
	outcome < 20) goto Fehler;

    argi++;

  } else outcome = 65;




  if (!argv[argi]) Error("sfk-file?");
  datei = argv[argi];

  argi++;    


  if (!argv[argi] || strlen(argv[argi]) > LAENGE-20)
    Error("idA nicht da oder zu lang");

  idA = argv[argi++];

  if (!argv[argi] || strlen(argv[argi]) > LAENGE-20)
    Error("idB nicht da oder zu lang");

  idB = argv[argi++];

  if (!argv[argi] || sscanf(argv[argi], "%f", &ZeitA) != 1)
    Error("timeA nicht da oder falsch");

  argi++;

  if (!argv[argi] || sscanf(argv[argi], "%f", &ZeitB) != 1)
    Error("timeB nicht da oder falsch");

  argi++;


  if (ZeitA <= 0 || ZeitB <= 0) Error("Zeit?");


  if (argv[argi]) goto Fehler;


  ZeitA *= 60; ZeitB *= 60;


  sprintf(ZU_1,  "%s"TO_PRAEFIX,   idA);
  sprintf(VON_1, "%s"FROM_PRAEFIX, idA);

  sprintf(ZU_2,  "%s"TO_PRAEFIX,   idB);
  sprintf(VON_2, "%s"FROM_PRAEFIX, idB);


  if (strlen(datei) < 4) Error("filename too short");

  is_sfk = !strcmp(&datei[strlen(datei)-4], ".sfk");


  fp = fopen(datei, "r");
  if (!fp) Error("can't open sfk/osp-file");

  FOR (i, 3)
    FOR (j, 3)
      erg[i][j] = sum[i][j] = 0;

  spielnr = 0;

  for (k=0; k < spielanz; k+= (f_single ? 1 : 2)) {

    if (is_sfk) {

      if (!fSfRead(fp, &sf, 1)) break;

      Partei = BLACK;

      StartNr = 1;
      Spiel[StartNr].Sf    = sf;
      Spiel[StartNr].AmZug = Partei;

    } else {


      if (fTabEin(fp, &sf)) break;

      if ((StartNr=TabToInfo(&sf, Spiel)) < 1) Error("game corrupt");

#if 0
      cout << StartNr << endl;

      if (outcome < 65) {
	if (StartNr > outcome-3) StartNr = outcome - 3;
      }

      cout << StartNr << endl;
#endif
 
      Partei = Spiel[StartNr].AmZug;
      sf     = Spiel[StartNr].Sf;

    }


    Beginner = (MyRand() & 64) != 0;	/* true => Program A */

    printf("begin: %d\n", Beginner);


    if (k+1 >= firstnum) {



    FOR (j, (f_single ? 1 : 2)) {

#if !TEST

/* beide Spieler in Wartestellung     */

      FOREVER {

        FOR (i1, 5) { if (SyncSendBREAK(ZU_1)) break; else SLEEP(3); } 

#if SEND_CLEAR
        FOR (i1, 5) { if (SyncSendCLEAR(ZU_1)) break; else SLEEP(3); } 
#endif

        FOR (j1, 5) { if (SyncSendBREAK(ZU_2)) break; else SLEEP(3); } 

#if SEND_CLEAR
        FOR (j1, 5) { if (SyncSendCLEAR(ZU_2)) break; else SLEEP(3); } 
#endif

        if (i1 < 5 && j1 < 5) break;
      }

/* letzte Nachrichten von Spielern verschlucken */
 
      Empf(VON_1, Nachricht); 
      Empf(VON_2, Nachricht); 

#endif


#ifdef AMIGA
      Chk_Abort();
#endif

      printf("\nGame %d ", spielnr+1);

      if (Beginner ^ j)

        EndNr = AutoSpiel(&sf, Partei, StartNr, Spiel, ZU_1, VON_1, ZeitA,
		       	      		    ZU_2, VON_2, ZeitB, outcome);
      else

        EndNr = AutoSpiel(&sf, Partei, StartNr, Spiel, ZU_2, VON_2, ZeitB,
			       		    ZU_1, VON_1, ZeitA, outcome);
      InfoToTab(Spiel, EndNr, &Tab);
      Tab.Marke = BLACK;

      if (Beginner ^ j) printf("\n%s - %s\n", idA, idB);
      else	        printf("\n%s - %s\n", idB, idA);

      fTabAus(stdout, &Tab);

      AnzB[j] = SfAnzBLACK(&Spiel[EndNr].Sf);
      AnzW[j] = SfAnzWHITE(&Spiel[EndNr].Sf); 

      printf("result= %d : %d\n\n", AnzB[j], AnzW[j]);

      spielnr++;

    }


    anz1A = Beginner ? AnzB[0]:AnzW[0]; 
    anz1B = Beginner ? AnzW[0]:AnzB[0]; 
    anz2A = Beginner ? AnzW[1]:AnzB[1]; 
    anz2B = Beginner ? AnzB[1]:AnzW[1]; 

    ret1AB = sgn(anz1A-anz1B);
    ret2AB = sgn(anz2A-anz2B);



    erg[ret1AB+1][ret2AB+1]++;

    sum[ret1AB+1][ret2AB+1] += anz1A + anz2A - anz1B - anz2B;


    sumA += anz1A + anz2A; sumQA += anz1A*anz1A + anz2A*anz2A;
    sumB += anz1B + anz2B; sumQB += anz1B*anz1B + anz2A*anz2B;

    printf("\nstatistics in view of program A:\n\n");
    printf("  +------L------+------D------+------W------+\n");

    FOR (i, 3) {

      char s[4] = "LDW";

      printf("%c |", s[i]); 

      FOR (j, 3) {
 
        if (erg[i][j]) 
	  printf("%4d (%+5.1f) |", erg[i][j], ((REAL)sum[i][j])/(2*erg[i][j])); 
	else
	  printf("     ---     |");

      }

      printf("\n");
    }


    printf("\n  +------L------+------D------+------W------+\n");

    FOR (i, 3) {

      char s[4] = "LDW";

      printf("%c |", s[i]); 

      FOR (j, 3) {
 
	if (j < i) {
          if (erg[i][j] + erg[j][i]) 
	    printf("%4d (%+5.1f) |", erg[i][j]+ erg[j][i],
		   ((REAL)(sum[i][j]+sum[j][i]))/(2*(erg[i][j]+erg[j][i]))); 
	  else
	    printf("     ---     |");

	} else if (i == j) {

          if (erg[i][j]) 
	    printf("%4d (%+5.1f) |", erg[i][j],((REAL)sum[i][j])/(2*erg[i][j])); 
	  else
	    printf("     ---     |");


	} else printf("      *      |");

      }

      printf("\n");
    }


    gewA  = 2* erg[W][W] +    erg[W][D] +    erg[W][L] + erg[D][W] + erg[L][W];
    unent =    erg[W][D] + 2* erg[D][D] +    erg[L][D] + erg[D][W] + erg[D][L];
    gewB  =    erg[L][W] +    erg[L][D] + 2* erg[L][L] + erg[D][L] + erg[W][L];


    printf("\n%d (%.2f) - %d - %d (%.2f)   (%.1f%%)\n", 

      gewA, ((REAL)sumA)/spielnr, 
      unent, 
      gewB, ((REAL)sumB)/spielnr,

      100.0*(gewA + unent*0.5)/(gewA + unent + gewB)
    );


/* normalized: without games pairs with same results */

    gewA  -= erg[W][L] + erg[L][W];
    gewB  -= erg[W][L] + erg[L][W];
    unent -= 2*erg[D][D];


    if (gewA + unent + gewB) {

      printf("\n%d - %d - %d   (%.1f%%)\n", 

        gewA, unent, gewB, 

        100.0*(gewA + unent*0.5)/(gewA + unent + gewB)
      );
    }

    fflush(stdout);
  }

  }  

  _abort();

  return 0;
}


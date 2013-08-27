// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* einfache Version: Textein- und Ausgabe, 1.93 */

#define BEGINNER     BLACK

#ifndef PRECOMP
#include "main.h"
#endif

#include <ctype.h>
#include "sys.h"
#include "sboard.h"
#include "playgm.h"
#include "int.h"
#include "filecom.h"
#include "goodies.h"


#define GAME	"otool.osp"


int	VERB = 1;		/* Ausgabe bei Zugermittlung */

extern	int	Enable_Abort;

bool	ZugCtrlC;
jmp_buf ZugCtrlCEnv;

char	TO_1  [100], TO_2  [100];
char	FROM_1[100], FROM_2[100];


SPIELINFO Spiel[120];

int ZugIndex;
int MaxIndex;



PROPINFO defpropinfo = {
  10*60, 2*60,			/* Zeiten		*/
  SP_MENSCH, SP_COMP1,		/* SpielerB, SpielerW	*/

  BLACK_AM_ZUG,			/* Am Zug bei Eingabe	*/
  SETZE_BLACK,
  false,			/* MoeglAnzeig		*/
  false				/* Beep			*/
};


PROPINFO propinfo;


void _abort(void)
{
  if (ZugCtrlC) LONGJMP(ZugCtrlCEnv);

#ifdef AMIGA
  Enable_Abort = 0;
#endif

/* Spieler killen */

  if (!SyncSendEXIT(TO_1)) { SLEEP(10); SyncSendEXIT(TO_1); }
  if (!SyncSendEXIT(TO_2)) { SLEEP(10); SyncSendEXIT(TO_2); }

  SLEEP(10);

  KillChannel(TO_1);   KillChannel(TO_2);
  KillChannel(FROM_1); KillChannel(FROM_2);

  exit(1);
}


#define INPUT_LEN 50


/* Spielertyp einlesen */


SP_TYP InputSpieler(char *name, SP_TYP alt)
{
  char s[INPUT_LEN+1];

  s[0] = 0;

  do {

    printf("%-7s    Spieler <M>ensch, Comp<1>,Comp<2>  (%c): ", 
		name, alt == SP_MENSCH ? 'M' : (alt == SP_COMP1 ? '1' : '2'));

    fflush(stdout);

    fgets(s, INPUT_LEN, stdin);
    if (strlen(s) == 1) return alt;

  } while (toupper(s[0]) != 'M' && s[0] != '1' && s[0] != '2');


  if (toupper(s[0]) == 'M') return SP_MENSCH;
  if (toupper(s[0]) == '1') return SP_COMP1;
  return SP_COMP2;
}



/* Zeit in Minuten einlesen */

int InputZeit(char *name, int alt)
{
  char s[INPUT_LEN+1];
  int d;


  s[0] = 0;

  do {

    printf("%-7s    Zeit in Minuten                  (%3d): ", name, alt/60); fflush(stdout);
  
    fgets(s, INPUT_LEN, stdin);

    if (strlen(s) == 1) return alt;
    if (!sscanf(s, "%d", &d)) d = 0;

  } while (d < 2 || d > 1000);

  return d * 60;
}




SFPOS InputZug(SPFELD *psf, PARTEI Partei)
{
  char cx, cy, s[INPUT_LEN+1];
  int  Zug;
  SPFELD sf0;


  FOREVER {

    printf("Zug (%2d): ", SfAnz(psf)-3); fflush(stdout); 
    fgets(s, INPUT_LEN, stdin);

    cx = cy = 0;

    sscanf(s, "%c%c", &cx, &cy);

    if (cx >= 'a' && cx <= 'h' && cy >= '1' && cy <= '8') {

      Zug = (cy - '1' + 1) * 10 + cx - 'a' + 1;

      sf0 = *psf;

      if (SfSetzen(&sf0, Partei, Zug)) return Zug;
    }

    printf("*** Eingabefehler!\n");
  }
}



void TextProps(PROPINFO *props)
{
  props->SpielerB  = InputSpieler("Schwarz", props->SpielerB);
  props->RestzeitB = InputZeit("Schwarz", props->RestzeitB);

  printf("\n"); 

  props->SpielerW  = InputSpieler("Weiß", props->SpielerW);
  props->RestzeitW = InputZeit("Weiß", props->RestzeitW);
}
 


void ParteiAus(SPIELINFO *psi, PARTEI Partei, PARTEI AmZug)
{
  int n;


  if (AmZug == Partei) printf("*"); else printf(" ");

  if (Partei == BLACK)	printf("Schwarz (%c):  %2d Steine von %d,",
			  psi->SpielerBLACK == SP_MENSCH ? 'M' :
			  (psi->SpielerBLACK == SP_COMP1  ? '1' : '2'),
			  SfAnzBLACK(&psi->Sf), SfAnz(&psi->Sf));
  else 			printf("Weiß    (%c):  %2d Steine von %d,",
			  psi->SpielerWHITE == SP_MENSCH ? 'M' :
			  (psi->SpielerWHITE == SP_COMP1  ? '1' : '2'),
			  SfAnzWHITE(&psi->Sf), SfAnz(&psi->Sf));


  printf(" Restzeit: ");

  if (Partei == BLACK) n = psi->RestBLACK; else n = psi->RestWHITE;
  
  if (n < 0) { printf("-"); n = -n; }

  printf("%d:%.2d.%.2d\n", n / 3600, (n % 3600) / 60, (n % 3600) % 60 );
}



void Ausgabe(SPIELINFO *psi)
{
  SfAus(&psi->Sf, psi->AmZug, 0);

  ParteiAus(psi, BLACK, psi->AmZug);
  ParteiAus(psi, WHITE, psi->AmZug);

  printf("\n");
}


void TextSpiel(void)
{
  int n, z, Zug, ZugAnz, Dauer;
  char *p, *TO, *FROM;
  SFPOS Zuege[65], letzterZug;
  char Nachricht[NACHRICHT_MAX], s[INPUT_LEN+1];
  ZEIT Startzeit, Endzeit;
  SP_TYP typ;
  SPIELINFO *psi;


  FOREVER {

    Zeit(&Startzeit);

    psi = &Spiel[ZugIndex];


    Ausgabe(psi);

    SpielSpeichern(MaxIndex, GAME);

    if (SfMoeglZuege(&psi->Sf, psi->AmZug,	   Zuege) == 0 && 
        SfMoeglZuege(&psi->Sf, GEGNER(psi->AmZug), Zuege) == 0) {

      SPFELD tab;
      FILE *fp;


      printf("\n\n Endergebnis:  Schwarz: %d  - %d :Weiß\n\n", 
		SfAnzBLACK(&psi->Sf), SfAnzWHITE(&psi->Sf)); 

      InfoToTab(Spiel, MaxIndex, &tab);

      if ((fp=fopen("otool.log", "a"))) {

        fTabAus(fp, &tab);

        fclose(fp);
      }

      return;
    }
 

    if (psi->AmZug == BLACK) {

      typ = psi->SpielerBLACK;
      z = psi->RestBLACK;

    } else {

      typ = psi->SpielerWHITE;
      z = psi->RestWHITE;

    }

    if (ZugIndex > 1) letzterZug = Spiel[ZugIndex-1].Zug; 
    else	      letzterZug = ZUG_PASSEN;

    ZugAnz = SfMoeglZuege(&psi->Sf, psi->AmZug, Zuege);

    
    if (typ == SP_MENSCH) {

      if (!ZugAnz) {

	printf("Passen! <ret>"); fflush(stdout); fgets(s, INPUT_LEN, stdin);

        Zug = ZUG_PASSEN;


      } else {

	Zug = InputZug(&psi->Sf, psi->AmZug);

      }


    } else {

      if (typ == SP_COMP1) { TO = TO_1; FROM = FROM_1; } 
      else 		   { TO = TO_2; FROM = FROM_2; }

      SyncSendBOARD(TO, psi->AmZug, z, letzterZug, &psi->Sf);

      while (!Empf(FROM, Nachricht)) SLEEP(5);	/* auf Zug warten */

printf("\a"); fflush(stdout);

      if (!ParseNumber(Nachricht, &n, &p) || n != SIG_MOVE ||
          !ParseNumber(p, &Zug, &p)       || (!ZUG(Zug) && Zug != ZUG_PASSEN)) {

	printf("%d %d %d\n", Zug, !ZUG(Zug), Zug != ZUG_PASSEN);
	printf("%s ", Nachricht); Error("Fehler in MOVE-Nachricht"); 
      }

      printf("Zug (%2d): ", SfAnz(&Spiel[ZugIndex].Sf)-3);
      KoorAus(Zug); printf("\n");
    }

    Spiel[ZugIndex+1] = Spiel[ZugIndex];

    if (Zug != ZUG_PASSEN) SfSetzen(&Spiel[ZugIndex+1].Sf, psi->AmZug, Zug);

    Zeit(&Endzeit);

    psi->Zug = Zug;

    Spiel[ZugIndex+1].AmZug = GEGNER(psi->AmZug);

    Dauer = ZeitDiff(&Endzeit, &Startzeit);

    if (psi->AmZug == BLACK) Spiel[ZugIndex+1].RestBLACK -= Dauer; 
    else		     Spiel[ZugIndex+1].RestWHITE -= Dauer;

    ZugIndex++;
    if (ZugIndex > MaxIndex) MaxIndex = ZugIndex; 
  }
}


void SpielSpeichern(int MaxIndex, char *name)
{
  SPFELD tab;
  FILE *fp;


  if (!(fp=fopen(name, "w"))) printf("*** '%s' nicht zu öffnen\n", name);
  else {
    InfoToTab(Spiel, MaxIndex, &tab);
    fTabAus(fp, &tab);

    fprintf(fp, "Schwarz: %d min\n", (int)(Spiel[MaxIndex].RestBLACK/60));
    fprintf(fp, "Weiß:    %d min\n", (int)(Spiel[MaxIndex].RestWHITE/60));
 
    fclose(fp);
  }
}



void main(int argc, char **argv)
{
  char Nachricht[NACHRICHT_MAX];
  char s[INPUT_LEN+1];


  propinfo = defpropinfo;

  if (argc != 3) Error("Aufruf otext Id1 Id2\n");

  if (strlen(argv[1]) + strlen(TO_PRAEFIX)   > 90 ||
      strlen(argv[2]) + strlen(TO_PRAEFIX)   > 90 ||
      strlen(argv[1]) + strlen(FROM_PRAEFIX) > 90 ||
      strlen(argv[2]) + strlen(FROM_PRAEFIX) > 90)
    Error("Id zu lang");

  sprintf(TO_1, TO_PRAEFIX"%s", argv[1]);
  sprintf(FROM_1, FROM_PRAEFIX"%s", argv[1]);

  sprintf(TO_2, TO_PRAEFIX"%s", argv[2]);
  sprintf(FROM_2, FROM_PRAEFIX"%s", argv[2]);

  InitCrt();				/* ctrl-c verbiegen */

  printf("\n\nOTOOL 0.93  (c) Michael Buro ["__DATE__"]\n\n");

  FOREVER {

    ZugCtrlC = false;		

/* TextProps(&propinfo); */

    SfGrund(&Spiel[1].Sf); 
    Spiel[1].AmZug        = BLACK; 
    Spiel[1].RestBLACK    = propinfo.RestzeitB;
    Spiel[1].RestWHITE    = propinfo.RestzeitW;
    Spiel[1].SpielerBLACK = propinfo.SpielerB;
    Spiel[1].SpielerWHITE = propinfo.SpielerW;

    ZugIndex = MaxIndex = 1;

    SETJMP(ZugCtrlCEnv);

    ZugCtrlC = true;		

    SyncSendBREAK(TO_1);     SyncSendBREAK(TO_2);
    Empf(FROM_1, Nachricht); Empf(FROM_2, Nachricht);

    printf("\n\n");


Menu:

    Ausgabe(&Spiel[ZugIndex]);

    printf("\n\n(W)eiter (N)eu (E)nde (Z)eit (T)abelle (S)peichern (L)aden ");

    if (ZugIndex > 1)        printf("(-) ");
    if (ZugIndex < MaxIndex) printf("(+) ");

    fflush(stdout); 

    ZugCtrlC = false;		

    fgets(s, INPUT_LEN, stdin);

    ZugCtrlC = true;		

    if      (toupper(s[0]) == 'E') { ZugCtrlC = false; break; }
    else if (toupper(s[0]) == 'N') continue;
    else if (toupper(s[0]) == 'W') { TextSpiel(); goto Menu; }
    else if (toupper(s[0]) == 'Z') { 
   
       propinfo.RestzeitB = Spiel[ZugIndex].RestBLACK;
       propinfo.RestzeitW = Spiel[ZugIndex].RestWHITE;
       propinfo.SpielerB  = Spiel[ZugIndex].SpielerBLACK;
       propinfo.SpielerW  = Spiel[ZugIndex].SpielerWHITE;

      TextProps(&propinfo);

      Spiel[ZugIndex].RestBLACK    = propinfo.RestzeitB;
      Spiel[ZugIndex].RestWHITE    = propinfo.RestzeitW;
      Spiel[ZugIndex].SpielerBLACK = propinfo.SpielerB;
      Spiel[ZugIndex].SpielerWHITE = propinfo.SpielerW;
      goto Menu;

    } else if (toupper(s[0]) == 'L') { 

      FILE *fp;

      printf("\n\nDateiname (ohne .osp): "); fflush(stdout);
 
      fgets(s, INPUT_LEN-5, stdin);

      s[strlen(s)-1] = 0;
      strcat(s, ".osp");
      if (!(fp=fopen(s, "r"))) printf("*** '%s' nicht zu öffnen\n", s);
      else {

	SPFELD tab;

        if (fTabEin(fp, &tab)) printf("*** Fehler in Tabelle\n");
	else {
	  int j = TabToInfo(&tab, Spiel);

	  if (j < 1) printf("*** Fehler in Tabelle\n");
	  else { 

	    int i;

	    ZugIndex = MaxIndex = j;

            for (i=1; i <= MaxIndex; i++) {
              Spiel[i].RestBLACK    = propinfo.RestzeitB;
              Spiel[i].RestWHITE    = propinfo.RestzeitW;
              Spiel[i].SpielerBLACK = propinfo.SpielerB;
              Spiel[i].SpielerWHITE = propinfo.SpielerW;
	    }

          }
	}
        fclose(fp);
      }


      goto Menu;


    } else if (toupper(s[0]) == 'S') {

      printf("Dateiname (ohne .osp): "); fflush(stdout);
      fgets(s, INPUT_LEN-5, stdin); s[strlen(s)-1] = 0;
      strcat(s, ".osp");
      SpielSpeichern(MaxIndex, s);

    } else if (toupper(s[0]) == 'T') { 

      SPFELD tab;

      InfoToTab(Spiel, MaxIndex, &tab);
      TabAus(&tab); 

      goto Menu;

    } else if (toupper(s[0]) == '-' && ZugIndex > 1) {

      ZugIndex--;
      goto Menu;

    } else if (toupper(s[0]) == '+' && ZugIndex < MaxIndex) {

      ZugIndex++;
      goto Menu;


    } else { printf(">>> Eingabefehler\n"); goto Menu; }
  }

  _abort();
}

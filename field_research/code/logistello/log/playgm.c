// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Ein Othello-Spiel durchführen, 26.7.91 - */

#ifndef PRECOMP
#include "main.h"
#endif

#include "playgm.h"
#include "sboard.h"
#include "lib.h"
#include "int.h"
#include "filecom.h"
#include "goodies.h"
#include "sys.h"


extern PROPINFO propinfo;	/* aus xm.c */

extern bool f_old;


void Auto_WARTEN(void);
void Auto_SPIEL1(void);
void Auto_SPIEL2(void);
void Auto_SPIEL3(void);


bool	Break1 = false, Break2 = false;


enum Zustand { Q_WARTEN, Q_SPIELA, Q_SPIELC, Q_EINGABE };


bool	Quit		= false;
bool	SpModus		= false;

bool	EingabeDa	= false;
bool	EinstellungenDa	= false;

int	SteinAnz, ZugAnz, Pos;
SFPOS	Zuege[64], last;
bool	RegEnde;
PARTEI	Partei;
SP_TYP	Typ;
REAL	Rest;
ZEIT	StartZeit, AktZeit;  


enum Zustand	Q;

int		ZugNr, MaxNr, MoeglZu;
SPIELINFO	Spiel[120];
SPIELER		SpBLACK, SpWHITE;

SPIELINFO	StartInfo;	/* Startspielfeld + Am Zug */
SPFELD		SfInput;


#define B BLACK
#define W WHITE

#ifdef xxx

SPFELD Sfx = {

0,0,B,W,W,W,W,0,
0,0,W,W,W,B,0,W,
0,W,W,W,W,W,W,W,
W,W,B,W,W,B,W,W,
0,W,W,B,W,B,B,W,
B,W,W,W,B,B,B,B,
W,0,B,W,B,B,B,0,
0,0,B,B,B,B,0,B
};

SPFELD Sfx1 = {

0,W,W,W,W,W,W,0,
B,B,B,B,B,B,0,W,
0,B,B,W,W,W,W,W,
W,W,B,B,W,B,W,W,
W,W,W,B,B,B,W,W,
W,W,W,W,B,B,B,B,
W,0,B,W,B,B,B,B,
0,0,B,B,B,B,W,B
};

SPFELD Sfx2 = {

0,W,W,W,W,W,W,0,
B,B,B,B,B,B,0,W,
0,B,B,W,W,W,W,W,
W,W,B,B,W,B,W,W,
W,W,W,B,B,B,W,W,
W,W,W,W,B,B,B,B,
W,W,W,W,B,B,B,B,
0,0,B,B,B,B,W,B
};

/*

B,B,B,0,0,W,0,W,
B,0,B,B,B,B,0,W,
0,W,B,B,B,W,B,W,
0,W,W,B,B,B,W,W,
0,B,W,W,B,B,0,W,
W,B,W,B,B,W,B,W,
W,B,B,W,B,B,W,W,
W,B,0,0,0,0,0,W
};
*/
#endif


REAL Restzeit(PARTEI Partei)
{
  XGetProps(&propinfo);

  if (Partei == BLACK)	return propinfo.RestzeitB * 60.0; 
  else			return propinfo.RestzeitW * 60.0;
}



SP_TYP Spielertyp(PARTEI Partei)
{
  XGetProps(&propinfo);

  if (Partei == BLACK)	return propinfo.SpielerB;
  else			return propinfo.SpielerW;
}



bool MoeglStatus(void)
{
  XGetProps(&propinfo);

  return propinfo.MoeglAnzeig;
}


/* Restzeiten und Spielertypen aus Gadgets in Spiel[ZugNr] übernehmen */

void NeueWerte(void)
{
  MaxNr = ZugNr;

  Spiel[ZugNr].RestBLACK = Restzeit(BLACK);
  Spiel[ZugNr].RestWHITE = Restzeit(WHITE);

  Spiel[ZugNr].SpielerBLACK = Spielertyp(BLACK);
  Spiel[ZugNr].SpielerWHITE = Spielertyp(WHITE);

  SpielfeldAus(Spiel, ZugNr, MaxNr);
  StatistikAus(&Spiel[ZugNr]);

  ClearMeldung();
}



void NeuesSpiel(void)
{
  ZugNr = 1;

  Spiel[0].Zug = ZUG_UNBEKANNT;		/* wegen 2x passen = Spielende */
  Spiel[1] = StartInfo;

  NeueWerte();
}




/* CallBack Funktionen */


void CB_WEITER(void)
{
/*printf("CB_WEITER\n"); */

  Auto_WARTEN();	/* beide Spieler in Wartestellung     */

  RegEnde = false;

  Auto_SPIEL1();
}


void CB_NEU(void)
{
/*printf("CB_NEU\n");*/

  NeuesSpiel();

  CB_WEITER();
} 


void CB_STOP(void)
{
/*printf("CB_STOP\n");*/

  if (Q == Q_WARTEN) Quit = true;
  Auto_WARTEN();
} 


void CB_TABELLE(void)
{
/*printf("CB_TABELLE\n");*/

  TabelleAus(Spiel, MaxNr);

  Auto_WARTEN();
} 


void CB_ZURUECK(void)
{
/*printf("CB_ZURUECK\n");*/

  if (ZugNr > 1) ZugNr--;

  SpielfeldAus(Spiel, ZugNr, MaxNr);
  StatistikAus(&Spiel[ZugNr]);

  Auto_WARTEN();
}


void CB_VOR(void)
{
/*printf("CB_VOR\n");*/

  if (ZugNr < MaxNr) ZugNr++;

  SpielfeldAus(Spiel, ZugNr, MaxNr);
  StatistikAus(&Spiel[ZugNr]);

  Auto_WARTEN();
}



void CB_ZURUECK_ZUM(void)
{
  ZugNr = 1;

  SpielfeldAus(Spiel, ZugNr, MaxNr);
  StatistikAus(&Spiel[ZugNr]);

  Auto_WARTEN();
}


void CB_VOR_ZUM(void)
{
  ZugNr = MaxNr;

  SpielfeldAus(Spiel, ZugNr, MaxNr);
  StatistikAus(&Spiel[ZugNr]);

  Auto_WARTEN();
}



void CB_EINSTELLUNGEN_EIN(void)
{
/*printf("CB_EINSTELLUNGEN_EIN\n");*/

  XEinstellungenEin();
  EinstellungenDa = true;

  XSetEingabe(false);

  Auto_WARTEN();
}


void CB_EINSTELLUNGEN_AUS(void)
{
/*printf("CB_EINSTELLUNGEN_AUS\n");*/

  XEinstellungenAus();
  EinstellungenDa = false;

  XSetEingabe(true);
}


void CB_EINGABE_EIN(void)
{

/*printf("CB_EINGABE_EIN\n");*/

  XEingabeEin();

  if (!EingabeDa) {

    XSetEinstellungen(false);
    EingabeModus();

    SfInput = Spiel[ZugNr].Sf;

    XGetProps(&propinfo);

    if (Spiel[ZugNr].AmZug == WHITE) propinfo.AmZug = WHITE_AM_ZUG;
    else                             propinfo.AmZug = BLACK_AM_ZUG;

    XSetProps(&propinfo);

    GfxSfAus(&SfInput, BLACK, 0);
  }

  EingabeDa = true;

  Auto_WARTEN();

  Q = Q_EINGABE;
}


void CB_EINGABE_AUS(void)
{
/*printf("CB_EINGABE_AUS\n");*/

  XEingabeAus();
  EingabeDa = false;

  XSetEinstellungen(true);

  SpielfeldAus(Spiel, ZugNr, MaxNr);

  Auto_WARTEN();
}


void CB_MOEGL(void) 
{
  XGetProps(&propinfo);

  SpielfeldAus(Spiel, ZugNr, MaxNr);
}


void CB_WECHSELN(void) 
{ 
  int t;


  XGetProps(&propinfo);

  t = propinfo.RestzeitB; 
  propinfo.RestzeitB = propinfo.RestzeitW;
  propinfo.RestzeitW = t;

  t = propinfo.SpielerB; 
  propinfo.SpielerB = propinfo.SpielerW;
  propinfo.SpielerW = t;

  XSetProps(&propinfo);
}


void CB_EINST_SPEI(void) 
{
/*  printf("CB_SPEICHERN\n");*/

  XGetProps(&propinfo);
}  


void CB_EINST_BEN(void) 
{
/*  printf("CB_EINST_BEN\n");*/

  NeueWerte();

  CB_EINSTELLUNGEN_AUS();

  Auto_WARTEN();
}  


void CB_ZEIT(void) { Auto_WARTEN(); }  


void CB_SPIELERB(int Spieler)
{
  XGetProps(&propinfo);

  propinfo.SpielerB = Spieler;

  XSetProps(&propinfo);

  Auto_WARTEN();
}

  
void CB_SPIELERW(int Spieler)
{
  XGetProps(&propinfo);

  propinfo.SpielerW = Spieler;

  XSetProps(&propinfo);

  Auto_WARTEN();
}
  


void Auto_WARTEN(void)
{
  char Nachricht[NACHRICHT_MAX];
  int  i;


  MenuModus();
  SpModus = false;



/* beide Spieler in Wartestellung     */

  if (!Break1) { 
    FOR (i, 5) { if (SyncSendBREAK(TO_1)) break; else WARTEN(3); } 
    if (i >= 5) printf(">>> SP_COMP1 antwortet nicht\n");
    Break1 = true; 
  }

  if (!Break2) { 
    FOR (i, 5) { if (SyncSendBREAK(TO_2)) break; else WARTEN(3); } 
    if (i >= 5) printf(">>> SP_COMP2 antwortet nicht\n");
    Break2 = true; 
  }


/* letzte Nachrichten von Spielern verschlucken */

  Empf(FROM_1, Nachricht); 
  Empf(FROM_2, Nachricht); 


  Q = Q_WARTEN;
}


int SendInfo(char *channel, int player, int time, SPIELINFO *pInfo, int MoveNr)
{
  SPFELD sf, tab;
  GAME   Game;
  

  SfGrund(&sf);

  if (!f_old && SfGleich(&sf, &Spiel[1].Sf)) {

/* new version & normal game => send moves */

    InfoToTab(pInfo, MoveNr, &tab);
    Tab2Game(&tab, &Game);

    return SyncSendGAME(channel, player, time, &Game, 1);

  } else {

/* send board */

    return SyncSendBOARD(channel, player, time,
	pInfo[MoveNr-1].Zug, &pInfo[MoveNr].Sf);
  }
}



void Auto_SPIEL1(void)
{
  int i;


  SpielModus();
  SpModus = true;

  last   = Spiel[ZugNr-1].Zug;

  Partei = Spiel[ZugNr].AmZug;

  if (Partei == BLACK) { 

    Rest = Spiel[ZugNr].RestBLACK;
    Typ  = Spiel[ZugNr].SpielerBLACK;

  } else {

    Rest = Spiel[ZugNr].RestWHITE;
    Typ  = Spiel[ZugNr].SpielerWHITE;
  }


/* Spielfeld senden */


  SteinAnz = SfAnz(&Spiel[ZugNr].Sf);

  if (SteinAnz < 64) {

    if (Typ == SP_COMP1) {

      FOR (i, 3) 
	if (SendInfo(TO_1, Partei, Rest, Spiel, ZugNr)) break;
        else BusyWait(5);

      if (i >= 3) { printf(">>> SP_COMP1?\n"); Auto_WARTEN(); return; }

      Break1 = false;

    } else if (Typ == SP_COMP2) { 

      FOR (i, 3)
 	if (SendInfo(TO_2, Partei, Rest, Spiel, ZugNr)) break;
        else BusyWait(5);

      if (i >= 3) { printf(">>> SP_COMP2?\n"); Auto_WARTEN(); return; }

      Break2 = false;
    }
  }

  Zeit(&StartZeit);

  SpielfeldAus(Spiel, ZugNr, MaxNr);
  StatistikAus(&Spiel[ZugNr]);

  if (SteinAnz == 64) { RegEnde = true; Auto_SPIEL3(); return; }


/* Zug holen */

  if (Typ == SP_MENSCH) {		/* von Terminal */

    ZugAnz = SfMoeglZuege(&Spiel[ZugNr].Sf, Partei, Zuege);

    if (!ZugAnz) {

      XmsgBlaBla("Passen");

      XPassenNotice();
      Pos = ZUG_PASSEN;
      Auto_SPIEL2();
      return;

    }


    Q = Q_SPIELA;

  } else {				/* von Programm */

    Q = Q_SPIELC;
  }
}


void CB_ZUG(SFPOS ZUGret)
{
  int i;

/*printf("CB_ZUG %d\n", ZUGret);*/



  if (Q == Q_SPIELA) {

    char s[100];

    if (ZUGret != ZUG_PASSEN && !ZUG(ZUGret)) Error("Zug erwartet!");

    ZugAnz = SfMoeglZuege(&Spiel[ZugNr].Sf, Partei, Zuege);

    FOR (i, ZugAnz) if (Zuege[i] == ZUGret) break;

    sKoorAus(s, ZUGret); strcat(s, ": UNGÜLTIGER ZUG");

    if (ZugAnz && i >= ZugAnz) XmsgBlaBla(s);
    else     		       { Pos = ZUGret; Auto_SPIEL2(); }

  } else if (Q == Q_EINGABE) {

    XGetProps(&propinfo);

    switch (propinfo.SetzModus) {
  
      case SETZE_BLACK:		SfInput.p[ZUGret] = BLACK; break;
      case SETZE_WHITE:		SfInput.p[ZUGret] = WHITE; break;
      case SETZE_LEER:		SfInput.p[ZUGret] = LEER;  break;
      case SETZE_WECHSEL:	
		if      (SfInput.p[ZUGret] == BLACK) SfInput.p[ZUGret] = WHITE;
		else if (SfInput.p[ZUGret] == WHITE) SfInput.p[ZUGret] = LEER;
		else 				     SfInput.p[ZUGret] = BLACK;
	        break;

      default: Error("unbekannter Code bei CB_ZUG");
    }

    GfxSfAus(&SfInput, BLACK, 0);
  }
}



void CB_NACHRICHT(char *Nachricht)
{
/*  printf("CB_NACHRICHT %s\n", Nachricht);*/

  if (Q == Q_SPIELC) {

    char *p, s[100];
    int  i, n;


    if (!ParseNumber(Nachricht, &n, &p) || n != SIG_MOVE ||
        !ParseNumber(p, &Pos, &p)       || (!ZUG(Pos) && Pos != ZUG_PASSEN)) {

printf("%d %d %d\n", Pos, !ZUG(Pos), Pos != ZUG_PASSEN);
printf("%s\n", Nachricht); 
      sprintf(s, "FEHLER IN ZUG-NACHRICHT");
      goto ZugFehler;
    }

    XGetProps(&propinfo);
    if (propinfo.Beep) XBeep();


    ZugAnz = SfMoeglZuege(&Spiel[ZugNr].Sf, Partei, Zuege);

    FOR (i, ZugAnz) if (Zuege[i] == Pos) break;

    sKoorAus(s, Pos); strcat(s, ": UNGÜLTIGER ZUG");

    if ((!ZugAnz && Pos != ZUG_PASSEN) || (ZugAnz && i >= ZugAnz)) {

ZugFehler:

      XmsgBlaBla(s); XBeep(); XBeep(); XBeep(); Auto_WARTEN();
    
    } else Auto_SPIEL2();
  }
}



void Auto_SPIEL2(void)
{
  ZugAnz = SfMoeglZuege(&Spiel[ZugNr].Sf, Partei, Zuege);

  Spiel[ZugNr+1] = Spiel[ZugNr];

  ClearMeldung(); 

  if (ZugAnz) {
    
    if (!SfSetzen(&Spiel[ZugNr+1].Sf, Partei, Pos)) Error("SfSet1");

    ZugAus(Pos, Partei);

    last = Pos;

  } else {

    ZugAus(ZUG_PASSEN, Partei);				/* passen */

/* 2x gepaßt -> Ende */

    if (last == ZUG_PASSEN) { RegEnde = true; Auto_SPIEL3(); return; } 

    last = ZUG_PASSEN;					/* kein Zug möglich */
  }

  Spiel[ZugNr].Zug     = last;
  Spiel[ZugNr+1].AmZug = GEGNER(Partei);
      

  Zeit(&AktZeit);

  Rest -= ZeitDiff(&AktZeit, &StartZeit);

  if (Partei == BLACK) Spiel[ZugNr+1].RestBLACK = Rest;
  else		       Spiel[ZugNr+1].RestWHITE = Rest;

  if (Rest < 0) MeldeZeitUeber();

  ZugNr++;
  MaxNr = ZugNr;

  SpielfeldAus(Spiel, ZugNr, MaxNr);

  XFlushGfx();

  Auto_SPIEL1();
}



void Auto_SPIEL3(void)
{
  FILE *fp;
  SPFELD sf;
/*  SPIEL  Sp; */


  if (RegEnde) {

    ErgebnisAus(&Spiel[ZugNr]);

    InfoToTab(Spiel, ZugNr, &sf);

    if ((fp=fopen("otool.log", "a"))) {

      fTabAus(fp, &sf);

      fclose(fp);
    }


  }

  Auto_WARTEN();
}


void CB_TIMER_LONG(void)
{
  REAL R, RestB, RestW;
  ZEIT AZeit;


  if (SpModus) {

    Zeit(&AZeit);
    R = ZeitDiff(&AZeit, &StartZeit);

    RestB = Spiel[ZugNr].RestBLACK;
    RestW = Spiel[ZugNr].RestWHITE;
     
    if (Partei == BLACK) RestB -= R; else RestW -= R;

    XmsgZeiten(RestB, RestW);
  }
}



void CB_TIMER(void)
{
  char Nachricht[NACHRICHT_MAX];


  if (Empf(FROM_1, Nachricht)) CB_NACHRICHT(Nachricht);
  if (Empf(FROM_2, Nachricht)) CB_NACHRICHT(Nachricht);
}




void CB_DREHEN(void)
{
  SfDrehen(&SfInput);

  GfxSfAus(&SfInput, BLACK, 0);
}


void CB_SPIEGELN(void)
{
  SfxSpiegeln(&SfInput);
  GfxSfAus(&SfInput, BLACK, 0);
}


void CB_GRUND(void)
{
  SfGrund(&SfInput);

  XGetProps(&propinfo);
  propinfo.AmZug = BLACK_AM_ZUG;
  XSetProps(&propinfo);

  GfxSfAus(&SfInput, BLACK, 0);
}


void CB_FUELLEN(void)
{
  XGetProps(&propinfo);

  switch (propinfo.SetzModus) {

    case SETZE_BLACK:	SfSet(&SfInput, BLACK); break;
    case SETZE_WHITE:	SfSet(&SfInput, WHITE); break;
    case SETZE_LEER:	SfSet(&SfInput, LEER);	break;
    case SETZE_WECHSEL:	SfInvert(&SfInput);	break;

    default: Error("unbekannter Code bei CB_FUELLEN");
  }
    
  GfxSfAus(&SfInput, BLACK, 0);
}


void CB_EINGABE_ABBRUCH(void) { CB_EINGABE_AUS(); }



void CB_EINGABE_OK(void)
{
  XGetProps(&propinfo);

  StartInfo.Sf    = SfInput;

  if (propinfo.AmZug == BLACK_AM_ZUG) StartInfo.AmZug = BLACK; 
  else			   	      StartInfo.AmZug = WHITE;

  NeuesSpiel();

  CB_EINGABE_AUS();
}



void CB_AMZUG(int code)
{
  XGetProps(&propinfo);

  propinfo.AmZug = code;

  XSetProps(&propinfo);
}


void CB_SETZMODUS(int code)
{
  XGetProps(&propinfo);

printf("->%d\n", code);

  propinfo.SetzModus = code;

  XSetProps(&propinfo);
}


void CB_ZUGFOLGE(void) {}

void CB_BENUTZEN(void) {}

void CB_ENDE(void)
{
  XDestroy();
  _abort();
}


void CB_LADEN_EIN(void)
{
/*printf("CB_LADEN_EIN\n");*/

  XLadenEin();

  Auto_WARTEN();
}


void CB_LADEN_AUS(void)
{
/*printf("CB_LADEN_AUS\n");*/

  XLadenAus();
}


void CB_SPEICHERN_EIN(void)
{
printf("CB_SPEICHERN_EIN\n");

  XSpeichernEin(); 

  Auto_WARTEN();
}


void CB_SPEICHERN_AUS(void)
{
printf("CB_SPEICHERN_AUS\n");

  XSpeichernAus();
}


void CB_SPIEL_LADEN(char *name)
{
  FILE *fp;


/*  printf("Laden: \"%s\"\n", name); */

  if (!(fp=fopen(name, "r"))) {		/* Datei nicht da? */

    fclose(fp);
    XNichtDaNotice();
    return;

  } else fclose(fp);


  if ((MaxNr = LoadGame(name, Spiel)) <= 0) XLesefehlerNotice();

  else {

    CB_LADEN_AUS();

    for (ZugNr=1; ZugNr <= MaxNr; ZugNr++) {
      Spiel[ZugNr].RestBLACK    = Restzeit(BLACK);
      Spiel[ZugNr].RestWHITE    = Restzeit(WHITE);
      Spiel[ZugNr].SpielerBLACK = Spielertyp(BLACK);
      Spiel[ZugNr].SpielerWHITE = Spielertyp(WHITE);
    }

    ZugNr--;

    NeueWerte();
    Auto_WARTEN();
  }
}


void CB_SPIEL_SPEICHERN(char *name)
{
  FILE *fp;


/*  printf("Speichern: \"%s\"\n", name); */

  if ((fp=fopen(name, "r")) != 0) {		/* Datei schon da? */
     
    fclose(fp);

    if (!XUeberschreibenNotice()) return;

  } else fclose(fp);


  if (!SaveGame(name, Spiel, MaxNr)) XSchreibfehlerNotice();
  else CB_SPEICHERN_AUS();  
}





void Loop(void)
{
  SfGrund(&StartInfo.Sf);

/*
StartInfo.Sf = Sfx;
*/

  XGetProps(&propinfo);
  propinfo.AmZug = BLACK_AM_ZUG;
  XSetProps(&propinfo);

  StartInfo.AmZug = BLACK;


  NeuesSpiel();
 
  Auto_WARTEN();

  Notifier();
}

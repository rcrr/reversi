// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Grafik-Interface und Notifier f¸r O */

#ifndef PRECOMP
#include "main.h"
#endif

#include "sys.h"

#include "int.h"
#include "playgm.h"

#include "sboard.h"
#include "attr.h"
#include "crtg.h"
#include "crt.h"
#include "filecom.h"
#include "goodies.h"



static	GFXFLAGS  Spielfeld[100];

static  int	  SfModus = GFX_SPIELMODUS;



static void GfxStein(SFPOS Pos, GFXFLAGS Inhalt)
{
  int   Farbe, Invers;


if (!ZUG(Pos)) Error("Pos");

/* printf("%d %d\n", Pos, Inhalt & GFX_NEU); */

  if (!(Inhalt & GFX_NEU) && !(Inhalt & GFX_REFRESH) && 
	(Inhalt & ~GFX_NEU & ~GFX_REFRESH) == Spielfeld[Pos]) return;

  if (!(Inhalt & GFX_REFRESH)) 
    Spielfeld[Pos] = Inhalt & ~GFX_NEU & ~GFX_REFRESH;
  else
    Inhalt = Spielfeld[Pos];


  if (Inhalt & GFX_BLACK) { 

    Farbe = C_BLACK; Invers = C_WHITE;

  } else {

    Farbe = C_WHITE; Invers = C_BLACK;
  }

  if (!(Inhalt & GFX_ST)) XLeer(Pos); else XStein(Pos, Farbe);

  if (Inhalt & GFX_X) XX(Pos, Farbe);

  if (Inhalt & GFX_ST) {


/*
    COLOR(Invers);

    if (Inhalt & GFX_PUNKT)) {

      MOVE(rp, xg-STRXLEN/2, yg-STRYLEN/2); DRAW(rp, xg+STRXLEN/2, yg+STRYLEN/2);
      MOVE(rp, xg-STRXLEN/2, yg+STRYLEN/2); DRAW(rp, xg+STRXLEN/2, yg-STRYLEN/2);
    }
*/
    if (Inhalt & GFX_ZAHL) XZahl(Inhalt & 0xff, Pos, Invers);


  }
}
  



void GfxSfAus(SPFELD *psf, PARTEI Partei, GFXFLAGS flags)
{
  int	 a, i, ZugAnz, Farbe;
  bool	 M[100];
  SFPOS  Pos, Zuege[60];


  if ((flags & GFX_SPIELMODUS) != SfModus || (flags & GFX_REFRESH)) { 
    SfModus = flags & GFX_SPIELMODUS;
    XSfModus(SfModus);
  }

  ZugAnz = SfMoeglZuege(psf, Partei, Zuege);

  FOR (i, 100)	  M[i]        = false;
  FOR (i, ZugAnz) M[Zuege[i]] = true;

  if (Partei == BLACK) Farbe = GFX_BLACK; else Farbe = GFX_WHITE;

  FOR_SFPOS10 (Pos) {

    a = psf->p[Pos];

    if      (a == BLACK) GfxStein(Pos, GFX_ST | GFX_BLACK | (flags & ~GFX_X));

    else if (a == WHITE) GfxStein(Pos, GFX_ST | (flags & ~GFX_X));

    else if (M[Pos]) 	 GfxStein(Pos, flags | Farbe);
    else	     	 GfxStein(Pos, flags & ~GFX_X);

  }
}




/********************************************************************************/



void MeldeKeineMoegl(void)
{ XmsgBlaBla("Keine Mˆglichkeit");   }


void MeldeZeitUeber(void)
{ XmsgBlaBla("ZEIT ‹BERSCHRITTEN!"); }


void ClearMeldung(void) { XClearBlaBla(); }


void ErgebnisAus(SPIELINFO *pSpInfo)
{
  char s[100];
  int  i;


  i = SfAnzBLACK(&pSpInfo->Sf) - SfAnzWHITE(&pSpInfo->Sf);

  if (i == 0) 

    strcpy(s, "Unentschieden!");

  else {

    if (i > 0) strcpy(s, "Schwarz"); else strcpy(s, "Weiﬂ");
	
    strcat(s, " hat gewonnen!");
  }

  XmsgBlaBla(s);
}




void SpielfeldAus(SPIELINFO *Spiel, int ZugNr, int MaxNr)
{
  GfxSfAus(&Spiel[ZugNr].Sf, Spiel[ZugNr].AmZug, (MoeglStatus() << GFX_LOG_X));

  XSetVor	(ZugNr < MaxNr);
  XSetZurueck	(ZugNr > 1);
  XSetVorZum	(MaxNr - ZugNr > 1);
  XSetZurueckZum(ZugNr > 2);

  if (Spiel[ZugNr].AmZug == BLACK) XmsgZug(ZUG_UNBEKANNT, Spiel[ZugNr-1].Zug);
  else				   XmsgZug(Spiel[ZugNr-1].Zug, ZUG_UNBEKANNT);
}



void StatistikAus(SPIELINFO *pSpInfo)
{
  XmsgBW();
  XmsgSteine(SfAnzBLACK(&pSpInfo->Sf), SfAnzWHITE(&pSpInfo->Sf));
  XmsgZeiten((int)pSpInfo->RestBLACK, (int)pSpInfo->RestWHITE);
}



void ZugAus(SFPOS Pos, PARTEI Partei)
{
  int i;


  if (ZUG(Pos)) {

    FOR (i, 3) {

      if (i != 0) {

        GfxStein(Pos, GFX_NEU);
        XFlushGfx();

#ifdef X
	BusyWait(1);
#else
	SLEEP(1);
#endif

      }

      if (Partei == BLACK) GfxStein(Pos, GFX_ST | GFX_BLACK | GFX_NEU);
      else		   GfxStein(Pos, GFX_ST | GFX_WHITE | GFX_NEU);
      XFlushGfx();

      if (i != 2) {

#ifdef X
	BusyWait(1);
#else
	SLEEP(1);
#endif
      }
    }
  }
}



void MenuModus(void)  
{ 
  XSetWeiter		(true);  
  XSetNeu		(true);  
  XSetStop		(false);
  XSetTabelle		(true);  
  XSetLaden		(true);  
  XSetSpeichern		(true);  

  XSfModus   (false); 
}


void SpielModus(void) 
{ 
  XSetWeiter		(false); 
  XSetNeu		(false); 
  XSetStop		(true);
  XSetTabelle		(true);  
  XSetLaden		(true);  
  XSetSpeichern		(true);  

  XSfModus   (true); 
}


void EingabeModus(void)
{
  XSetWeiter		(false);  
  XSetNeu		(false);  
  XSetStop		(false);
  XSetVor		(false);
  XSetZurueck		(false);
  XSetVorZum		(false);
  XSetZurueckZum	(false);
  XSetTabelle		(false);  
  XSetLaden		(false);  
  XSetSpeichern		(false);  

  XSfModus		(false); 
}


void TabelleAus(SPIELINFO *Spiel, int maxnr)
{
  int i, nr;


  GfxSfAus(&Spiel[1].Sf, BLACK, 0); 


  nr = 1;

  for (i=1; i < maxnr; i++) {

    if (ZUG(Spiel[i].Zug)) {

      GfxStein(Spiel[i].Zug, 
        GFX_ST | ((Spiel[i].AmZug == BLACK) << GFX_LOG_BLACK) |
	(GFX_ZAHL + nr));

      nr++;
    }
  }  
}




void InitInter(int *pargc, char **argv, PROPINFO *propinfo)
{
  int i;


  XInitInter(pargc, argv, propinfo);

  XSetProps(propinfo);

  FOR (i, 100) Spielfeld[i] = 0;

  XSpielbrett();
}


void FreeInter(void) { XFreeInter(); }

void Notifier(void)  { XNotifier();  }




/* Eigenschaftsfenster ˆffnen */

void PropsOn(void)  {}


/* und wieder schlieﬂen */

void PropsOff(void) {}

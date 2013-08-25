// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Amiga Grafik-Interface und Notifier für O */

#ifndef PRECOMP
#include "amiga.h"
#endif

#include "int.h"
#include "playgm.h"

#include "sboard.h"
#include "attr.h"
#include "crt.h"
#include "filecom.h"
#include "sys.h"


#define BRETT_FARBE		3

#define SPIEL_GITTER_FARBE	1
#define MENU_GITTER_FARBE	0

#define BLACK_FARBE		1
#define WHITE_FARBE		2

#define	TEXTHEIGHT		8



#define PUNKTE		1000	/* max. Anzahl Punkte für AreaFill */

#define SCREEN_BREITE	640

#define BREITE		380	/* Fenstergröße */
#define HOEHE		245

#define DX		40	/* Feldgröße */
#define DY		20
#define RXSTEIN		17	/* Steingröße */
#define RYSTEIN  	8

#define X0		34	/* Ecke links oben des Spielfeldes */
#define Y0		24

#define XM0		(DX/2)	/* Steinmittelpunkt */
#define YM0		(DY/2)


#define DRAW(r,x,y)	Draw	 (r, (long)x, (long)y)
#define MOVE(r,x,y)	Move     (r, (long)x, (long)y)
#define COLOR(r,c)	SetAPen  (r, (long)c)
#define FILLCOLOR(r,c)	SetOPen	 (r, (long)c)
#define MODE(r,m)	SetDrMode(r, (long)c)


#define GADGET_ANZ_EINSTELLUNGEN \
(sizeof(GadgetsEinstellungen)/sizeof(GadgetsEinstellungen[0]))
#define GADGET_ANZ_EINGABE	\
(sizeof(GadgetsEingabe)/sizeof(GadgetsEingabe[0]))

#define GADGET_ANZ		\
(GADGET_ANZ_EINSTELLUNGEN + GADGET_ANZ_EINGABE)


#define GAD_EINST_SPEICHERN	0
#define GAD_EINST_WECHSELN	1
#define GAD_EINST_UEBERNEHMEN	2
#define GAD_EINST_SPIELERB	3
#define GAD_EINST_SPIELERW	4
#define GAD_EINST_ZEITB		5
#define GAD_EINST_ZEITW		6
#define GAD_EINST_MOEGL		7
#define GAD_EINST_PARTEIB	8
#define GAD_EINST_PARTEIW	9

#define GAD_EINGA_AMZUG		10
#define GAD_EINGA_SETZMODUS	11
#define GAD_EINGA_DREHEN	12
#define GAD_EINGA_SPIEGELN	13
#define GAD_EINGA_GRUND		14
#define GAD_EINGA_FUELLEN	15
#define GAD_EINGA_UEBERNEHMEN	16


typedef struct {

  struct NewGadget	newg;
  uint4   	  	Typ, Activation;
  struct TagItem	Tags[6];

} GADTYPE;




struct Library		*GadToolsBase	= NULL;
struct Library		*AslBase	= NULL;
struct IntuitionBase	*IntuitionBase	= NULL;
struct GfxBase		*GfxBase	= NULL;
int        		fen	= 0;

static struct Screen	*screen = NULL;
static struct VisualInfo *VInfo	= NULL;
static struct RastPort 	*rp	= NULL;
static struct Window   	*wi	= NULL, *propwin = NULL, *inputwin = NULL;
static PLANEPTR		plane	= NULL;

static WORD   		areabuffer[PUNKTE*3];
static struct TmpRas   	tmpras;
static struct AreaInfo	areainfo;

static struct TextAttr MyFont = {
  "topaz.font", TOPAZ_EIGHTY, FS_NORMAL, FPF_ROMFONT
};


/* Screen */

static struct NewScreen ns = {
  0, 0,
  SCREEN_BREITE, 256,
  2,
  0, 1,
  HIRES,
  CUSTOMSCREEN,
  &MyFont,
  TITEL,
  NULL,
  NULL
};


/* Screen-Farben */

static UWORD colors[4] = {
  0x0ccc,		/* grau     */
  0x0000,		/* schwarz  */
  0x0fff,		/* weiß	    */
  0x038f		/* hellblau */
};


/* Spielfeld */

static struct NewWindow nw = {
  280, 0,					/* Ecke links oben (x,y)*/
  BREITE, HOEHE,				/* Breite, Höhe		*/
  0, 1,						/* Detail-, Block-pen	*/
  MENUPICK | MOUSEMOVE | MOUSEBUTTONS |		/* IDCMPFlags */
  CLOSEWINDOW,
  WFLG_CLOSEGADGET | ACTIVATE | WINDOWDRAG |	/* Flags */
  WINDOWDEPTH | REPORTMOUSE,			
  NULL,						/* Gadget *		*/
  NULL,						/* Image *		*/
  (uint1*)"Spielfeld",				/* Text im Kopf		*/
  NULL,						/* Screen *		*/
  NULL,						/* BitMap *		*/
  50, 50,					/* MinWidth,MinHeight	*/
  200,100,					/* MaxWidth,MaxHeight	*/
  CUSTOMSCREEN					/* Type			*/
};


/* Einstellungen */

static struct NewWindow newpropwin = {
  0, 0, 260, 170, 0, 1,
  GADGETUP | VANILLAKEY | CLOSEWINDOW,
  WFLG_ACTIVATE | WFLG_RMBTRAP | WFLG_CLOSEGADGET | WFLG_DRAGBAR | WFLG_DEPTHGADGET,
  NULL, NULL, (uint1*) "Einstellungen", NULL, NULL, 5, 5, 30, 30, CUSTOMSCREEN
};


/* Eingabe */

static struct NewWindow newinputwin = {
  0, 0, 260, 130, 0, 1,
  GADGETUP | VANILLAKEY | CLOSEWINDOW,
  WFLG_ACTIVATE | WFLG_RMBTRAP | WFLG_CLOSEGADGET | WFLG_DRAGBAR | WFLG_DEPTHGADGET,
  NULL, NULL, (uint1*) "Eingabe", NULL, NULL, 5, 5, 30, 30, CUSTOMSCREEN
};


/* Reihenfolgen korrespondieren mit Definitionen in o_umsp.h! */

static char *text_Spieler[]	= { "Computer1", "Computer2", "Mensch", NULL };
static char *text_AmZug[]	= { "Schwarz",  "Weiß",   NULL };
static char *text_SetzModus[]	= { "Schwarz",  "Weiß",   "Wechsel", "Leer", NULL };


static GADTYPE GadgetsEinstellungen[] = {

{ 82, 97, 97, 14, "Wechseln",	 
  NULL, GAD_EINST_WECHSELN,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0,			TAG_END },
{ 22, 146, 97, 14, "Übernehmen", 
  NULL, GAD_EINST_UEBERNEHMEN,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0,			TAG_END },
{ 145, 146, 97, 14, "Speichern", 
  NULL, GAD_EINST_SPEICHERN,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0, GA_Disabled, true,	TAG_END },


{ 15,  42,  97, 14, NULL,
  NULL, GAD_EINST_SPIELERB,	PLACETEXT_IN,	NULL, NULL,
  CYCLE_KIND,    0,			GTCY_Labels, text_Spieler, TAG_END },
{ 148, 42,  97, 14, NULL,	 
  NULL, GAD_EINST_SPIELERW,	PLACETEXT_IN,	NULL, NULL,
  CYCLE_KIND,    0,			GTCY_Labels, text_Spieler, TAG_END },

{ 35,  68,  52, 14, " Restzeit", 
  NULL, GAD_EINST_ZEITB,	PLACETEXT_RIGHT, NULL, NULL,
  INTEGER_KIND,  GACT_STRINGCENTER,	GTIN_MaxChars, 4, GTIN_Number, 2, TAG_END },
{ 177, 68,  52, 14, NULL,	 
  NULL, GAD_EINST_ZEITW,	0,               NULL, NULL,
  INTEGER_KIND,  GACT_STRINGCENTER,	GTIN_MaxChars, 4, GTIN_Number, 2, TAG_END },

{ 222, 123, 52, 14, "Zugmöglichkeiten anzeigen", 
  NULL, GAD_EINST_MOEGL,    PLACETEXT_LEFT,  NULL, NULL,
  CHECKBOX_KIND, 0,			TAG_END },

{ 30,   21, 70, 14, NULL,
  NULL, GAD_EINST_PARTEIB,  PLACETEXT_IN,    NULL, NULL,
  TEXT_KIND,     0,			GTTX_Text, "Schwarz", TAG_END },
{ 170,  21, 70, 14, NULL,
  NULL, GAD_EINST_PARTEIW,  PLACETEXT_IN,    NULL, NULL,
  TEXT_KIND,     0,			GTTX_Text, "  Weiß ", TAG_END }

};


static GADTYPE GadgetsEingabe[] = {

{ 120, 20,  97, 14, "Am Zug",
  NULL, GAD_EINGA_AMZUG,	PLACETEXT_LEFT, NULL, NULL,
  CYCLE_KIND,    0,			GTCY_Labels, text_AmZug,     TAG_END },
{ 120, 40,  97, 14, "Setzmodus",
  NULL, GAD_EINGA_SETZMODUS,	PLACETEXT_LEFT, NULL, NULL,
  CYCLE_KIND,    0,			GTCY_Labels, text_SetzModus, TAG_END },

{ 15,  65,  120, 14, "Drehen",
  NULL, GAD_EINGA_DREHEN,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0,	TAG_END },
{ 140, 65,  100, 14, "Spiegeln",
  NULL, GAD_EINGA_SPIEGELN,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0,			TAG_END },
{ 15,  81,  120, 14, "Grundstellung",
  NULL, GAD_EINGA_GRUND,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0,			TAG_END },
{ 140, 81,  100, 14, "Füllen",
  NULL, GAD_EINGA_FUELLEN,	PLACETEXT_IN,	NULL, NULL,
  BUTTON_KIND,   0,			TAG_END },

/*
{ 15 , 110, 80, 14, "Zugfolge",
  NULL, GAD_EINGA_ZUGFOLGE,	PLACETEXT_IN,   NULL, NULL,
  BUTTON_KIND,   0,	GA_Disabled, true,			TAG_END },
{ 186, 110, 52, 14, "Zugnummer", 
  NULL, GAD_EINGA_ZUGNR,	PLACETEXT_LEFT,  NULL, NULL,
  INTEGER_KIND,  GACT_STRINGCENTER,	GTIN_MaxChars, 2, GTIN_Number, 1, 
  GA_Disabled, true, TAG_END },
*/

{ 82,  107, 97, 14, "Übernehmen",         
  NULL, GAD_EINGA_UEBERNEHMEN,	PLACETEXT_IN,   NULL, NULL,
  BUTTON_KIND,   0,			TAG_END }

};




static struct Gadget *Gadgets[GADGET_ANZ];



/************************** Menus ****************************/





/* !!!
struct MenuItem {
  *NextItem, LeftEdge, TopEdge, Width, Height, Flags,
  MutualExclude, ItemFill, SelectFill, Command, *SubItem, NextSelect
}
*/




/*********************** Menu 1 *************************/


#define MENU1			0

#define ITEM_UEBER		0
#define ITEM_LADEN		1
#define ITEM_SPEICHERN		2
#define ITEM_TABELLE		3
#define ITEM_EINGABE		4
#define ITEM_EINSTELLUNGEN	5

#define MENU1_ANZ	((int)(sizeof(ItemStrMenu1) / sizeof(char *)))

static char *ItemStrMenu1[] = {
  "Über OTool...   ",
  "Laden...        ",
  "Speichern...    ",
  "Tabelle anzeigen",
  "Eingabe...      ",
  "Einstellungen..."
};

static struct IntuiText Menu1Texte[MENU1_ANZ];


#define ITEMBREITE1 128


static struct MenuItem Mi16 = {
  0,     0, 50, ITEMBREITE1, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu1Texte[ITEM_EINSTELLUNGEN], 0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi15 = {
  &Mi16, 0, 40, ITEMBREITE1, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu1Texte[ITEM_EINGABE], 0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi14 = {
  &Mi15, 0, 30, ITEMBREITE1, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu1Texte[ITEM_TABELLE], 0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi13 = {
  &Mi14, 0, 20, ITEMBREITE1, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu1Texte[ITEM_SPEICHERN],  0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi12 = {
  &Mi13, 0, 10, ITEMBREITE1,  9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu1Texte[ITEM_LADEN],     0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi11 = {
  &Mi12, 0, 0, ITEMBREITE1, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu1Texte[ITEM_UEBER], 0, 0, (struct MenuItem *) NULL, 0
};



/*********************** Menu 2 *************************/




#define MENU2			1

#define ITEM_NEU		0
#define ITEM_WEITER		1
#define ITEM_STOP		2
#define ITEM_ZURUECK		3
#define ITEM_ZURUECK_ZUM_ANFANG	4
#define ITEM_VOR		5
#define ITEM_VOR_ZUM_ENDE	6


#define MENU2_ANZ	((int)(sizeof(ItemStrMenu2) / sizeof(char *)))

static char *ItemStrMenu2[] = {
  "Neues Spiel      ",
  "Weiter           ",
  "Stop             ",
  "Zurück           ",
  "Zurück zum Anfang",
  "Vor              ", 
  "Vor zum Ende     ", 
};

static struct IntuiText Menu2Texte[MENU2_ANZ];

#define ITEMBREITE2 (144+COMMWIDTH)


static struct MenuItem Mi27 = {
  0, 0, 60, ITEMBREITE2, 9, ITEMTEXT | HIGHCOMP,
  0, (APTR)&Menu2Texte[ITEM_VOR_ZUM_ENDE],     0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi26 = {
  &Mi27, 0, 50, ITEMBREITE2, 9, ITEMTEXT | HIGHCOMP | COMMSEQ,
  0, (APTR)&Menu2Texte[ITEM_VOR],     0, 'V', (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi25 = {
  &Mi26, 0, 40, ITEMBREITE2, 9, ITEMTEXT | HIGHCOMP,
  0, (APTR)&Menu2Texte[ITEM_ZURUECK_ZUM_ANFANG], 0, 0, (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi24 = {
  &Mi25, 0, 30, ITEMBREITE2, 9, ITEMTEXT | HIGHCOMP | COMMSEQ,
  0, (APTR)&Menu2Texte[ITEM_ZURUECK], 0, 'Z', (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi23 = {
  &Mi24, 0, 20, ITEMBREITE2, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED | COMMSEQ,
  0, (APTR)&Menu2Texte[ITEM_STOP],    0, 'S', (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi22 = {
  &Mi23, 0, 10, ITEMBREITE2, 9, ITEMTEXT | HIGHCOMP | ITEMENABLED | COMMSEQ,
  0, (APTR)&Menu2Texte[ITEM_WEITER],  0, 'W', (struct MenuItem *) NULL, 0
};

static struct MenuItem Mi21 = {
  &Mi22, 0, 0, ITEMBREITE2,  9, ITEMTEXT | HIGHCOMP | ITEMENABLED,
  0, (APTR)&Menu2Texte[ITEM_NEU],     0, 0, (struct MenuItem *) NULL, 0
};



/***************** Menu-Leiste ***********************/

/* !!!
struct Menu {
  *NextMenu, LeftEdge, TopEdge?, Width, Height?, Flags, *MenuName, *FirstItem
}
*/


#define MENU_TITEL1	"Dies und Das"
#define MENU_BREITE1	(12*8+8)	/* strlen(MENU_TITEL1)*8 + 8 */

#define MENU_TITEL2	"Spiel"
#define MENU_BREITE2	(5*8+8)		/* strlen(MENU_TITEL2)*8 + 8 */

#define MENU_ABSTAND	20


#define MENU_X (SCREEN_BREITE/2-(MENU_BREITE1+MENU_ABSTAND+MENU_BREITE2)/2)

static struct Menu M2 = {
  NULL, MENU_X+MENU_BREITE1+MENU_ABSTAND, 0, MENU_BREITE2, 0, 
  MENUENABLED, MENU_TITEL2, &Mi21
};

static struct Menu M1 = {
  &M2,  MENU_X, 0, MENU_BREITE1, 0, 
  MENUENABLED, MENU_TITEL1, &Mi11
};



/********************************************************************************/

  
static void InitText(char *Text, struct IntuiText *pIntuiText)
{
  pIntuiText->FrontPen  = 0; 
  pIntuiText->BackPen   = 1; 
  pIntuiText->DrawMode  = JAM2;
  pIntuiText->LeftEdge  = 0; 
  pIntuiText->TopEdge   = 0;
  pIntuiText->ITextFont = (struct TextAttr *)NULL;
  pIntuiText->IText	 = (uint1*) Text;
  pIntuiText->NextText  = (struct IntuiText *)NULL;
}


static void InitTexte(char **ppTexte, struct IntuiText *pIntuiTexte, int anz)
{
  int i;
  
 
  FOR (i, anz) InitText(ppTexte[i], pIntuiTexte++);

}




static struct Gadget *InitGads(
  GADTYPE		*Gads, 
  int			Anz, 
  struct NewWindow 	*nw,
  struct Gadget		*GadAdr[]
)
{
  struct Gadget		*gad, *GadgetList;
  int	 		i;
  bool			ok = true;


  if (!screen) Error("screen=0?");
  if (!VInfo)  Error("VInfo=0?");


  GadgetList = NULL;



  FOR (i, Anz) {
    Gads[i].newg.ng_VisualInfo = VInfo;
    Gads[i].newg.ng_TextAttr   = screen->Font;
  }

  if (!(gad=CreateContext(&GadgetList))) Error("Context?");

  nw->FirstGadget = gad;


  FOR (i, Anz) {

    gad = CreateGadgetA(Gads[i].Typ, gad, &Gads[i].newg, Gads[i].Tags);

    if (!gad) ok = false; else gad->Activation |= Gads[i].Activation;

    GadAdr[i] = gad;

  }

  if (!ok) Error("Fehler bei Gadgeterzeugung");

  return GadgetList;
}



static void InitFreeGads(bool Init)
{
  static struct Gadget *GadgetListEinstellungen=NULL, *GadgetListEingabe=NULL;

  if (Init) {

    if (GadgetListEinstellungen || GadgetListEingabe) Error("GadgetList schon da");

    GadgetListEinstellungen = InitGads(
      GadgetsEinstellungen,
      GADGET_ANZ_EINSTELLUNGEN, 
      &newpropwin, Gadgets
    );

    GadgetListEingabe	  = InitGads(
      GadgetsEingabe, 
      GADGET_ANZ_EINGABE, 
      &newinputwin, 
      Gadgets+GADGET_ANZ_EINSTELLUNGEN
    );

    if (!GadgetListEinstellungen || !GadgetListEingabe) 
      Error("Fehler bei Gadgeterzeugung");

  } else {

    if (!GadgetListEinstellungen || !GadgetListEingabe) 
      Error("GadgetList=0?");

    FreeGadgets(GadgetListEingabe);
    FreeGadgets(GadgetListEinstellungen);

    GadgetListEinstellungen = GadgetListEingabe = NULL; 
  }
}



static void InitMenuAndGadgets(void)
{
  if (!IntuitionBase || !GfxBase || !GadToolsBase) {
    printf("*** Intuition, Gfx oder GadTools nicht geöffnet\n");
    _abort();
  }

  InitTexte(ItemStrMenu1, Menu1Texte, MENU1_ANZ);
  InitTexte(ItemStrMenu2, Menu2Texte, MENU2_ANZ);

  if (!wi) Error("Fenster nicht geöffnet");

  SetMenuStrip(wi, &M1);
  MenuModus();
  InitFreeGads(true);
}


static void FreeMenuAndGadgets(void)
{
  if (IntuitionBase && GfxBase && wi) ClearMenuStrip(wi);
  InitFreeGads(false);
}

/***************************************************************************/



void StrCenter(char *s, int len, char *t)
{
  int anz, i, slen;


  slen = strlen(s);

  if (len) anz = (len+1 - slen)/2; else anz = 0;

  if (anz < 0) Error("len zu klein!");


  FOR (i, anz) t[i] = ' ';

  FOR (i, slen) t[anz+i] = s[i];

  FOR (i, anz) t[anz+slen+i] = ' ';

  t[anz+slen+anz] = 0;   
}



static void XCenterTextJam(int x, int y, int col, char *s, int len, int Jam)
{
  char	 t[100];
  struct IntuiText InText;


  if (len > 90) Error("String zu lang");

  StrCenter(s, len, t);

  COLOR(rp, col);

  InText.FrontPen  = col; 
  InText.BackPen   = 0; 
  InText.DrawMode  = Jam;
  InText.LeftEdge  = 0; 
  InText.TopEdge   = 0;
  InText.ITextFont = (struct TextAttr  *)NULL;
  InText.NextText  = (struct IntuiText *)NULL;
  InText.IText     = (uint1*) t;

  PrintIText(rp, &InText, x - IntuiTextLength(&InText)/2, y - TEXTHEIGHT/2+1);
}


static void XCenterTextIm(int x, int y, int col, char *s, int len)
{
  XCenterTextJam(x, y, col, s, len, JAM2);
}


static void XCenterText(int x, int y, int col, char *s, int len)
{
  XCenterTextJam(x, y, col, s, len, JAM1);
}


#define MITTE_BLACK	115
#define MITTE_WHITE	274
#define MSG_Y		194
#define MSG_DY		14


void XmsgBW(void) 
{
  XCenterText(MITTE_BLACK, MSG_Y, BLACK_FARBE, "Schwarz", 0);
  XCenterText(MITTE_WHITE, MSG_Y, BLACK_FARBE, "Weiß",    0);
}

 
void XmsgBlaBla(char *s)
{
#ifdef JONAS
  XCenterTextIm(MITTE_WHITE, MSG_Y, BLACK_s<y        nn    hhgfft7ujj  xcxxxxd zhbbhjjjjy  8llll,lllll    pp  mmmnn
       l            >d hhh  <mmmmnnn bbPp999ppoiii  mmmFARBE, "Weiß",    0);
#endif

  XCenterTextIm((MITTE_WHITE+MITTE_BLACK)/2, MSG_Y+3*MSG_DY+1, BLACK, s, 25);
}      


void XClearBlaBla(void) { XmsgBlaBla(" "); }


void XmsgSteine(int AnzB, int AnzW)
{
  char s[100];

  sprintf(s, " %d ", AnzB);
  XCenterTextIm(MITTE_BLACK+DX/2, MSG_Y+MSG_DY, BLACK_FARBE, s, 2);

  sprintf(s, " %d ", AnzW);
  XCenterTextIm(MITTE_WHITE-DX/2, MSG_Y+MSG_DY, BLACK_FARBE, s, 2);

  sprintf(s, " %d ", AnzB+AnzW);
  XCenterTextIm((MITTE_WHITE+MITTE_BLACK)/2, MSG_Y+MSG_DY, BLACK_FARBE, s, 2);
}



static void ZeitStr(int Zeit, char *s)
{
  if (Zeit < 0) sprintf(s, "-%d:%.2d", (-Zeit) / 60, (-Zeit) % 60);
  else		sprintf(s,  "%d:%.2d",   Zeit / 60,    Zeit % 60);
}

  
void XmsgZeiten(int ZeitB, int ZeitW)
{
  char s[100];


  ZeitStr(ZeitB, s);
  XCenterTextIm(MITTE_BLACK-DX, MSG_Y+MSG_DY, BLACK_FARBE, s, 8);

  ZeitStr(ZeitW, s);
  XCenterTextIm(MITTE_WHITE+DX, MSG_Y+MSG_DY, BLACK_FARBE, s, 8);
}


void XmsgZug(SFPOS PosB, SFPOS PosW)
{
  char s[100];

  sKoorAus(s, PosB);
  XCenterTextIm(MITTE_BLACK-1, MSG_Y+2*MSG_DY, BLACK_FARBE, s, 2);

  sKoorAus(s, PosW);
  XCenterTextIm(MITTE_WHITE-1, MSG_Y+2*MSG_DY, BLACK_FARBE, s, 2);
}



static void SetItem1(int item, bool f)
{
  if (f) OnMenu (wi, (long) SHIFTMENU(MENU1) | SHIFTITEM(item));
  else   OffMenu(wi, (long) SHIFTMENU(MENU1) | SHIFTITEM(item));
}


static void SetItem2(int item, bool f)
{
  if (f) OnMenu (wi, (long) SHIFTMENU(MENU2) | SHIFTITEM(item));
  else   OffMenu(wi, (long) SHIFTMENU(MENU2) | SHIFTITEM(item));
}


void XSetEingabe 	(bool f)	{ SetItem1(ITEM_EINGABE, f); }

void XSetEinstellungen	(bool f)	{ SetItem1(ITEM_EINSTELLUNGEN, f); }

void XSetTabelle	(bool f)	{ SetItem1(ITEM_TABELLE, f); }

void XSetLaden		(bool f)	{ SetItem1(ITEM_LADEN, f); }

void XSetSpeichern	(bool f)	{ SetItem1(ITEM_SPEICHERN, f); }


void XSetZurueck	(bool f)	{ SetItem2(ITEM_ZURUECK, f); }

void XSetVorZum   	(bool f)	{ SetItem2(ITEM_VOR_ZUM_ENDE, f); }

void XSetZurueckZum	(bool f)	{ SetItem2(ITEM_ZURUECK_ZUM_ANFANG, f); }

void XSetVor    	(bool f)	{ SetItem2(ITEM_VOR, f); }

void XSetWeiter 	(bool f)	{ SetItem2(ITEM_WEITER, f); }

void XSetStop   	(bool f)	{ SetItem2(ITEM_STOP, f); }

void XSetNeu    	(bool f)	{ SetItem2(ITEM_NEU, f); }



static UWORD dummy[] = { 0xffff };


void XInitInter(int *pargc, char **argv, PROPINFO *pi)
{
  if (!IntuitionBase) {

    IntuitionBase = (struct IntuitionBase *) OpenLibrary("intuition.library", 37);

    if (!IntuitionBase)
      Error("Kann intuition.library Vers. >= 37 nicht öffnen");
  }

  if (!GfxBase) {

    GfxBase = (struct GfxBase *) OpenLibrary("graphics.library",  37);

    if (!GfxBase) Error("Kann graphics.library Vers. >= 37 nicht öffnen");
  }

  if (!GadToolsBase) {

    GadToolsBase = (struct Library *) OpenLibrary("gadtools.library",  37);

    if (!GadToolsBase) Error("Kann gadtools.library Vers. >= 37 nicht öffnen");
  }

  if (!AslBase) {

    AslBase = (struct Library *) OpenLibrary("asl.library",  37);

    if (!AslBase) Error("Kann asl.library Vers. >= 37 nicht öffnen");
  }

  if (!(screen=(struct Screen *)OpenScreenTags(&ns, SA_Pens, dummy, TAG_END))) 
    Error("Kann Screen nicht öffnen");

  nw.Screen = newpropwin.Screen = newinputwin.Screen = screen;

  LoadRGB4(&screen->ViewPort, colors, 4);

  if ((VInfo=GetVisualInfo(screen, TAG_END)) == NULL) Error("VInfo");


  nw.LeftEdge = pi->winx;
  nw.TopEdge  = pi->winy;

  if (!(wi=OpenWindow(&nw))) Error("Kann Fenster nicht öffnen");

  rp  = wi->RPort;

/* AreaFill vorbereiten */

  InitArea(&areainfo, areabuffer, PUNKTE);
  rp->AreaInfo = &areainfo;
  if (!(plane=AllocRaster(BREITE, HOEHE))) Error("kein Speicher für Plane");

  rp->TmpRas   = InitTmpRas(&tmpras, plane, RASSIZE(BREITE, HOEHE));

  InitMenuAndGadgets();
}





void XFreeInter(void)
{
  FreeMenuAndGadgets();

  if (plane) FreeRaster(plane, BREITE, HOEHE);
  plane = NULL;


  if (wi) CloseWindow(wi);
  wi = 0;

  if (propwin) CloseWindow(propwin);
  propwin = NULL;

  if (inputwin) CloseWindow(inputwin);
  inputwin = NULL;

  if (VInfo) FreeVisualInfo(VInfo);
  VInfo = NULL;

  if (screen)  CloseScreen(screen);
  screen = NULL;

  if (IntuitionBase) CloseLibrary((struct Library *) IntuitionBase);
  IntuitionBase = NULL;

  if (GfxBase)	     CloseLibrary((struct Library *) GfxBase);
  GfxBase = NULL;

  if (GadToolsBase)  CloseLibrary((struct Library *) GadToolsBase);
  GadToolsBase = NULL;

  if (AslBase)  CloseLibrary((struct Library *) AslBase);
  AslBase = NULL;
}




/* Einstellungsfenster öffnen */

void XEinstellungenEin(void)
{
  if (!propwin) {

    newpropwin.LeftEdge = propinfo.propwinx;
    newpropwin.TopEdge  = propinfo.propwiny;

    propwin = OpenWindow(&newpropwin);

    XSetProps(&propinfo);

    GT_RefreshWindow(propwin, NULL);

  } else WindowToFront(propwin);
}


/* und wieder schließen */

void XEinstellungenAus(void)
{
  if (propwin) { XGetProps(&propinfo); CloseWindow(propwin); propwin = NULL; }
}



void XSetProps(PROPINFO *pi)
{
  if (propwin) {

    GT_SetGadgetAttrs(Gadgets[GAD_EINST_PARTEIB], propwin, NULL, 
			GTTX_Text, "Schwarz", TAG_END);
    GT_SetGadgetAttrs(Gadgets[GAD_EINST_PARTEIW], propwin, NULL, 
			GTTX_Text, "  Weiß ", TAG_END);


    GT_SetGadgetAttrs(Gadgets[GAD_EINST_SPIELERB], propwin, NULL, 
			GTCY_Active, pi->SpielerB, TAG_END);

    GT_SetGadgetAttrs(Gadgets[GAD_EINST_SPIELERW], propwin, NULL, 
			GTCY_Active, pi->SpielerW, TAG_END);

    GT_SetGadgetAttrs(Gadgets[GAD_EINST_ZEITB], propwin, NULL, 
			GTIN_Number, pi->RestzeitB, TAG_END);
    GT_SetGadgetAttrs(Gadgets[GAD_EINST_ZEITW], propwin, NULL, 
			GTIN_Number, pi->RestzeitW, TAG_END);

    GT_SetGadgetAttrs(Gadgets[GAD_EINST_MOEGL], propwin, NULL, 
			GTCB_Checked, pi->MoeglAnzeig, TAG_END);


    GT_RefreshWindow(propwin, NULL);
  }

  if (inputwin) {

    GT_SetGadgetAttrs(Gadgets[GAD_EINGA_AMZUG], inputwin, NULL, 
			GTCY_Active, pi->AmZug, TAG_END);

    GT_SetGadgetAttrs(Gadgets[GAD_EINGA_SETZMODUS], inputwin, NULL, 
			GTCY_Active, pi->SetzModus, TAG_END);


    GT_RefreshWindow(inputwin, NULL);
  }
}


void XGetProps(PROPINFO *pi)
{
  if (propwin) {

    pi->RestzeitB = 
      ((struct StringInfo *)Gadgets[GAD_EINST_ZEITB]->SpecialInfo)->LongInt;
    pi->RestzeitW = 
      ((struct StringInfo *)Gadgets[GAD_EINST_ZEITW]->SpecialInfo)->LongInt;

    pi->MoeglAnzeig = ((bool)Gadgets[GAD_EINST_MOEGL]->Flags & SELECTED) != 0;
  }

  if (inputwin) {

/* Zugnummer abholen */

  }


/* Fensterkoordinaten holen */

  if (wi) { 
    pi->winx = wi->LeftEdge; 
    pi->winy = wi->TopEdge; 
  }
 
  if (propwin) { 
    pi->propwinx = propwin->LeftEdge; 
    pi->propwiny = propwin->TopEdge;
  }

  if (inputwin) { 
    pi->inputwinx = inputwin->LeftEdge; 
    pi->inputwiny = inputwin->TopEdge;
  }
}


static void XDrawLine(int x0, int y0, int x1, int y1, int col)
{
  COLOR(rp, col); MOVE(rp, x0, y0); DRAW(rp, x1, y1);
}


void XLeer(SFPOS Pos)
{
  int x, y;

  x = X0 + (Pos % 10 - 1) * DX; 
  y = Y0 + (Pos / 10 - 1) * DY;

  COLOR(rp, BRETT_FARBE); 
  RectFill(rp, x+1, y+1, x+DX-1, y+DY-1);
}


void XStein(SFPOS Pos, int col)
{
  int x, y;


/* Mittelpunkt */

  x = X0 + XM0 + (Pos % 10 - 1) * DX; 
  y = Y0 + YM0 + (Pos / 10 - 1) * DY;

/* Stein */

  if (col == C_BLACK) COLOR(rp, BLACK_FARBE); else COLOR(rp, WHITE_FARBE);
  AreaEllipse(rp, x, y, RXSTEIN, RYSTEIN);
  AreaEnd(rp);

/* Rand invers */

/*
  if (col != C_BLACK) COLOR(rp, BLACK_FARBE); else COLOR(rp, WHITE_FARBE);
  DrawEllipse(rp, x, y, RXSTEIN, RYSTEIN);
*/
}


void XX(SFPOS Pos, int colbw)
{
  int x, y;

  if (colbw == C_BLACK) colbw = BLACK_FARBE; else colbw = WHITE_FARBE;

  x = X0 + XM0 + (Pos % 10 - 1) * DX; 
  y = Y0 + YM0 + (Pos / 10 - 1) * DY;

  XDrawLine(x-RXSTEIN/4,   y-RYSTEIN/4, x+RXSTEIN/4,   y+RYSTEIN/4, colbw);
  XDrawLine(x-RXSTEIN/4-1, y-RYSTEIN/4, x+RXSTEIN/4-1, y+RYSTEIN/4, colbw);
  XDrawLine(x-RXSTEIN/4,   y+RYSTEIN/4, x+RXSTEIN/4,   y-RYSTEIN/4, colbw);
  XDrawLine(x-RXSTEIN/4-1, y+RYSTEIN/4, x+RXSTEIN/4-1, y-RYSTEIN/4, colbw);
}


void XZahl(int Zahl, SFPOS Pos, int colbw)
{
  int    x, y;
  char   s[100];


  x = X0 + XM0 + (Pos % 10 - 1) * DX; 
  y = Y0 + YM0 + (Pos / 10 - 1) * DY;

  if (colbw == C_BLACK) colbw = BLACK_FARBE; else colbw  = WHITE_FARBE; 

  sprintf(s, "%d", Zahl);
 
  XCenterText(x, y, colbw, s, 2);
}



void XFlushGfx(void) {}


/* systemabh. Spielfeld zeichnen */

void XSpielbrett(void)
{
  int    i;
  char   s[100];
  SPFELD Sf;			/* Dummy */


/* Gitter */

  FOR (i, 9) {
    XDrawLine(X0, Y0+i*DY, X0+8*DX, Y0+i*DY, BLACK_FARBE);
    XDrawLine(X0+i*DX, Y0, X0+i*DX, Y0+8*DY, BLACK_FARBE);
  }


/* Koordinaten */

  s[1] = 0;

  FOR (i, 8) {

    s[0] = 'a' + i; 

    XCenterText(X0+i*DX+DX/2, Y0-2-TEXTHEIGHT/2, BLACK_FARBE, s, 0);

    s[0] = '1' + i;

    XCenterText(X0-12, Y0 + i * DY + DY/2, BLACK_FARBE, s, 0);
  }


/* aktuelles Spielfeld erstellen */

  GfxSfAus(&Sf, BLACK, GFX_REFRESH);
}



void XSfModus(bool Modus)
{
  int i, col;


  if (Modus) col = SPIEL_GITTER_FARBE; else col = MENU_GITTER_FARBE;

  FOR (i, 9) {
    XDrawLine(X0, Y0+i*DY, X0+8*DX, Y0+i*DY, col);
    XDrawLine(X0+i*DX, Y0, X0+i*DX, Y0+8*DY, col);
  }
}


void XDefProps(PROPINFO *pi)
{
  pi->winx      = 260;  pi->winy      = 11;
  pi->propwinx  = 0;	pi->propwiny  = 11;
  pi->inputwinx = 0;	pi->inputwiny = 11;
}




/* Eingabefenster öffnen */

void XEingabeEin(void)
{
  if (!inputwin) {

    newinputwin.LeftEdge = propinfo.inputwinx;
    newinputwin.TopEdge  = propinfo.inputwiny;

    inputwin = OpenWindow(&newinputwin);

    GT_RefreshWindow(inputwin, NULL);

  } else WindowToFront(inputwin);
}


/* und wieder schließen */

void XEingabeAus(void)
{
  if (inputwin) { XGetProps(&propinfo); CloseWindow(inputwin); inputwin = NULL; }
}


static struct EasyStruct easyUeber = {
    sizeof(struct EasyStruct), 0, "",  
    "OTool von Michael Buro ist Public Domain!",
   "Gut"
};

static struct EasyStruct easyPassen = {
    sizeof(struct EasyStruct), 0, "",  
    "Leider gibt es keine Zugmöglichkeit!", "Passen"
};

static struct EasyStruct easyUeberschreiben = {
    sizeof(struct EasyStruct), 0, "", 
    "Die Datei gibt es schon!", "Überschreiben|Abbruch"
};

static struct EasyStruct easyNichtDa = {
    sizeof(struct EasyStruct), 0, "",
    "Datei nicht gefunden!", "Weiter"
};

static struct EasyStruct easyLesefehler = {
    sizeof(struct EasyStruct), 0, "",
    "Lesefehler oder Datei korrupt!", "Weiter"
};

static struct EasyStruct easySchreibfehler = {
    sizeof(struct EasyStruct), 0, "",
    "Schreibfehler!", "Weiter"
};


void XUeberNotice(void) 
{ EasyRequest(wi, &easyUeber, NULL, NULL); }

void XPassenNotice(void) 
{ EasyRequest(wi, &easyPassen, NULL, NULL); }

bool XUeberschreibenNotice(void) 
{ return EasyRequest(wi, &easyUeberschreiben, NULL, NULL); }

void XNichtDaNotice(void) 
{ EasyRequest(wi, &easyNichtDa, NULL, NULL); }

void XLesefehlerNotice(void) 
{ EasyRequest(wi, &easyLesefehler, NULL, NULL); }

void XSchreibfehlerNotice(void) 
{ EasyRequest(wi, &easySchreibfehler, NULL, NULL); }



char *Req(char *Titel)
{
  char   *name=NULL;
  struct FileRequester *FileRequester;

  static char dir [110] = "";
  static char file[110] = "";


  FileRequester = AllocAslRequest(ASL_FileRequest, NULL);

  if (FileRequester) {

    if (AslRequestTags(FileRequester,
			ASL_Window,	(uint4)wi,
			ASL_Hail,	(uint4)Titel,
			ASL_Pattern,	(uint4)"#?.oth",
			ASL_Height,	220,
			ASL_File,	file,
			ASL_Dir,	dir,
			ASL_FuncFlags,	FILF_PATGAD,
			TAG_DONE)) {

      name = malloc(strlen(FileRequester->rf_Dir)+strlen(FileRequester->rf_File)+10);

      if (name) {

	if (strlen(FileRequester->rf_Dir)  < 100) strcpy(dir,  FileRequester->rf_Dir); 
	if (strlen(dir) && dir[strlen(dir)-1] != '/') strcat(dir, "/");
	if (strlen(FileRequester->rf_File) < 100) strcpy(file, FileRequester->rf_File);

	sprintf(name, "%s%s", dir, file);
      }

    } else printf("Cancel\n");

    FreeFileRequest(FileRequester);
  }

  return name;
}



void XLadenEin(void) 
{
  char *name;


  if (name=Req("Spiel laden")) {
    CB_SPIEL_LADEN(name);
    free(name);
  }
}

void XLadenAus(void) {}



void XSpeichernEin(void)
{
  char *name;


  if (name=Req("Spiel speichern")) {
    CB_SPIEL_SPEICHERN(name);
    free(name);
  }
}

void XSpeichernAus(void) {}


void XDestroy(void) {}


void XNotifier(void)
{
  long			class, code, num, id;
  struct IntuiMessage	*message;
  struct MenuItem	*item;
  int			x, y, Mx, My;


  FOREVER {

    while (wi && (message=GT_GetIMsg(wi->UserPort))) {

      class = message->Class; 
      code  = message->Code;
      Mx    = message->MouseX; 
      My    = message->MouseY;

      GT_ReplyIMsg(message);
 
      switch (class) {

	case IDCMP_CLOSEWINDOW:

	  CB_ENDE();

	  return;
	  break;

        case MOUSEMOVE:

	  break;


        case MENUPICK:

	  while (code != MENUNULL) {

	    item = ItemAddress(&M1, code);
  	    num  = ITEMNUM(code);

	    if (MENUNUM(code) == MENU1) {

 	      switch (num) {

	        case ITEM_UEBER:		XUeberNotice();		break;
	        case ITEM_LADEN:		XLadenEin();		break;
	        case ITEM_SPEICHERN:  		XSpeichernEin();	break;
		case ITEM_TABELLE:		CB_TABELLE();		break;
	        case ITEM_EINSTELLUNGEN:	CB_EINSTELLUNGEN_EIN();	break;
	        case ITEM_EINGABE:		CB_EINGABE_EIN();	break;

		default:			Error("unbekannter Menu-Code");
	      }

	    } else if (MENUNUM(code) == MENU2) {

 	      switch (num) {

		case ITEM_NEU:		CB_NEU();		break;
		case ITEM_WEITER:  	CB_WEITER();		break;
		case ITEM_STOP:		CB_STOP();		break;
		case ITEM_VOR:		CB_VOR();		break;
		case ITEM_ZURUECK:	CB_ZURUECK();		break;
		case ITEM_VOR_ZUM_ENDE:	      CB_VOR_ZUM();	break;
		case ITEM_ZURUECK_ZUM_ANFANG: CB_ZURUECK_ZUM();	break;

		default:			Error("unbekannter Menu-Code");
	      }
	    }

	    code = item->NextSelect;

	  } 
          break;


      case MOUSEBUTTONS:

	switch(code) {

	  case SELECTDOWN:

	    FOR (x, 8)
	      if (X0+1+ x * DX <= Mx && Mx <= X0-1 + (x+1) * DX) break;

	    if (x == 8) break;

	    FOR (y, 8)
	      if (Y0+1+ y * DY <= My && My <= Y0-1 + (y+1) * DY) break;

	    if (y == 8) break;

	    CB_ZUG((y+1) * 10 + x + 1);

	    break;
	}
	break;
      }
    }


/* Einstellungsfenster */

    while (propwin && (message=GT_GetIMsg(propwin->UserPort))) {

      class    = message->Class; 
      code     = message->Code;

      if (class == IDCMP_GADGETUP) 
	id = ((struct Gadget*)message->IAddress)->GadgetID;

      GT_ReplyIMsg(message);

      switch (class) {

	case IDCMP_CLOSEWINDOW: 
		CB_EINSTELLUNGEN_AUS(); 
		break;

	case IDCMP_REFRESHWINDOW: 
		GT_BeginRefresh(propwin);
		GT_EndRefresh(propwin, true);
		break;

	case IDCMP_GADGETUP:

		switch (id) {

		  case GAD_EINST_MOEGL:		CB_MOEGL();		break;
		  case GAD_EINST_WECHSELN:	CB_WECHSELN();		break;
		  case GAD_EINST_UEBERNEHMEN:	CB_EINST_BEN();		break;
		  case GAD_EINST_SPEICHERN:	CB_EINST_SPEI();	break;
		  case GAD_EINST_ZEITB:		CB_ZEIT();		break;
		  case GAD_EINST_ZEITW:		CB_ZEIT();		break;
		  case GAD_EINST_SPIELERB:	CB_SPIELERB(message->Code); break;
		  case GAD_EINST_SPIELERW:	CB_SPIELERW(message->Code); break;
		
		  default: break;
		}

		break;

	default: break;
      }
    }


/* Eingabefenster */


    while (inputwin && (message=GT_GetIMsg(inputwin->UserPort))) {

      class    = message->Class; 
      code     = message->Code;

      if (class == IDCMP_GADGETUP) 
	id = ((struct Gadget*)message->IAddress)->GadgetID;

      GT_ReplyIMsg(message);


      switch (class) {

	  case IDCMP_CLOSEWINDOW: 
	    CB_EINGABE_AUS(); 
	    break;

	  case IDCMP_REFRESHWINDOW:
		GT_BeginRefresh(inputwin);
		GT_EndRefresh(inputwin, true);
		break;

	  case IDCMP_GADGETUP:

		switch (id) {

		  case GAD_EINGA_AMZUG:		CB_AMZUG(code);		break;
		  case GAD_EINGA_SETZMODUS:	CB_SETZMODUS(code);	break;
		  case GAD_EINGA_DREHEN:	CB_DREHEN();		break;
		  case GAD_EINGA_SPIEGELN:	CB_SPIEGELN();		break;
		  case GAD_EINGA_GRUND:		CB_GRUND();		break;
		  case GAD_EINGA_FUELLEN:	CB_FUELLEN();		break;
		  case GAD_EINGA_UEBERNEHMEN:	CB_EINGABE_OK();	break;
		
		  default: break;
		}

		break;

	default: break;
      }
    }    

    CB_TIMER(); 
    CB_TIMER_LONG(); 
    SLEEP(1);			/* 0.1 Sek. warten */
  }
}


void XBeep(void) {}

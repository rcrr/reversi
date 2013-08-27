// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Prototypen von playgm.c */


#ifndef SP_H
#define SP_H

#include "sboard.h"
#include "crt.h"

typedef enum { SETZE_BLACK, SETZE_WHITE, SETZE_WECHSEL, SETZE_LEER } SETZMODUS;


typedef struct {

  int		RestzeitB, RestzeitW;		/* für Spiel   */
  SP_TYP	SpielerB, SpielerW;

  AM_ZUG	AmZug;				/* für Eingabe */
  SETZMODUS	SetzModus;

  bool		MoeglAnzeig;			/* für Interface */
  bool		Beep;				/* Beep bei Comp.Zug */
  int		winx, winy, 
		propwinx, propwiny, 
		inputwinx, inputwiny,
		ladenwinx, ladenwiny,
		speichernwinx, speichernwiny;

} PROPINFO;




typedef struct {

  SP_TYP Typ;
  REAL	 Gesamtzeit;
  REAL   Restzeit;

} SPIELER;




extern void Loop(void);


extern	int		ZugNr, MaxNr, MoeglZu;
extern	SPIELINFO	Spiel[120];


/* CallBack - Funktionen für Notifier */

extern void CB_SPIEL_LADEN	(char *);
extern void CB_SPIEL_SPEICHERN	(char *);
extern void CB_ENDE		(void);
extern void CB_WEITER		(void);
extern void CB_NEU		(void);
extern void CB_TABELLE		(void);
extern void CB_VOR	 	(void);
extern void CB_ZURUECK		(void);
extern void CB_VOR_ZUM	 	(void);
extern void CB_ZURUECK_ZUM	(void);
extern void CB_STOP		(void);
extern void CB_ZUG	 	(SFPOS ZUGret);
extern void CB_NACHRICHT	(char *Nachricht);
extern void CB_EINGABE_EIN	(void);
extern void CB_EINGABE_AUS	(void);
extern void CB_EINSTELLUNGEN_EIN(void);
extern void CB_EINSTELLUNGEN_AUS(void);
extern void CB_TIMER		(void);
extern void CB_TIMER_LONG	(void);


/* propwin */

extern void CB_MOEGL		(void);
extern void CB_WECHSELN		(void);
extern void CB_EINST_SPEI	(void);
extern void CB_EINST_BEN	(void);
extern void CB_ZEIT		(void);
extern void CB_SPIELERB		(int Spieler);
extern void CB_SPIELERW		(int Spieler);


/* inputwin */ 

extern void CB_AMZUG		(int code);
extern void CB_SETZMODUS	(int code);
extern void CB_DREHEN		(void);
extern void CB_SPIEGELN		(void);
extern void CB_GRUND		(void);
extern void CB_FUELLEN		(void);
extern void CB_EINGABE_OK	(void);
extern void CB_EINGABE_ABBRUCH	(void);
extern void CB_ZUGFOLGE		(void);


/* ladewin */

extern void CB_LADEN_EIN	(void);
extern void CB_LADEN_AUS	(void);
extern void CB_SPIEL_LADEN	(char *);


/* speichernwin */

extern void CB_SPEICHERN_EIN	(void);
extern void CB_SPEICHERN_AUS	(void);
extern void CB_SPIEL_SPEICHERN	(char *);




extern	REAL	Gesamtzeit	(PARTEI Partei);
extern	SP_TYP	Spielertyp	(PARTEI Partei);
extern	bool	MoeglStatus	(void);

extern	PROPINFO propinfo;

#endif

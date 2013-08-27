// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Header von sys.c, systemabhängige Funktionen */

#ifndef SYS_H
#define SYS_H

#include "playgm.h"

#define C_BLACK	0
#define C_WHITE 1

#ifdef GPP
#define C	"C"
#else
#define C
#endif

extern C void XInitInter	(int *pargc, char **argv, PROPINFO *pi);
extern C void XFreeInter	(void);
extern C void XDestroy		(void);

extern C void XBeep		(void);

extern C void XDefProps		(PROPINFO *pi);

extern C void XNotifier		(void);
extern C void XFlushGfx		(void);
extern C void XSpielbrett	(void);
extern C void XSfModus		(bool);

extern C void XStein		(SFPOS Pos, int Farbe);
extern C void XX		(SFPOS Pos, int Farbe);
extern C void XLeer		(SFPOS Pos);
extern C void XZahl		(int Zahl, SFPOS Pos, int col);

extern C void XSetZurueck	(bool);
extern C void XSetVor		(bool);
extern C void XSetZurueckZum	(bool);
extern C void XSetVorZum	(bool);
extern C void XSetWeiter	(bool);
extern C void XSetStop		(bool);
extern C void XSetNeu		(bool);
extern C void XSetTabelle	(bool);
extern C void XSetLaden		(bool);
extern C void XSetSpeichern	(bool);
extern C void XSetEingabe	(bool);
extern C void XSetEinstellungen	(bool);


extern C void XmsgBlaBla	(char *);
extern C void XClearBlaBla	(void);
extern C void XmsgSteine	(int AnzB,  int AnzW);
extern C void XmsgZeiten	(int ZeitB, int ZeitW);
extern C void XmsgZug		(SFPOS PosB, SFPOS PosW);
extern C void XmsgBW		(void);

extern C void XPassenNotice		(void);
extern C bool XUeberschreibenNotice	(void);
extern C void XNichtDaNotice		(void);
extern C void XLesefehlerNotice		(void);
extern C void XSchreibfehlerNotice	(void);


extern C void XEinstellungenAus	(void);
extern C void XEinstellungenEin	(void);
extern C void XGetProps		(PROPINFO *);
extern C void XSetProps		(PROPINFO *);

extern C void XEingabeAus	(void);
extern C void XEingabeEin	(void);

extern C void XLadenAus		(void);
extern C void XLadenEin		(void);

extern C void XSpeichernAus	(void);
extern C void XSpeichernEin	(void);

#endif

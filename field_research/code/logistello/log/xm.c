// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#define BEGINNER     BLACK

#ifndef PRECOMP
#include "main.h"
#endif

#include "sys.h"
#include "sboard.h"
#include "playgm.h"
#include "int.h"
#include "filecom.h"


int	VERB = 1;		/* Ausgabe bei Zugermittlung */

extern	int	Enable_Abort;


char	TO_1[100],   TO_2[100];
char	FROM_1[100], FROM_2[100];

bool f_old=false;


PROPINFO defpropinfo = {
  2, 10,			/* Zeiten		*/
  SP_COMP1, SP_MENSCH,		/* SpielerB, SpielerW	*/

  BLACK_AM_ZUG,			/* Am Zug bei Eingabe	*/
  SETZE_BLACK,
  true,				/* MoeglAnzeig		*/
  false				/* Beep			*/
};


PROPINFO propinfo;

#ifdef xxx

SPFELD st = {

1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 2, 1, 2, 1, 0, 0,
1, 2, 1, 2, 1, 2, 0, 0,
1, 1, 2, 1, 0, 0, 0, 0,
1, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0

};

SPFELD st1 = {

0,0,0,0,0,0,0,0,
0,0,0,2,0,0,0,0,
0,2,2,2,2,2,2,0,
0,0,0,2,1,2,0,0,
0,0,0,2,1,1,2,0,
0,0,0,0,1,1,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0
};

#define B BLACK
#define W WHITE

SPFELD Sf2 = {

B,0,B,0,B,W,0,W,
0,B,B,B,B,B,0,W,
W,W,B,B,B,W,B,W,
W,W,W,B,B,B,W,W,
W,B,W,W,B,B,W,W,
W,B,W,B,B,W,B,W,
W,B,B,W,B,B,W,W,
W,B,W,W,W,W,W,W
};


SPFELD Sf3 = {
B,B,W,B,B,B,B,W,
B,B,W,B,B,B,W,W,
B,W,B,B,W,B,W,W,
W,W,W,B,B,0,0,W,
W,B,W,0,B,W,0,W,
W,W,W,B,B,B,B,B,
0,0,0,B,B,0,W,W,
B,0,B,B,0,B,B,W
};


#endif



void _abort(void)
{
#ifdef AMIGA
  Enable_Abort = 0;
#endif

  FreeInter();

  // kill player

  if (pipes) {

    // if (!PipeSyncSendEXIT(TO_1)) { SLEEP(10); SyncSendEXIT(TO_1); }
    // if (!PipeSyncSendEXIT(TO_2)) { SLEEP(10); SyncSendEXIT(TO_2); }
    
  } else {

    if (!SyncSendEXIT(TO_1)) { SLEEP(10); SyncSendEXIT(TO_1); }
    if (!SyncSendEXIT(TO_2)) { SLEEP(10); SyncSendEXIT(TO_2); }

    SLEEP(10);
    
    KillChannel(TO_1);   KillChannel(TO_2);
    KillChannel(FROM_1); KillChannel(FROM_2);
  }

  exit(1);
}



void main(int argc, char **argv)
{
  propinfo = defpropinfo;

  XDefProps(&propinfo);

  InitInter(&argc, argv, &propinfo);

  if (argc != 3 && argc != 4) {

error:
    Error("call: ox com1 com2 [-old]\n");
  }

  if (strlen(argv[1]) + strlen(TO_PRAEFIX)   > 90 ||
      strlen(argv[2]) + strlen(TO_PRAEFIX)   > 90 ||
      strlen(argv[1]) + strlen(FROM_PRAEFIX) > 90 ||
      strlen(argv[2]) + strlen(FROM_PRAEFIX) > 90)
    Error("Id zu lang");


  if (argv[3])
    if (!strcmp(argv[3], "-old")) f_old = true;
    else goto error;


  sprintf(TO_1, "%s"TO_PRAEFIX, argv[1]);
  sprintf(FROM_1, "%s"FROM_PRAEFIX, argv[1]);

  sprintf(TO_2, "%s"TO_PRAEFIX, argv[2]);
  sprintf(FROM_2, "%s"FROM_PRAEFIX, argv[2]);


  Loop();

  _abort();
}

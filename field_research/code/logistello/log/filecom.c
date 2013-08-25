// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Kommunikationsmodul fuer Othellospieler / 6.1.92 */

#include "main.h"
#include "filecom.h"
#include "crt.h"
#include "goodies.h"

int KommAus = 0;


/* Nachricht s auf Kanal Kanal senden
 *   Format: xxx s\0
 *     xxx ist die Länge der Datei 
 */

bool SyncSend(char *Kanal, char *s)
{
  FILE *fp;
  char t[NACHRICHT_MAX+10];
  struct stat buffer;
  int i;


Chk_Abort();

  if (strlen(s)+5 > NACHRICHT_MAX)  Error("Nachricht zu lang");

if (KommAus) { printf(">>> Send: %s %s\n", Kanal, s); }


  if ((i=stat(Kanal, &buffer)) != -1) {

if (KommAus) printf("::: %d\n", i); 

    if ((fp=fopen(Kanal, "r"))) { fclose(fp); return false; }

  }

if (KommAus) printf(">>> Kanal ok\n");

#if 1
  if ((fp=fopen(Kanal, "rb")) != 0) {	/* alte Nachricht noch nicht weg */

    fclose(fp); return false;
  }
#endif


/* Nachricht ablegen */

  if (!(fp=fopen(Kanal, "wb"))) Error("Kanal nicht zu öffnen");

  sprintf(t, "%.3d %s", strlen(s)+4, s);

  fwrite((char*)t, (size_t) 1, (size_t) strlen(t)+1, fp);

/*printf(">>> <%s>\n", t);*/

  if (ferror(fp)) Error("Schreibfehler");
  fclose(fp);

  return true;
}




void Send(char *Kanal, char *s)
{
/* solange warten, bis alte Nachricht weg ist */

  FOREVER {

    if (SyncSend(Kanal, s)) break;

    WARTEN(2);
    printf("w"); fflush(stdout); 
  }
}  




/* Nachricht s von Kanal Kanal empfangen, ret != 0 <=> was da */

bool Empf(char *Kanal, char *s)
{
  int  i, e;
  FILE *fp;
  struct stat buffer;

if (KommAus) printf(">>> Empf: %s\n", Kanal);

Chk_Abort();

/* ist Nachricht da? nein->raus */


  if (stat(Kanal, &buffer) == -1) return false;

  if (!(fp=fopen(Kanal, "rb"))) Error("Datei nicht lesbar"); 

#ifdef alt

  if (!(fp=fopen(Kanal, "rb"))) {


if (KommAus) printf(">>> nichts da\n");

    return false;
  }
#endif



/* Nachricht einlesen */

  i = fread((char*)s, (size_t) 1, (size_t) NACHRICHT_MAX, fp);

  if (i < 4) { fclose(fp); return false; }


  if (ferror(fp)) Error("Lesefehler");

  s[i] = 0;

  fclose(fp);


/*printf("%d %d %s:<%s>\n", strlen(s), i, Kanal, s);*/

  if (strlen(s) < 4) return false;		/* nicht vollständig! */


  if (!isdigit(s[0]) || !isdigit(s[1]) || !isdigit(s[2]) || s[3] != ' ') {

printf("%d %d %d %d <%s>\n", s[0], s[1], s[2], s[3], s);

    Error("Nachricht korrupt: Anzahl?");
  }

						
  i = strlen(s) - ((s[0]-'0')*100 + (s[1]-'0')*10 + (s[2]-'0'));

  if (i < 0) return false;	/* nicht vollständig! */

  if (i > 0) Error("Nachrichtenlänge zu klein");


/* Anzahl löschen */

  i = 4; e = strlen(s)+1;
  while (i < e) { s[i-4] = s[i]; i++; }

  
if (KommAus) printf(">>> -> %s\n", s);

/* Nachricht löschen */

  FOR (i, 100) {
    if (!unlink(Kanal)) break;
    WARTEN(1+((IRAN & 8)!=0));
  }

if (i >  0)   printf("::: %d Löschversuche\n", i);
if (i >= 100) Error("kann Datei nicht löschen");


  return true;
}




/* auf Nachricht warten (höchstens MaxZeit * 0.1 Sekunden)	*/
/* true zurück <=> Nachricht da					*/

bool SyncEmpf(char *Kanal, char *s, int MaxZeit)
{
  int r, zeit=0;


  do { 

    s[0] = 0;
    r = Empf(Kanal, s); 

    if (!r) { 

      WARTEN(1); 
      zeit++;

 if (KommAus) printf(">>> Versuch\n");

    }

  } while (!r && (MaxZeit == 0 || zeit < MaxZeit));


  return r != 0;  
}



void KillChannel(char *Kanal)
{
  char sync[DATEINAME_MAX], nachricht[DATEINAME_MAX];


  if (strlen(Kanal) > DATEINAME_MAX-10) Error("Dateiname zu lang");  

if (KommAus) printf(">>> Empf: %s\n", Kanal);

  sprintf(sync, "%s.sync", Kanal);
  sprintf(nachricht, "%s.dat", Kanal);

  unlink(sync); unlink(nachricht);
}



/********************************************************/


/* Spiel-Kommunikation */



bool SyncSendCLEAR(char *Kanal)
{
  char s[NACHRICHT_MAX];


/* printf("SEND SIG_CLEAR\n"); */

  sprintf(s, "%d", SIG_CLEAR);
  return SyncSend(Kanal, s);
}  





bool SyncSendEXIT(char *Kanal)
{
  char s[NACHRICHT_MAX];


/* printf("SEND SIG_EXIT\n"); */

  sprintf(s, "%d", SIG_EXIT);
  return SyncSend(Kanal, s);
}  



bool SyncSendBREAK(char *Kanal)
{
  char s[NACHRICHT_MAX];


/*printf("SEND SIG_BREAK\n");*/

  sprintf(s, "%d", SIG_BREAK);
  return SyncSend(Kanal, s);
}  



bool SyncSendBOARD(char *Kanal, PARTEI Partei, int Sek, SFPOS LetzterZug, SPFELD *psf)
{
  char s[NACHRICHT_MAX], s1[NACHRICHT_MAX];
  SFPOS p;


/* printf("SEND SIG_BOARD\n"); */

  sprintf(s, "%d %d %d %d ", SIG_BOARD, Partei, Sek, LetzterZug);

  FOR_SFPOS10 (p) {

    sprintf(s1, " %d", psf->p[p]);
    strcat(s, s1);

  }


  return SyncSend(Kanal, s);
}



bool SyncSendGAME(char *channel, int player, int time, GAME *pgame, bool to_move)
{
  char s[NACHRICHT_MAX];

/* printf("SEND SIG_GAME\n"); */

  sprintf(s, "%d %d %d %d ", SIG_GAME, player, time, to_move != 0);

  sWriteGame(&s[strlen(s)], pgame);
 
  return SyncSend(channel, s);
}



void SendMOVE(char *Kanal, SFPOS Zug, int MoveTime)
{
  char s[NACHRICHT_MAX];


/* printf("SEND SIG_MOVE %d %d\n", Zug, MoveTime); */

  sprintf(s, "%d %d %d", SIG_MOVE, Zug, MoveTime);
  Send(Kanal, s);
}  





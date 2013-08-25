// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// Header für move.c

#ifndef MOVE_H
#define MOVE_H

#include "board.h"
#include "goodies.h"
#include "game.h"
#include "hash.h"


typedef struct {		/* zum Sortieren */

  SFPOS  Zug;
  int    Wert;

} ZUGDAT;



/* Killertabellenstrukturen */


typedef struct { 
  
  SFPOS Liste[100][64];

} KILLTAB;


typedef struct {

  int		FreiAnz;
  SFPOS		DefaultKill[64];
  KILLTAB	*KillBLACK, *KillWHITE;		/* Killertabellen */

} KILLDAT;



typedef enum { MIDGAME, ENDGAME, MIX } ValType;

#define MIX_CONST   100
#define T4T_MSG_LEN 300

/* Daten für CompZug */

typedef struct {

  jmp_buf	timeout_env;		/* Ansprung-Punkt bei SIG_TIMEOUT */ 

  PARTEI	Partei;			/* Partei am Zug (empfangen) */ 
  SFPOS		LetzterZug;		/* letzter Zug	 (empfangen) */
  SPFELD	Sf;			/* Spielfeld	 (empfangen) */
  REAL		Restzeit;		/* Restzeit	 (empfangen) */

  bool		vermutet;		/* true <=> Situtation vermutet   */
  bool		selbst;	 		/* true <=> eigene Zeit		  */
  bool		ZugDa;	 		/* mindestens ein Zug behandelt   */ 
  bool		timeout;		/* Abbruch wg. SIG_TIMEOUT	  */
  ZEIT		Startzeit;		/* Start der eigenen Suche	  */
  REAL		NormDauer;		/* Normale Zeit für Zug		  */
  REAL		MaxDauer;		/* Maximale Zeit für Zug (ALARM)  */
  REAL		DauerExtr;		/* Extrapolierte Zeit für Zug	  */

  BEWFKTP	mBewMitte;		/* mögl. Bewertungsfunktion */
  int		count;			/* max. Zählerwert für Signalabfrage */
  ZEIT		LastCheck;		/* Letzter Zeitpunkt einer Abfrage */

  bool		Ausgabe;		/* Zeit ausgeben? */
  char		*id;			/* Kommunikations-id */


/* Output von CompZug */

  WERT		IterPathValue;	/* bisher bester Wert der Stellung	*/
  WERT		IterPathAl, IterPathBe;	/* Window			*/
  ValType	IterPathType;	
  int		IterPathLen;	/* beste Hauptvariante der Iteration	*/
  SFPOS		IterPath[65];
  bool          NewPath;
  
  WERT          Average;        // average of last two evaluations
  WERT          val1, val2;     // last two evaluations
  
  int		BewAnz;			/* untere Schranke für Anzahl   */
					/* innerer Bewertungen		*/


/* globaler Zustand */

  int           StopM, StopN;           /* stop after WLD at m <= # <= n */
  int           ForcedDepth;            /* ==0: normal search >0:depth   */
  int           ForcedTime;             /* ==0: normal search >0:time    */
  int           ForcedEndDiscNum;       /* ==0: normal, >0:endgame disc# */
  float         TimeFrac;
  bool          Selective;
  float		Percentile1, Percentile2; // cut niveaus
  bool		Quiescence;		/* quiescence search (false)	 */
  float         EndCutNiveau;
  bool          NoAutomaticEndgame;
  int           EarlyEndN;
  bool          IterOutcome;
  bool          OnlyKomi;
  float         game_komi;              // for black
  int           game_rand;              // 0 - no rand, >0 - #discs
  bool          game_synchro;           // is synchro game?
  bool          UseTestGameKomi;
  float         test_game_komi;
  bool          UseTestGameRand;
  int           test_game_rand;
  bool          t4t;
  bool          p2;
  float         t4t_score;
  bool          t4t_updated;
  char          t4t_msg[T4T_MSG_LEN+1];
  int		state;
  int		search_state;
  jmp_buf	swait_env;
  jmp_buf	states_env;

} COMZIO;



/* Arten der Suche */

typedef enum {

  MODUS_NORMAL=1, MODUS_ASPIRATION, MODUS_GEWINN, MODUS_DIFFERENZ, MODUS_NEGAC,
  MODUS_SELEKTIV, MODUS_ITER 

} MODUS;



typedef enum {

  SM_MIDGAME, SM_ENDGAME, SM_ITERATION

} SEARCHMODE;



struct ZUGIO_XXX;

/* Signal-Handler */
typedef void CHECKFKT(COMZIO *pcio, struct ZUGIO_XXX *pzio, bool no_count, bool block); 



/* globale Variablen für Zugermittlung */

typedef struct ZUGIO_XXX {

/* Input */

  PARTEI	Partei;			/* Partei am Zug		*/ 
  SFPOS		LetzterZug;		/* letzer Zug			*/
  SPFELD	Sf;			/* Spielfeld			*/
  int		MaxTiefe;		/* maximale Suchtiefe		*/
  MODUS		Modus;			/* Suchart			*/
  bool		Quiescence;		/* Ruhesuche (false)		*/
  bool		Selective;		/* Selektive Baumsuche (false)	*/
  float		Percentile1, Percentile2; // PC-niveaus
  float         EndCutNiveau;
  bool          Cut;                    /* futility cutoffs             */
  int           (*EvalCut)(BRETT *, PARTEI, int be);

  bool		ZugVorgabe;		/* true <=> nehme nur unten-	*/
  int		VorgabeAnz;		/* stehende Züge		*/
  SFPOS		Zuege[60];	

  BEWFKTP	BewMitte;		/* Bewertungsfunktionen		*/
  WERT		al, be;			/* Intervall für NegaC*, Aspi.  */

  int		la;			/* look-ahead für Vorsortierung */
  REAL		gamma[HashEntry::MAX_HEIGHT];	/* Schnittfaktoren für		*/
					/* selektive Suche		*/

  bool		Ausgabe;		/* Züge in Tiefe 1 ausgeben	*/
  SEARCHMODE	SearchMode;		/* für Ausgabe			*/

	
/* Output */

  int		ZugAnz;			/* Anzahl möglicher Züge	*/
  bool		fertig;			/* Baum wurde ganz durchsucht	*/
  WERT		Wert;			/* Wurzel-Spielwert		*/
  SFPOS		BestZug;		/* bester Zug			*/

  int		MoveNumber;		/* des 1. Zuges			*/
  int		MovesTotal;		/* Anzahl Wurzelzüge		*/
  SFPOS		Move1;			/* aktueller Wurzelzug		*/	
  SFPOS		Path[64];		/* Hauptvariante		*/
  int		PathLen;		/* Länge hiervon		*/
  WERT		PathValue;		/* Wert hiervon			*/
  WERT		PathAl, PathBe;		/* Window			*/
  ValType	PathType;		/* MIDGAME, ENDGAME		*/

/* lokale Daten für Suche */

  int		DiscNum0;		/* initial disc number		*/
  BRETT		Brett;			/* Spielbrett			*/

  HashTab       hash0;
  HashTab       hash;
  HashTab       hash1, hash2;

  HashTab       endhash0;
  HashTab       endhash;

  KILLDAT	killer;			/* Killerdaten			*/

/* etc.: Übergeordnete Aktionen */

  CHECKFKT 	*Check;			/* Signal-Handler */
  COMZIO	cio;

} ZUGIO;




/* Funktionen */

void	InitZug		(ZUGIO *pzio, BEWFKT BewMitte, CHECKFKT *Check, 
			 int HashBits, int HashMaxDepth, 
			 int HashBits0=HASHBITS0, int HashMaxDepth0=HASHMAXDEPTH0);
void	FreeZug		(ZUGIO *pzio);
void    SetzTest	(BRETT *pbrO, BRETT *pbrH, SFPOS AktZug, PARTEI Partei);
void	Zugermittlung	(ZUGIO *pzio);
int	compZUGDAT	(const void *a, const void *b);

void    komi_ab(int to_move, float game_komi, int &al, int &be);


/* benötigte Daten und Funktionen */

extern int	VERB;


#endif

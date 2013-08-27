// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef O_MERKM
#define O_MERKM

#include "board.h"
#include "goodies.h"

#define RATIONAL(a,b)   \
  (((REAL)((a) - (b))) / (REAL)((a) + (b) + 1.0))

/*
#define RATIONAL(a,b)   \
  (((REAL)((a) - (b))) + (sgn((a)-(b)) / (REAL)((a) + (b) + 0.1)))
*/




#define LABEL_ANZ	3

#define	BEISP_MAX	15000
#define WERT_MAX	100
#define MERKMAL_MAX	15
#define KLASSEN_MAX	3


typedef REAL MERKMAL(BRETT *pbr);

typedef REAL MULTI_T;


typedef struct {

  MERKMAL *fun;
  char    *Name;

} MERKMAL_B;



typedef struct {

  char	*Name;
  int	dabei[LABEL_ANZ];

} KLASSE;



typedef struct {

  char *Name;
  int  id;

} LABEL;



typedef struct { 

  struct {

    REAL  s [KLASSEN_MAX],		/* Summe von Merkmal bzgl. Klasse */
          qs[KLASSEN_MAX]; 		/* Quadratsumme			  */
    REAL  d [KLASSEN_MAX][WERT_MAX];	/* Anzahlen zu einzelnen Werten	  */

  } m[MERKMAL_MAX];

  int		AnzInKlasse[KLASSEN_MAX];  /* Anzahl der Beispiele in Klassen	*/
  int		AnzInLab   [LABEL_ANZ];	   /*   ... in Labeln			*/

  MULTI_T	*dmulti	   [KLASSEN_MAX];
  int		*bayes;

  int		Faktoren   [MERKMAL_MAX];
  int		IndexLimit;

  int		LabelGebr[LABEL_ANZ];

  int		GesamtAnz, BeispAnz, MerkmalAnz, KlassenAnz;


  KLASSE	*pKlassen   [KLASSEN_MAX];
  MERKMAL_B	*pMerkmale  [MERKMAL_MAX];
  LABEL		*pLabels    [LABEL_ANZ];

  int		Magic;

} DATEN;


#define MAGIC 12345

#endif

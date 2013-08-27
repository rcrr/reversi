// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef WERT_H
#define WERT_H

#include "board.h"


#if 1                                  /*!!! nice try */
#define ANTI_RET(x)      return (x)
#else
#define ANTI_RET(x)      return (-(x))
#endif


typedef	int	WERT;		/* Typ des Wertes einer Stellung */

#define WERTFAKTOR	100000.0
#define WERTFAKTORINV   0.00001

#define WERTGEWINN	1500000	/* 15.0 is maximum */

#define WERTMAX		1600000
#define WERTMIN		(-WERTMAX)

#define REAL_TO_WERT(r)	((r) * WERTFAKTOR)  /* rounding is too expensive */
	
#define WERT_TO_REAL(w)	(((REAL)(w)) * WERTFAKTORINV)


typedef WERT 	BEWFKT	   (BRETT *, PARTEI);
typedef WERT	(*BEWFKTP) (BRETT *, PARTEI);


#define DISCFAKTOR      10


#if 0

// old setting

#define EXPWERT(x)	(1.0/(1.0+exp(-(x))))
#define LOGIT(x)	log((x) / (1.0 - (x)))

#else

// new setting: factor 2 to stretch area around 0

#define EXPWERT(x)	(1.0/(1.0+exp(-(2.0*x))))
#define LOGIT(x)	(0.5*log((x) / (1.0 - (x))))

#endif


//#define ADJ_KOMI(val) (round(val*1.33*0.5)*2)

// was 1.39 till 13.Sept.99

#define ADJ_KOMI(val) (val*1.33)

#endif

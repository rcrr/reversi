// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOGALL_H
#define LOGALL_H

#include "logmat.h"

typedef REAL DISKRFKT(MATRIX *pX, MATRIX *pbeta, int z);


void Indikator	(MATRIX *pX, MATRIX **ppI);
void Compress	(MATRIX **ppX, MATRIX **ppn, MATRIX **ppy);
void Fehler(
  MATRIX *pX, 
  MATRIX *pn, 
  MATRIX *py, 
  MATRIX *pbeta, 
  REAL   *pFehler01,
  REAL   *pFehler10,
  REAL	 *pDisk
);

DISKRFKT logdis;

#endif
 

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOGIO_H
#define LOGIO_H

#include "logm.h"
#include "logmat.h"

int BspInput(
  char   *Datei, 
  char   *InterDatei,
  MATRIX **ppX, 
  MATRIX **ppn, 
  MATRIX **ppK,
  int	 *pMerkmAnz,
  char   Merkmale[][LENMAX],
  char   Input[DATMAX]
);

#endif

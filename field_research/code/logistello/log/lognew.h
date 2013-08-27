// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOGNEW_H
#define LOGNEW_H

#include "logmat.h"

int	NR(MATRIX *pX, MATRIX *pn, MATRIX *py, MATRIX **ppstart, REAL *pL);
REAL	L(MATRIX *pX, MATRIX *py, MATRIX *pn, MATRIX *pbeta);

#endif

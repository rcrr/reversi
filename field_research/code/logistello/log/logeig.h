// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOGEIG_H
#define LOGEIG_H

#include "logm.h"

extern	void	tred2	(MATRIX *pa, MATRIX *pd, MATRIX *pe);
extern	void	tqli	(MATRIX *pd, MATRIX *pe, MATRIX *pz);
extern  void	eigsrt	(MATRIX *pd, MATRIX *pa);
  
#endif

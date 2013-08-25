// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOGMAT_H
#define LOGMAT_H

#include "logm.h"

#define MATCHECK(pM, s) \
  if ((pM)->dimx <= 0 || (pM)->dimy <= 0 || !(pM)->e) Error(s);

#ifdef MAT_CHECK

#define MAT(pm, y, x) \
  (pm)->e[(x) + (pm)->dimx * (y) + checkMAT(pm, y, x, __FILE__, __LINE__)]
#define VEK(pm, i) \
  (pm)->e[i + checkVEK(pm, i, __FILE__, __LINE__)] 

#else

#define MAT(pm, y, x) (pm)->e[(x) + (pm)->dimx * (y)]
#define VEK(pm, i)    (pm)->e[i] 

#endif

#define NMAX 50

typedef struct { int dimx, dimy; REAL *e; } MATRIX;

MATRIX	*MatMake	(int dimy, int dimx);
void	MatDel		(MATRIX *pM);
void	MatAus		(char *Name, MATRIX *pM);
void	VekAus		(char *Name, MATRIX *pM);
int	MatInv		(MATRIX *pM);
REAL	MatDet		(MATRIX *pM);
void	MatMult		(MATRIX *p1, MATRIX *p2, MATRIX *pe);
void	MatAdd		(MATRIX *p1, MATRIX *p2, MATRIX *pe);
void	MatSub		(MATRIX *p1, MATRIX *p2, MATRIX *pe);
REAL	MatMaxRel	(MATRIX *p1, MATRIX *p2);
void	MatCopy		(MATRIX *p1, MATRIX *p2);
int	checkMAT	(MATRIX *pm, int y, int x, char *datei, int line);
int	checkVEK	(MATRIX *pm, int i, char *datei, int line);

  
#endif

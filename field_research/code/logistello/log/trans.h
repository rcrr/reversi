// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef TRANS_H
#define TRANS_H


typedef int (*TRANS)(int);


/* prototypen */

void	CheckTrans(void);
void	Transform(SPFELD *pin, SPFELD pout[8]);
void	TransformN(SPFELD *pin, SPFELD *pout, int n);

/* Daten */

extern TRANS Trans[8];
extern int TransInv[8], TransMat[8][8];
extern int *TransTab[8]; 
extern int Schichten[64];
#endif

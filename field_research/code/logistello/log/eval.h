// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Prototypen von eval? */

#ifndef EVAL_H
#define EVAL_H

#include "move.h"

#define DATEI_MAX	100

extern  void	InitBewert	(void);
extern  bool	ParameterEinlesen(char *datei, int Anzahl, float *Feld);

extern  int 	BewDiff     	(BRETT *, PARTEI);
extern  int 	BewEntscheid	(BRETT *, PARTEI);

extern	BEWFKT	EvalA, EvalASlow, EvalASimple, EvalL, EvalLSlow, EvalK, EvalKSlow, 
                EvalD, AntiEval, NewEndEval, EvalB;

extern  String	ParameterFile;
extern  String  DataPath;

extern  int     EvalACut(BRETT *pb, PARTEI Partei, int be);

#endif

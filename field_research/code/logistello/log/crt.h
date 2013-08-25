// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* implementierungsabhängige Ausgaben */

#ifndef CRT_H
#define CRT_H

#include <stdio.h>
#include "sboard.h"


void	InitCrt(void);
void	GotoXY(int x, int y);
void	KoorAus(SFPOS SfPos);
void	fKoorAus(FILE *fp, SFPOS SfPos);
void	sKoorAus(char *s, SFPOS SfPos);
void	GKoorAus(SFPOS SfPos);
void	fGKoorAus(FILE *fp, SFPOS SfPos);
void	sGKoorAus(char *s, SFPOS SfPos);
void	Error(char *message);

extern int Enable_Abort;

#endif


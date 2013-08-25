// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PSFAST_H
#define PSFAST_H

#include "main.h"
#include "psconf.h"

extern void check1(Config &conf, int n, uint4 *pati[]);
extern void check2(Config &conf, int n, uint4 *pati[]);
extern void check3(Config &conf, int n, uint4 *pati[]);
extern void checkn(int sq_num, Config &conf, int n, uint4 *pati[]);
extern void genn(int sq_num, int n, uint4 *pati[], uint4 *pgen);
extern void statn(Config &conf, int n, uint4 *pati[], 
		  vector<BoardInfo> &infos, sint4 &res_sum, sint4 &res_square);


#endif


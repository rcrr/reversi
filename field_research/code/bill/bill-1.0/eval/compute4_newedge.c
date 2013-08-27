/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * like compute4(), but for the new edge table
 *
 * Joe Keane 1/16/87 created from eval.c
 * Joe Keane 4/12/87 rewritten
 */

#include <node.h>
#include <bill-features.h>
#include <color.h>
#include <extract.h>
#include <pack.h>

static int mscore_min = -200;
static void mscore_break();
extern float New_Edge_Scale;

/* how much to subtract from the totaled-up wscore to make it signed
   again (had to make it unsigned to get the bit field stuff to work
   right: ie., so we could just add the whole int w/o worrying about
   bit fields.  The 27 is the # of additions to pwmscore, including the
   initial assignment, so it's the # of times we've added the offset
   from signed to unsigned. */
#define W_FUDGE (27 * (1 << (W_ACTUAL - 1)));

compute4_newedge (board, color, features)
     board_t board;
     color_t color;
     feature_t features[4];
{
  register square_t (*b)[8] = (square_t (*)[8]) board; /* this BETTER be in a register */
  unsigned int	  emoves, rmoves, pwmscore, pwmscore2;
  int		  index_H1, index_H8, index_Va, index_Vh, index_D80_1, index_D80_2;

  {
    extern int Disp[2][12][4];
    register int    (*d)[4] = Disp[color],
		    index;
    extern short    New_Edge_Table[59049];	/* short to save space */
    short	    escore;
    extern unsigned char tobyte80[6561], tobyte44[6561], tobyte53[6561], tobyte60[729], tobyte70[2187];
    extern struct moves
      {
	unsigned int emoves, rmoves;
      }
		    H1[256], H2[256], H3[256], H4[256], H5[256], H6[256], H7[256], H8[256],
		    Va[256], Vb[256], Vc[256], Vd[256], Ve[256], Vf[256], Vg[256], Vh[256],
		    D80_1[256], D80_2[256],
		    D44_1[256], D44_2[256],
		    D53_1[256], D53_2[256], D53_3[256], D53_4[256],
		    D60_1[64], D60_2[64], D60_3[64], D60_4[64],
		    D70_1[128], D70_2[128], D70_3[128], D70_4[128];
    register struct moves *q;
    extern unsigned int
		    HV_Table1[6561][4], HV_Table2[6561], HV_Table3[6561],
		    Diag8[6561][2][2], Diag7[2187], Diag6[729], Diag5[243], Diag4[81], Diag53[6561], Diag44[6561];

    index_H1 = index = d[0][b[0][0]] + d[1][b[0][1]] + d[2][b[0][3]] + d[3][b[0][3]] + d[4][b[0][4]] + d[5][b[0][5]] + d[6][b[0][6]] + d[7][b[0][7]];
    escore = New_Edge_Table[index + d[8][b[1][1]] + d[9][b[1][6]]]; /* not += */
    emoves = H1[tobyte80[index]].emoves;	/* not |=, no rmoves */

    index = d[0][b[1][0]] + d[1][b[1][1]] + d[2][b[1][2]] + d[3][b[1][3]] + d[4][b[1][4]] + d[5][b[1][5]] + d[6][b[1][6]] + d[7][b[1][7]];
    pwmscore = HV_Table1[index][d[10][b[0][0]] + d[11][b[0][7]]]; /* not += */
    q = &H2[tobyte80[index]];
    emoves |= q->emoves;
    rmoves = q->rmoves;			/* not |= */

    index = d[0][b[2][0]] + d[1][b[2][1]] + d[2][b[2][2]] + d[3][b[2][3]] + d[4][b[2][4]] + d[5][b[2][5]] + d[6][b[2][6]] + d[7][b[2][7]];
    pwmscore += HV_Table2[index];
    q = &H3[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[3][0]] + d[1][b[3][1]] + d[2][b[3][2]] + d[3][b[3][3]] + d[4][b[3][4]] + d[5][b[3][5]] + d[6][b[3][6]] + d[7][b[3][7]];
    pwmscore += HV_Table3[index];
    q = &H4[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[4][0]] + d[1][b[4][1]] + d[2][b[4][2]] + d[3][b[4][3]] + d[4][b[4][4]] + d[5][b[4][5]] + d[6][b[4][6]] + d[7][b[4][7]];
    pwmscore += HV_Table3[index];
    q = &H5[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[5][0]] + d[1][b[5][1]] + d[2][b[5][2]] + d[3][b[5][3]] + d[4][b[5][4]] + d[5][b[5][5]] + d[6][b[5][6]] + d[7][b[5][7]];
    pwmscore += HV_Table2[index];
    q = &H6[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[6][0]] + d[1][b[6][1]] + d[2][b[6][2]] + d[3][b[6][3]] + d[4][b[6][4]] + d[5][b[6][5]] + d[6][b[6][6]] + d[7][b[6][7]];
    pwmscore += HV_Table1[index][d[10][b[7][0]] + d[11][b[7][7]]];
    q = &H7[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index_H8 = index = d[0][b[7][0]] + d[1][b[7][1]] + d[2][b[7][2]] + d[3][b[7][3]] + d[4][b[7][4]] + d[5][b[7][5]] + d[6][b[7][6]] + d[7][b[7][7]];
    escore += New_Edge_Table[index + d[8][b[6][1]] + d[9][b[6][6]]];
    emoves |= H8[tobyte80[index]].emoves;	/* no rmoves */

    index_Va = index = d[0][b[0][0]] + d[1][b[1][0]] + d[2][b[2][0]] + d[3][b[3][0]] + d[4][b[4][0]] + d[5][b[5][0]] + d[6][b[6][0]] + d[7][b[7][0]];
    escore += New_Edge_Table[index + d[8][b[1][1]] + d[9][b[6][1]]];
    emoves |= Va[tobyte80[index]].emoves;	/* no rmoves */

    index = d[0][b[0][1]] + d[1][b[1][1]] + d[2][b[2][1]] + d[3][b[3][1]] + d[4][b[4][1]] + d[5][b[5][1]] + d[6][b[6][1]] + d[7][b[7][1]];
    pwmscore += HV_Table1[index][d[10][b[0][0]] + d[11][b[7][0]]];
    q = &Vb[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][2]] + d[1][b[1][2]] + d[2][b[2][2]] + d[3][b[3][2]] + d[4][b[4][2]] + d[5][b[5][2]] + d[6][b[6][2]] + d[7][b[7][2]];
    pwmscore += HV_Table2[index];
    q = &Vc[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][3]] + d[1][b[1][3]] + d[2][b[2][3]] + d[3][b[3][3]] + d[4][b[4][3]] + d[5][b[5][3]] + d[6][b[6][3]] + d[7][b[7][3]];
    pwmscore += HV_Table3[index];
    q = &Vd[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][4]] + d[1][b[1][4]] + d[2][b[2][4]] + d[3][b[3][4]] + d[4][b[4][4]] + d[5][b[5][4]] + d[6][b[6][4]] + d[7][b[7][4]];
    pwmscore += HV_Table3[index];
    q = &Ve[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][5]] + d[1][b[1][5]] + d[2][b[2][5]] + d[3][b[3][5]] + d[4][b[4][5]] + d[5][b[5][5]] + d[6][b[6][5]] + d[7][b[7][5]];
    pwmscore += HV_Table2[index];
    q = &Vf[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][6]] + d[1][b[1][6]] + d[2][b[2][6]] + d[3][b[3][6]] + d[4][b[4][6]] + d[5][b[5][6]] + d[6][b[6][6]] + d[7][b[7][6]];
    pwmscore += HV_Table1[index][d[10][b[0][7]] + d[11][b[7][7]]];
    q = &Vg[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index_Vh = index = d[0][b[0][7]] + d[1][b[1][7]] + d[2][b[2][7]] + d[3][b[3][7]] + d[4][b[4][7]] + d[5][b[5][7]] + d[6][b[6][7]] + d[7][b[7][7]];
    escore += New_Edge_Table[index + d[8][b[1][6]] + d[9][b[6][6]]];
    features[0] = escore * New_Edge_Scale;
    emoves |= Vh[tobyte80[index]].emoves; /* no rmoves */

    index_D80_1 = index = d[0][b[0][0]] + d[1][b[1][1]] + d[2][b[2][2]] + d[3][b[3][3]] + d[4][b[4][4]] + d[5][b[5][5]] + d[6][b[6][6]] + d[7][b[7][7]];
    q = &D80_1[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index_D80_2 = index = d[0][b[7][0]] + d[1][b[6][1]] + d[2][b[5][2]] + d[3][b[4][3]] + d[4][b[3][4]] + d[5][b[2][5]] + d[6][b[1][6]] + d[7][b[0][7]];
    q = &D80_2[tobyte80[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[4][0]] + d[1][b[5][1]] + d[2][b[6][2]] + d[3][b[7][3]] + d[4][b[0][4]] + d[5][b[1][5]] + d[6][b[2][6]] + d[7][b[3][7]];
    pwmscore += Diag44[index];
    q = &D44_1[tobyte44[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[7][4]] + d[1][b[6][5]] + d[2][b[5][6]] + d[3][b[4][7]] + d[4][b[3][0]] + d[5][b[2][1]] + d[6][b[1][2]] + d[7][b[0][3]];
    pwmscore += Diag44[index];
    q = &D44_2[tobyte44[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][5]] + d[1][b[1][6]] + d[2][b[2][7]] + d[3][b[0][3]] + d[4][b[1][4]] + d[5][b[2][5]] + d[6][b[3][6]] + d[7][b[4][7]];
    pwmscore2 = Diag53[index];
    q = &D53_1[tobyte53[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[2][0]] + d[1][b[1][1]] + d[2][b[0][2]] + d[3][b[4][0]] + d[4][b[3][1]] + d[5][b[2][2]] + d[6][b[1][3]] + d[7][b[0][4]];
    pwmscore2 += Diag53[index];
    q = &D53_2[tobyte53[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[5][0]] + d[1][b[6][1]] + d[2][b[7][2]] + d[3][b[3][0]] + d[4][b[4][1]] + d[5][b[5][2]] + d[6][b[6][3]] + d[7][b[7][4]];
    pwmscore2 += Diag53[index];
    q = &D53_3[tobyte53[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[7][5]] + d[1][b[6][6]] + d[2][b[5][7]] + d[3][b[7][3]] + d[4][b[6][4]] + d[5][b[5][5]] + d[6][b[4][6]] + d[7][b[3][7]];
    pwmscore2 += Diag53[index];
    q = &D53_4[tobyte53[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][2]] + d[1][b[1][3]] + d[2][b[2][4]] + d[3][b[3][5]] + d[4][b[4][6]] + d[5][b[5][7]];
    pwmscore2 += Diag6[index];
    q = &D60_1[tobyte60[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[5][0]] + d[1][b[4][1]] + d[2][b[3][2]] + d[3][b[2][3]] + d[4][b[1][4]] + d[5][b[0][5]];
    pwmscore2 += Diag6[index];
    q = &D60_2[tobyte60[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[2][0]] + d[1][b[3][1]] + d[2][b[4][2]] + d[3][b[5][3]] + d[4][b[6][4]] + d[5][b[7][5]];
    pwmscore2 += Diag6[index];
    q = &D60_3[tobyte60[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[7][2]] + d[1][b[6][3]] + d[2][b[5][4]] + d[3][b[4][5]] + d[4][b[3][6]] + d[5][b[2][7]];
    pwmscore2 += Diag6[index];
    q = &D60_4[tobyte60[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[0][1]] + d[1][b[1][2]] + d[2][b[2][3]] + d[3][b[3][4]] + d[4][b[4][5]] + d[5][b[5][6]] + d[6][b[6][7]];
    pwmscore2 += Diag7[index];
    q = &D70_1[tobyte70[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[6][0]] + d[1][b[5][1]] + d[2][b[4][2]] + d[3][b[3][3]] + d[4][b[2][4]] + d[5][b[1][5]] + d[6][b[0][6]];
    pwmscore2 += Diag7[index];
    q = &D70_2[tobyte70[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[1][0]] + d[1][b[2][1]] + d[2][b[3][2]] + d[3][b[4][3]] + d[4][b[5][4]] + d[5][b[6][5]] + d[6][b[7][6]];
    pwmscore2 += Diag7[index];
    q = &D70_3[tobyte70[index]];
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    index = d[0][b[7][1]] + d[1][b[6][2]] + d[2][b[5][3]] + d[3][b[4][4]] + d[4][b[3][5]] + d[5][b[2][6]] + d[6][b[1][7]];
    pwmscore2 += Diag7[index];
    q = &D70_4[tobyte70[index]];		/* problem in Diag7 */
    emoves |= q->emoves;
    rmoves |= q->rmoves;

    pwmscore2 += Diag8[index_D80_1][EXTRACT_BIT(rmoves, 16)][EXTRACT_BIT(rmoves, 0)] + Diag8[index_D80_2][EXTRACT_BIT(rmoves, 5)][EXTRACT_BIT(rmoves, 21)]; /* Diag8 hacks */
  }

  {
    packed mess, mess2;
    extern short    Count_Rest[65536],	/* interior move worth */
		    Count_Edge[256],	/* edge move worth */
                    Edge_Penalty[65536]; /* edge move penalty */
    extern double Mscale;
    int mscore_r, mscore_s, mscore_e0, mscore_e1, mscore_e2, mscore_e3;
    register int    mscore,
    		    edge_moves;
    extern unsigned short
		    String_To_Quat[256], /* to convert to base 4 */
                    Edge_To_Quat[6561];	/* ditto, for edge index */

    mess.all = pwmscore;
    mess2.all = pwmscore2;
    features[1] = mess.scores.pscore + mess2.scores.pscore;
    features[2] = mess.scores.wscore + mess2.scores.wscore - W_FUDGE;

    mscore_r = (Count_Rest[EXTRACT_WORD(rmoves, 0)] + Count_Rest[EXTRACT_WORD(rmoves, 1)]) * Mscale;
    mscore_s = mess.scores.mscore + mess2.scores.mscore;
    edge_moves = EXTRACT_BYTE(emoves, 0);
    mscore_e0 = Count_Edge[edge_moves] - Edge_Penalty[String_To_Quat[edge_moves] + Edge_To_Quat[index_H1]];
    edge_moves = EXTRACT_BYTE(emoves, 1);
    mscore_e1 = Count_Edge[edge_moves] - Edge_Penalty[String_To_Quat[edge_moves] + Edge_To_Quat[index_Vh]];
    edge_moves = EXTRACT_BYTE(emoves, 2);
    mscore_e2 = Count_Edge[edge_moves] - Edge_Penalty[String_To_Quat[edge_moves] + Edge_To_Quat[index_H8]];
    edge_moves = EXTRACT_BYTE(emoves, 3);
    mscore_e3 = Count_Edge[edge_moves] - Edge_Penalty[String_To_Quat[edge_moves] + Edge_To_Quat[index_Va]];
    mscore = mscore_r - mscore_s + mscore_e0 + mscore_e1 + mscore_e2 + mscore_e3;
    if (mscore < mscore_min)
	mscore_break(mscore);
    features[3] = mscore > -200 ? mscore : -200;
  }
}


static void mscore_break(mscore)
    int mscore;
{
}

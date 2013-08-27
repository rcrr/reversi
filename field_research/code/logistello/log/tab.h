// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef TAB_H
#define TAB_H

#include "sboard.h"




#define DISC_MIN	8
#define DISC_MAX	63
#define TAB_LEN		(DISC_MAX-DISC_MIN+1)

#define EXTR_PC		(0.06)	/* extrapolation percentage */


#define I0	12
#define IN	60
#define INUM	12
#define IWIDTH	(48/INUM)


#define V_MAX		254	/* maximum value in table */


#define INFO_NUM	1	/* Info-Buffer length */


 
typedef struct {

  int Index, DiscNum, DeltaY, DeltaN;

} DELTA_INFO;


typedef struct {

  int Y, N;

} FILE_ENTRY;



typedef struct _DELTA_BUFFER {

  uint1      DiscNum, SubDiscNum;
  bool       Big;
  DELTA_INFO Infos[INFO_NUM];
  int        InfoNum;
  FILE_ENTRY *entries;
  FILE_ENTRY *sub;
  float      *tab;
  sint1      *ftab;
  int        *trans;

} DELTA_BUFFER;



typedef struct XXX {

  int	     N, Y, Nnew, Ynew, Nnew2, Ynew2;
  float	     Pold, Pnew, Ptab;
  bool       valid;

} PAT_INFO[65];


#ifdef ALPHA
typedef int	TABTYPE;
#else
typedef sint1	TABTYPE;
#endif

typedef TABTYPE *TABLE;






extern DELTA_BUFFER *NewDeltaBuffer(int DiscNum, bool Big, int SubDiscNum);
extern void	    FreeDeltaBuffer(DELTA_BUFFER **ppdb);
extern void	    FlushDeltaBuffer(DELTA_BUFFER *pdb);
extern void	    WriteDeltaBuffer(DELTA_BUFFER *pdb, int Index, int Num, int y, int n);

extern DELTA_BUFFER *GetDeltaBuffer(char *FileName);
extern void	    SaveDeltaBuffer(DELTA_BUFFER *pdb, char *FileName);


extern void	    ComputeP0(DELTA_BUFFER *pdb, float P0[65]);
extern int	    ComputePatInfo(DELTA_BUFFER *pdb, float *P0, int pnum,
					PAT_INFO PInf);
extern sint1	    *ComputeTab(DELTA_BUFFER *pdb);
extern sint1	    *ComputeBigTab(DELTA_BUFFER *pdb);
extern void	    SymmDeltaBuffer(DELTA_BUFFER *pdb);
extern void         SubUpdate(DELTA_BUFFER *pdb, int Index, int y, int n); 

extern void	    PattOut(FILE *fp, int n, int SteinAnz);

extern void	    Smooth(PAT_INFO PInf);

extern void         SteinEinfAus(FILE *fp, PARTEI Partei);

extern int Pot3[18];
extern bool f_tabout;

#endif

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


typedef struct _STAIR {

  float x, y;

  struct _STAIR *pstair;

} STAIR;


typedef struct {

  int   n;
  STAIR *s;

  float pmax;

} DISTR;


extern float Phi(float x);
extern void  MeanVar(DISTR *pdis, float *pmean, float *pvar);
extern DISTR NormalDistr(float mean, float var, float step, int num);
extern DISTR OnePeak(float x);
extern DISTR Triangle(int n, float mean, float step, float p0);
extern float GetMachineEps(void);
extern DISTR NewDistr(int n);
extern void  FreeDistr(DISTR *pdistr);
extern void  CopyDistr(DISTR *dest, DISTR *src);
extern void  PrintDistr(char *name, DISTR *pdistr);
extern void  StripDistr(DISTR *pdistr);
extern void  ResolveEqualAbs(DISTR *pdistr);
extern void  CompressDistr(DISTR *pdistr, int k);
extern void  Dens2Distr(DISTR *pdistr);
extern void  Distr2Dens(DISTR *pdistr);
extern void  NegateDistr(DISTR *pdistr, float zero);
extern DISTR MaxDistr(DISTR *pdistr, int n);
extern DISTR NegaMaxDistr(DISTR *pdistr, int n);
extern void  ProbMax(DISTR *pdistr, int n);
extern float NormDiff(DISTR *pda, DISTR *pdb);
extern DISTR PNegaMaxDistr(DISTR *pdistr, int n, float *pmax);

#define MAX_N 100

extern void  DMax(DISTR *pdistr, int n);

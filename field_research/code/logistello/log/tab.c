// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// table stuff / 2.94

#include "main.h"
#include "tab.h"
#include "crt.h"
#include "fpatt.h"
#include "patt.h"
#include "eval.h"


#define TEST		false

const bool SYMMETRICAL = true;
const REAL FACTOR      = 0.5;   // set SYMMETRICAL to false if != 0.5


bool f_tabout=false;

int Pot3[18] = { 
  1, 3, 9, 27, 81, 243, 729, 2187, 6561, 
  19683, 59049, 177147, 531441, 1594323, 4782969, 14348907,
  43046721, 129140163
};


void SteinEinfAus(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}



void PattOut(FILE *fp, int n, int discnum)
{
  int i, r, Cont[20];

  if (discnum > 20) Error("PattOut: too many discs");

  FOR (i, discnum) {

    r = n % 3;
    
    if      (r == 0) Cont[i] = WHITE;
    else if (r == 1) Cont[i] = LEER;
    else             Cont[i] = BLACK;

    n /= 3;
  }

  FOR (i, discnum) SteinEinfAus(fp, Cont[discnum-1-i]);

}
  



int compDELTA_INFO(const void *a, const void *b)
{
  int r;

  r = ((DELTA_INFO*) a)->Index - ((DELTA_INFO*) b)->Index;

  if (r) return r;

  r = ((DELTA_INFO*) a)->DiscNum - ((DELTA_INFO*) b)->DiscNum;

  return r;
}





DELTA_BUFFER *NewDeltaBuffer(int DiscNum, bool Big, int SubDiscNum)
{
  DELTA_BUFFER *pdb;
  int tab_len = Big ? 1 : TAB_LEN;

  if (DiscNum < 1 || DiscNum > 14) Error("NewDeltaBuffer: DiscNum?");

  pdb = (DELTA_BUFFER*) malloc(sizeof(DELTA_BUFFER));

  if (!pdb) Error("NewDeltaBuffer: no memory");


  pdb->InfoNum = 0;
  pdb->DiscNum = DiscNum;
  pdb->SubDiscNum = SubDiscNum;
  pdb->Big     = Big;

  pdb->entries = (FILE_ENTRY*) calloc(sizeof(FILE_ENTRY), Pot3[DiscNum] * tab_len);
  pdb->tab     = (float*) malloc(tab_len * Pot3[DiscNum] * sizeof(float));

  if (!pdb->tab || !pdb->entries) Error("NewDeltaBuffer: no memory");

  pdb->sub = 0;
  pdb->trans = 0;

  if (Big) {

    if (SubDiscNum) {

      if (SubDiscNum < 1 || SubDiscNum > 14) Error("SubDiscNum?");

      pdb->sub =  (FILE_ENTRY*) calloc(sizeof(FILE_ENTRY), Pot3[SubDiscNum]);    
      if (!pdb->sub) Error("NewDeltaBuffer: no memory");

      pdb->trans = (int*) malloc(sizeof(int) * Pot3[DiscNum]);
      if (!pdb->trans) Error("NewDeltaBuffer: no memory");

    }

  } else if (SubDiscNum) Error("SubDiscNum only for big tables");

  return pdb;
}



void FreeDeltaBuffer(DELTA_BUFFER **ppdb)
{
  free((char*)(*ppdb)->tab);
  free((char*)(*ppdb)->entries);
  free((char*)(*ppdb)->sub);
  free((char*)(*ppdb)->trans);
  free((char*)(*ppdb));

  *ppdb = 0;
}



void FlushDeltaBuffer(DELTA_BUFFER *pdb)
{
  int  i, factor, offset;
  DELTA_INFO *Infos=pdb->Infos;


  if (!pdb->InfoNum) return;


  qsort((char*)Infos, (size_t) pdb->InfoNum, sizeof(DELTA_INFO),
	 compDELTA_INFO);


/*printf("flushing big=%d\n", pdb->Big);*/


  factor = Pot3[pdb->DiscNum];

  FOR (i, pdb->InfoNum) {

    if (pdb->Big) 
      offset = Infos[i].Index;
    else
      offset = (Infos[i].DiscNum - DISC_MIN) * factor + Infos[i].Index;

/*
if (offset >= Pot3[10] || offset < 0) { printf("o=%d\n", offset); exit(10); }
*/ 

    if (pdb->entries[offset].N > MAXINT-10000) Error("n int overflow");
    if (abs(pdb->entries[offset].Y) > MAXINT-10000) Error("y int overflow");

    pdb->entries[offset].N += Infos[i].DeltaN;
    pdb->entries[offset].Y += Infos[i].DeltaY;

  }

  pdb->InfoNum = 0;
}

#if 0
extern SPFELD sf0;
#endif


void WriteDeltaBuffer(DELTA_BUFFER *pdb, int Index, int Num, int y, int n)
{
  DELTA_INFO *pdi = &pdb->Infos[pdb->InfoNum];

  //  if (Num > 58) return;

#if 0
if (Index == 0) {

  PattOut(stdout, Index, pdb->DiscNum); printf(" n=%d v=%d\n", Num, Value);
  SfAus(&sf0, 0, 0);
}
#endif

/*
if (Index < 0 || Index >= Pot3[10]) { printf("I=%d\n", Index); exit(10); }
*/

  pdi->Index   = Index;
  pdi->DiscNum = Num;
  pdi->DeltaN  = n;
  pdi->DeltaY  = y;

  pdb->InfoNum++;
 
  if (pdb->InfoNum >= INFO_NUM) FlushDeltaBuffer(pdb);
}


DELTA_BUFFER *GetDeltaBuffer(char *FileName)
{
  int   fd, len;
  uint1 discnum, subdiscnum;
  bool  big;
  DELTA_BUFFER *pdb;
  int tab_len;

  if (sizeof(big) != sizeof(pdb->Big)) Error("different sizes: big");
  if (sizeof(discnum) != sizeof(pdb->DiscNum)) Error("different sizes: discnum");
  if (sizeof(subdiscnum) != sizeof(pdb->SubDiscNum)) Error("different sizes: subdiscnum");

  if ((fd=open(FileName, O_RDONLY)) < 0) return 0;

  if (read(fd, (char*)&discnum, sizeof(discnum)) != sizeof(discnum) ||
      discnum < 1 || discnum > 14) { 

    close(fd); return 0; 
  }

//printf("disc# = %d\n", discnum);

  if (read(fd, (char*)&big, sizeof(big)) != sizeof(big)) { 

    close(fd); return 0; 
  }

  subdiscnum = 0;

  if (big) {

    if (read(fd, (char*)&subdiscnum, sizeof(subdiscnum)) != sizeof(subdiscnum) ||
	subdiscnum > discnum) { 

      close(fd); return 0; 
    }

  }

  tab_len = big ? 1 : TAB_LEN;

  pdb = NewDeltaBuffer(discnum, big, subdiscnum);

  len = tab_len * Pot3[discnum] * sizeof(FILE_ENTRY);

  printf("reading %s\n", FileName);

  if (read(fd, (char*)pdb->entries, len) != len) { close(fd); return 0; }


  if (subdiscnum) {

    if (read(fd, (char*)pdb->sub, Pot3[subdiscnum]*sizeof(FILE_ENTRY)) !=
        int(Pot3[subdiscnum]*sizeof(FILE_ENTRY)) ) {

      close(fd); return 0; 
    }

    if (read(fd, (char*)pdb->trans, Pot3[discnum] * sizeof(int)) !=
        int(Pot3[discnum] * sizeof(int))) {

      close(fd); return 0; 
    }
  }

  close(fd);

  return pdb;
}


void SaveDeltaBuffer(DELTA_BUFFER *pdb, char *FileName)
{
  int fd, len;
  int tab_len = pdb->Big ? 1 : TAB_LEN;


  unlink(FileName);

  if ((fd=open(FileName, O_WRONLY | O_CREAT, 0777)) < 0) 
    Error("SaveTab: can't open file");

  write(fd, (char*)&pdb->DiscNum,    sizeof(pdb->DiscNum));
  write(fd, (char*)&pdb->Big,        sizeof(pdb->Big));

  if (pdb->Big) write(fd, (char*)&pdb->SubDiscNum, sizeof(pdb->SubDiscNum));

  len = tab_len * Pot3[pdb->DiscNum] * sizeof(FILE_ENTRY);

  printf("writing %s\n", FileName);

  write(fd, (char*)pdb->entries, len);

  if (pdb->SubDiscNum) {

    write(fd, (char*)pdb->sub,   Pot3[pdb->SubDiscNum] * sizeof(FILE_ENTRY));
    write(fd, (char*)pdb->trans, Pot3[pdb->DiscNum] * sizeof(int));

  }


  close(fd);
}



void SubUpdate(DELTA_BUFFER *pdb, int Index, int y, int n)
{
  pdb->sub[Index].N += n;
  pdb->sub[Index].Y += y;  
}





void SymmDeltaBuffer(DELTA_BUFFER *pdb)
{
  int i, *mirror, *symm, num, pnum, discs;
  FILE_ENTRY *pe;


  if (pdb->Big) Error("SymmDeltaBuffer does not support big tables");

  if (pdb->DiscNum > 14) Error("discnum > 14");

  num = Pot3[pdb->DiscNum];

  mirror = (int*) malloc(num * sizeof(*mirror));

  if (!mirror) Error("no memory");
 
  symm = (int*) malloc(num * sizeof(*symm));

  if (!symm) Error("no memory");
 


  FOR (pnum, num) {

    int p[20], n=pnum;

    FOR (i, pdb->DiscNum) { p[i] = n % 3; n /= 3; }

    n = 0;

    FOR (i, pdb->DiscNum) n = n * 3 + p[i];

    mirror[pnum] = n;

#if 0
if (pnum == 3280) printf("%d %d\n", pnum, n);
#endif


  }


  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

    memset(symm, 0, sizeof(*symm) * num);


    pe = &pdb->entries[(discs-DISC_MIN)*num];

    FOR (pnum, num) {

      if (!symm[pnum]) {

        int N = pe[pnum].N + pe[mirror[pnum]].N,
            Y = pe[pnum].Y + pe[mirror[pnum]].Y;
#if 0
if (N) {

PattOut(stdout, pnum, pdb->DiscNum); printf(" : ");
PattOut(stdout, mirror[pnum], pdb->DiscNum); printf(" - ");

printf("%d %d: %d %d %d %d -> %d %d\n", pnum, mirror[pnum], 
pe[pnum].Y, pe[mirror[pnum]].Y, pe[pnum].N, pe[mirror[pnum]].N, Y, N);
}
#endif

        symm[pnum] = symm[mirror[pnum]] = true;

        pe[pnum].N = pe[mirror[pnum]].N = N;
        pe[pnum].Y = pe[mirror[pnum]].Y = Y;
      }
    }

  }


  free((char*)symm);
  free((char*)mirror);
}



void SearchP(PAT_INFO PInf, int n, int start, int end)
{
  int i, discs, N=0, Y=0;

  if (start >= end) Error("start >= end");



  for (discs=start; discs <= end && N < EXTR_PC * n; discs += 2) {

    N += PInf[discs].N; Y += PInf[discs].Y;

  }

  for (i=discs-2; i >= start; i -= 2) {

/* printf("%d: %d %d\n", i, N, Y); */
    PInf[i].Nnew = N;
    PInf[i].Ynew = Y;
  }

}


void SearchM(PAT_INFO PInf, int n, int start, int end)
{
  int i, discs, N=0, Y=0;

  if (start <= end) Error("start <= end");


  for (discs=start; discs >= end && N < EXTR_PC * n; discs -= 2) {

    N += PInf[discs].N; Y += PInf[discs].Y;

  }

  for (i=discs+2; i <= start; i += 2) {
    PInf[i].Nnew = N;
    PInf[i].Ynew = Y;
  }

}



/* smooth values Pnew -> Ptab */

void Smooth(PAT_INFO PInf)
{
  int discs, i;


  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

    int   N=0;
    float P=0;

    for (i=discs-2; i <= discs+1; i++) {

      int j;

      if      (i < DISC_MIN) j = DISC_MIN;
      else if (i > DISC_MAX) j = DISC_MAX;
      else 		     j = i;

      P += PInf[j].Pnew;
      N++;

    }

    PInf[discs].Ptab = P/N;

  }
}



/* compute pattern information and return number of pattern occurence */

int ComputePatInfo(DELTA_BUFFER *pdb, float *P0, int pnum, PAT_INFO PInf)
{
  int i, NS=0, discs;


  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

    PInf[discs].Y = pdb->entries[(discs-DISC_MIN)*Pot3[pdb->DiscNum] + pnum].Y;
    PInf[discs].N = pdb->entries[(discs-DISC_MIN)*Pot3[pdb->DiscNum] + pnum].N;

    NS += PInf[discs].N;

  }


  /* extrapolate data using a look-ahead-mean */


  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {
    PInf[discs].Nnew = PInf[discs].N;
    PInf[discs].Ynew = PInf[discs].Y;
  }


  /* searching min discnumber */

  SearchP(PInf, NS, DISC_MIN,   DISC_MAX);
  SearchP(PInf, NS, DISC_MIN+1, DISC_MAX);

  /* searching max discnumber */

  SearchM(PInf, NS, DISC_MAX,   DISC_MIN);
  SearchM(PInf, NS, DISC_MAX-1, DISC_MIN);



#if 0
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

printf("(%d %d) %d %d, ", PInf[discs].N, PInf[discs].Y, PInf[discs].Nnew, PInf[discs].Ynew);

      }
printf("\n");
#endif




/* smooth data using a small window */


#define WL	(-4)
#define WR	(+3)
#define WNUM	(WR-WL+1)


  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

    int   N=0, Y=0;


    for (i=discs+2*WL; i <= discs+2*WR; i += 2) {

      int j;


      if      (i < DISC_MIN) j = DISC_MIN;
      else if (i > DISC_MAX) j = DISC_MAX;
      else 		     j = i;

      N += PInf[j].Nnew; Y += PInf[j].Ynew;

    }

    PInf[discs].Nnew2 = N; 
    PInf[discs].Ynew2 = Y; 

#if 0
printf("%d (%d %d) %d %d\n ", discs,
PInf[discs].N, PInf[discs].Y, PInf[discs].Nnew, PInf[discs].Ynew);
#endif


    PInf[discs].valid = (N != 0);
  }


  /* transform data using apriori values */

  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {
	
    int N = PInf[discs].Nnew2,
        Y = PInf[discs].Ynew2;
    float P;


    P = (Y+0.5)/(N+1.0);

    PInf[discs].Pold = P;

    if (!N)

      P = 0.5; 

    else {

      float f1=(1.0-P)/P, 
            f2=P0[discs]/(1.0-P0[discs]);

      P = 1.0/(1.0+f1*f2);

    }
 
    PInf[discs].Pnew = P;
      
  }


  return NS;
}



/* compute apriori values */

void ComputeP0(DELTA_BUFFER *pdb, float P0[65])
{
  int i, discs;


  for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

    int N=0, Y=0;
    FILE_ENTRY *pe=&pdb->entries[(discs-DISC_MIN)*Pot3[pdb->DiscNum]];

    for (i=Pot3[pdb->DiscNum]; i > 0; i--) { 
   
      N += pe->N; Y += pe->Y;
      pe++;
    }

    P0[discs] = ((float)Y+0.5)/(N+1.0);
  }
}





sint1 *ComputeTab(DELTA_BUFFER *pdb)
{
  int   i, j, discs, pnum, N1, N2;
  sint1 *tab;
  PAT_INFO PInf, PInfC;
  float  P0[65];



  tab = (sint1*) malloc((INUM+1)*Pot3[pdb->DiscNum]);

  if (!tab) Error("ComputeTab: no memory");


  ComputeP0(pdb, P0);

  pnum = Pot3[pdb->DiscNum];

  FOR (i, pnum/2+1) {   /* !!! +1 */

    N1 = ComputePatInfo(pdb, P0, i,        PInf);
    N2 = ComputePatInfo(pdb, P0, pnum-1-i, PInfC);

    if (f_tabout) {

      printf("Y/N:\n");

      PattOut(stdout, i, pdb->DiscNum); printf("  %d\n", N1);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	  printf("(%d,%d) ", PInf[discs].Y, PInf[discs].N);

      printf("\n");

      PattOut(stdout, pnum-1-i, pdb->DiscNum); printf("  %d\n", N2);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	  printf("(%d,%d) ", PInfC[discs].Y, PInfC[discs].N);
      
      printf("\n");
    }


    if (f_tabout) {

      printf("Pnew:\n");

      PattOut(stdout, i, pdb->DiscNum); printf("  %d\n", N1);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	  printf("%2d ", my_round(PInf[discs].Pnew * 100));

      printf("\n");

      PattOut(stdout, pnum-1-i, pdb->DiscNum); printf("  %d\n", N2);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	printf("%2d ", my_round(PInfC[discs].Pnew * 100));
      
      printf("\n");
    }

     
    for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {

      float p1, p2;

      p1 = FACTOR * PInf [discs].Pnew + (1.0-PInfC[discs].Pnew)*(1.0-FACTOR);
      p2 = FACTOR * PInfC[discs].Pnew + (1.0-PInf [discs].Pnew)*(1.0-FACTOR);

      PInf [discs].Pnew = p1;
      PInfC[discs].Pnew = p2;

    }


    if (f_tabout) {

      printf("Pnew (factor):\n");

      PattOut(stdout, i, pdb->DiscNum); printf("  %d\n", N1);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	printf("%2d ", my_round(PInf[discs].Pnew * 100));
      
      printf("\n");

      PattOut(stdout, pnum-1-i, pdb->DiscNum); printf("  %d\n", N2);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	printf("%2d ", my_round(PInfC[discs].Pnew * 100));
      
      printf("\n");
    }

    Smooth(PInf); Smooth(PInfC);


    if (f_tabout) {
	
      printf("smooth:\n");

      PattOut(stdout, i, pdb->DiscNum); printf("  %d\n", N1);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
	printf("%2d ", my_round(PInf[discs].Ptab * 100));

      printf("\n");

      PattOut(stdout, pnum-1-i, pdb->DiscNum); printf("  %d\n", N2);
      for (discs=DISC_MIN; discs <= DISC_MAX; discs++) 
        printf("%2d ", my_round(PInfC[discs].Ptab * 100));

      printf("\n");
    }
    
    for (discs=DISC_MIN; discs <= DISC_MAX; discs++) {
      pdb->tab[(discs-DISC_MIN)*pnum + i]        = PInf [discs].Ptab;
      pdb->tab[(discs-DISC_MIN)*pnum + pnum-1-i] = PInfC[discs].Ptab;
    }


    /* compute integer table entries */

    FOR (j, INUM+1) {

      tab[j*pnum+i] = my_round(PInf [I0+j*IWIDTH].Ptab * V_MAX - V_MAX/2);

      if (SYMMETRICAL) 
	tab[j*pnum+pnum-1-i] = -tab[j*pnum+i];
      else
	tab[j*pnum+pnum-1-i] = my_round(PInfC[I0+j*IWIDTH].Ptab * V_MAX - V_MAX/2);

    }


    if (f_tabout) {

      printf("tab:\n");

      PattOut(stdout, i, pdb->DiscNum); printf("  %d\n", N1);
      FOR (j, INUM+1) printf("%2d:%+3d ", I0+j*IWIDTH, tab[j*pnum+i]);
      printf("\n");

      PattOut(stdout, pnum-1-i, pdb->DiscNum); printf("  %d\n", N2);
      FOR (j, INUM+1) printf("%2d:%+3d ", I0+j*IWIDTH, tab[j*pnum+pnum-1-i]);
      printf("\n");

      printf("\n");
    }
  }

  return tab;
}



#define K 3


sint1 *ComputeBigTab(DELTA_BUFFER *pdb)
{
  int   i, j, pnum, spnum;
  int   N1, N1N, N2, N2N, Y1, Y1N, Y2, Y2N;
  int   SN1, SN1N, SN2, SN2N, SY1, SY1N, SY2, SY2N;
  double Q;
  sint1 *tab;


  tab = (sint1*) malloc(Pot3[pdb->DiscNum]);

  if (!tab) Error("ComputeBigTab: no memory");

  pnum  = Pot3[pdb->DiscNum];
  spnum = Pot3[pdb->SubDiscNum];

  FOR (i, pnum/2+1) {   /* !!! +1 */


    N1 = pdb->entries[i].N;
    Y1 = pdb->entries[i].Y;

    N2 = pdb->entries[pnum-1-i].N;
    Y2 = pdb->entries[pnum-1-i].Y;

    Y1N = Y1 + (N2 - Y2);
    Y2N = Y2 + (N1 - Y1);
    N1N = N2N = N1 + N2;


    j = pdb->trans[i];

    SN1 = pdb->sub[j].N;
    SY1 = pdb->sub[j].Y;

    SN2 = pdb->sub[spnum-1-j].N;
    SY2 = pdb->sub[spnum-1-j].Y;

    SY1N = SY1 + (SN2 - SY2);
    SY2N = SY2 + (SN1 - SY1);
    SN1N = SN2N = SN1 + SN2;

    
    

#if 0
printf("%d: %d %d , %d %d -> %d %d , %d %d\n", i, Y1, N1, Y2, N2, Y1N, N1N, Y2N, N2N);

printf("::: %d %d\n", pnum+i, pnum-i-1);
#endif


#if 1

    // skeleton convex combination

#define N0 40  // was 40 for b1 b2

    if (N1N >= N0) Q = (Y1N + 0.5) / (N1N + 1.0);

    else { 

      double AL = ((float)N1N)/N0;

      Q = AL*(Y1N + 0.5)/(N1N + 1.0) + (1.0 - AL)*(SY1N + 0.5)/(SN1N + 1.0);
  
    }

#else

    GY1 = K * Y1N + SY1N;
    GN1 = K * N1N + SN1N;

    GY2 = K * Y2N + SY2N;
    GN2 = K * N2N + SN2N;

    Q = (GY1+0.5)/(GN1+1.0);

#endif

    tab[i] = my_round(Q * V_MAX - V_MAX/2);

    if (SYMMETRICAL) 
      tab[pnum-1-i] = -tab[i];
    else
      tab[pnum-1-i] = my_round((1.0-Q) * V_MAX - V_MAX/2);


    if (f_tabout) {

      int k;


      PattOut(stdout, i, pdb->DiscNum); 
      printf(" %6d %6d %f\n", Y1, N1, float(Y1)/(N1+0.01));

      PattOut(stdout, j, pdb->SubDiscNum); 
      for (k=pdb->SubDiscNum; k < pdb->DiscNum; k++) printf(" ");
      printf(" %6d %6d %f\n", SY1, SN1, float(SY1)/(SN1+0.01));
  

      PattOut(stdout, pnum-1-i, pdb->DiscNum);
      printf(" %6d %6d %f\n", Y2, N2, float(Y2)/(N2+0.01));

      PattOut(stdout, spnum-1-j, pdb->SubDiscNum);
      for (k=pdb->SubDiscNum; k < pdb->DiscNum; k++) printf(" ");
      printf(" %6d %6d %f\n", SY2, SN2, float(SY2)/(SN2+0.01));

      printf("-> %6d %6d  /  %6d %6d  :  %.2f (%.2f) %+4d (%+4d)\n",
	     Y1N, N1N, SY1N, SN1N, Q, (SY1N+0.5)/(SN1N+1.0), 
	     tab[i], my_round((SY1N+0.5)/(SN1N+1.0) * V_MAX - V_MAX/2));
 
      printf("\n");
    }
  }

  return tab;
}

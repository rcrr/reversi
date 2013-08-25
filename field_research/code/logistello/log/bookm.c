// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// book manager 12.93, 2-3.94, 2.97

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "goodies.h"
#include "newgame.h"
#include "tab.h"
#include "patt.h"
#include "weight.h"
#include "book.h"

#define SFTEST	false

#define TEST	false

#if TEST
#define TESTOUT(x) cout << x"\n")
#else
#define TESTOUT(x)
#endif

const int WIN_LIMIT = 58;


typedef
  void (*UPDATEF)(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n);


typedef struct {

  char    Name[20];
  int     DiscNum;
  bool    Big;
  int     SubDiscNum;
  UPDATEF Update;  
  void    (*SymF)(DELTA_BUFFER *delta);

} PATT_LIST;



static int PattNum=0;



typedef enum { V_DIFF, V_PROB, V_CLASS } VTYPE;

static bool f_sd=false, f_sp=false, f_sc=false, f_show=false, 
     f_boards=false, f_randboards=false, f_tab=false, f_rnd=false, f_rnd2=false,
     f_bad=false, f_div=false, f_stat=false, f_resweight=false, f_libweight=false,
     f_write=false;

static float div_prob=0.0;
static int libweight=1;

// goal result density

static float result_distr[33] = {

// 0  2   4   6   8   10 12 14 16 18 20 22 24 26 28 30 32

  5, 100, 80, 40, 20, 10, 5, 2, 1,.5,.2,.1,.1,.1,.1,.1,.1,

// 34 36 38 40  42  44  46  48  50  52  54  56  58  60  62  64
   .1,.1,.1,.08,.08,.08,.06,.06,.04,.04,.04,.04,.04,.04,.04,.04
};


void _abort(void) { exit(0); }

static int  boardnum=0;
static int  maxnum=0;
static int  randnum=0;
static int  discmin=4, discmax=64, freq=1;
static int  BoardNum[65];
static int  Results[64][33];
static bool count1=false;


static DELTA_BUFFER *delta=0;

#if SFTEST
SPFELD sf0;
#endif

#define WRITEBU(x)   WriteDeltaBuffer(delta, x, DiscNum, y, n);

#define SUBUPDATE(x) SubUpdate(delta, x, y, n) 



void MakeTrans(DELTA_BUFFER *delta, int discnum, int subdiscnum, int *subseq)
{
  int i, j, n, p[20], pnum = Pot3[discnum];

  if (discnum > 14) Error("MakeTrans: discnum > 14");

  FOR (j, subdiscnum) {
    if (subseq[j] <= 0 || subseq[j] >= discnum) Error("corrupt subseq");
  }


  FOR (i, pnum) {

    n = i;

    FOR (j, discnum) {
 
      p[discnum-1-j] = n % 3;
      n /= 3;
    }

    n = 0;

    FOR (j, subdiscnum) {

      n *= 3;
      n += p[subseq[j]-1];

    }

    delta->trans[i] = n;
  }
}



void UpdateB1(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

#if SFTEST
sf0 = *psf;
#endif

  WRITEBU(PATT10(+,A1,B1,C1,D1,A2,B2,C2,D2,E1,E2));
  WRITEBU(PATT10(+,H1,G1,F1,E1,H2,G2,F2,E2,D1,D2));
  WRITEBU(PATT10(+,A8,B8,C8,D8,A7,B7,C7,D7,E8,E7));
  WRITEBU(PATT10(+,H8,G8,F8,E8,H7,G7,F7,E7,D8,D7));
  WRITEBU(PATT10(+,A1,A2,A3,A4,B1,B2,B3,B4,A5,B5));
  WRITEBU(PATT10(+,H1,H2,H3,H4,G1,G2,G3,G4,H5,G5));
  WRITEBU(PATT10(+,A8,A7,A6,A5,B8,B7,B6,B5,A4,B4));
  WRITEBU(PATT10(+,H8,H7,H6,H5,G8,G7,G6,G5,H4,G4));

  SUBUPDATE(PATT8(+,A1,B1,C1,D1,A2,B2,C2,D2));
  SUBUPDATE(PATT8(+,H1,G1,F1,E1,H2,G2,F2,E2));
  SUBUPDATE(PATT8(+,A8,B8,C8,D8,A7,B7,C7,D7));
  SUBUPDATE(PATT8(+,H8,G8,F8,E8,H7,G7,F7,E7));
  SUBUPDATE(PATT8(+,A1,A2,A3,A4,B1,B2,B3,B4));
  SUBUPDATE(PATT8(+,H1,H2,H3,H4,G1,G2,G3,G4));
  SUBUPDATE(PATT8(+,A8,A7,A6,A5,B8,B7,B6,B5));
  SUBUPDATE(PATT8(+,H8,H7,H6,H5,G8,G7,G6,G5)); 
}


int SubSeqB1[] = { 1, 2, 3, 4, 5, 6, 7, 8, 0 };


void MakeB1Tab(DELTA_BUFFER *delta)
{
  MakeTrans(delta, 10, 8, SubSeqB1);
}



void UpdateB2(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT10(+,A1,B1,C1,D1,E1,F1,G1,H1,B2,G2));
  WRITEBU(PATT10(+,H1,G1,F1,E1,D1,C1,B1,A1,G2,B2));
  WRITEBU(PATT10(+,A8,B8,C8,D8,E8,F8,G8,H8,B7,G7));
  WRITEBU(PATT10(+,H8,G8,F8,E8,D8,C8,B8,A8,G7,B7));
  WRITEBU(PATT10(+,A1,A2,A3,A4,A5,A6,A7,A8,B2,B7));
  WRITEBU(PATT10(+,H1,H2,H3,H4,H5,H6,H7,H8,G2,G7));
  WRITEBU(PATT10(+,A8,A7,A6,A5,A4,A3,A2,A1,B7,B2));
  WRITEBU(PATT10(+,H8,H7,H6,H5,H4,H3,H2,H1,G7,G2));

  SUBUPDATE(PATT8(+,A1,B1,C1,D1,E1,F1,G1,H1));
  SUBUPDATE(PATT8(+,H1,G1,F1,E1,D1,C1,B1,A1));
  SUBUPDATE(PATT8(+,A8,B8,C8,D8,E8,F8,G8,H8));
  SUBUPDATE(PATT8(+,H8,G8,F8,E8,D8,C8,B8,A8));
  SUBUPDATE(PATT8(+,A1,A2,A3,A4,A5,A6,A7,A8));
  SUBUPDATE(PATT8(+,H1,H2,H3,H4,H5,H6,H7,H8));
  SUBUPDATE(PATT8(+,A8,A7,A6,A5,A4,A3,A2,A1));
  SUBUPDATE(PATT8(+,H8,H7,H6,H5,H4,H3,H2,H1)); 
}


int SubSeqB2[] = { 1, 2, 3, 4, 5, 6, 7, 8, 0 };


void MakeB2Tab(DELTA_BUFFER *delta)
{
  MakeTrans(delta, 10, 8, SubSeqB2);
}


void UpdateLA(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT14(+,A1,B1,C1,D1,E1,F1,G1,H1,B2,C2,D2,E2,F2,G2));
  WRITEBU(PATT14(+,H1,G1,F1,E1,D1,C1,B1,A1,G2,F2,E2,D2,C2,B2));
  WRITEBU(PATT14(+,A8,B8,C8,D8,E8,F8,G8,H8,B7,C7,D7,E7,F7,G7));
  WRITEBU(PATT14(+,H8,G8,F8,E8,D8,C8,B8,A8,G7,F7,E7,D7,C7,B7));
  WRITEBU(PATT14(+,A1,A2,A3,A4,A5,A6,A7,A8,B2,B3,B4,B5,B6,B7));
  WRITEBU(PATT14(+,H1,H2,H3,H4,H5,H6,H7,H8,G2,G3,G4,G5,G6,G7));
  WRITEBU(PATT14(+,A8,A7,A6,A5,A4,A3,A2,A1,B7,B6,B5,B4,B3,B2));
  WRITEBU(PATT14(+,H8,H7,H6,H5,H4,H3,H2,H1,G7,G6,G5,G4,G3,G2));

  SUBUPDATE(PATT8(+,A1,B1,C1,D1,E1,F1,G1,H1));
  SUBUPDATE(PATT8(+,H1,G1,F1,E1,D1,C1,B1,A1));
  SUBUPDATE(PATT8(+,A8,B8,C8,D8,E8,F8,G8,H8));
  SUBUPDATE(PATT8(+,H8,G8,F8,E8,D8,C8,B8,A8));
  SUBUPDATE(PATT8(+,A1,A2,A3,A4,A5,A6,A7,A8));
  SUBUPDATE(PATT8(+,H1,H2,H3,H4,H5,H6,H7,H8));
  SUBUPDATE(PATT8(+,A8,A7,A6,A5,A4,A3,A2,A1));
  SUBUPDATE(PATT8(+,H8,H7,H6,H5,H4,H3,H2,H1)); 
}


int SubSeqLA[] = { 1, 2, 3, 4, 5, 6, 7, 8, 0 };


void MakeLATab(DELTA_BUFFER *delta)
{
  MakeTrans(delta, 14, 8, SubSeqLA);
}


void Update4x3(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT12(+,A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3));
  WRITEBU(PATT12(+,H1,G1,F1,E1,H2,G2,F2,E2,H3,G3,F3,E3));
  WRITEBU(PATT12(+,A8,B8,C8,D8,A7,B7,C7,D7,A6,B6,C6,D6));
  WRITEBU(PATT12(+,H8,G8,F8,E8,H7,G7,F7,E7,H6,G6,F6,E6));
  WRITEBU(PATT12(+,A1,A2,A3,A4,B1,B2,B3,B4,C1,C2,C3,C4));
  WRITEBU(PATT12(+,H1,H2,H3,H4,G1,G2,G3,G4,F1,F2,F3,F4));
  WRITEBU(PATT12(+,A8,A7,A6,A5,B8,B7,B6,B5,C8,C7,C6,C5));
  WRITEBU(PATT12(+,H8,H7,H6,H5,G8,G7,G6,G5,F8,F7,F6,F5));

  SUBUPDATE(PATT9(+,A1,B1,C1,A2,B2,C2,A3,B3,C3));
  SUBUPDATE(PATT9(+,H1,G1,F1,H2,G2,F2,H3,G3,F3));
  SUBUPDATE(PATT9(+,A8,B8,C8,A7,B7,C7,A6,B6,C6));
  SUBUPDATE(PATT9(+,H8,G8,F8,H7,G7,F7,H6,G6,F6));
  SUBUPDATE(PATT9(+,A1,A2,A3,B1,B2,B3,C1,C2,C3));
  SUBUPDATE(PATT9(+,H1,H2,H3,G1,G2,G3,F1,F2,F3));
  SUBUPDATE(PATT9(+,A8,A7,A6,B8,B7,B6,C8,C7,C6));
  SUBUPDATE(PATT9(+,H8,H7,H6,G8,G7,G6,F8,F7,F6)); 
}

int SubSeq4x3[] = { 1, 2, 3, 5, 6, 7, 9, 10, 11, 0 };


void Make4x3Tab(DELTA_BUFFER *delta)
{
  MakeTrans(delta, 12, 9, SubSeq4x3);
}



void Update3x3(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT9(+,A1,B1,C1,A2,B2,C2,A3,B3,C3));
  WRITEBU(PATT9(+,H1,G1,F1,H2,G2,F2,H3,G3,F3));
  WRITEBU(PATT9(+,A8,B8,C8,A7,B7,C7,A6,B6,C6));
  WRITEBU(PATT9(+,H8,G8,F8,H7,G7,F7,H6,G6,F6));
  WRITEBU(PATT9(+,A1,A2,A3,B1,B2,B3,C1,C2,C3));
  WRITEBU(PATT9(+,H1,H2,H3,G1,G2,G3,F1,F2,F3));
  WRITEBU(PATT9(+,A8,A7,A6,B8,B7,B6,C8,C7,C6));
  WRITEBU(PATT9(+,H8,H7,H6,G8,G7,G6,F8,F7,F6));

  SUBUPDATE(PATT6(+,A1,B1,C1,A2,B2,A3));
  SUBUPDATE(PATT6(+,H1,G1,F1,H2,G2,H3));
  SUBUPDATE(PATT6(+,A8,B8,C8,A7,B7,A6));
  SUBUPDATE(PATT6(+,H8,G8,F8,H7,G7,H6));
  SUBUPDATE(PATT6(+,A1,A2,A3,B1,B2,C1));
  SUBUPDATE(PATT6(+,H1,H2,H3,G1,G2,F1));
  SUBUPDATE(PATT6(+,A8,A7,A6,B8,B7,C8));
  SUBUPDATE(PATT6(+,H8,H7,H6,G8,G7,F8)); 
}

int SubSeq3x3[] = { 1, 2, 3, 4, 5, 7, 0 };


void Make3x3Tab(DELTA_BUFFER *delta)
{
  MakeTrans(delta, 9, 6, SubSeq3x3);
}



void UpdateR1(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT8(+,A1,B1,C1,D1,A2,B2,C2,D2));
  WRITEBU(PATT8(+,H1,G1,F1,E1,H2,G2,F2,E2));
  WRITEBU(PATT8(+,A8,B8,C8,D8,A7,B7,C7,D7));
  WRITEBU(PATT8(+,H8,G8,F8,E8,H7,G7,F7,E7));
  WRITEBU(PATT8(+,A1,A2,A3,A4,B1,B2,B3,B4));
  WRITEBU(PATT8(+,H1,H2,H3,H4,G1,G2,G3,G4));
  WRITEBU(PATT8(+,A8,A7,A6,A5,B8,B7,B6,B5));
  WRITEBU(PATT8(+,H8,H7,H6,H5,G8,G7,G6,G5));
}

void UpdateHV1(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

#if SFTEST
sf0 = *psf;
#endif

  WRITEBU(PATT8(+,A1,B1,C1,D1,E1,F1,G1,H1));
  WRITEBU(PATT8(+,A8,B8,C8,D8,E8,F8,G8,H8));
  WRITEBU(PATT8(+,A1,A2,A3,A4,A5,A6,A7,A8));
  WRITEBU(PATT8(+,H1,H2,H3,H4,H5,H6,H7,H8));
}

void UpdateHV2(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT8(+,A2,B2,C2,D2,E2,F2,G2,H2));
  WRITEBU(PATT8(+,A7,B7,C7,D7,E7,F7,G7,H7));
  WRITEBU(PATT8(+,B1,B2,B3,B4,B5,B6,B7,B8));
  WRITEBU(PATT8(+,G1,G2,G3,G4,G5,G6,G7,G8));
}

void UpdateHV3(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT8(+,A3,B3,C3,D3,E3,F3,G3,H3));
  WRITEBU(PATT8(+,A6,B6,C6,D6,E6,F6,G6,H6));
  WRITEBU(PATT8(+,C1,C2,C3,C4,C5,C6,C7,C8));
  WRITEBU(PATT8(+,F1,F2,F3,F4,F5,F6,F7,F8));
}

void UpdateHV4(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT8(+,A4,B4,C4,D4,E4,F4,G4,H4));
  WRITEBU(PATT8(+,A5,B5,C5,D5,E5,F5,G5,H5));
  WRITEBU(PATT8(+,D1,D2,D3,D4,D5,D6,D7,D8));
  WRITEBU(PATT8(+,E1,E2,E3,E4,E5,E6,E7,E8));
}

void UpdateD1(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT8(+,A8,B7,C6,D5,E4,F3,G2,H1));
  WRITEBU(PATT8(+,A1,B2,C3,D4,E5,F6,G7,H8));
}

void UpdateD2(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT7(+,A7,B6,C5,D4,E3,F2,G1));
  WRITEBU(PATT7(+,B8,C7,D6,E5,F4,G3,H2));
  WRITEBU(PATT7(+,A2,B3,C4,D5,E6,F7,G8));
  WRITEBU(PATT7(+,B1,C2,D3,E4,F5,G6,H7));
}

void UpdateD3(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT6(+,A6,B5,C4,D3,E2,F1));
  WRITEBU(PATT6(+,C8,D7,E6,F5,G4,H3));
  WRITEBU(PATT6(+,A3,B4,C5,D6,E7,F8));
  WRITEBU(PATT6(+,C1,D2,E3,F4,G5,H6));
}

void UpdateD4(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT5(+,A5,B4,C3,D2,E1));
  WRITEBU(PATT5(+,D8,E7,F6,G5,H4));
  WRITEBU(PATT5(+,A4,B5,C6,D7,E8));
  WRITEBU(PATT5(+,D1,E2,F3,G4,H5));
}

void UpdateD5(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT4(+,A4,B3,C2,D1));
  WRITEBU(PATT4(+,E8,F7,G6,H5));
  WRITEBU(PATT4(+,A5,B6,C7,D8));
  WRITEBU(PATT4(+,E1,F2,G3,H4));
}

void UpdateD6(DELTA_BUFFER *delta, SPFELD *psf, int DiscNum, int y, int n)
{
  sint1 *p=psf->p;

  WRITEBU(PATT3(+,A3,B2,C1));
  WRITEBU(PATT3(+,F8,G7,H6));
  WRITEBU(PATT3(+,A6,B7,C8));
  WRITEBU(PATT3(+,F1,G2,H3));
}    



PATT_LIST PattList[] = {

  { "r1",  8, false, 0, UpdateR1,  0 },
  { "hv1", 8, false, 0, UpdateHV1, SymmDeltaBuffer },
  { "hv2", 8, false, 0, UpdateHV2, SymmDeltaBuffer },
  { "hv3", 8, false, 0, UpdateHV3, SymmDeltaBuffer },
  { "hv4", 8, false, 0, UpdateHV4, SymmDeltaBuffer },
  { "d1",  8, false, 0, UpdateD1,  SymmDeltaBuffer },
  { "d2",  7, false, 0, UpdateD2,  SymmDeltaBuffer },
  { "d3",  6, false, 0, UpdateD3,  SymmDeltaBuffer },
  { "d4",  5, false, 0, UpdateD4,  SymmDeltaBuffer },
  { "d5",  4, false, 0, UpdateD5,  SymmDeltaBuffer },
  { "d6",  3, false, 0, UpdateD6,  SymmDeltaBuffer },
  { "b1", 10, true,  8, UpdateB1,  MakeB1Tab       },
  { "b2", 10, true,  8, UpdateB2,  MakeB2Tab       }, 
  { "3x3", 9, true,  6, Update3x3, Make3x3Tab      }, 
  { "4x3", 12, true, 9, Update4x3, Make4x3Tab      }, 
  { "la", 14, true,  8, UpdateLA,  MakeLATab       }, 
  { "" }
};



/********************** save stuff **********************/


static void SaveBoard(
  FILE   *fp, 
  SPFELD &sfin,
  int    discnum,
  int    ValueB, 
  int    player,
  VTYPE  vtype, 
  int    bound,
  SFPOS  next_move    // 0 if unknown (for randboards)
)
{
  SPFELD sf=sfin;

#if TEST
int x;
#endif

  if ((++boardnum % 10000) == 0) { cout << boardnum << "\r" << flush; }


  if (count1) {

    BoardNum[discnum]++;

    if (ValueB < -64 || ValueB > 64) Error("ValueB?");

    ValueB = abs(ValueB);
    if (ValueB & 1) ValueB++;
    ValueB /= 2;

    if (ValueB < 0 || ValueB > 32) Error("ValueB abs?");

    Results[discnum][ValueB]++;
    return;

  }

  if (player == WHITE) { ValueB = -ValueB; SfInvert(&sf); }

  int cla = 0;  // class


  switch (vtype) {

  case V_DIFF: 
    sf.Marke = MA_DIFF + 64 + ValueB; 
    break;

  case V_PROB: 
    Error("V_PROB not functioning");
    sf.Marke = MA_WKEIT + my_round(100*(1.0/(1.0+exp(-0.1*ValueB))));
    break;


  case V_CLASS:   

    if (ValueB == 0 && bound == 0) {
	
      sf.Marke = MA_WKEIT+50;

    } else if (ValueB >=  bound) { 

      cla = 1; sf.Marke = MA_WKEIT+99;
      
    } else if (ValueB <= -bound) {
	
      cla = 0; sf.Marke = MA_WKEIT+1; 

    } else return;

    break; 


  default: 

    Error("SaveBoard: unkown type");
  }
  

#if TEST
  if (discnum != SfAnz(psf)) Error("different");
#endif

  
  if (f_div) {

    if (FRAN <= div_prob) {

      static bool opened = false;
      static FILE *fpdiv;

      if (!opened) {

	opened = true;

	fpdiv = fopen("obook_div.sfk", "w");

	if (!fpdiv) Error("can't open obook_div.sfk");
      }

      fSfWrite(fpdiv, &sf, 1); 

      return;
    }
  }


  if (f_tab && f_sc && discnum >= DISC_MIN && discnum <= DISC_MAX) {

#if TEST
    //SfAus(&sf, 0, 0);
cout << "class=%d valb=%d (#=%d)\n", cla, ValueB, SfAnz(&sf));
#endif

    if (libweight > 1) {

      if (libweight & 1) Error("libweight must be even");

      if (bound == 0 && ValueB == 0) { // 50%

	PattList[PattNum].Update(delta, &sf, discnum, libweight/2, libweight);

      } else {

	PattList[PattNum].Update(delta, &sf, discnum, libweight*cla, libweight);

      }

    } else {

      // old method

      PattList[PattNum].Update(delta, &sf, discnum, cla, 1);

      if (bound == 0 && ValueB == 0) { /* draw: save both values */
	PattList[PattNum].Update(delta, &sf, discnum, 1-cla, 1);
      }
    }
  }


  if (f_tab && f_sd && discnum >= DISC_MIN && discnum <= DISC_MAX) {

#if TEST
    //SfAus(&sf, 0, 0);
cout << "class=%d valb=%d (#=%d)\n", cla, ValueB, SfAnz(&sf));
#endif

    if (f_resweight) {

      // n = weight

      int n = Weight(ValueB);
      int y;

      if      (ValueB > 0) y = n;
      else if (ValueB < 0) y = 0;
      else                 y = n/2;

      //cout << "v=%d y=%d n=%d\n", ValueB, y, n);


      PattList[PattNum].Update(delta, &sf, discnum, y, n);

    } else {

      // n = 1

      Error("not tested");

      PattList[PattNum].Update(delta, &sf, discnum, ValueB, 1);

    }

  }

  if (f_boards && discnum >= discmin && discnum <= discmax &&
      (!BoardNum[discnum] || IRAN % BoardNum[discnum] <= maxnum)) {

    fSfWrite(fp, &sf, 1);
  }

  if (f_randboards && discnum >= discmin && discnum <= discmax) {

    // generate randnum different successor boards and save them
    // avoid next_move

    SFPOS moves[64];
    int move_num = SfMoeglZuege(&sf, BLACK, moves);
    int save_num = randnum;
    int i, j;

    if (!move_num) return;  // no moves -> return

    if (next_move > 0) {

    // delete next_move

      FOR (i, move_num) 
	if (moves[i] == next_move) break;

      if (i >= move_num) Error("next_move not found");

      moves[i] = moves[move_num-1];
      moves[move_num-1] = 0;
      move_num--;
      if (!move_num) return;  // no moves -> return

    }

    if (save_num > move_num) save_num = move_num;

    FOR (i, save_num) {

      do {

	j = IRAN % move_num;

      } while (!moves[j]);

      SFPOS move = moves[j];

      moves[j] = 0;

      SPFELD sf1 = sf;

      if (!SfSetzen(&sf1, BLACK, move)) Error("move impossible");
 
      SfInvert(&sf1);
      fSfWrite(fp, &sf1, 1);

      // SfAus(&sf1, 0, 0);
    }
  }
}






static void fWriteBoards(
  FILE *fp, SPFELD &sfin, int discnum, Book &book, PosInfo &pi, 
  VTYPE vtype, int bound
)
{
  int     i, movenum, player, value;
  SPFELD  sf, sf1;
  SFPOS   moves[65];
  PosInfo new_pi;


  if (!pi.p_node) Error("pL=0 -1?");


  if (pi.move_index < 0) {		// branch

    int val, maxval=-100;


    TESTOUT("BRANCH");

    if (!pi.p_node->son_num) Error("no son?");

    player = pi.p_node->sons[0].moves[0].get_player();


    FOR (i, pi.p_node->son_num) {

      sf = sfin; 

      if (!SfSetzen(&sf, player, pi.p_node->sons[i].moves[0].get_move())) {

        SfAus(&sf, 0, 0); KoorAus(pi.p_node->sons[i].moves[0].get_move()); 
        cout << pi.p_node->sons[i].moves[0].get_player();

        Error("fWriteBoards: illegal move!");
      }

      new_pi.p_node     = pi.p_node;
      new_pi.move_index = i;
      new_pi.path_index = 1;

      val = pi.p_node->sons[i].value;
      if (player == WHITE) val = -val;
      if (val > maxval) maxval = val;

      fWriteBoards(fp, sf, discnum+1, book, new_pi, vtype, bound);
    }

    if (player == WHITE) maxval = -maxval;

    SaveBoard(fp, sfin, discnum, maxval, player, vtype, bound, 0);

  } else {                             // path

    sf = sfin;

    if (pi.path_index >= pi.p_node->sons[pi.move_index].move_num) {

      // path end

      new_pi.p_node     = pi.p_node->sons[pi.move_index].next;
      new_pi.move_index = -1;

      if (new_pi.p_node) fWriteBoards(fp, sf, discnum, book, new_pi, vtype, bound);
      else {

	// save board at game end

        SaveBoard(fp, sf, discnum, 
		  pi.p_node->sons[pi.move_index].value, BLACK,
		  vtype, bound, 0);
      }

    } else {


      // save next path board

      player =
	pi.p_node->sons[pi.move_index].moves[pi.path_index].get_player();

      SaveBoard(fp, sf, discnum, 
        pi.p_node->sons[pi.move_index].value,
	player,
        vtype, 
	bound,
        pi.p_node->sons[pi.move_index].moves[pi.path_index].get_move()
      );

      if (!SfSetzen(&sf, player,
          pi.p_node->sons[pi.move_index].moves[pi.path_index].get_move())) {

        SfAus(&sf, 0, 0); 
	KoorAus(pi.p_node->sons[pi.move_index].moves[pi.path_index].get_move()); 
        cout << pi.p_node->sons[pi.move_index].moves[pi.path_index].get_move();

        Error("fWriteBoards: illegal move!");
      }

      new_pi = pi;
      new_pi.path_index++;

      fWriteBoards(fp, sf, discnum+1, book, new_pi, vtype, bound);
    }
  }
}


static void fWriteGameTreeBoards(FILE *fp, Book &book, VTYPE vtype, int bound)
{
  SPFELD sf;
  PosInfo pi;

  SfGrund(&sf);

  pi.p_node = book.get_root();
  pi.move_index = -1;

  fWriteBoards(fp, sf, SfAnz(&sf), book, pi, vtype, bound);
}




int main(int argc, char **argv)
{
  int      argi, f_depth, f_uniq=false, f_alt=false, f_good=false, i;
  Book     book;
  NewGame  game, showgame;
  FILE     *fp, *fpout;
  char     *book_file=0, *alt_file=0, *draws_file=0, 
           newfile[200], *res_file = 0, 
           *rnd_file=0, *rnd2_file=0;
  int      bound;

 
  if (argc == 1) {

error:

    cerr << "*** call: obook option oko-file [-alt eval-file]\n\n\
    -sd          :    mark boards with disc-difference (.d.sfk)\n\
    -sp          :      -\"-            probabilities   (.p.sfk)\n\
    -sc bound    :      -\"-            classes         (.c.sfk)\n\
                            (diff >= bound ->1 / <= -bound -> 0)\n\
    -div prob    :    store prob*100%% examples in obook_div.sfk (don't learn)\n\
    -tab pattern [-rnd rnd-file [-rnd file2]]:\n\
                      generate table for pattern [ using also rnd-sfk-file(s)]\n\n\
                       available: ";

    for (i=0; PattList[i].Name[0]; i++) 
      cerr << PattList[i].Name << " ";

    cerr << "\n\n\
    -stat                 : compute board statistics\n\
    -resweight            : use result-weight for table approximation\n\
    -libweight w          : weight of lib-positions (compared to rnd-pos)\n\
    -boards [min max [#]] : save boards (min<=discs<=max, at most # per discnum)\n\n\
    -randboards min max # : save # boards reached after random moves\n\
    -show depth [path] [-draws draws.file]  :  show first levels of tree\n\
    -res oko-file:  print minmax-results of prefixes in file\n\
    -alt         :  find alternatives (.alt)\n\
    -good        :  find good alternatives (.good)\n\
    -uniq        :  save only different games & alt (.uniq)\n\
    -write n [m] :  write prefixes that occurred >= n times up to disc-number m\n";

    exit(20);
  }

  for (argi=1; argv[argi]; argi++) {

    if (!strcmp(argv[argi], "-stat")) {

      f_stat = true;

    } else if (!strcmp(argv[argi], "-resweight")) {

      f_resweight = true;

    } else if (!strcmp(argv[argi], "-sd")) {

      f_sd = true;

    } else if (!strcmp(argv[argi], "-sp")) {

      f_sp = true;

    } else if (!strcmp(argv[argi], "-div")) {
 
      f_div = true;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%f", &div_prob) != 1) goto error;

      if (div_prob <= 0 || div_prob >= 1) goto error;

    } else if (!strcmp(argv[argi], "-tab")) {

      f_tab = true;

      if (!argv[++argi]) goto error;

      for (i=0; PattList[i].Name[0]; i++) 
        if (!strcmp(argv[argi], PattList[i].Name)) break;

      if (!PattList[i].Name[0]) goto error;

      PattNum = i;

      if (!argv[argi+1]) goto error;

      if (!strcmp(argv[argi+1], "-rnd")) {

	f_rnd = true;

	argi++;
	argi++;

        if (!argv[argi]) goto error;
	
	rnd_file = argv[argi];

      }

      if (!strcmp(argv[argi+1], "-rnd")) {

	f_rnd2 = true;

	argi++;
	argi++;

        if (!argv[argi]) goto error;
	
	rnd2_file = argv[argi];

      }

    } else if (!strcmp(argv[argi], "-boards")) {
 
      f_boards = true;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &discmin) == 1) { 

        if (!argv[++argi]) goto error;

        if (sscanf(argv[argi], "%d", &discmax) != 1) goto error;

        if (discmin < 4 || discmin > discmax || discmax > 64) goto error;

        if (!argv[++argi]) goto error;

        if (sscanf(argv[argi], "%d", &maxnum) != 1) argi--;
        else if (maxnum < 1 || maxnum > 1000000) goto error;

      } else argi--;

    } else if (!strcmp(argv[argi], "-write")) {
 
      f_write = true;

      if (!argv[++argi]) goto error;

      if (!sscanf(argv[argi], "%d", &freq) == 1) goto error;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &discmax) != 1) goto error;

      if (discmin < 4 || discmin > discmax || discmax > 64) goto error;

    } else if (!strcmp(argv[argi], "-randboards")) {
 
      f_randboards = true;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &discmin) != 1) goto error;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &discmax) != 1) goto error;

      if (discmin < 4 || discmin > discmax || discmax > 64) goto error;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &randnum) != 1) goto error;

      if (randnum < 1 || randnum > 64) goto error;

    } else if (!strcmp(argv[argi], "-sc")) {
 
      f_sc = true;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &bound) != 1 || bound < 0 || bound > 64) 
        goto error;

    } else if (!strcmp(argv[argi], "-libweight")) {

      f_libweight = true;

      if (!argv[++argi]) goto error;

      if (sscanf(argv[argi], "%d", &libweight) != 1 || libweight < 2) goto error; 


    } else if (!strcmp(argv[argi], "-show")) {

      f_show = true;

      if (!argv[++argi] || sscanf(argv[argi], "%d", &f_depth) != 1 || 
	  f_depth < 1 || f_depth > 60) goto error;

      showgame.get_pm(0).set_raw(0);
      showgame.set_move_num(0);

      if (argv[argi+1] && argv[argi+1][0] == '+') {

        showgame.s_read(argv[++argi]);

      }

      if (argv[argi+1] && !strcmp(argv[argi+1], "-draws")) {

	argi++;

	if (!argv[argi+1]) goto error;

	draws_file = argv[++argi];

      }


    } else if (!strcmp(argv[argi], "-res")) {

      if (!argv[++argi]) goto error;

      res_file = argv[argi];

    } else if (!strcmp(argv[argi], "-uniq")) {

      f_uniq = true;

    } else if (!strcmp(argv[argi], "-alt")) {

      f_alt = true;

    } else if (!strcmp(argv[argi], "-good")) {

      f_good = true;

    } else { book_file=argv[argi++]; break; }

  }

  if (argv[argi]) {

    if (strcmp(argv[argi++], "-alt")) { 

      cerr << argv[argi-1];
      Error(" no -alt?");
    }

    alt_file = argv[argi];

    if (!alt_file) Error("no -alt 2?");

  }


  if (!book_file) { Error("no lib-file"); }

  if (!f_tab && !f_boards && !f_randboards && !f_uniq && !f_show && !f_alt && !f_good && 
      !res_file && !f_stat && !f_write)
    Error("no action?");

  if (alt_file && (f_boards || f_tab || res_file)) 
    Error("-alt can not be used only with -boards and -tab");

  if (f_good && !alt_file) Error("-good only with -alt");

  fp = fopen(book_file, "r");
  if (!fp) Error("can't open library");


  InitCrt();

  if (f_uniq) {

    sprintf(newfile, "%s.uniq", book_file); 

    fpout = fopen(newfile, "w");
    if (!fpout) Error("can't open output-file");

  }

  int gn = 0;

  for (i=0;; i++) {

    if (!game.f_read_packed(fp)) break;

    if (game.get_move_num() && game.get_pm(0).get_move() != D3) 
      Error("game is not normalized!");

    if (!book.append_game(game)) { 

      // cout << "."; fflush(stdout);

    } else {
      gn++;

      if (f_uniq)
	if (!game.f_write_packed(fpout)) Error("write error");
    }
  }

  if (f_uniq) fclose(fpout);  

  fclose(fp);


  cout << i << " game(s) read, " << gn <<" different" << endl;


  if (draws_file) {

    // mark public draws

    fp = fopen(draws_file, "r");

    int gn = 0;

    if (fp) {

      cout << "[ reading draws '" << draws_file << "' ..." << flush;

      FOREVER {
	  
	if (!game.f_read_packed(fp)) break;

	game.is_public_draw(true);

	if (!book.append_game(game)) { cout << "." << flush; }
	else gn++;

      }

      cout << "OK - " << gn << " draws(s) ]" << endl;

      fclose(fp);
      
    } else cout << "[ can't read draws-file!\a ]" << endl;
  }

  if (alt_file) {

    int valid=0;

    fp = fopen(alt_file, "r");
    if (!fp) Error("can't open alt-file");

    if (f_uniq) {

      sprintf(newfile, "%s.uniq", alt_file); 

      fpout = fopen(newfile, "w");
      if (!fpout) Error("can't open output-file");
    }

    for (i=0;; i++) {

      if (!game.f_read_packed(fp)) break;

      game.is_alternative(true);

      if (game.get_move_num() && game.get_pm(0).get_move() != D3) 
        Error("game is not normalized!");

      if (!book.append_game(game)) { 

	// cout << "."); fflush(stdout);

      } else {

	valid++;

        if (f_uniq) {

          if (!game.f_write_packed(fpout)) Error("write error");

        }
      }
    }


    if (f_uniq) fclose(fpout);  


    fclose(fp);

    cout << i << " positions(s) read, " << valid << " valid" << endl;

  }



  { int dummy;
    bool du2;

    book.rec_visit();
  }



#if 0
  { int trans;
    LIBNODE *pV;


  game.moves[0] = SMOVE_GEN(D3, BLACK);
  game.moves[1] = SMOVE_GEN(C5, WHITE);
  game.moves[2] = SMOVE_GEN(F6, BLACK);
  game.moves[3] = SMOVE_GEN(F5, WHITE);
  game.moves[4] = SMOVE_GEN(E6, BLACK);
  game.moves[5] = SMOVE_GEN(E3, WHITE);
  game.moves[6] = SMOVE_GEN(D6, BLACK);
  game.moves[7] = SMOVE_GEN(E7, WHITE);
  game.moves[8] = SMOVE_GEN(F7, BLACK);
  game.moves[9] = 0;
  game.move_num = 9;

  

  pV = SearchVertex(pT, &game, &trans);

  if (pV) {

    cout << "val=" << pV->vValueBLACK << " num=" << pV->GameNum << endl;
    

  } else cout << "pV==0" << endl;

  }
#endif


#if 1

  if (f_write) {

    NewGame Game;
    FILE *fp;

    sprintf(newfile, "%s.pre", book_file);

    if (!(fp=fopen(newfile, "w"))) Error("can't open new pre-file");

    book.f_write_prefixes(fp, freq, discmax);
    fclose(fp);

    exit(0);
  }

#endif




  if (f_good) {

    FILE *fp;

    sprintf(newfile, "%s.good", book_file);
    if (!(fp=fopen(newfile, "w"))) Error("can't open new alt-file");

    book.f_write_good(fp); // was (fp , pT, pT->pRoot, Game, 0);
    fclose(fp);
    exit(0);
  }


#if 0

  if (res_file) {

    int dummy;
    bool du2;

    // minimax book

    book.rec_visit(); // EvalLibNode(pT, pT->pRoot, &dummy, &du2);


    // search paths

    if (!(fp=fopen(res_file, "r"))) Error("can't open new res-file");

    FOREVER {
    
      if (!game.f_read_packed(fp)) break;

      PosInfo PI;

      if (!SearchPosition(pT, game, &PI)) { 

	cout << "path not found" << endl;

      } else {

	int val;

	if (PI.move_index >= 0) {

          // move node 

	  val = PI.p_node->sons[PI.move_index].value;

	} else {

	  // lib node => minimax

          int player = PI.p_node->sons[0].moves[0].get_player(), maxval=-100;
	 
	  FOR (i, PI.p_node->son_num) {

	    if (PI.p_node->sons[i].is_finished())) {
	      val = PI.pNode->sons[i].value;
	      if (player == WHITE) val = -val;
	      if (val > maxval) maxval = val;
	    }
	  }

	  if (player == WHITE) maxval = -maxval;

	  val = maxval;
	}

	cout << val << endl;

      }
    }

    fclose(fp);
  }

#endif


  if (f_alt) {
   
    NewGame Game;
    FILE *fp;

    Game.set_value(0);

    sprintf(newfile, "%s.alt", book_file);
    if (!(fp=fopen(newfile, "w"))) Error("can't open new alt-file");

    book.f_write_alternatives(fp); // FindAlt(fp, pT, pT->pRoot, Game, 0);
    fclose(fp);
    exit(0);

  } else if (f_show) {

    if (book.show_sub_tree(showgame, f_depth)) {
      Error("path not found or it doesn't end in branch-node");     
    }

  } else if (f_boards || f_randboards || f_tab) {

    cout << book_file << endl;

    if ((f_sd || f_sc) && f_tab) {

      delta = NewDeltaBuffer(PattList[PattNum].DiscNum, 
                             PattList[PattNum].Big,
			     PattList[PattNum].SubDiscNum);
    }

    if (!f_sd && !f_sp && !f_sc) Error("specify marker");

    if (maxnum != 0 || f_stat) { 

      cout << "counting boards ..." << endl;

      count1 = true; 
      fWriteGameTreeBoards(fp, book, V_DIFF, 0);
      count1 = false;

      boardnum = 0;

      cout << endl << "OK" << endl;

      if (f_stat) {

	cout << "Statistics:" << endl << endl;

	int num = 0;
	int nums[33];

	FOR (i, 33) nums[i] = 0;

	for (i=4; i <= 64; i++) {

	  num += BoardNum[i];

	  cout << i << " " << BoardNum[i] << " ";

	  int j;

	  FOR (j, 33) {
	    cout << my_round(Results[i][j]*1000.0/(BoardNum[i])+0.00001) << " ";
	    nums[j] += Results[i][j];
	  }
	  cout << endl;
	}

	cout << "\n\ntotal:\n\n";
	
	cout << num << " ";

	int j;
	int min = MAXINT;

	FOR (j, 33)
	  if (nums[j] > 0 && nums[j] < min) min = nums[j];

	FOR (j, 33)
	  if (!nums[j]) { nums[j] = min; num += min; }
	
	FOR (j, 33)
	  cout << my_round(nums[j]*1000.0/(num+0.00001)) << " ";

	puts("");

	float min_weight = MAXINT;

	FOR (j, 33) {
	  float w = result_distr[j]/(nums[j]*1000.0/(num+0.00001));
	  if (min_weight > w) min_weight = w;
	}

	FOR (j, 33) {
	  cout << 
	    my_round(2*result_distr[j]/(nums[j]*1000.0/(num+0.00001))/min_weight) <<
            ", ";
        }

        cout << endl;
     	return 0;
      }
    }


    if (f_sd) sprintf(newfile, "%s.d.sfk", book_file); 
    if (f_sp) sprintf(newfile, "%s.p.sfk", book_file); 
    if (f_sc) sprintf(newfile, "%s.c.sfk", book_file); 

    if (f_boards || f_randboards) {
      fp = fopen(newfile, "w");
      if (!fp) Error("can't open output-file");
    }

    if (f_sd) fWriteGameTreeBoards(fp, book, V_DIFF, 0);
    if (f_sp) fWriteGameTreeBoards(fp, book, V_PROB, 0);
    if (f_sc) fWriteGameTreeBoards(fp, book, V_CLASS, bound);
    
    if (f_boards || f_randboards) fclose(fp);  

    if (f_tab && f_rnd) {

      // process rnd-file(s)

      FILE *fpin;

      libweight = 1;   // allways 1

      FOREVER {

	cout << "\nrnd-file: " << rnd_file << endl;

	fpin = fopen(rnd_file, "r");
	if (!fpin) Error("can't read rnd-file");

	FOREVER {

	  SPFELD sf;

	  if (!fSfRead(fpin, &sf, 1)) break;
	
	  SFPOS m;
	  int valb;

	  if (MoveWldDecode(sf.Marke, m, valb)) { // not wld-move

	    if (MoveValDecode(sf.Marke, m, valb)) // not val-move

	      Error("label in rnd-file corrupt"); 

	    if      (valb <= -(WIN_LIMIT-50)) valb = -1;
	    else if (valb >=   WIN_LIMIT-50)  valb = +1;
	    else                              valb =  0;

	    //SfAus(&sf, 0, 0);
	    //cout << "%d\n", valb);

	  }

	  // save first board

	  int discs = SfAnz(&sf);

	  SaveBoard(fp, sf, discs, valb, BLACK, V_CLASS, bound, 0);

	  // check and make move

	  if (m > 0) {

	    if (!SfSetzen(&sf, BLACK, m)) Error("illegal move");

	    // move possible 

	    discs++;

	  } else {

	    SFPOS moves[65];

	    if (SfMoeglZuege(&sf, BLACK, moves) != 0) Error("move exists!");

	  }

	  // save second board

	  SfInvert(&sf); // black to move
	  SaveBoard(fp, sf, discs, -valb, BLACK, V_CLASS, bound, 0);
	}

	fclose(fpin);

	if (rnd2_file == 0) break;
	rnd_file = rnd2_file;
	rnd2_file = 0;
      }
    }

    if ((f_sd || f_sc) && f_tab) {

      char Name[300];

      FlushDeltaBuffer(delta);

      if (PattList[PattNum].SymF) {
        cout << "make pattern ...\n";
        PattList[PattNum].SymF(delta);
      }

      sprintf(Name, "%s.raw", PattList[PattNum].Name);
      SaveDeltaBuffer(delta, Name);
    }
  } 

  return 0;
}


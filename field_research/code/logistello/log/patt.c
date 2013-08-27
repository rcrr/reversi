// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Pattern stuff, 5.93, 2.94 */
 
#ifndef PRECOMP
#include "main.h"
#endif

#include "crt.h"
#include "move.h"
#include "game.h"
#include "trans.h"
#include "eval.h"
#include "killer.h"
#include "patt.h"
#include "tab.h"


#define TABID	1



void TabSwap(TABTYPE *tab, int discnum, TABTYPE *swaptab)
{
  int i, j, pnum=Pot3[discnum];

  FOR (i, pnum) 
    FOR (j, INUM+1) swaptab[j*pnum+i] = tab[j*pnum+pnum-1-i];
}



/* fp == NULL => only allocate memory */

void TabRead(FILE *fp, int discnum, TABTYPE **ptabb, TABTYPE **ptabw)
{ 
  int a, k, num=Pot3[discnum]*(INUM+1);


  if (fp) {

    printf("."); fflush(stdout);

    if ((a=fgetc(fp)) != TABID) { 
      printf("%d\n", a); Error("file error: wrong id"); }

  }

  if (!((*ptabb)=(TABTYPE*) malloc(num))) Error("TabRead: no memory");
  if (!((*ptabw)=(TABTYPE*) malloc(num))) Error("TabRead: no memory");


  if (fp) {
  
    if (fread((char*)*ptabb, num, 1, fp) != 1) Error("TabRead: read error");

/* empty pattern must have value 0 */

    FOR (k, INUM+1) {
      int d=k*Pot3[discnum]+(Pot3[discnum]-1)/2;
   
      if ((*ptabb)[d]) { printf("*"); fflush(stdout); }
      (*ptabb)[d] = 0;
    }


    TabSwap(*ptabb, discnum, *ptabw);
  }

}


void BigTabSwap(TABTYPE *tab, int discnum, TABTYPE *swaptab)
{
  int i, pnum=Pot3[discnum];

  FOR (i, pnum) 
    swaptab[i] = tab[pnum-1-i];
}



/* fp == NULL => only allocate memory */

void BigTabRead(FILE *fp, int discnum, TABTYPE **ptabb, TABTYPE **ptabw)
{ 
  int a, num=Pot3[discnum];


  if (fp) {

    printf("."); fflush(stdout);

    if ((a=fgetc(fp)) != TABID) { 
      printf("%d\n", a); Error("file error: wrong id"); }

  }

  if (!((*ptabb)=(TABTYPE*) malloc(num))) Error("TabRead: no memory");
  if (!((*ptabw)=(TABTYPE*) malloc(num))) Error("TabRead: no memory");


  if (fp) {
  
    if (fread((char*)*ptabb, num, 1, fp) != 1) Error("TabRead: read error");

/* empty pattern must have value 0 */

    {
      int d=(Pot3[discnum]-1)/2;
   
      if ((*ptabb)[d]) { printf("*"); fflush(stdout); }
      (*ptabb)[d] = 0;
    }

    BigTabSwap(*ptabb, discnum, *ptabw);
  }
}


void TabWrite(FILE *fp, TABTYPE *tab, int discnum)
{
  printf("."); fflush(stdout);

  fputc(TABID, fp);

  if (fwrite((char*)tab, Pot3[discnum]*(INUM+1), 1, fp) != 1) 
    Error("TabWrite: write error");
}


void BigTabWrite(FILE *fp, TABTYPE *tab, int discnum)
{
  printf("."); fflush(stdout);

  fputc(TABID, fp);

  if (fwrite((char*)tab, Pot3[discnum], 1, fp) != 1) 
    Error("BigTabWrite: write error");
}




/* FileName == NULL => only allocate memory */

bool ReadTables(const String &FileName, TABLES *tabsb, TABLES *tabsw)
{
  FILE *fp=NULL;

  if (FileName != "") {
    cout << "[ read tables '" << FileName << "' ..." << flush;
    fp = fopen(FileName.c_str(), "r");
    if (!fp) Error("ReadTables: file not found");
  }

  TabRead(fp, 8, &tabsb->r1, &tabsw->r1);

/*
  TabRead(fp, 8, &tabsb->r2, &tabsw->r2);
  TabRead(fp, 8, &tabsb->r3, &tabsw->r3);

  TabRead(fp, 8, &tabsb->m1, &tabsw->m1);
  TabRead(fp, 8, &tabsb->m2, &tabsw->m2);
*/

  TabRead(fp, 8, &tabsb->hv1, &tabsw->hv1);
  TabRead(fp, 8, &tabsb->hv2, &tabsw->hv2);
  TabRead(fp, 8, &tabsb->hv3, &tabsw->hv3);
  TabRead(fp, 8, &tabsb->hv4, &tabsw->hv4);

  TabRead(fp, 8, &tabsb->d1, &tabsw->d1);
  TabRead(fp, 7, &tabsb->d2, &tabsw->d2);
  TabRead(fp, 6, &tabsb->d3, &tabsw->d3);
  TabRead(fp, 5, &tabsb->d4, &tabsw->d4);
  TabRead(fp, 4, &tabsb->d5, &tabsw->d5);
  TabRead(fp, 3, &tabsb->d6, &tabsw->d6);

#if 1
  BigTabRead(fp, 10, &tabsb->b1, &tabsw->b1);
  BigTabRead(fp, 10, &tabsb->b2, &tabsw->b2);
#endif

  if (FileName != "") {

    fgetc(fp);
    if (!feof(fp)) Error("ReadTables: file too long");
    fclose(fp);
    cout << "OK ]" << endl;
  }

  return true;
}



bool WriteTables(TABLES *psz, const String &FileName)
{
  FILE *fp;

  cout << "write table file '" << FileName << "' ..." << flush;
  if (FileName == "") Error("WriteTables: empty filename");

  fp = fopen(FileName.c_str(), "w");
  if (!fp) Error("WriteTables: can't open file");

  TabWrite(fp, psz->r1, 8);

/*
  TabWrite(fp, psz->r2, 8); 
  TabWrite(fp, psz->r3, 8); 

  TabWrite(fp, psz->m1, 8); 
  TabWrite(fp, psz->m2, 8); 
*/
 
  TabWrite(fp, psz->hv1, 8); 
  TabWrite(fp, psz->hv2, 8); 
  TabWrite(fp, psz->hv3, 8); 
  TabWrite(fp, psz->hv4, 8); 
  
  TabWrite(fp, psz->d1, 8); 
  TabWrite(fp, psz->d2, 7); 
  TabWrite(fp, psz->d3, 6); 
  TabWrite(fp, psz->d4, 5); 
  TabWrite(fp, psz->d5, 4); 
  TabWrite(fp, psz->d6, 3); 

  BigTabWrite(fp, psz->b1, 10); 
  BigTabWrite(fp, psz->b2, 10); 
  
  fclose(fp);

  printf("OK\n");

  return true;
}



void ComputePatt(Square *p, int player, PATT *pst)
{
  if (player == BLACK) {

/*
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->r1a = PATT8(+,A1,B1,C1,D1,A2,B2,C2,D2);
    pst->r1b = PATT8(+,H1,G1,F1,E1,H2,G2,F2,E2);
    pst->r1c = PATT8(+,A8,B8,C8,D8,A7,B7,C7,D7);
    pst->r1d = PATT8(+,H8,G8,F8,E8,H7,G7,F7,E7);
    pst->r1e = PATT8(+,A1,A2,A3,A4,B1,B2,B3,B4);
    pst->r1f = PATT8(+,H1,H2,H3,H4,G1,G2,G3,G4);
    pst->r1g = PATT8(+,A8,A7,A6,A5,B8,B7,B6,B5);
    pst->r1h = PATT8(+,H8,H7,H6,H5,G8,G7,G6,G5);

/*
::::::::
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->r2a = PATT8(+,A2,B2,C2,D2,A3,B3,C3,D3);
    pst->r2b = PATT8(+,H2,G2,F2,E2,H3,G3,F3,E3);
    pst->r2c = PATT8(+,A7,B7,C7,D7,A6,B6,C6,D6);
    pst->r2d = PATT8(+,H7,G7,F7,E7,H6,G6,F6,E6);
    pst->r2e = PATT8(+,B1,B2,B3,B4,C1,C2,C3,C4);
    pst->r2f = PATT8(+,G1,G2,G3,G4,F1,F2,F3,F4);
    pst->r2g = PATT8(+,B8,B7,B6,B5,C8,C7,C6,C5);
    pst->r2h = PATT8(+,G8,G7,G6,G5,F8,F7,F6,F5);

/*
::::::::
::::::::
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->r3a = PATT8(+,A3,B3,C3,D3,A4,B4,C4,D4);
    pst->r3b = PATT8(+,H3,G3,F3,E3,H4,G4,F4,E4);
    pst->r3c = PATT8(+,A6,B6,C6,D6,A5,B5,C5,D5);
    pst->r3d = PATT8(+,H6,G6,F6,E6,H5,G5,F5,E5);
    pst->r3e = PATT8(+,C1,C2,C3,C4,D1,D2,D3,D4);
    pst->r3f = PATT8(+,F1,F2,F3,F4,E1,E2,E3,E4);
    pst->r3g = PATT8(+,C8,C7,C6,C5,D8,D7,D6,D5);
    pst->r3h = PATT8(+,F8,F7,F6,F5,E8,E7,E6,E5);

/*
::OOOO::
::OOOO::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/

    pst->m1a = PATT8(+,C1,D1,E1,F1,C2,D2,E2,F2);
    pst->m1b = PATT8(+,C8,D8,E8,F8,C7,D7,E7,F7);
    pst->m1c = PATT8(+,A3,A4,A5,A6,B3,B4,B5,B6);
    pst->m1d = PATT8(+,H3,H4,H5,H6,G3,G4,G5,G6);

/*
::::::::
::OOOO::
::OOOO::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->m2a = PATT8(+,C2,D2,E2,F2,C3,D3,E3,F3);
    pst->m2b = PATT8(+,C7,D7,E7,F7,C6,D6,E6,F6);
    pst->m2c = PATT8(+,B3,B4,B5,B6,C3,C4,C5,C6);
    pst->m2d = PATT8(+,G3,G4,G5,G6,F3,F4,F5,F6);


    pst->s1   = PATT8(+,A1,B1,C1,D1,E1,F1,G1,H1);
    pst->s2   = PATT8(+,A2,B2,C2,D2,E2,F2,G2,H2);
    pst->s3   = PATT8(+,A3,B3,C3,D3,E3,F3,G3,H3);
    pst->s4   = PATT8(+,A4,B4,C4,D4,E4,F4,G4,H4);
    pst->s5   = PATT8(+,A5,B5,C5,D5,E5,F5,G5,H5);
    pst->s6   = PATT8(+,A6,B6,C6,D6,E6,F6,G6,H6);
    pst->s7   = PATT8(+,A7,B7,C7,D7,E7,F7,G7,H7);
    pst->s8   = PATT8(+,A8,B8,C8,D8,E8,F8,G8,H8);

    pst->sA   = PATT8(+,A1,A2,A3,A4,A5,A6,A7,A8);
    pst->sB   = PATT8(+,B1,B2,B3,B4,B5,B6,B7,B8);
    pst->sC   = PATT8(+,C1,C2,C3,C4,C5,C6,C7,C8);
    pst->sD   = PATT8(+,D1,D2,D3,D4,D5,D6,D7,D8);
    pst->sE   = PATT8(+,E1,E2,E3,E4,E5,E6,E7,E8);
    pst->sF   = PATT8(+,F1,F2,F3,F4,F5,F6,F7,F8);
    pst->sG   = PATT8(+,G1,G2,G3,G4,G5,G6,G7,G8);
    pst->sH   = PATT8(+,H1,H2,H3,H4,H5,H6,H7,H8);

    pst->dp3  = PATT3(+,A3,B2,C1);
    pst->dp4  = PATT4(+,A4,B3,C2,D1);
    pst->dp5  = PATT5(+,A5,B4,C3,D2,E1);
    pst->dp6  = PATT6(+,A6,B5,C4,D3,E2,F1);
    pst->dp7  = PATT7(+,A7,B6,C5,D4,E3,F2,G1);
    pst->dp8  = PATT8(+,A8,B7,C6,D5,E4,F3,G2,H1);
    pst->dp9  = PATT7(+,B8,C7,D6,E5,F4,G3,H2);
    pst->dp10 = PATT6(+,C8,D7,E6,F5,G4,H3);
    pst->dp11 = PATT5(+,D8,E7,F6,G5,H4);
    pst->dp12 = PATT4(+,E8,F7,G6,H5);
    pst->dp13 = PATT3(+,F8,G7,H6);

    pst->dm3  = PATT3(+,A6,B7,C8);
    pst->dm4  = PATT4(+,A5,B6,C7,D8);
    pst->dm5  = PATT5(+,A4,B5,C6,D7,E8);
    pst->dm6  = PATT6(+,A3,B4,C5,D6,E7,F8);
    pst->dm7  = PATT7(+,A2,B3,C4,D5,E6,F7,G8);
    pst->dm8  = PATT8(+,A1,B2,C3,D4,E5,F6,G7,H8);
    pst->dm9  = PATT7(+,B1,C2,D3,E4,F5,G6,H7);
    pst->dm10 = PATT6(+,C1,D2,E3,F4,G5,H6);
    pst->dm11 = PATT5(+,D1,E2,F3,G4,H5);
    pst->dm12 = PATT4(+,E1,F2,G3,H4);
    pst->dm13 = PATT3(+,F1,G2,H3);


  } else {

/*
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->r1a = PATT8(-,A1,B1,C1,D1,A2,B2,C2,D2);
    pst->r1b = PATT8(-,H1,G1,F1,E1,H2,G2,F2,E2);
    pst->r1c = PATT8(-,A8,B8,C8,D8,A7,B7,C7,D7);
    pst->r1d = PATT8(-,H8,G8,F8,E8,H7,G7,F7,E7);
    pst->r1e = PATT8(-,A1,A2,A3,A4,B1,B2,B3,B4);
    pst->r1f = PATT8(-,H1,H2,H3,H4,G1,G2,G3,G4);
    pst->r1g = PATT8(-,A8,A7,A6,A5,B8,B7,B6,B5);
    pst->r1h = PATT8(-,H8,H7,H6,H5,G8,G7,G6,G5);

/*
::::::::
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->r2a = PATT8(-,A2,B2,C2,D2,A3,B3,C3,D3);
    pst->r2b = PATT8(-,H2,G2,F2,E2,H3,G3,F3,E3);
    pst->r2c = PATT8(-,A7,B7,C7,D7,A6,B6,C6,D6);
    pst->r2d = PATT8(-,H7,G7,F7,E7,H6,G6,F6,E6);
    pst->r2e = PATT8(-,B1,B2,B3,B4,C1,C2,C3,C4);
    pst->r2f = PATT8(-,G1,G2,G3,G4,F1,F2,F3,F4);
    pst->r2g = PATT8(-,B8,B7,B6,B5,C8,C7,C6,C5);
    pst->r2h = PATT8(-,G8,G7,G6,G5,F8,F7,F6,F5);

/*
::::::::
::::::::
OOOO::::
OOOO::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->r3a = PATT8(-,A3,B3,C3,D3,A4,B4,C4,D4);
    pst->r3b = PATT8(-,H3,G3,F3,E3,H4,G4,F4,E4);
    pst->r3c = PATT8(-,A6,B6,C6,D6,A5,B5,C5,D5);
    pst->r3d = PATT8(-,H6,G6,F6,E6,H5,G5,F5,E5);
    pst->r3e = PATT8(-,C1,C2,C3,C4,D1,D2,D3,D4);
    pst->r3f = PATT8(-,F1,F2,F3,F4,E1,E2,E3,E4);
    pst->r3g = PATT8(-,C8,C7,C6,C5,D8,D7,D6,D5);
    pst->r3h = PATT8(-,F8,F7,F6,F5,E8,E7,E6,E5);

/*
::OOOO::
::OOOO::
::::::::
::::::::
::::::::
::::::::
::::::::
::::::::
*/

    pst->m1a = PATT8(-,C1,D1,E1,F1,C2,D2,E2,F2);
    pst->m1b = PATT8(-,C8,D8,E8,F8,C7,D7,E7,F7);
    pst->m1c = PATT8(-,A3,A4,A5,A6,B3,B4,B5,B6);
    pst->m1d = PATT8(-,H3,H4,H5,H6,G3,G4,G5,G6);

/*
::::::::
::OOOO::
::OOOO::
::::::::
::::::::
::::::::
::::::::
::::::::
*/
    pst->m2a = PATT8(-,C2,D2,E2,F2,C3,D3,E3,F3);
    pst->m2b = PATT8(-,C7,D7,E7,F7,C6,D6,E6,F6);
    pst->m2c = PATT8(-,B3,B4,B5,B6,C3,C4,C5,C6);
    pst->m2d = PATT8(-,G3,G4,G5,G6,F3,F4,F5,F6);



    pst->s1   = PATT8(-,A1,B1,C1,D1,E1,F1,G1,H1);
    pst->s2   = PATT8(-,A2,B2,C2,D2,E2,F2,G2,H2);
    pst->s3   = PATT8(-,A3,B3,C3,D3,E3,F3,G3,H3);
    pst->s4   = PATT8(-,A4,B4,C4,D4,E4,F4,G4,H4);
    pst->s5   = PATT8(-,A5,B5,C5,D5,E5,F5,G5,H5);
    pst->s6   = PATT8(-,A6,B6,C6,D6,E6,F6,G6,H6);
    pst->s7   = PATT8(-,A7,B7,C7,D7,E7,F7,G7,H7);
    pst->s8   = PATT8(-,A8,B8,C8,D8,E8,F8,G8,H8);

    pst->sA   = PATT8(-,A1,A2,A3,A4,A5,A6,A7,A8);
    pst->sB   = PATT8(-,B1,B2,B3,B4,B5,B6,B7,B8);
    pst->sC   = PATT8(-,C1,C2,C3,C4,C5,C6,C7,C8);
    pst->sD   = PATT8(-,D1,D2,D3,D4,D5,D6,D7,D8);
    pst->sE   = PATT8(-,E1,E2,E3,E4,E5,E6,E7,E8);
    pst->sF   = PATT8(-,F1,F2,F3,F4,F5,F6,F7,F8);
    pst->sG   = PATT8(-,G1,G2,G3,G4,G5,G6,G7,G8);
    pst->sH   = PATT8(-,H1,H2,H3,H4,H5,H6,H7,H8);

    pst->dp3  = PATT3(-,A3,B2,C1);
    pst->dp4  = PATT4(-,A4,B3,C2,D1);
    pst->dp5  = PATT5(-,A5,B4,C3,D2,E1);
    pst->dp6  = PATT6(-,A6,B5,C4,D3,E2,F1);
    pst->dp7  = PATT7(-,A7,B6,C5,D4,E3,F2,G1);
    pst->dp8  = PATT8(-,A8,B7,C6,D5,E4,F3,G2,H1);
    pst->dp9  = PATT7(-,B8,C7,D6,E5,F4,G3,H2);
    pst->dp10 = PATT6(-,C8,D7,E6,F5,G4,H3);
    pst->dp11 = PATT5(-,D8,E7,F6,G5,H4);
    pst->dp12 = PATT4(-,E8,F7,G6,H5);
    pst->dp13 = PATT3(-,F8,G7,H6);

    pst->dm3  = PATT3(-,A6,B7,C8);
    pst->dm4  = PATT4(-,A5,B6,C7,D8);
    pst->dm5  = PATT5(-,A4,B5,C6,D7,E8);
    pst->dm6  = PATT6(-,A3,B4,C5,D6,E7,F8);
    pst->dm7  = PATT7(-,A2,B3,C4,D5,E6,F7,G8);
    pst->dm8  = PATT8(-,A1,B2,C3,D4,E5,F6,G7,H8);
    pst->dm9  = PATT7(-,B1,C2,D3,E4,F5,G6,H7);
    pst->dm10 = PATT6(-,C1,D2,E3,F4,G5,H6);
    pst->dm11 = PATT5(-,D1,E2,F3,G4,H5);
    pst->dm12 = PATT4(-,E1,F2,G3,H4);
    pst->dm13 = PATT3(-,F1,G2,H3);
  }
}


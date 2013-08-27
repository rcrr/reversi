// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "psearchm.h"
#include "crt.h"
#include <vector.h>

#if __i386_
#define ASM86(x) asm(x)
#else
#define ASM86(x)
#endif

inline int number1(ULL x)
{
  int n = 0;

  while (x) { x &= (x-1); n++; }

  return n;
}
 

inline int number1(uint4 x)
{
  int n = 0;

  while (x) { x &= (x-1); n++; }

  return n;
}


void check1(Config &conf, int n, uint4 *pati[])
{
  register uint4 *p1   = pati[0];
  register int    k    = n;
  register uint4 and;

  do {
    if ((and = (*p1++))) conf.n += number1(and);
  } while (--k);

}
 
void check2(Config &conf, int n, uint4 *pati[])
{
  register uint4 *p1  = pati[0];
  register uint4 *p2  = pati[1];
  register int    k   = n;
  register uint4 and;

  do {
    if ((and = (*p1++ & *p2++))) conf.n += number1(and);
  } while (--k);
}

void check3(Config &conf, int n, uint4 *pati[])
{
  register uint4 *p1 ASM86("%ecx") = pati[0];
  register uint4 *p2 ASM86("%ebx") = pati[1];
  register uint4 *p3 ASM86("%edi") = pati[2];
  register int   k = n;
  register uint4 and;

  do {
    if ((and = (*p1++ & *p2++ & *p3++))) conf.n += number1(and);
  } while (--k);
}

 
void checkn(int sq_num, Config &conf, int n, uint4 *pati[])
{
  register int k = n;

  if (sq_num < 2) Error("sq_num < 2");

  do {

    register int     j ASM86("%ecx") = sq_num-2;
    register uint4 and ASM86("%ebx") = *(pati[0]++);
    register uint4 **p ASM86("%edi") = &pati[1];
    
    do { and &= *(*p++)++; } while (--j >= 0);
      
    if (and) conf.n += number1(and);
      
  } while (--k);
}


#if 1

// new: 20% faster

void genn(int sq_num, int n, uint4 *pati[], uint4 *pgen)
{
  register int k = n;

  if (sq_num < 2) Error("sq_num < 2");

  do {

    register int     j ASM86("%ecx") = sq_num-1;
    register uint4 and ASM86("%ebx") = *(pati[0]++);
    register uint4 **p ASM86("%edi") = &pati[1];

    if (j & 1) and &= *(*p++)++;
    j >>= 1;

    while (and && j > 0) { 
      j--; 
      and &= *(*p++)++;
      and &= *(*p++)++; 
    }

    while (j > 0) { j--; (*p++)++; (*p++)++; }

    *pgen++ = and;
      
  } while (--k);
}

#else 


// old 

void genn(int sq_num, int n, uint4 *pati[], uint4 *pgen)
{
  register int k = n;

  if (sq_num < 2) Error("sq_num < 2");

  do {

    register int     j ASM86("%ecx") = sq_num-2;
    register uint4 and ASM86("%ebx") = *(pati[0]++);
    register uint4 **p ASM86("%edi") = &pati[1];
    
    do { and &= *(*p++)++; } while (--j >= 0);
    *pgen++ = and;
      
  } while (--k);
}

#endif


void statn(Config &conf, int n, uint4 *pati[], 
	   vector<BoardInfo> &infos, sint4 &res_sum, sint4 &res_square)
{
  register int k = n;
  int index = 0;

  do {

    register int     j ASM86("%ecx") = conf.size-2;
    register uint4 and ASM86("%ebx") = *(pati[0]++);
    register uint4 **p ASM86("%edi") = &pati[1];
    
    if (j >= 0) 
      do { and &= *(*p++)++; } while (--j >= 0);

    if (and) {

      int sub_index = index;

      while (and) {

	if (and & 1) {
	  conf.n++;
	  res_sum    += infos[sub_index].result;
	  res_square += square(infos[sub_index].result);
	}

	sub_index++;
	and >>= 1;
      }

    }

    index += 32;
      
  } while (--k);

}

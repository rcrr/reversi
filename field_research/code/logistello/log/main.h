// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef MAIN_H
#define MAIN_H

#include "ClientIncludes.H"

#if 0
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <math.h>
#include <float.h>
#include <assert.h>

// C++

#include <cctype>
#include <csignal>
#include <cassert>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>
#include <iomanip.h>

#endif

#include <cctype>
#include <csignal>
#include <setjmp.h>
#include <values.h>
#include <limits.h>
#include <fcntl.h>
#include "ClientString.H"

#undef PI


#ifndef SMALL_HASH
#error SMALL_HASH not defined
#endif

#if SMALL_HASH
#define HASHBITS0    15
#define HASHMAXDEPTH0 6
#define HASHBITS     17
#define HASHMAXDEPTH 64
#else
#define HASHBITS0    18
#define HASHMAXDEPTH0 9  
#define HASHBITS     20
#define HASHMAXDEPTH 64  // was 10
#endif


#define TO_PRAEFIX	".To."
#define FROM_PRAEFIX	".From."

#define QUOTE(x) 	#x
#define STRING(x)	QUOTE(x)

#define REAL_IS_DOUBLE

#ifdef REAL_IS_FLOAT

#define REAL		float
#define R_F		"f"
#define R_E		"e"
#define REAL_MAX  	FLT_MAX

#else

#define REAL		double
#define R_F		"lf"
#define R_E		"le"
#define REAL_MAX  	DBL_MAX

#endif


#if INTSIZE == 4
#define L_F		"d"
#else
#define L_F		"ld"
#endif

#define P_REAL(x)	printf("%" R_F "\n", x)

#define EPS 		(1e-5)	/* fabs(x-y) < EPS <=> x = y */

#define RGLEICH(r,s) 	(fabs((r)-(s)) < EPS)




#ifdef UNIX

#ifdef GPP
#include <std.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <memory.h>
#include <unistd.h>

#define IRAN		rand()
#define SRAN		srand

#ifndef USLEEP
#error USLEEP undef.
#endif

#if USLEEP
#define SLEEP(x)	usleep((x) * 100000L)	// wait x * 0.1 sec
#else
#define SLEEP(x)        sleep((x) <= 10 ? 1 : my_round(((x)/10.0)))
#endif

#define WARTEN(x)	BusyWait(x)
#define SETJMP(env) 	setjmp(env)
#define LONGJMP(env)	longjmp(env, 1)

#define	CHECK_COUNT	30000	// #nodes -> file check


#define X		// for BusyWait


using namespace std;


// default float type 

typedef real4          real;


// misc.

template <class T> 
inline T square(T x) { return x*x; }

//template <class T> 
//inline T min(T a, T b) { return (a < b) ? a : b; }

//template <class T> 
//inline T max(T a, T b) { return (a > b) ? a : b; }

#ifndef IRIX  
template <class T> 
inline T abs(T arg) { return (arg < T(0)) ? -arg : arg; }
#endif

inline int odd(int arg)  { return arg & 1; } 
inline int even(int arg) { return !odd(arg); }

#define true		1
#define false		0

extern void Chk_Abort	(void);


#endif


#define FRAN 		((REAL)IRAN/RAND_MAX)


extern char TO_1[], FROM_1[], TO_2[], FROM_2[];

extern void	_abort(void);

#include "crt.h"

#endif

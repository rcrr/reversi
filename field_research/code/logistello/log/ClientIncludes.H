// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ClientIncludes_H
#define ClientIncludes_H

#include <cstdio>
#include <cassert>
#include <time.h>
#include <math.h>
#include <iostream>
#include <iomanip>
#include <sstream>

#include "ClientPTR.H"

using namespace std;

typedef   signed char        sint1;
typedef unsigned char        uint1;
typedef   signed short       sint2;
typedef unsigned short       uint2;
typedef   signed int         sint4;
typedef unsigned int         uint4;
typedef               float  real4;
typedef               double real8;
typedef          long double realC;
typedef   signed long long   sint8;
typedef unsigned long long   uint8;

typedef void* vptr;
typedef char* cptr;

typedef const void* cvptr;
typedef const char* ccptr;

typedef const void* const cvptrc;
typedef const char* const ccptrc;


const sint1 min_sint1 = 0x80;
const sint1 max_sint1 = 0x7F;
const uint1 min_uint1 = 0x00;
const uint1 max_uint1 = 0xFF;
const sint2 min_sint2 = 0x8000;
const sint2 max_sint2 = 0x7FFF;
const uint2 min_uint2 = 0x0000;
const uint2 max_uint2 = 0xFFFF;
const sint4 min_sint4 = 0x80000000;
const sint4 max_sint4 = 0x7FFFFFFF;
const uint4 min_uint4 = 0x00000000;
const uint4 max_uint4 = 0xFFFFFFFF;
const sint8 min_sint8 = 0x8000000000000000LL;
const sint8 max_sint8 = 0x7FFFFFFFFFFFFFFFLL;
const uint8 min_uint8 = 0x0000000000000000LL;
const uint8 max_uint8 = 0xFFFFFFFFFFFFFFFFLL;

#define FOR(i,n)        for (i=0; i < (n); i++)
#define FORS(i,n)       for (sint4 i=0; i < (n); ++i)
#define FORU(i,n)       for (uint4 i=0; i < (n); ++i)
#define FOREVER         for (;;)

#define STR2STRING(os, s) \
{\
  (s).erase(); (s).append((os).str(), (os).pcount());\
  (os).rdbuf()->freeze(false);\
} 


// inline int round(double x) { return int(rint(x)); }

extern ostream& form( ostream& os, const char* fmt, ... );
extern ostream& vform( ostream& os, const char* fmt, ... );

class String;

#if defined(__GNUC__)

#define ERR(s) {\
  cerr << endl << __FILE__ << " " << __FUNCTION__ << "() (line " << __LINE__ << "): "\
  << s << endl; ::abort();\
}

#define ERR2(s1, s2) {\
  cerr << endl << __FILE__ << " " << __FUNCTION__ << "() (line " << __LINE__ << "): "\
  << s1 << " " << s2 << endl; ::abort();\
}

#else



#define ERR(s) {\
  cerr << endl << __FILE__  << " (line " << __LINE__ << "): "\
  << s << endl; ::abort();\
}

#define ERR2(s1, s2) {\
  cerr << endl << __FILE__  << " (line " << __LINE__ << "): "\
  << s1 << " " << s2 << endl; ::abort();\
}
#endif



#endif

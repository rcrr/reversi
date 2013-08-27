// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// Random Number Generator

#ifndef Rnd_H
#define Rnd_H

#include "main.h"
#include "MLCG.h"   // new
#include "RndInt.h" // 2.7.2 version, 2.6.3 gives weird warnings
#include <Uniform.h> 

class Rnd {

private:
  static MLCG gen;

public:

  // seed type

  struct SeedData { int s1, s2; };


  // return a random a<=x<=b number using Number(a,b)

  static RandomInteger Number;
  static Uniform       Uni01;

  // set seed

  static void Seed(SeedData &s) 
  {
    gen.plain_reseed(s.s1, s.s2);
  }


  // get seed 

  static SeedData Seed()
  {
    SeedData s;

    s.s1 = gen.seed1();
    s.s2 = gen.seed2();

    return s;
  }

};

#endif

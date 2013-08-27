// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// board weights determined by olib -stat

// weights for each abs(result)/2 [0..32]

#include "main.h"
#include "crt.h"

// weight[0] must be even

#if 0
static int weight[33] = {
  38, 433, 379, 217, 125, 76, 43, 21, 12, 7, 3, 2,
  2, 3, 3, 4, 5, 6, 8, 10, 8, 11, 13, 13, 16, 24,
  38, 36, 77, 54, 67, 65, 35
};
#else
static int weight[33] = {
  2, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
};
#endif

int Weight(int val)
{
  val = abs(val);

  if (val & 1) val++;

  val /= 2;

  if (val < 0 || val > 32) Error("val?");
  
  return weight[val];
}

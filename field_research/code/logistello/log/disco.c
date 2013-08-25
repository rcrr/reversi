// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "disco.h"
#include <algo.h>

float discordance(Values &val0, Values &val1)
{
  int i, j, num0=val0.size(), num1=val1.size();
  int su;

  sort(val0.begin(), val0.end());
  sort(val1.begin(), val1.end());

#if 0
  FOR (i, num0) cout << val0[i] << " ";
  cout << endl;
  
  FOR (i, num1) cout << val1[i] << " ";
  cout << endl;
#endif

  // count pairs with val1 >= val2 but cl1=loss and cl2 = win

  su = 0; i = 0;

  FOR (j, num0) {
    while (i < num1 && val0[j] >= val1[i]) i++;
    su += i;
  }

  //  cout << "s=" << su << " d=" << double(su) / (num0*num1) << endl;

  return double(su) / ((num0+num1)*(num0+num1-1)/2);
}

 

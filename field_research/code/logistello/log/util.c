// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// utility computation for "util-cut" / 2.97

#include "main.h"
#include "normal.h"

const float CONST = 0.7978845;  // sqrt(2/PI) 

float utility(float a, float b, float s, float sigma)
{
  float EA = 1.0 - 2*Normal::Phi(a, s, sigma);
  float EB = 1.0 - 2*Normal::Phi(b, s, sigma);
  float PA = Normal::phi(a, s, sigma);
  float PB = Normal::phi(b, s, sigma);

  if (s <= a) {

    return sigma*sigma*(PA-PB) + 0.5*(s-a)*(EA-EB)+(b-a)*(0.5+0.5*EB);

  } else if (s >= b) {

    return sigma*sigma*(PB-PA) + 0.5*(s-b)*(EB-EA)+(b-a)*(0.5-0.5*EA);

  } else {

    return (s-a)*(0.5-0.5*EA) + (b-s)*(0.5+0.5*EB) + sigma*(CONST-sigma*(PA+PB));

  }
}


#if 0

void _abort() { exit(0); }


main()
{
  cout << utility(-1, 3, 0, 4) << endl;
  cout << utility(-5, 6, 8, 4) << endl;
  cout << utility(3, 7, 2, 4) << endl;

}

#endif

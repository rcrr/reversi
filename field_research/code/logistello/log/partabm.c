// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// generate 4x4 parity table

#include "main.h"
#include "sboard.h"
#include "fpatt.h"

const int NUM16 = 43046721;
const int NUM16DIV2 = (NUM16-1)/2+1;

const bool OUTPUT = false;
const bool COMPARE = true;

void _abort(void) { exit(20); }
 
int main()
{
  int   i, n, m, x, y;
  SPFELD sf = SfNull;
  sint1 *p=sf.p;

  cout << NUM16DIV2 << endl;

  int last_num = 99;
  int count = 0;

  FOR (n, NUM16DIV2) {

    if (COMPARE) {
      if ((n % 10000) == 0) cout << (n * 100) / NUM16DIV2 << "\r" << flush;
    }

    // generate region n

    int m = n;

    for (y=3; y >= 0; y--)
      for (x=3; x >= 0; x--) {
	
	int ind = y*10+x+11;
	
	p[ind] = m % 3 - 1;
	m = m/3;
      }
    
    // analyse region

    int num=0;
    RegInfo regi;

    AnalyseRegion(p, A1, regi);

    FOR (i, regi.num) {
      if ((regi.regs[i].size & 1) && !regi.regs[i].both) {

	if (regi.regs[i].color == BLACK) num++; else num--;
      }
    }


    if (num > 0) num =  1;
    if (num < 0) num = -1;

    if (COMPARE) {

      if (num != GetParTabVal(n-(NUM16DIV2-1))) {

	cout << "!=  " << n << endl;

      }


      int ni = 0;
      FOR (y, 4) {
	FOR (x, 4) {
	
	  int ind = x*10+y+11;
	
	  ni = 3*ni + p[ind] + 1;

	}
      }

cout << n << " " << ni << endl;



      if (num != GetParTabVal(ni-(NUM16DIV2-1))) {

	cout << "inv !=  " << n << " " << ni << endl;

      }
      

    }


    if (OUTPUT) {

      if (num == last_num) {

	count++;

      } else {

	if (count > 0) cout << last_num << ":" << count << " ";
	last_num = num;
	count = 1;

      }
    }


#if 0
    if      (num > 0) cout << "+";
    else if (num < 0) cout << "-";
    else cout << "0";
#endif

#if 0
    if (regi.num > 2) {

      cout << "-> " << num << endl;
      SfAus(&sf, 0, 0);

    }
#endif


  }

  if (OUTPUT) {
    if (count > 0) cout << last_num << ":" << count << " ";
  }

  return 0;
}

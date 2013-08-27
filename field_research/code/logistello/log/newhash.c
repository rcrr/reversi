// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "newhash.h"


void NewHashTab::init_tabs()
{
  int i, j, *tr;

  sMyRand(3);

  FOR (i, 100) { 

    ZufBLACK1[i] = MyRand() >> 3; 
    ZufWHITE1[i] = MyRand() >> 3;
    ZufBW1[i]    = ZufBLACK1[i] ^ ZufWHITE1[i];         // disc changes color

    ZufBLACK2[i] = MyRand(); 
    ZufWHITE2[i] = MyRand();
    ZufBW2[i]    = ZufBLACK2[i] ^ ZufWHITE2[i];         // disc changes color

  }

#if 0
  
  FOR (i, 8) {

    tr = TransTab[i];

    FOR (j, 100) {
      ZufBLACK1T[i][j] = ZufBLACK1[tr[j]];
      ZufWHITE1T[i][j] = ZufWHITE1[tr[j]];
    }
  }

#endif
}


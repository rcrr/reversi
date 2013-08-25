// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "hash.h"
#include "goodies.h"

uint4 HashTab::ZufBLACK1[100],
      HashTab::ZufWHITE1[100],
      HashTab::ZufBW1[100],  // hash
  
      HashTab::ZufBLACK2[100],
      HashTab::ZufWHITE2[100],
      HashTab::ZufBW2[100];  // lock

bool  HashTab::initialized = 0;

void HashTab::init_tabs()
{
  if (initialized) return;

  initialized = true;

  int i;

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



uint4 HashEntry::board_hash(BRETT *pbr, PARTEI to_move)
{
  uint4 h=0;
  int   i;

  h = 0;

  FOR_SFPOS10 (i) {
    if      (pbr->p[i] == BLACK) h ^= HashTab::ZufBLACK1[i];
    else if (pbr->p[i] == WHITE) h ^= HashTab::ZufWHITE1[i];
  }

  if (to_move == WHITE) return hash_white(h); else return h;
}


uint4 HashEntry::board_lock(BRETT *pbr, PARTEI to_move)
{
  uint4 h=0;
  int   i;

  h = 0;

  FOR_SFPOS10 (i) {

    if      (pbr->p[i] == BLACK) h ^= HashTab::ZufBLACK2[i];
    else if (pbr->p[i] == WHITE) h ^= HashTab::ZufWHITE2[i];

  }

  return h;
}



uint4 HashEntry::sboard_lock(SPFELD *psf)
{
  uint4 h=0;
  int   i;
  sint1 *p=psf->p;

  h = 0;

  FOR_SFPOS10 (i) {

    if      (p[i] == BLACK) h ^= HashTab::ZufBLACK2[i];
    else if (p[i] == WHITE) h ^= HashTab::ZufWHITE2[i];

  }

  return h;
}




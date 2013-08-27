// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* (Pot)Mobilitätstabellen bestimmen / 24.9.93, 28.10.93 */

#ifndef PRECOMP
#include "main.h"
#endif

#include "board.h"


typedef struct {

  int   anz;
  sint1 st[8];

} MS;


void _abort(void) { exit(1); }


void MSaus(MS *pms)
{
  int i;


  FOR (i, pms->anz) 

    if      (pms->st[i] == BLACK) printf("X");
    else if (pms->st[i] == WHITE) printf("O");
    else 		          printf("-");
}



int MSsetzen(MS *pms, int Pos, PARTEI Partei)
{
  int j, veraendert = 0;


  pms->st[Pos] = Partei;


  if (Pos >= 2 && pms->st[Pos-1] == GEGNER(Partei)) {

/* nach links */ 

    j = Pos-1;

    while (j > 0 && pms->st[j] == GEGNER(Partei)) j--;

    if (pms->st[j] == Partei) {

      j++;

      while (j < Pos) { veraendert |= (1 << j); pms->st[j++] = Partei; }
    }
  }


  if (Pos <= pms->anz-3 && pms->st[Pos+1] == GEGNER(Partei)) {

/* nach rechts */ 

   j = Pos+1;

   while (j < pms->anz-1 && pms->st[j] == GEGNER(Partei)) j++;

   if (pms->st[j] == Partei) {

     j--;

     while (j > Pos) { veraendert |= (1 << j); pms->st[j--] = Partei; }
   }
  }

  return veraendert;
}


/*  art == 0:   (8) MOB(8) MOBEB(8)  MOBEW(8)	 (MOB ohne Ecken)
 *  art == 1:   (8) MOB(8) POTMOB(8) POTMOBE(8)  (MOB mit Ecken)
 */

void MSMobs(int msanz, int art)
{
  int i, j, k, anz=1, MobEB, MobEW, MobDiff, PotMobDiff, PotMobEDiff, Nummer;
  MS  ms, ms0;
  int maxp[9] = { 0, 0, 0, 2, 2, 2, 3, 4, 5 };	/* max. # potmob */
  int maxm[9] = { 0, 0, 0, 3, 3, 3, 3, 3, 3 };	/* max. # mob    */


  if (art == 0) printf("/* MOB(ohne Ecken) MOBEB MOBEW */\n");
  else          printf("/* MOB(mit Ecken) POTMOB POTMOBE */\n");

  ms.anz = msanz;

  FOR (i, msanz) anz *= 3;


  FOR (i, anz) {

    Nummer = i;

    FOR (j, msanz) {			/* Kante aus Nummer erstellen */

      k = Nummer % 3; Nummer /= 3;

      if      (k == 2) ms.st[j] = BLACK;
      else if (k == 1) ms.st[j] = 0;
      else 	       ms.st[j] = WHITE;

    }  

    ms0 = ms;


    printf("/* "); MSaus(&ms0);


/* aktuelle Mobilität */


    MobEB = MobEW = MobDiff = 0;

    FOR (j, msanz) {

      if (!ms0.st[j]) {

        ms = ms0;
        if (MSsetzen(&ms, j, BLACK)) 
	  if (j == 0 || j == msanz-1) MobEB++; else MobDiff++;
        ms = ms0;
        if (MSsetzen(&ms, j, WHITE))
	  if (j == 0 || j == msanz-1) MobEW++; else MobDiff--;
      }
    }




/* potentielle Mobilität */


    PotMobEDiff = PotMobDiff = 0;

    for (j=1; j < msanz-1; j++) {

      if (ms0.st[j]) {

        ms = ms0;
        if (!ms0.st[j-1] && !MSsetzen(&ms, j-1, GEGNER(ms0.st[j]))) 
	  if (j-1 == 0) PotMobEDiff += GEGNER(ms0.st[j]);
	  else 		PotMobDiff  += GEGNER(ms0.st[j]);

        ms = ms0;
        if (!ms0.st[j+1] && !MSsetzen(&ms, j+1, GEGNER(ms0.st[j]))) 
	  if (j+1 == msanz-1) PotMobEDiff += GEGNER(ms0.st[j]);
	  else		      PotMobDiff  += GEGNER(ms0.st[j]);

      }
    }

/* bei Länge 3 keine POTMOB */

    if (msanz == 3) PotMobEDiff = PotMobDiff = 0;  


    if (art == 0) {
 
      printf("  %+2d %+2d %+2d */", MobDiff, MobEB, MobEW);

      printf(" 0x%08x,\n", 
          256*(
            256*(
  	      MobDiff+maxm[msanz]
	    )+MobEB
          )+MobEW);

    } else {

      printf("  %+2d %+2d %+2d */", 
	MobDiff+MobEB-MobEW, PotMobDiff, PotMobEDiff);

      printf(" 0x%08x,\n", 
          256*(
            256*(
  	      MobDiff+MobEB-MobEW+maxm[msanz]
	    )+PotMobDiff+maxp[msanz]
          )+PotMobEDiff+maxp[msanz]);

    }
  }
}




void main(void)
{
  int i;

i = 8;

printf("/* Strahlpotmobilitäten %d */\n\n", i);

MSMobs(i, 0);


}


// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define PLAYER_MAX 10000
#define STR_MAX    50
#define PAIR_MAX   500

/* Igor's rating code */

#define ESTAB 10

static double game_adapt_K( double w )
{
  return 32 + 32 * fabs(w);
}

static double game_adapt_result( double w )
{
   if ( w == 0 ) return 0.0;
   if ( w < 0 ) return -1.0;
   return 1.0;;
}

/* 
   f   = weighted win = outcome/64
   ogb = (old) no of black games 
   ogw = (old) no of white games
   orb = (old) black rating 
   orw = (old) white rating
   nrb = (new) black rating
   nrw = (new) white rating
*/

static double exp10(double x)
{
  return pow(10, x);
}  

void ios_game_rate( double f, int ogb, int ogw, 
                             int orb, int orw, int *nrb, int *nrw )
{
double vb, vw, nb, nw;
double gb, gw, rb, rw, K, w;

   w  = game_adapt_result( f );
   K  = game_adapt_K( f );

   gb = (double) ogb;
   gw = (double) ogw;
   rb = (double) orb;
   rw = (double) orw;
   if ( rb == 0 ) rb = 1600;
   if ( rw == 0 ) rw = 1600;
   if ( gb < ESTAB && gw < ESTAB ) { /* provisional both */
      vb  = (rb+rw)/2 + 200 * w;
      nb  = (rb * gb + vb) / (gb + 1);

      vw  = (rb+rw)/2 - 200 * w;
      nw  = (rw * gw + vw) / (gw + 1);
      goto END;
   } else
   if ( gb < ESTAB && gw >= ESTAB ) { /* black prov. white est. */
      vb = rw + 400 * w;
      nb = (rb * gb + vb) / (gb + 1);

      vw = (1 - w) / 2;
      nw = rw + K * (vw - 1/(1 + exp10((rb-rw)/400))) * gb / ESTAB; 
      goto END;
   } else
   if ( gb >= ESTAB && gw < ESTAB ) { /* black est. white prov. */
      vw = rb - 400 * w;
      nw = (rw * gw + vw) / (gw + 1);

      vb = (1 + w) / 2;
      nb = rb + K * (vb - 1/(1 + exp10((rw-rb)/400))) * gw / ESTAB; 
      goto END;
   } else { /* established both */
      vb = (1 + w) / 2;
      nb = rb + K * (vb - 1/(1 + exp10((rw-rb)/400))); 

      vw = (1 - w) / 2;
      nw = rw + K * (vw - 1/(1 + exp10((rb-rw)/400))); 
      goto END;
   }
   END:;
   *nrb = rint(nb);
   *nrw = rint(nw);
}



typedef struct {
  char name[STR_MAX+1];
  int  game_num, rating;
} PLAYER; 



int comp_player(const void *a, const void *b)
{
  const PLAYER *pa=a, *pb=b;

  return pb->rating - pa->rating;
}



int main(int argc, char **argv)
{
  FILE *fp;
  PLAYER players[PLAYER_MAX];
  int  player_num=0, game_num=0, i, k, black_i, white_i, black_num, white_num;
  char s[STR_MAX+1], black_name[STR_MAX+1], white_name[STR_MAX+1];
  int pair_num = 0;
  struct { char s1[STR_MAX], s2[STR_MAX]; } pairs[PAIR_MAX];


  if (argc == 3) {

    if (strcmp(argv[1], "-rename")) goto error;

    fp = fopen(argv[2], "r");
    if (!fp) {
 
      printf("*** pair-file not found\n");
      exit(20);
    }

    for (;;) {

      if (!fgets(s, 3*STR_MAX-1, fp)) break;
      
      if (sscanf(s, "%s %s", pairs[pair_num].s1, pairs[pair_num].s2) != 2) {  
        printf(">>%s\n*** syntax error in pair-file\n", s);
        exit(10);
      }
      pair_num++;

      if (pair_num >= PAIR_MAX) {  
        printf("*** too many pairs\n");
        exit(10);
      }
    }

    fclose(fp);

  } else if (argc > 1) {

error:

    printf("*** usage: orating [-rename pairs-file] < game-file\n");
    exit(20);

  }



  for (;;) {

/* read result line. format: Black_name White_name Black_num White_num  */

    if (!fgets(s, 3*STR_MAX-1, stdin)) break;

    if (sscanf(s, "%s %s %d %d", black_name, white_name, &black_num, &white_num) != 4) {
      printf(">>%s\n*** syntax error\n", s);
      exit(10);
    }

game_num++;

if (game_num % 100 == 0) { printf("%6d\r", game_num); fflush(stdout); }

if (black_num < 0 || black_num > 64 || white_num < 0 || white_num > 64) {

printf("%s : num?\n", s);

}

/* rename */

    for (i=0; i < pair_num; i++)
      if (!strcmp(pairs[i].s1, black_name)) break;

    if (i <  pair_num) strcpy(black_name, pairs[i].s2);

    for (i=0; i < pair_num; i++)
      if (!strcmp(pairs[i].s1, white_name)) break;

    if (i <  pair_num) strcpy(white_name, pairs[i].s2);

/* search names */

    for (i=0; i < player_num; i++) if (!strcmp(black_name, players[i].name)) break;

    if (i >= player_num) {

      strcpy(players[i].name, black_name); players[i].game_num = 0;

      player_num++;

      if (player_num >= PLAYER_MAX-1) {
        printf("*** too many players\n");
        exit(10);
      }

    }

    black_i = i;
    

    for (i=0; i < player_num; i++) if (!strcmp(white_name, players[i].name)) break;

    if (i >= player_num) {

      strcpy(players[i].name, white_name); players[i].game_num = 0;

      player_num++;

      if (player_num >= PLAYER_MAX-1) {
        printf("*** too many players\n");
        exit(10);
      }

    }

    white_i = i;


/* update rating */

#if 1 

    ios_game_rate((black_num-white_num)/64.0,
                   players[black_i].game_num, players[white_i].game_num, 
                   players[black_i].rating, players[white_i].rating, 
                   &players[black_i].rating, &players[white_i].rating
                  );
#endif

    players[black_i].game_num++;
    players[white_i].game_num++;
    
  }

  printf("%d players, %d games\n", player_num, game_num);

  qsort(players, player_num, sizeof(players[0]), comp_player);

/* print player statistics */

printf("ratings of the players with at least 10 games:\n");

  k = 0;

  for (i=0; i < player_num; i++) {

    if (players[i].game_num >= ESTAB) {
      k++;
      printf("%4d: %-20s  %5d  %5.0d \n",
	     k, players[i].name, players[i].game_num, players[i].rating);
    }
  }


  return 0;
}

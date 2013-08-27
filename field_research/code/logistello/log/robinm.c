// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/*   round-robin ios tournament manager by Michael Buro
 *
 *   25.9.94, 4.10.95, 17.10.97, 21.10.98
 *
 *   16.8.99 - komi/rand games
 */

#if 0

game email example:

rated GAME: Misty ( 15   0 120) vs. Qwagga ( 15   0 120)

LM: g5D7e2D2g4E7b5B6c7F1c2D8a6G3e8F8c8B8h3F2c1D1e1B1g2A4a5A7b2H1g1A1g8H8b7H2b4A8b3G7g6A3a2H4h5H6h7

-> ()

   A  B  C  D  E  F  G  H
1 |32|24|21|22|23|10|31|30| 1
2 |43|29|11| 4| 3|20|25|36| 2
3 |42|39|()|()|##|()|14|19| 3
4 |26|37|()|##|()|##| 5|44| 4
5 |27| 7|##|##|()|##| 1|45| 5
6 |13| 8|##|()|()|()|41|46| 6
7 |28|35| 9| 2| 6|##|40|47| 7
8 |38|18|17|12|15|16|33|34| 8
   A  B  C  D  E  F  G  H

##:() - 54:10 K+2.00 R17

#endif


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <string.h>

#define true      1
#define false     0
#define BLACK     1
#define WHITE     (-BLACK)
#define FOR(i,n)  for (i=0; i < (n); i++)
#define FOREVER   for (;;)


#define CHECK_COLORS true    /* s-r-r color check     */
#define GEN_GAMES    false   /* generate test games    */
#define PLAYERNUM 30          /* max. number of players */


typedef struct { 
  
  int   round;
  int   color[2];
  int   result[2];
  float komi[2];
  int   seen[2];
  int   dummy;             /* true <=> PASS game */

} GAME;


typedef struct { 

  int num, tiebreaker;     /* starting number & tie. */
  char name[80];

  float fpoints0;          /* for input            */
  int  points0, n0;        /* player's seed values */
  float diff0;
  int points, n;           /* player's score       */
  float diff;
                           /* double points are stored */
  int oldi, oldn;          /* index of player and #players in first part */

} PLAYERDATA;


typedef enum { ROBIN1, ROBIN2 } MODE;


int random_games = 0;


void Error(char *s)
{
  fprintf(stderr, "*** %s\n", s);
  exit(20);
}



/* compare two players
 *
 * priority: avg. points (0/0:=0.5)
 *           avg. discs  (0/0:=0.0)
 *           tiebreaker
 *           starting pos.
 */
 
 
int cmpPLAYERDATA(const void *a, const void *b)
{
  float avpa=0.5, avpb=0.5, avda=0, avdb=0;
  const PLAYERDATA *pa=(PLAYERDATA*)a, *pb=(PLAYERDATA*)b;


  if (pa->n) {
    avpa = (pa->points*0.5)/pa->n;
    avda = ((float)pa->diff)  /pa->n;
  } 

  if (pb->n) {
    avpb = (pb->points*0.5)/pb->n;
    avdb = ((float)pb->diff)  /pb->n;
  }

  if (avpa > avpb + 1e-5) return -1;
  if (avpb > avpa + 1e-5) return +1;

  if (avda > avdb + 1e-5) return -1;
  if (avdb > avda + 1e-5) return +1;

  if (pa->tiebreaker < pb->tiebreaker) return +1;
  if (pa->tiebreaker > pb->tiebreaker) return -1;

  return pa->num - pb->num;
}


/* compare two players (only points)
 *
 * priority: avg. points (0/0:=0.5)
 *
 */
 
int cmpPLAYERDATA2(const void *a, const void *b)
{
  float avpa=0.5, avpb=0.5;
  const PLAYERDATA *pa=(PLAYERDATA*)a, *pb=(PLAYERDATA*)b;


  if (pa->n) {
    avpa = (pa->points*0.5)/pa->n;
  } 

  if (pb->n) {
    avpb = (pb->points*0.5)/pb->n;
  }

  if (avpa > avpb + 1e-5) return -1;
  if (avpb > avpa + 1e-5) return +1;

  return 0;
}



/* generate tournament matrix: playnum-1 rounds & "almost even" colors */

void generate(GAME matrix[PLAYERNUM][PLAYERNUM], int playernum, int pass)
{ 
  int i, j, startround = 0, round, startcolor = BLACK, color;

  FOR (i, playernum) 
    FOR (j, playernum) matrix[i][j].seen[0] = matrix[i][j].seen[1] = false;


  FOR (i, playernum-1) {

    round = startround;
    color = startcolor;

    FOR (j, playernum-1) {

      if (i == j) {

        matrix[playernum-1][i].round = round;
        matrix[playernum-1][i].color[0] = -startcolor;
        matrix[playernum-1][i].color[1] =  startcolor;	

        matrix[i][playernum-1].round = round;
        matrix[i][playernum-1].color[0] =  startcolor;
	matrix[i][playernum-1].color[1] = -startcolor;
        
        matrix[playernum-1][i].dummy = matrix[i][playernum-1].dummy = pass;

      } else {

/*printf("%d %d %d\n", i, j, color);*/

        matrix[i][j].round = round;
        matrix[i][j].color[0] = color;
	matrix[i][j].color[1] = -color;

        matrix[j][i].dummy = matrix[i][j].dummy = false;

        color = -color;
   
      }

      round++;

      if (round >= playernum-1) round = 0;

    }

    startround++;
    startcolor = -startcolor;
  }
}


void print_help()
{
    printf("                                                        \n\n\
orobin - a round-robin Othello tournament manager                  \n\
\n\
° robin can manage single/double round-robin tournaments. It reads\n\
a list of players and a list of games played so far and prints a\n\
round-by-round tournament table and statistics. Additionally,   \n\
after completion of a single round-robin tournament it can split\n\
the player field into n groups in which the return games will   \n\
be played. The split-output can be used as input for the second \n\
part of the tournament.                                         \n\
\n\
° tournament-file syntax:                                         \n\
\n\
# preceeds comments                                             \n\
\n\
list of players ending with %%:                                 \n\
\n\
[ d1. s2  [f3/d4 d5 d6 d7/d8] ]+                                \n\
%%                                                              \n\
\n\
where d1 is ignored, s2 is the player's name,                   \n\
f3-d8 are optional seed values which are generated              \n\
by splitting the tournament: f3/d4 d5 is the first part         \n\
result, d6 can be used for tie-break in case of splitting       \n\
ambiguities, d7 is the player's starting position in the first  \n\
part, d8 is the number of players in first part (this data is   \n\
used to determine the colors for the return matches)           \n\
\n\
Example:                                                        \n\
\n\
1. Brutus                                                     \n\
2. Bugs                                                       \n\
3. eclipse                                                    \n\
4. keyano                                                     \n\
5. logtest                                                    \n\
6. MrOth                                                      \n\
%%                                                            \n\
\n\
° list of games with the following format:                        \n\
\n\
rated GAME: player_black (t t t) vs. player_white (t t t)       \n\
\n\
-> ##                                                           \n\
\n\
A  B  C  D  E  F  G  H                                       \n\
1 |  |  |  |  |  |  |  |  | 1                                   \n\
2 |xx|  |  |  |  |  |  |  | 2                                   \n\
3 |  |  | 2| 1|  |  |  |  | 3                                   \n\
4 |  |  |  |()|##|  |  |  | 4                                   \n\
5 |  |  |  |##|()|  |  |  | 5                                   \n\
6 |  |  |  |  |  |  |  |  | 6                                   \n\
7 |  |  |  |  |  |  |  |  | 7                                   \n\
8 |  |  |  |  |  |  |  |  | 8                                   \n\
A  B  C  D  E  F  G  H                                       \n\
\n\
##:() - 9:55 K+19 R20                                           \n\
\n\
\n\
° round-by-round table:                                           \n\
\n\
\n\
rd 1  rd 2  rd 3  rd 4  rd 5  rd 6  rd 7           \n\
+-----+-----+-----+-----+-----+-----+-----+          \n\
player     |B   8|B   2|W   3|B   4|W   5|B   6|W   7|          \n\
no.     1: |  %%  |  0  |  0  |  1  |  1  |  0  |  0  |         \n\
|     | 9-55|15-49|53-11|47-17|21-43|19-45|          \n\
+-----+-----+-----+-----+-----+-----+-----+          \n\
|B   7|W   1|W   8|B   3|W   4|B   5|W   6|          \n\
2: |  0  |  1  |  %%  |  0  |  1  |  0  |  0  |         \n\
|23-41|55-9 |     |31-33|37-27| 8-56|24-40|          \n\
+--^--+-----+--^--+-----+-----+-----+-----+          \n\
...   |           |                                     \n\
|        game vs. PASS                            \n\
|                                                 \n\
result of round-1-game                                  \n\
player 2 (BLACK) vs. player 7 (WHITE):                  \n\
0 points and 23 discs for player 2,                     \n\
1 point  and 41 discs for player 7                      \n");
}


int main(int argc, char **argv)
{
  int i, argi, j, k, nB, nW, dB, dW, gamenum, gameind,
      split_n=0, split_i[100], rnd=0;
  int playernum=0, points, n, splitted_n=0;
  float diff;
  int pass=false, gr_num[PLAYERNUM], dummy;
  char s[301], t[301], sB[301], sW[301], *filename;
  GAME matrix[PLAYERNUM][PLAYERNUM];
  FILE *fp;
  PLAYERDATA players[PLAYERNUM], *pl;
  MODE mode = ROBIN1;
  time_t ti;
  bool opt_p=false, opt_g=false, opt_t=false, opt_r=false;

  time(&ti);
  srand(ti);
  
  if (argc < 2) {
    
  error:
    
    Error("usage: robin -help | [-p][-g][-t][-r][-rand] (-rnd | -1 | -2 | -split n i1..in) tournament-file\n\
-help   : print a detailed description                               \n\
-p      : print player list                                          \n\
-g      : print game list                                            \n\
-t      : print game table                                           \n\
-r      : print ranking                                              \n\
-rand   : random games                                               \n\
-rnd    : permute list of players randomly                           \n\
-1      : single round-robin                                         \n\
-2      : double round-robin                                         \n\
-split n i1..in : split tournament into n parts when single r-r is complete\n");
    
  }
  
  argi = 1;
  
  if (!strcmp(argv[argi], "-help")) {

    print_help();
    exit(10);

  }

  FOREVER {
    if (!strcmp(argv[argi], "-p")) { opt_p = true; argi++; continue; }
    if (!strcmp(argv[argi], "-g")) { opt_g = true; argi++; continue; }
    if (!strcmp(argv[argi], "-t")) { opt_t = true; argi++; continue; }
    if (!strcmp(argv[argi], "-r")) { opt_r = true; argi++; continue; }
    if (!strcmp(argv[argi], "-rand")) { random_games = 1; argi++; continue; }
    break;
  }

  if (!argv[argi]) goto error;

  if (!strcmp(argv[argi], "-1"))   mode = ROBIN1;
  else   if (!strcmp(argv[argi], "-2"))   mode = ROBIN2; 
  else   if (!strcmp(argv[argi], "-rnd")) { mode = ROBIN1; rnd = 1; }
  else   if (!strcmp(argv[argi], "-split")) { 

    mode = ROBIN1;

    if (!argv[++argi]) Error("n?");
    if (sscanf(argv[argi], "%d", &split_n) != 1) Error("n?"); 
    if (split_n <= 1) Error("n <= 1");
    
    FOR (i, split_n) {
      if (!argv[++argi]) Error("ik?");
      if (sscanf(argv[argi], "%d", &split_i[i]) != 1) Error("ik?");
      if (split_i[i] <= 1) Error("ik?");
    }
    
  } else goto error;
  
  argi++;

  filename = argv[argi];

  if (!filename) Error("filename?");

  fp = fopen(filename, "r");

  if (!fp) Error("can't open tournament-file");

  /* read player's description */

  FOREVER {

    if (!fgets(s, 300, fp)) Error("end of file?");

    if (strlen(s) > 1) {
      
      if (s[0] == '%') break;
      
      if (s[0] != '#') {
	
	if (playernum >= PLAYERNUM-1) Error("too many players");      
	
	pl = &players[playernum];
	
	pl->num = playernum;
	pl->fpoints0 = pl->diff0 = 
	  pl->n0 = pl->tiebreaker = pl->oldi = pl->oldn = 0;
	
	i = sscanf(s, "%d. %s %f/%d %f %d %d/%d", 
		   &dummy,
		   pl->name, 
		   &pl->fpoints0, &pl->n0,  
		   &pl->diff0, &pl->tiebreaker,
		   &pl->oldi, &pl->oldn);
	
	if (i == 8) {
	  
	  if (pl->n0 < 0) Error("n0 < 0");
	  if (pl->fpoints0 < 0) Error("points0 <= 0");
	  if (pl->points0 >  pl->n0) 
	    Error("points0 > games0");
	  
	  if (pl->diff0 > +64 * pl->n0) 
	    Error("diff0 > +64 * games0");
	  
	  if (pl->diff0 < -64 * pl->n0) 
	    Error("diff0 < -64 * games0");
	  
	  if ((pl->oldn && pl->oldi < 1) || pl->oldi > pl->oldn) 
	    Error("oldi?");
	  if (pl->oldn < 0) Error("oldn?");
          
	}
	
	if      (i <= 1) Error("player name?");
	else if (i != 2 && i != 8) Error("incomplete player data");
	
	if (pl->name[strlen(pl->name)-1] == '\n')
	  pl->name[strlen(pl->name)-1] = 0;
	
	/* store double points ... */
	
	pl->points0 = int(rint(pl->fpoints0 * 2.0));  
	
	if (fabs(pl->points0 - pl->fpoints0 * 2.0) > 0.001) 
	  Error("points0 not multiple of 0.5");
	
	playernum++;
      }
    }
  }

  if (playernum & 1) { 
    
    /* introduce player "PASS" if #players is odd */
    
    pl = &players[playernum];
    pl->num = playernum;
    pl->points0 = pl->n0 = pl->tiebreaker = pl->oldi = pl->oldn = 0;
    pl->diff0 = 0;

    strcpy(pl->name, "PASS");
    pass = true;
    
    playernum++;
  }
  

  if (rnd) {
    
    /* permute list of players randomly and print it */
    
    printf("[seed=%lu]\n\n", ti);
    
    FOR (i, playernum-pass) {
      
      FOREVER {
	
	j = rand() % (playernum-pass);
	
	if (players[j].name[0]) break;
	
      }
      
      printf("%2d. %s\n", i+1, players[j].name);
      players[j].name[0] = 0;
      
    }
    
    printf("%%\n\n");
    
    exit(0);
  }
  
  /* print list of players */
  
  if (opt_p) {

    printf("\n");
    printf("PLAYERS & SEED VALUES:\n");
    printf("======================\n\n");
    printf("no. name       pts0/games0   diff0      tiebr.  oldi/oldn\n");
    printf("---------------------------------------------------------\n\n");
    
    FOR (i, playernum) { 
      
      printf("%2d. %-12s ", i+1, players[i].name);
      
      printf("%4.1f/%-2d   %+f   %+4d     %2d/%-2d\n", 
	     players[i].points0/2.0, players[i].n0, 
	     players[i].diff0, players[i].tiebreaker,
	     players[i].oldi, players[i].oldn);
      
    }
  
    printf("\n");
  }
  
  /* generate tournament matrix: playnum-1 rounds & "almost even" colors */
  
  generate(matrix, playernum, pass);
  
  
#if GEN_GAMES
  
  FOR (i, playernum-pass) {

    int komi;
    
    FOR (j, i) {
    
    char *sB, *sW;
    int b;
    
    if ((random_games && (rand() & 16) != 0) ^ (matrix[i][j].color[0] == BLACK)) { 
      sB = players[i].name; 
      sW = players[j].name; 
    } else {
      sW = players[i].name; 
      sB = players[j].name; 
    }
    
    b = rand() % 65;
    komi = -64 + rand() % 129;
    
    printf("
rated GAME: %s ( 15   0 120) vs. %s ( 15   0 120)  \n\
\n\
LM: g5D7e2D2g4E7b5B6c7F1c2D8a6G3e8F8c8B8h3F2c1D1e1B1g2\n\
\n\
\n\
-> ##                                              \n\
\n\
A  B  C  D  E  F  G  H                          \n\
1 |56|57|55|43|44|45|58|54| 1  \n\
2 |37|48|46|47|42|41|59|31| 2  \n\
3 |34|27|14|16| 2|12|24|30| 3  \n\
4 |36|29| 1|()|##| 7|23|21| 4  \n\
5 |33|25| 6|##|()| 5|13|20| 5  \n\
6 |32|22|17|10| 4| 3| 8|15| 6  \n\
7 |35|60|26|18|11| 9|49|52| 7  \n\
8 |53|40|39|28|19|38|51|50| 8  \n\
A  B  C  D  E  F  G  H      \n\
\n\
##:() - %d:%d K%+d R20              \n\n",
	   sB, sW, b, 64-b, komi);
    
    if (mode == ROBIN2) {

      if ((random_games && (rand() & 16) != 0) ^ (matrix[i][j].color[0] == WHITE)) { 
	sB = players[i].name; 
	sW = players[j].name; 
      } else {
	sW = players[i].name; 
	sB = players[j].name; 
      }
      
      b = rand() % 65;
      komi = rand() % 65;
	
      printf("
rated GAME: %s ( 15   0 120) vs. %s ( 15   0 120)  \n\
\n\
LM: g5D7e2D2g4E7b5B6c7F1c2D8a6G3e8F8c8B8h3F2c1D1e1B1g2\n\
\n\
\n\
-> ##                                              \n\
\n\
A  B  C  D  E  F  G  H                          \n\
1 |56|57|55|43|44|45|58|54| 1  \n\
2 |37|48|46|47|42|41|59|31| 2  \n\
3 |34|27|14|16| 2|12|24|30| 3  \n\
4 |36|29| 1|()|##| 7|23|21| 4  \n\
5 |33|25| 6|##|()| 5|13|20| 5  \n\
6 |32|22|17|10| 4| 3| 8|15| 6  \n\
7 |35|60|26|18|11| 9|49|52| 7  \n\
8 |53|40|39|28|19|38|51|50| 8  \n\
A  B  C  D  E  F  G  H      \n\
\n\
##:() - %d:%d K%+d R20                \n\n",
	   sB, sW, b, 64-b, komi);
    }
    }
  }

  exit(0);

#endif



  /* test whether it is a splitted tournament, split_n = #players in 1st part */
  
  FOR (i, playernum-pass) {
    
    if (splitted_n > 0) {
      
      if (players[i].oldn != splitted_n) Error("inconsistent oldn");
      
      
    } else {
      
      if (!i) splitted_n = players[i].oldn;
      else if (players[i].oldn > 0) Error("inconsistent oldn");
      
    }
    
    if (players[i].oldn < 0)                     Error("oldn < 0");
    if (players[i].oldi < 0)                     Error("oldi < 0");
    if (players[i].oldi > players[i].oldn)       Error("oldi > oldn");
    if (players[i].oldn && players[i].oldi <= 0) Error("oldi <= 0");
    
  }
  
  if (splitted_n > 0) { 
    
    int splitted_pass=0;
    GAME mat[PLAYERNUM][PLAYERNUM];
    
    printf("SPLITTED TOURNAMENT!\n\n");
    
    
    if (mode != ROBIN1) Error("splitted only with -1");
    
    if (splitted_n & 1) { splitted_n++; splitted_pass = 1; }
    
    /* generate matrix of first part */
    
    generate(mat, splitted_n, splitted_pass);
    
    /* invert colors */
    
    FOR (i, playernum-pass)
      FOR (j, playernum-pass)
      if (i != j) 
	matrix[i][j].color[0] = 
	  -mat[players[i].oldi-1][players[j].oldi-1].color[0];
    
  }
  
  
  /* read games */

  if (opt_g) {
    printf("\nGAME(S) SO FAR:\n");
    printf("===============\n\n");
  }

  gamenum = 0;
  
  FOREVER {
    
    if (!fgets(s, 300, fp)) break;
    
    if (strlen(s) > 1) {
      
      int d;
      
      if (sscanf(s, "%s", t) == 1 && !strcmp(t, "rated")) {

	if (opt_g) printf("%3d: ", gamenum+1);
	
	if (sscanf(s, "rated GAME: %s (%d %d %d) vs. %s (%d %d %d)", 
		   sB, &d, &d, &d, sW, &d, &d, &d) != 8) 
	  Error("Syntax error in game header");
	
	if (opt_g) printf("%-10s vs. %-10s  ", sB, sW); 
	
	FOR (i, playernum) if (!strcmp(players[i].name, sB)) break;
	if (i >= playernum) { 
	  fprintf(stderr, "%s: ", sB); 
	  Error("unknown player");
	}
	
	nB = i;
	
	FOR (i, playernum) if (!strcmp(players[i].name, sW)) break;
	if (i >= playernum) {
	  fprintf(stderr, "%s: ", sW); 
	  Error("unknown player");
	}
	
	nW = i;
	
	if (nB == nW) Error("same player?");
      
	gameind = 0;
      
	if (random_games) {

	  if (matrix[nB][nW].seen[0]) {
	    gameind = 1;
	    if (matrix[nB][nW].seen[1]) Error("more than two games per pair");
	  } 
	  
	  if (!matrix[nB][nW].seen[gameind]) {

	    matrix[nB][nW].seen[gameind] = true;

	    matrix[nB][nW].seen[gameind] = 1;
	    matrix[nB][nW].color[gameind] = BLACK;	    
	    
	    matrix[nW][nB].seen[gameind] = 1;
	    matrix[nW][nB].color[gameind] = WHITE;
	  }

	} else {

	  if ((nB < nW && matrix[nB][nW].color[0] != BLACK ) || 
	     (nB > nW && matrix[nW][nB].color[0] != WHITE )) {
	    if (mode == ROBIN1) Error("wrong colors!");
	    else gameind = 1;
	  }
	  if (matrix[nB][nW].seen[gameind]) Error("game already seen");

	}
	
	if (gameind > 0 && mode == ROBIN1) Error("too many games per pair");

	matrix[nB][nW].seen[gameind] = matrix[nW][nB].seen[gameind] = true;
	
	FOR (i, 7) {
	  if (!fgets(s, 300, fp)) Error("file too short");
	  if (!strncmp(s, "-> ##", strlen("-> ##"))) break;
	}
	
	if (i >= 7) Error("no -> ##");
	
	FOR (i, 12) if (!fgets(s, 300, fp)) break;
	if (i < 12 || !fgets(s, 300, fp)) Error("file too short");
	
	float komi;
	int rand;
	
	if (sscanf(s, "##:() - %d:%d K%f R%d", &dB, &dW, &komi, &rand) != 4)
	  Error("corrupt result line");

	if (opt_g) printf("r=%d  %2d-%2d = %+3d k=%+7.3f bpts=%3.1f bdiff=%+5.1f\n",
			  rand, dB, dW, dB-dW, komi,
			  (komi==dB-dW)? 0.5 : ((komi<dB-dW) ? 1 : 0),
			  dB-dW-komi);

	int black_result = dB-dW;
	
	/* gameind == 0 -> 1, == 1 -> -1 */

	matrix[nB][nW].komi[gameind] = komi;
	matrix[nB][nW].result[gameind] = black_result;
	
	matrix[nW][nB].komi[0] = matrix[nB][nW].komi[0];
	matrix[nW][nB].komi[1] = matrix[nB][nW].komi[1];
	
	matrix[nW][nB].result[0] = matrix[nB][nW].result[0];
	matrix[nW][nB].result[1] = matrix[nB][nW].result[1];
	
	gamenum++;
      }
    }
  }
  
  
  fclose(fp);
  
  if (opt_g) printf("\n");
  
  
  if (opt_t) {

    /* print round results */
    
    printf("\n\nROUND BY ROUND RESULTS:\n");
    printf("=======================\n\n");
    
    int part_num = (mode == ROBIN2) + 1;
    int part;
    
    FOR (part, part_num) {
      
      printf("          ");
      FOR (j, playernum-1) printf("rd %-2d ", (part == 0) ? j+1 : j+playernum);
      
      printf("\n");
      
      printf("         +");
      FOR (j, playernum-1) printf("-----+");
      
      printf("\n");
      
      
      FOR (i, playernum) {
	
	/* 1. row: color and opponent */
	
	printf("%-8s |", players[i].name);  
	
	FOR (j, playernum-1) {
	  
	  FOR (k, playernum) 
	    if (k != i && matrix[i][k].round == j) {

	      if (random_games) {

		if (matrix[i][k].seen[part])
		  printf("%c", (matrix[i][k].color[part] == BLACK) ? 'B' : 'W');
		else
		  printf(" ");

		printf("  %2d|", k+1);
		  
	      } else {
		
		printf("%c  %2d|",
		     (matrix[i][k].color[0] == BLACK) ^ (part==0) ? 'B' : 'W', k+1);
	      }
	    }
	}
	
	printf("\n");
	
	/* 2. row: points */
	
	printf("(%2d)     |", i+1);
	
	FOR (j, playernum-1) {
	  
	  FOR (k, playernum) {
	    if (k != i && matrix[i][k].round == j) {
	      
	      if (matrix[i][k].seen[part]) {

		float res = matrix[i][k].result[part] - matrix[i][k].komi[part];

		if (matrix[i][k].color[part] == WHITE) res = -res;
		  
		  printf("%s",
			 res == 0.0 ? " 1/2 |" : 
		       (res < 0 ? "  0  |" : "  1  |"));
		
	      } else {
		
		if (matrix[i][k].dummy) printf("  %%  |");
		else                    printf("     |");
		
	      }
	    }
	  }
	}
	
	printf("\n");
	
	
	/* 3. row: result */
	
	printf("         |");    
	
	FOR (j, playernum-1) {
	  
	  FOR (k, playernum) {
	    if (k != i && matrix[i][k].round == j) {
	      
	      if (matrix[i][k].seen[part]) {
		
		printf("RE%+3d|", matrix[i][k].result[part]);
	      } else printf("     |");
	    }
	  }
	}
	
	printf("\n");

	/* 4. row: komi */
	
	printf("         |");    
	
	FOR (j, playernum-1) {
	  
	  FOR (k, playernum) {
	    if (k != i && matrix[i][k].round == j) {
	      
	      if (matrix[i][k].seen[part]) {
		
		printf("%+5.1f|", matrix[i][k].komi[part]);
		
	      } else printf("     |");
	    }
	  }
	}
	
	printf("\n");
	
	printf("         +");
	FOR (j, playernum-1) printf("-----+");
	
	printf("\n");
	
      }  
      printf("\n");
    }
  }    

  /* sort players according pointavg and diffavg */
    
  FOR (i, playernum-pass) {
    
    points = n = 0;
    diff = 0;
    
    FOR (j, playernum) {
      
      if (matrix[i][j].seen[0]) {

	float res = matrix[i][j].result[0] - matrix[i][j].komi[0];
	if (matrix[i][j].color[0] == WHITE) res = -res;

	// printf("0: %d %d %d %f\n", i, j, matrix[i][j].color[0], res);
	
	points += res == 0.0 ? 1 : (res < 0.0 ? 0 : 2);
	diff += res;
	n++;
      }
      
      if (matrix[i][j].seen[1]) { 
 
	float res = matrix[i][j].result[1] - matrix[i][j].komi[1];
	if (matrix[i][j].color[1] == WHITE) res = -res;
	
	// printf("1: %d %d %d %f\n", i, j, matrix[i][j].color[1], res);

        points += res == 0.0 ? 1 : (res < 0.0 ? 0 : 2);
	diff += res;
	n++;
      }
    }

    // printf("%d -> %f %f\n", i, diff, players[i].diff0);
       
    players[i].points = points + players[i].points0;
    players[i].diff   = diff + players[i].diff0;
    players[i].n      = n + players[i].n0;
  }
  
  qsort(players, (size_t) playernum-pass, sizeof(players[0]), cmpPLAYERDATA);
  
  if (opt_r) {
  
    /* print ranking */
    
    printf("\n\nRANKING:\n");
    printf("========\n\n");
    printf("rank player        pts/games pointavg   diff   diffavg\n");
    printf("------------------------------------------------------\n");
    
    FOR (i, playernum-pass) {
      
      if (i > 0 && !cmpPLAYERDATA2(&players[i-1], &players[i])) printf(" =");
      else printf("%2d", i+1);
      
      printf("   %-12s   %4.1f/%-2d    ", 
	     players[i].name, players[i].points/2.0, players[i].n);
      
      if (players[i].n) 
	printf("%4.2f   %+7.2f  %+6.2f ", 
	       ((float)players[i].points/2.0)/players[i].n,
	       players[i].diff,
	       ((float)players[i].diff)/players[i].n); 
      else
	printf("0.50     +0.00   +0.00");
      
      
      printf("\n");   
    }
    
    printf("\n");
  }
    
  if (split_n > 0) {
    
    int start;
    
    playernum -= pass;
    
#if 1
    if (gamenum != playernum * (playernum - 1) / 2) 
      Error("split senseless: single round-robin not complete");
#endif
    
    // if (playernum <= split_n) Error("#player <= n"); 
    
    int sum = 0;
    FOR (i, split_n) sum += split_i[i];
    
    if (playernum != sum) Error("#player != i1..in");
    
    printf("\nSPLIT:\n");
    printf("======\n\n");
    
    /* split field into split_n parts */
    
    FOR (i, split_n) gr_num[i] = split_i[i];
    
    // old split:
    // FOR (i, split_n) gr_num[i] = playernum / split_n;
    // rem = playernum - split_n * (playernum/split_n);
    // for (i=0; rem && i < split_n; i++) { gr_num[i]++; rem--; }
    
    start = 0;
    
    FOR (i, split_n) {
      
      printf("# Group %d\n\n", i+1);
      
      FOR (j, gr_num[i]) {
	
	printf("%2d. %-12s  ", j+1, players[start+j].name);
	
	if (players[start+j].n) 
	  printf(" %4.1f/%-2d   %+f", 
		 (float)players[start+j].points/2.0,
		 players[start+j].n,
	       players[start+j].diff);
	else
	  printf("  0.0/0     +0 ");
	
	printf("   %+4d     %2d/%-2d\n", 
	       players[start+j].tiebreaker,
	       players[start+j].num+1,
	       playernum);
      }
      
      printf("%%\n\n");
      
      start += gr_num[i]; 
      
    }
  }
  
  return 0;
}



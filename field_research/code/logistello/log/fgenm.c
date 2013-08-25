// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* generate update functions / 7.95 */

#include "main.h"
#include "sboard.h"
#include "board.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"

#if 0

/* header */

#include "main.h"
#include "board.h"

extern STRAHLTYP *gps;
extern BRETT *gpb;

#define FU(f) void f(void)
#define CA(f) \
register BRETT *pb = gpb;\
register STRAHLTYP *ps = gps;\
register sint1 *p = pb->p;\
*gpb->pcalled++ = f;
#define FI    if (!pb->first_call) { pb->first_call=1;
#define EL    } else {
#define HA(h1,h2) \
  pb->Hash1 ^= h1;\
  pb->Hash2 ^= h2;
#define PA(p,d) ps[p] += d;
#define DI(i,d) p[i] = d;
#define DF(d)   pb->StDiffBW += d;

#endif




/* parameters: -col,move,line,lpos,l,r, 
               col(b,w),move,line,lpos,l,r, 
               disc-diff diff,
               update_prefix,
              -col,move,line,lpos,l,r, update_first, update */

char *fmask = "\
FU(f_%s_%d_%d_%d_%d_%d);\n\
FU(f_%s_%d_%d_%d_%d_%d) {\n\
CA(f_%s_%d_%d_%d_%d_%d)\n\
DF(%d)\n\
%s\
FI\n\
%s\
EL\n\
%s\
}}\n\n";


/* parameters: hash1,hash2,pattern-updates,disc-updates */

char *upmask = "\
%s\
%s\
";


/* parameters: line,offset */

char *pmask = "PA(%d,%d)\n";

/* parameters: pos,col(-1,1) */

char *dmask = "DI(%d,%d)\n";



void _abort(void) {};



#define PADJ(v)				   \
  { int i; FOR (i, DELTA_MAX) {            \
    *(pda->s[i]) =*(pda->s[i]) v pda->d[i];\
  }}

#define PADJ2(v)			    \
  { int i; FOR (i, DELTA_MAX) {             \
    *(pda->s[i]) =*(pda->s[i]) v pda->dh[i];\
  }}


void flip_disc(BRETT *pb, int pos)
{
  POSDATEN *pda=&pb->daten[pos];

  if (pb->p[pos] != BLACK && pb->p[pos] != WHITE) Error("flip_disc: empty!");

  pb->Hash1 ^= pda->ZufBW1;
  pb->Hash2 ^= pda->ZufBW2;

  if (pb->p[pos] == WHITE) { PADJ(+) } else { PADJ(-) }

  pb->p[pos] = - pb->p[pos];
}


void set_disc(BRETT *pb, int pos, int col)
{
  POSDATEN *pda=&pb->daten[pos];

  if (pb->p[pos] != LEER) Error("set_disc: not empty!");

  pb->p[pos] = col;


  if (col == BLACK) { 
 
    PADJ2(+)
    pb->Hash1 ^= pda->ZufBLACK1; 
    pb->Hash2 ^= pda->ZufBLACK2;

  } else { 

    PADJ2(-) 
    pb->Hash1 ^= pda->ZufWHITE1;
    pb->Hash2 ^= pda->ZufWHITE2;

  }
}


void clear_board(BRETT *pb)
{
  memset(pb->p, 0, sizeof(pb->p));
  memset(pb->NewPatt.st, 0, sizeof(pb->NewPatt.st));
  pb->Hash1 = pb->Hash2 = 0;
}


extern PATTD_INFO DeltaInfo[100][DELTA_MAX];

void SignalCheck(COMZIO *pcio, ZUGIO *pzio, bool no_count) {}

typedef struct { 
  int a[100], b[100];
  int num;
} PAIRS;


int main(void)
{
  char s[100], supre[10000], sup1[10000], sup2[10000], 
       sp1[10000], sp2[10000], sd1[10000], sd2[10000];
  int i, j, col, move, l, r, k, lpos, line, 
      Hash1, Hash2;
  BRETT board;
  BSTRAHLEN patterns;
  SPFELD sf;
  ZUGIO zio;
  PAIRS d_set, d_noset, p_set, p_noset; 


  InitZug(&zio, NULL, SignalCheck, 10);


  SfGrund(&sf);
  SfBrett(&sf, &board);

#if 1

  FOR_SFPOS10(move) {

    FOR (k, DELTA_MAX) {

      line = DeltaInfo[move][k].pnum;

      if (DeltaInfo[move][k].is_line) {

        lpos = DeltaInfo[move][k].log3;

        FOR (l, 7)
          FOR (r, 7) 

            if ((l || r) &&
		(l == 0 || l < lpos) && 
                (r == 0 || r < DeltaInfo[move][k].discs - lpos - 1)) {

	      for (col=-1; col <= 1; col+=2) {

printf("/* move=%d line=%d lpos=%d l=%d r=%d diff=%d */\n", move, line, lpos, l, r, DeltaInfo[move][k].diff);

/************ update with move position *************/

                d_set.num = p_set.num = 0;

                clear_board(&board);

                for (i=-l; i <= r; i++) {

		  if (i) 
		    set_disc(&board, move - i * DeltaInfo[move][k].diff, -col);

		}

                memcpy(patterns.st, board.NewPatt.st, sizeof(board.NewPatt.st));

                for (i=-l; i <= r; i++) {

                  if (i) { 
                    flip_disc(&board, move - i * DeltaInfo[move][k].diff);
		  }

		  d_set.a[d_set.num] = move - i * DeltaInfo[move][k].diff;
		  d_set.b[d_set.num] = 1; 
		  d_set.num++;

		}

	        set_disc(&board, move, col);


                FOR (i, ALLPATT_NUM) {

		  if (patterns.st[i] != board.NewPatt.st[i]) {

		    p_set.a[p_set.num] = i;
		    p_set.b[p_set.num] = board.NewPatt.st[i] - patterns.st[i]; 
		    p_set.num++;
	    
		  }

		}


/************ update without move position *************/

                d_noset.num = p_noset.num = 0;

                clear_board(&board);

                for (i=-l; i <= r; i++) {

                  if (i) 
		    set_disc(&board, move - i * DeltaInfo[move][k].diff, -col);

		}

                memcpy(patterns.st, board.NewPatt.st, sizeof(board.NewPatt.st));
		Hash1 = board.Hash1; Hash2 = board.Hash2;


                for (i=-l; i <= r; i++) {

                  if (i) { 
                    flip_disc(&board, move - i * DeltaInfo[move][k].diff);

		    d_noset.a[d_noset.num] = move - i * DeltaInfo[move][k].diff;
		    d_noset.b[d_noset.num] = 1; 
		    d_noset.num++;

		  }

		}

                Hash1 ^= board.Hash1;
                Hash2 ^= board.Hash2;

                FOR (i, ALLPATT_NUM) {

		  if (patterns.st[i] != board.NewPatt.st[i]) {

		    p_noset.a[p_noset.num] = i;
		    p_noset.b[p_noset.num] = board.NewPatt.st[i] - patterns.st[i]; 
		    p_noset.num++;
	    
		  }

		}



              supre[0] = 0;

/* hash update */

              sprintf(supre+strlen(supre), "HA(0x%x,0x%x)\n", Hash1, Hash2); 

/* board updates */
	
              FOR (i, d_noset.num) {
		if (d_noset.b[i]) 
		  sprintf(supre+strlen(supre), dmask, d_noset.a[i], col); 
              }

/* cancel double occurrences in pattern update lists */

              FOR (i, p_set.num)
                FOR (j, p_noset.num)
		  if (p_set.b[i] && p_noset.b[j] &&
		      p_set.a[i] == p_noset.a[j] &&
		      p_set.b[i] == p_noset.b[j]) {

		    sprintf(supre+strlen(supre), pmask, p_set.a[i], p_set.b[i]); 
		    p_set.b[i] = p_noset.b[j] = 0;
		  }
		


              sd1[0] = sd2[0] = 0;

              sp1[0] = sp2[0] = 0;

              FOR (i, p_set.num) {
		if (p_set.b[i]) 
		  sprintf(sp1+strlen(sp1), pmask, p_set.a[i], p_set.b[i]); 
              }
             
              FOR (i, p_noset.num) {
		if (p_noset.b[i]) 
		  sprintf(sp2+strlen(sp2), pmask, p_noset.a[i], p_noset.b[i]); 
              }


	      sprintf(sup1, upmask, sd1, sp1);

	      sprintf(sup2, upmask, sd2, sp2);


		printf(fmask, 
		       col == WHITE ? "b" : "w", move, line, lpos, l, r,
		       col == BLACK ? "b" : "w", move, line, lpos, l, r,
		       col == WHITE ? "b" : "w", move, line, lpos, l, r,
		       2*(l+r)*col,
		       supre,
		       sup1,
		       sup2);




	      }
	    }
      }
    }
  }

#endif


  printf("\n\n\n\n\n/* function table init */\n\n");

  printf("void (*finit[100*4*2*11+1])(void) = {\n\n");


  FOR (move, 100) {

    FOR (k, 4) {

      { 


	if (!TabZug[move]) {
          line = 0;
        } else { 
          line = board.daten[move].movedata[k].l - board.NewPatt.st;
        }

        lpos = board.daten[move].movedata[k].p;

        for (col=BLACK; col >= WHITE; col -= 2) {

	  i = 1;

	  if (line) 
          for (; i <= 11; i++) {

	    if (lpos == 0 || lpos == 1) {

              if (i > board.daten[move].movedata[k].len - 2 - lpos) break;

	      r = i; l = 0;

            } else if (lpos < board.daten[move].movedata[k].len - 2) {

	      r = i / lpos; l = i % lpos;

	      if (lpos + r + 1 >= board.daten[move].movedata[k].len) break;

            } else {

	      if (i > lpos - 1) break;

	      r = 0; l = i; 

            }

            sprintf(s, "f_%s_%d_%d_%d_%d_%d, ", 
		      col == BLACK ? "b":"w", move, line, lpos, l, r);

            printf("%-17s", s);
	  }

	  for (; i <= 11; i++) printf("%-17s", "NULL, ");
          printf("\n");

        }
      }
    }    
  }

  printf("\nNULL\n};\n");


  return 0;
}





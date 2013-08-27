/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Non-edge table for the 5-diagonal 
 *
 * Kai-Fu Lee 10/6/85 
 *
 * Modified for NewBill - Sanjoy Mahajan - 4 August 1986
 * also to convert the output into an int.  
 */

#include <stdio.h>
#include <tables.h>
#include <pack.h> /* for packing mscore, pscore, and wscore into 32-bits */

/* #define PRINT */
#define TRUE 1
#define FALSE 0
#define Opposite(x) (!(x))
#define BLACK 0
#define WHITE 1
#define BLANK 2
#define LOW_X 1				/* number of lower x-square in diag8 */
#define HIGH_X 6			/* for upper x-sq */
#define X_SQUARE_PENALTY 1150		/* penalty for x-square moves w/
					 * corner empty */

#define Abs(x) (((x) > 0) ? (x) : (-x))
#define Max(x,y) (((x) > (y)) ? (x) : (y))
#define Min(x,y) (((x) > (y)) ? (y) : (x))

/* constants needed to normalize stuff */

#define RANGE_W ((double) ((1<<W_ACTUAL) - 1))
#define RANGE_P ((double) ((1<<P_ACTUAL) - 1))
#define RANGE_M ((double) ((1<<M_ACTUAL) - 1))

FILE *fp;

double          Fudge_Factor[NUM_TABLES] =	/* for fudging when there are
						 * two dirs */
{
 1.0, 1.0, 1.5, 1.3, 1.2, 1.0,		/* diags */
 2.0, 1.5, 1.0,				/* tb's */
};

char            Name[NUM_TABLES][20] =
{"Diag3", "Diag4", "Diag5", "Diag6", "Diag7", "Diag8", "HV_Table1", "HV_Table2", "HV_Table3"};

struct table
{
    int             mscore,		/* move penalty */
                    wscore,		/* weighted square: using unstable,
					 * stable, etc. */
                    pscore;		/* potential mobility */
};

int             Total[10] =		/* 3^i,except for Total[9], which is
					 * for DIAG8 */
{
 1, 3, 9, 27, 81, 243, 729, 2187, 6561, 6561 * 4
};

struct table    Tables[NUM_TABLES][6561],
                Diag8[6561 * 4],	/* patch for Diag8 x-square legals */
                Tb1[6561 * 4];

struct flips_type
{
    int             start,		/* starting square # */
                    flips;		/* how mnay discs flipped */
};

extern int      Value[NUM_TABLES][NUM_TYPES][MAX_PIECES][MAX_PIECES + 1],
                Move_Penalty[NUM_TABLES][MAX_PIECES - 1][MAX_PIECES + 1],
                Pot_Val[NUM_TABLES][MAX_PIECES];

static int             debug,      Tb_Num,             Size;

main(argc, argv)
     int             argc;
     char          **argv;
{
  int        j;
  int             edge[MAX_PIECES];
  
  debug = argc > 1;
  for (Tb_Num = 0; Tb_Num < NUM_TABLES; Tb_Num++)	/* do each table */
    {
#ifdef PRINT
      fprintf(stderr, "Doing %s\n", Name[Tb_Num]);
#endif
      Size = pieces(Tb_Num);
      for (j = 0; j < Total[Size]; j++)	/* do each entry */
	{
	  int debug_flag;
	  
	  to_edge(edge, j);		/* convert to edge, edge[0] most
					 * signficant */
/*
	  if (j == 11 || j == 4617)
	    debug_flag = TRUE;
 */
	  initialize_edge(edge, j, 1);/* fill in values */
	}
    }
  Tb_Num = DIAG8;
  patch_diag8 ();
  Tb_Num = TB1;
  patch_tb1 (); 
  
  normalize();
  for (Tb_Num = 0; Tb_Num < NUM_TABLES; Tb_Num++)
    {
      if (Tb_Num == DIAG8)
	{
	  Size = 8;
	  print_fudged_table(Diag8);
	} else if (Tb_Num == TB1)
	  {
	    Size = 8;
	    print_fudged_table(Tb1); 
	  } else
	    {
	      Size = pieces(Tb_Num);
	      print(Tables[Tb_Num],Size);  
	    }
    }
  print53();
  print44();
  return 0;
}

print(tab,size)
struct table   *tab;
int size;
{
    if (debug)
	print_result2(tab,size);
    else
	print_result(tab,size);
}

/*
 * normalizes wscores and pscores in the range -1000 to +1000.  Doesn't do
 * mscore, because that is to be subtracted from a total value, then from the
 * value above the tree. Pscore and wscore, are subtracted directly from the
 * level above (in the tree search). The min mscore and max are printed out
 * just for fun. 
 */

normalize()
{
    int        i,
                    j,
                    size;
    static struct table min = {1000000, 1000000, 1000000},
                    max = {-1000000, -1000000, -1000000};
    static struct
    {
	double          wscore,
	                pscore,
			mscore;
    }               delta, scale;

    for (i = 0; i < NUM_TABLES; i++)	/* first find min's and max's */
    {
	size = Total[pieces(i)];
	for (j = 0; j < size; j++)
	{
	    min.wscore = Min(min.wscore, Tables[i][j].wscore);
	    min.pscore = Min(min.pscore, Tables[i][j].pscore);
	    min.mscore = Min(min.mscore, Tables[i][j].mscore);

	    max.wscore = Max(max.wscore, Tables[i][j].wscore);
	    max.pscore = Max(max.pscore, Tables[i][j].pscore);
	    max.mscore = Max(max.mscore, Tables[i][j].mscore);
	}
    }
    for (i=0;i<6561*4;i++)
    {
	max.mscore = Max(max.mscore,Diag8[i].mscore);
	max.mscore = Max(max.mscore,Tb1[i].mscore);
	min.mscore = Min(min.mscore,Diag8[i].mscore);
	min.mscore = Min(min.mscore,Tb1[i].mscore);
    }
#ifdef PRINT
    fprintf(stderr, "wscore: [%5d, %5d]\n", min.wscore, max.wscore);
    fprintf(stderr, "pscore: [%5d, %5d]\n", min.pscore, max.pscore);
    fprintf(stderr, "mscore: [%5d, %5d]\n", min.mscore, max.mscore);
#endif

    scale.wscore = RANGE_W / (max.wscore - min.wscore);
    scale.pscore = RANGE_P / (max.pscore - min.pscore);
    scale.mscore = RANGE_M / (max.mscore - min.mscore);
    delta.wscore = RANGE_W - max.wscore * scale.wscore;
    delta.pscore = RANGE_P - max.pscore * scale.pscore;
    delta.mscore = RANGE_M - max.mscore * scale.mscore;

    fp = fopen("misc/scale.c", "w");
    if (!fp)
        fprintf(stderr,"Couldn't create scale.c file.\n");
    fprintf(fp,"double Mscale = %f;\n", scale.mscore);
    fclose(fp);
    for (i = 0; i < 6561*4; i++)
    {
	    Tb1[i].wscore = Tb1[i].wscore * scale.wscore + delta.wscore;
	    Tb1[i].mscore = Tb1[i].mscore * scale.mscore + delta.mscore;
	    Tb1[i].pscore = Tb1[i].pscore * scale.pscore + delta.pscore;

	    Diag8[i].wscore = Diag8[i].wscore * scale.wscore + delta.wscore;
	    Diag8[i].mscore = Diag8[i].mscore * scale.mscore + delta.mscore;
	    Diag8[i].pscore = Diag8[i].pscore * scale.pscore + delta.pscore;
    }
    for (i = 0; i < NUM_TABLES; i++)
    {
	size = Total[pieces(i)];	/* number of elements in table */
	for (j = 0; j < size; j++)
	{
	    Tables[i][j].wscore = Tables[i][j].wscore * scale.wscore + delta.wscore;
	    Tables[i][j].mscore = Tables[i][j].mscore * scale.mscore + delta.mscore;
	    Tables[i][j].pscore = Tables[i][j].pscore * scale.pscore + delta.pscore;
	}
    }
}

/* convert the index: i to an edge */

to_edge(edge, i)
int        edge[MAX_PIECES],
                i;
{
    int        j;
    int             save = i;

    for (j = Size - 1; j >= 0; j--)
    {
	edge[j] = i % 3;
	i /= 3;
    }
    if (i != 0)
    {
        fprintf(stderr, "save = %d   i = %d    size = %d\n", save, i, Size);
	exit(1);
    }
}

/****************************************************************
                       IO Routines 
 *****************************************************************/


print_result(tab1,size)
struct table    *tab1;
int size;
{
    int        i;
    unsigned int convert();

    printf("unsigned int %s[%d] =\n{\n   ", Name[Tb_Num], Total[size]);
    for (i = 0; i < Total[size]; i++)
    {
	printf(" %9u,", convert(&tab1[i])); 
	if (i % 5 == 4)
	    printf("\n   ");
    }
    printf("\n};\n\n");
}

unsigned
convert(s)
 struct table *s;
{
    static packed   temp;

    if (s->pscore > (int) RANGE_P)
    {
	fputs("p overflow\n", stderr);
	exit(1);
    }
    if (s->wscore > (int) RANGE_W)
    {
	fputs("w overflow\n", stderr);
	exit(1);
    }
    if (s->mscore > (int) RANGE_M)
    {
	fputs("m overflow\n", stderr);
	exit(1);
    }
    temp.scores.pscore = s->pscore;
    temp.scores.mscore = s->mscore;

    /*  convert wscore from [0,2^#bits-1] to [-(2^(#bits-1)), 2^(#bits-1)-1].
	In other words, make it signed and centered about zero.  This was
	done becuase of overflow considerations 

	temp.scores.wscore = s->wscore - (1 << (W_ACTUAL - 1)); */

    /* actually shouldn't make it signed, because addition of signed
       bit fields can't be done by adding the whole long word, without
       bugs that is.  So have to make everything unsigned. Have to
       subtract the subtraction amount in compute4() or compute4_newedge()
       before wscore is used. */
    temp.scores.wscore = s->wscore;

    return temp.all;		/* return long */
}

/* Verbose print result */

print_result2(tab1,size)
 struct table *tab1;
int size;
{
    int        i,
                    edge[MAX_PIECES];

    printf("Table: %s\n", Name[Tb_Num]);
    for (i = 0; i < Total[size]; i++)
    {
	to_edge(edge, i);
	print_edge(edge);
	printf("-->   %5d %5d %5d\n",
	       (int) tab1[i].mscore,
	       (int) tab1[i].pscore,
	       (int) tab1[i].wscore);
    }
    printf("\n");
}

print_fudged_table (tab)
 struct table *tab;
{
    int        i,
                    j,
                    edge[MAX_PIECES];

    if (!debug)
	print_result(tab, 9);
    else
    {
	printf("Table: %s\n", Name[Tb_Num]);
	for (i = 0; i < 6561; i++)
	{
	    to_edge(edge, i);
	    for (j = 0; j < 4; j++)	/* do each corner combination */
	    {
		printf("%1d %1d: ", j & 1, (j & 2) >> 1);
		print_edge(edge);
		printf("-->   %5d %5d %5d\n",
		       (int) tab[4 * i + j].mscore,
		       (int) tab[4 * i + j].pscore,
		       (int) tab[4 * i + j].wscore);
	    }
	}
	printf("\n");
    }
}

/* Printing functions for verbose output */

print_edge(edge)
int      *edge;
{
    int     i;
    static char name[4]  = {'B','W','.','?'};	/* ? is for Diag8 legal */

    for (i = 0; i < Size; i++)
    {
        printf("%c", name[edge[i]]);
	printf(" ");
    }
    fflush(stdout);
}

/***************************************************************
                 Static Evaluation Procedures
 ***************************************************************/

/*
 * Compute the static edge value by using the Value array on
 * stable/NOT_UNSTABLE/unstable pieces.  These static values are to be used
 * for the minimax search. 
 */

initialize_edge(edge, entry, pass)
int       *edge,
                entry;
int             pass;			/* 1 or 2.  2 is last pass */
{
    int             stability[MAX_PIECES],
                    moves[MAX_PIECES];
    int        i;

    for (i = 0; i < Size; i++)		/* unstable until proven otherwise */
	stability[i] = UNSTABLE;
    if (pass == 1)
    {
	find_moves(edge, moves);
	find_stable(edge, stability);
	find_semi(edge, stability);
    }
    Tables[Tb_Num][entry].wscore = weighted_square(edge, stability);
    Tables[Tb_Num][entry].pscore = potential_mobility(edge);
    Tables[Tb_Num][entry].mscore = move_penalty(edge, moves, Tables[Tb_Num][entry].wscore);
}

/*
 * Finds all the stable pieces by: start from each of the two corners find
 * last non-blank disc (last) find the first disc in same-color sequence
 * leading up to last (first) if there is no change of color, everything up to
 * last is stable. if there is a change of color (first != real first disc),
 * everything from the corner to the disc before first is stable. STABLE = Can
 * never be flipped. NO_MOVES = Can be unstable only after 2 (max) "stupid"
 * forced moves. ONE_MOVE = Can be unstable only after 1 "stupid" forced move. 
 */

find_stable(edge, stability)
int       *edge;
int       *stability;
{
    int        i,
                    last,
                    first;
    int             numblanks,
                    first1,
                    found_blank = FALSE;

    if (edge[0] != BLANK)
    {
	for (i = 0; i < Size; i++)
	    if (edge[i] == edge[0])
		stability[i] = STABLE;
	    else
		break;
    }
    if (edge[Size - 1] != BLANK)
    {
	for (i = Size - 1; i >= 0; i--)
	    if (edge[i] == edge[Size - 1])
		stability[i] = STABLE;
	    else
		break;
    }
}

/*
 * Finds all other types of discs.  Calls make_move to get unstable discs. 
 */

find_semi(edge, stability)
int            *edge,
               *stability;
{
    int        i,
                    j,
                    ok;
    int             white_seq,
                    black_seq;
    int             unstable[MAX_PIECES];

    for (i = 0; i < Size; i++)
	unstable[i] = 0;

    for (i = 0; i < Size; i++)
	if (edge[i] == BLANK)
	    make_move(unstable, edge, i);	/* finds some more types */

    for (i = 0; i < Size; i++)
	if (edge[i] != BLANK && !unstable[i] && (stability[i] != STABLE))
	{
	    stability[i] = NOT_UNSTABLE;
	    ok = 0;
	    for (j = i + 1; j < Size; j++)
		if (edge[j] == Opposite(edge[i]))
		{
		    ok = 1;
		    break;
		} else
		if (edge[j] == BLANK)
		    break;
	    if (ok)
		for (j = i - 1; j >= 0; j++)
		    if (edge[j] == Opposite(edge[i]))
		    {
			ok = 1;
			break;
		    } else
		    if (edge[j] == BLANK)
			break;
	    if (!ok)
		continue;
	    ok = 0;
	    for (j = i + 2; j < Size; j++)
		if (edge[j] == BLANK)
		{
		    if (edge[j - 1] == Opposite(edge[i]))
			ok++;
		    break;
		}
	    for (j = i - 2; j >= 0; j--)
		if (edge[j] == BLANK)
		{
		    if (edge[j + 1] == Opposite(edge[i]))
			ok++;
		    break;
		}
	    if (ok == 2)
		stability[i] = TWO_MOVES;
	    else
	    if (ok == 1)
		stability[i] = ONE_MOVE;
	    else
		stability[i] = NO_MOVES;
	}
}

/*
 * Tries the (only) two possible moves in the two directions (+1, -1), finds
 * pieces that are unstable and mark them in the unstable array as such. Don't
 * need to worry which color is to move since only one color can move for each
 * direction. 
 *
 * Marks unstable pieces as the ones that get flipped. 
 */

make_move(unstable, edge, move)
int            *unstable,
               *edge,
                move;
{
    int        i,
                    j;

    if (move != Size - 1)
    {
	if (edge[move + 1] != BLANK)
	    for (i = move + 2; i < Size; i++)
	    {
		if (edge[i] == BLANK)
		{
		    break;
		} else
		if (edge[i] == Opposite(edge[move + 1]))
		{
		    for (j = move + 1; j <= i - 1; j++)
			unstable[j] = 1;
		    break;
		}
	    }
    }
    if (move != 0)
    {
	if (edge[move - 1] != BLANK)
	    for (i = move - 2; i >= 0; i--)
	    {
		if (edge[i] == BLANK)
		{
		    break;
		} else
		if (edge[i] == Opposite(edge[move - 1]))
		{
		    for (j = move - 1; j >= i + 1; j--)
			unstable[j] = 1;
		    break;
		}
	    }
    }
}

/*
 * Gives bonus for BLACK owning the discs he does.  Considers types of discs
 * they are, where they are, and how long the sequence is.  Incorporates the
 * sequence into the value by having it included in Value[][][]. 
 */

weighted_square(edge, stability)
int        edge[MAX_PIECES];
int            *stability;
{
    int        i;
    int             length = 0,		/* for calculating length of sequence */
                    total = 0,		/* total penalty */
                    where = 0;		/* for keeping start of sequences */

    for (i = 0; i < Size; i++)
    {
	if (edge[i] == BLACK)
	{
	    if (length == 0)		/* start of new sequence? */
		where = i;
	    length++;
	} else				/* means we have come to end of
					 * sequence. Have to add penalty */
	{
	/*
	 * stability[where] is the type of disc sequence this is (e.g.
	 * UNSTABLE). where is the location of the start of this sequence.
	 * length is how many discs long the sequence is. 
	 */
	    total += Value[Tb_Num][stability[where]][where][length];
	    length = 0;			/* look for new sequence */
	}
    }
    if (length != 0)			/* means we had a BLACK sequence ended
					 * by boundary */
	total += Value[Tb_Num][stability[where]][where][length];
    return (total);
}

/*
 * Computes the potential mobility of BLACK, by considering how many WHITE
 * discs are near empty squares.  No account of the pot_mob for white is
 * taken.  That part is done by the level above stuff in Bill
 */

potential_mobility(edge, moves)
int        edge[MAX_PIECES];
{
    int        i;
    double          total = 0;

    for (i = 0; i < Size; i++)
    {
	if (edge[i] == BLANK)
	{
	    if (i > 1)
		if (edge[i - 1] == WHITE)
		    total += Pot_Val[Tb_Num][i];
	    if (i < Size - 2)
		if (edge[i + 1] == WHITE)
		    total += Pot_Val[Tb_Num][i];
	} else
	{
	    if (i > 0 && i < Size - 1)
	    {
		if (edge[i - 1] == BLANK)
		    if (edge[i] == WHITE)
			total += (Pot_Val[Tb_Num][i - 1] / 2.0);
		if (edge[i + 1] == BLANK)
		    if (edge[i] == WHITE)
			total += (Pot_Val[Tb_Num][i + 1] / 2.0);
	    }
	}
    }
    return ((int) total);
}

/*
 * compute the value of having moves on the line (for BLACK to move). wscore
 * is the weighed square score for edge. 
 */

move_penalty(edge, moves, wscore)
int            *edge,
               *moves[MAX_PIECES],
                wscore;
{
    double          total = 0,
                    one_move_penalty();
    int             i;

    for (i = 0; i < Size; i++)
	if (moves[i])			/* a legal move? */
	    total += one_move_penalty(edge, i, wscore);
    return ((int) total);
}

to_index(edge)
int        edge[MAX_PIECES];
{
    int        ret = 0,
                    i;

    for (i = Size - 1; i >= 0; i--)
	ret = 3 * ret + edge[i];
    return (ret);
}

/* return the penalty from making a move to position i in the edge */

double one_move_penalty(edge, move, wscore)
int            *edge,
                move,
                wscore;
{
    int        new_edge[MAX_PIECES];
    int        i;
    double          total = 0,
                    left;
    struct flips_type ret[2];
    int             delta;
    int             w;

    for (i = 0; i < Size; i++)
	new_edge[i] = edge[i];
    really_make_move(new_edge, move, ret);
    if (move > ret[0].start)		/* got to reverse stuff? (not right
					 * format) */
	ret[0].start = Size - move;
    if (ret[1].flips == 0)		/* only one direction? */
	total = Move_Penalty[Tb_Num][ret[0].start][ret[0].flips];
    else
    {
	if (move > ret[1].start)
	    ret[1].start = Size - move;
	total = (Move_Penalty[Tb_Num][ret[0].start][ret[0].flips] +
		 Move_Penalty[Tb_Num][ret[1].start][ret[1].flips]) / 2.0;
	total *= Fudge_Factor[Tb_Num];	/* to reduce penalty */
    }
    return (total);
}

really_make_move(edge, move, ret)
int        edge[MAX_PIECES];
int             move;
struct flips_type ret[2];		/* for first and second dir */
{
    int        i,
                    j;
    int             flips = 0,
                    dirs = 0;		/* 1 means first, 2 means second, 3
					 * both */

    ret[1].start = move + 1;
    ret[0].flips = ret[1].flips = 0;
    if (move != Size - 1)
    {
	if (edge[move + 1] == WHITE)
	    for (i = move + 2; i < Size; i++)
	    {
		if (edge[i] == BLANK)
		{
		    break;
		} else
		if (edge[i] == Opposite(edge[move + 1]))
		{
		    dirs++;
		    for (j = move + 1; j <= i - 1; j++)
		    {
			edge[j] = BLACK;
			ret[1].flips++;
		    }
		    break;
		}
	    }
    }
    if (move != 0)
    {
	if (edge[move - 1] == WHITE)
	    for (i = move - 2; i >= 0; i--)
	    {
		if (edge[i] == BLANK)
		{
		    break;
		} else
		if (edge[i] == Opposite(edge[move - 1]))
		{
		    dirs += 2;
		    for (j = move - 1; j >= i + 1; j--)
		    {
			edge[j] = BLACK;
			ret[0].flips++;
			flips++;
		    }
		    ret[0].start = i + 1;
		    break;
		}
	    }
    }
    if (dirs == 1)			/* only time we have to switch */
    {
	struct flips_type temp;
	temp = ret[0];
	ret[0] = ret[1];
	ret[1] = temp;
    }
    if (edge[move] != BLANK)
    {
	fprintf(stderr, "Tried to make illegal move.\n");
	kill(getpid(), 11);
    } else
	edge[move] = BLACK;
}

find_moves(line, moves)
int        line[MAX_PIECES];
int             moves[MAX_PIECES];
{
    int        i;

    for (i = 0; i < Size; i++)		/* check if each square is legal */
    {
	int        j;
	int             found_white;

	moves[i] = FALSE;
	if (line[i] != BLANK)
	    continue;

	found_white = FALSE;
	for (j = i + 1; j < Size; j++)	/* go forward */
	{
	    if (line[j] == WHITE)
		found_white = TRUE;
	    else
	    if (line[j] == BLACK)
	    {
		if (found_white)
		{
		    moves[i] = TRUE;
		    break;
		} else
		    break;
	    } else
		break;
	}				/* END: for (j ...) */

	found_white = FALSE;
	for (j = i - 1; j >= 0; j--)	/* go backward */
	{
	    if (line[j] == WHITE)
		found_white = TRUE;
	    else
	    if (line[j] == BLACK)
	    {
		if (found_white)
		{
		    moves[i] = TRUE;
		    break;
		} else
		    break;
	    } else
		break;
	}				/* END: for (j ...) */
    }					/* END: for (i ...) */
}

opp_find_moves(line)
int        line[MAX_PIECES];
{
    int        i;


    for (i = 0; i < Size; i++)		/* check if each square is legal */
    {
	int        j;
	int             found_black;

	if (line[i] != BLANK)
	    continue;

	found_black = FALSE;
	for (j = i + 1; j < Size; j++)	/* go forward */
	{
	    if (line[j] == BLACK)
		found_black = TRUE;
	    else
	    if (line[j] == WHITE)
	    {
		if (found_black)
		{
		    return (1);
		    break;
		} else
		    break;
	    } else
		break;
	}				/* END: for (j ...) */

	found_black = FALSE;
	for (j = i - 1; j >= 0; j--)	/* go backward */
	{
	    if (line[j] == BLACK)
		found_black = TRUE;
	    else
	    if (line[j] == WHITE)
	    {
		if (found_black)
		{
		    return (1);
		    break;
		} else
		    break;
	    } else
		break;
	}				/* END: for (j ...) */
    }					/* END: for (i ...) */
    return (0);
}

/*
 * horrible hack to take into account x-square moves.  The index for the diag8
 * entry is computed as follows: ??
 */

patch_diag8()
{
    int        edge[MAX_PIECES],new_edge[MAX_PIECES],
                    i,
		    moves[MAX_PIECES],
		    fudge1,fudge2;
    struct flips_type ret[2];

    Size = 8;
    for (i = 0; i < 6561; i++)		/* hack each configuration */
    {
	to_edge(edge, i);
	find_moves(edge,moves);
	if (moves[LOW_X])		/* move to low x-square? */
	{
	    to_edge(new_edge, i);
	    really_make_move(new_edge, LOW_X, ret);
	    fudge1 = Move_Penalty[DIAG8][ret[0].start][ret[0].flips];
	} else
	    fudge1 = 0;
	if (moves[HIGH_X])
	{
	    to_edge(new_edge, i);
	    really_make_move(new_edge, HIGH_X, ret);
	    if (HIGH_X > ret[0].start)	/* got to reverse stuff? (not right
					 * format) */
		ret[0].start = 8 - HIGH_X;
	    else
		fprintf(stderr, "Error cd.\n");
	    fudge2 = Move_Penalty[DIAG8][ret[0].start][ret[0].flips];
	} else
	    fudge2 = 0;
    /*
     * first copy all the data that will be the same (wscore and pscore)
     * because they don't matter on whether there are x-square moves or not. 
     * Also copy mscores, in case they don't change.  Will fix them later. 
     */
	Diag8[4 * i] = Diag8[4 * i + 1] =
	    Diag8[4 * i + 2] = Diag8[4 * i + 3] = Tables[DIAG8][i];
    /*
     * if lower corner is empty, positions with moves to the lower x-square
     * have to be penalized so that those moves are worth nothing (by
     * adding X_SQUARE_PENALTY to the penalty.  Whew!)
     *
     * In addition, the fudge factors (in case an x-square move is legal on
     * diag8) have to be accounted for.
     */
     /* do lower corner (x-square).  Must have empty corner and x-square. */
	if (edge[0] == BLANK && edge[LOW_X] == BLANK)
	{
	/* pos with only lower x-sq move */
	    Diag8[4 * i + 1].mscore += X_SQUARE_PENALTY - fudge1;
	/* pos with both x-sq moves */
	    Diag8[4 * i + 3].mscore += X_SQUARE_PENALTY - fudge1;
	}
    /* similarly for upper corner */
	if (edge[7] == BLANK && edge[HIGH_X] == BLANK)
	{
	/* pos with only upper x-sq move */
	    Diag8[4 * i + 2].mscore += X_SQUARE_PENALTY - fudge2;
	/* pos with both x-sq moves */
	    Diag8[4 * i + 3].mscore += X_SQUARE_PENALTY - fudge2;
	}
    }
}

/*
 * another disgusting hack.  tb1's value depends on whether the corner's are
 * occupied. If the corner next to an x-square move is occupied, the normal
 * penalties apply. Otherwise, no points are taken off, because Diag8 will
 * completely discount the x-square move (it knows if it is legal, even if it
 * doesn't flip along diag8) 
 *
 * To compute the new index, one bit (corner-occuped?) for each corner is added.
 * So, shift the old index by 2 bits, then add 0-3 in to tell what the corner
 * situation is. 
 */

patch_tb1()
{
    int        edge[MAX_PIECES],new_edge[MAX_PIECES],
                    i,
                    moves[MAX_PIECES],
                    fudge1,
                    fudge2;
    static struct flips_type ret[2];

    Size = 8;
    for (i = 0; i < 6561; i++)
    {
	to_edge(edge, i);
	find_moves(edge, moves);
	Tb1[4 * i] = Tb1[4 * i + 1] = Tb1[4 * i + 2] = Tb1[4 * i + 3] =
	    Tables[TB1][i];
	if (moves[LOW_X])		/* move to low x-square? */
	{
	    to_edge(new_edge, i);
	    really_make_move(new_edge, LOW_X, ret);
	    fudge1 = Move_Penalty[TB1][ret[0].start][ret[0].flips];
	} else
	    fudge1 = 0;
	if (moves[HIGH_X])
	{
	    to_edge(new_edge, i);
	    really_make_move(new_edge, HIGH_X, ret);
	    if (HIGH_X > ret[0].start)	/* got to reverse stuff? (not right
					 * format) */
		ret[0].start = 8 - HIGH_X;
	    else
		fprintf(stderr, "Error abcd.\n");
	    fudge2 = Move_Penalty[TB1][ret[0].start][ret[0].flips];
	} else
	    fudge2 = 0;
    /*
     * now remove penalties assesed earlier (in move_penalty), because here we
     * know if the corners are occupeid.  If a corner is occuped, don't remove
     * the penalty.  Otherwise, remove it (because Diag8 is going to KILL this
     * move). 
     */
	Tb1[4 * i].mscore -= fudge1 + fudge2;	/* none occupied.  Both will
						 * be killed by diag8 */
	Tb1[4 * i + 1].mscore -= fudge2;/* lower corner occupied only.  high
					 * will be killed */
	Tb1[4 * i + 2].mscore -= fudge1;/* upper corner occupied only.  low
					 * will be killed */
    }
}

print53 ()
{
    int i,j;

    printf("unsigned int Diag53[6561] = \n{\n    ");   
    for (i=0;i<243;i++)
        for (j=0;j<27;j++)
	{  unsigned int x;
	    x = convert(&Tables[DIAG5][i]) + convert(&Tables[DIAG3][j]);
	    printf(" %10u,",x);
	    if ((i*27+j) % 5 == 4)
	        printf("\n    ");
	}
    printf("\n};\n\n");
}

print44 ()
{
    int i,j;

    printf("unsigned int Diag44[6561] = \n{\n    ");   
    for (i=0;i<81;i++)
        for (j=0;j<81;j++)
	{  unsigned int x;
	    x = convert(&Tables[DIAG4][i]) + convert(&Tables[DIAG4][j]);
	    printf(" %10u,",x);
	    if ((i*81+j) % 5 == 4)
	        printf("\n    ");
	}
    printf("\n};\n\n");
}


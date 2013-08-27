// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

From jcw@alofi.etca.fr Mon Jun  6 12:03:09 1994
Date: Mon, 6 Jun 1994 12:00:39 +0200
From: Jean-Christophe Weill <jcw@alofi.etca.fr>
To: buro@uni-paderborn.de
Subject: the code
Content-Length: 7177



Hello,


  To boot our exchange of ideas : a small part of Thumpy code ,



/*                               Copyright 1994                         */
/*                            Jean-Christophe Weill                     */
/*                                                                      */
/*                        NOT TO BE USED OR REPRODUCED                  */
/*                  WITHOUT WRITTEN PERMISSION OF THE AUTHOR            */
/*                                                                      */




/* a double linked list for keeping a trace of empty squares */

struct file_cases
  {
    int square;
    struct file_cases *avant;
    struct file_cases *apres;
  }
tete, *queue, pointeurs[91];


/* The 8 legal direction */

const int dir[] =
{1, -1, 8, -8, 9, -9, 10, -10, 0};

enum
  {
    DIR1 = 1, DIRM1 = 2, DIR8 = 4, DIRM8 = 8, DIR9 = 16, DIRM9 = 32,
    DIR10 = 64, DIRM10 = 128
  };

int pref[] =
{
  A1, H1, A8, H8,
  C1, F1, A3, H3, A6, H6, C8, F8,
  C3, F3, C6, F6, D1, E1, A4, H4, A5, H5, D8, E8,
  D3, E3, C4, F4, C5, F5, D6, E6, D2, E2, B4, G4, B5, G5, D7, E7,
  C2, F2, B3, G3, B6, G6, C7, F7, B1, G1, A2, H2, A7, H7, B8, G8,
  B2, G2, B7, G7,
  D4, E4, D5, E5
};

/* fast return */

inline int
retour_rap_log (int *ptab0, int j, int c, int negc, int ***ptrr)
{
  int *ptab = ptab0 + j;

  if (*ptab == negc)
    {
      int count = 1;
      ptab += j;
      if (*ptab == negc)
	{
	  count++;		/* 2 */
	  ptab += j;
	  if (*ptab == negc)
	    {
	      count++;		/* 3 */
	      ptab += j;
	      if (*ptab == negc)
		{
		  count++;	/* 4 */
		  ptab += j;
		  if (*ptab == negc)
		    {
		      count++;	/* 5 */
		      ptab += j;
		      if (*ptab == negc)
			{
			  count++;	/* 6 */
			  ptab += j;
			}
		    }
		}
	    }
	}
      if (*ptab == c)
	{
	  int g = count;
	  do
	    {
	      ptab -= j;
	      *ptab = c;
	      *(*ptrr)++ = ptab;
	    }
	  while (--g);
	  return count;
	}

    }
  return 0;
}

int
retour_rapide (int x, int c, int **ptrr)
{
  int j = 0, n = 0, negc = 3 - c, *ptab0;

  unsigned int qudir;

  ptab0 = &tableau[x];

  qudir = whdir[x];
  if (qudir & DIR1)
    n = retour_rap_log (ptab0, 1, c, negc, &ptrr);
  if (qudir & DIRM1)
    n += retour_rap_log (ptab0, -1, c, negc, &ptrr);
  if (qudir & DIR8)
    n += retour_rap_log (ptab0, 8, c, negc, &ptrr);
  if (qudir & DIRM8)
    n += retour_rap_log (ptab0, -8, c, negc, &ptrr);
  if (qudir & DIR9)
    n += retour_rap_log (ptab0, 9, c, negc, &ptrr);
  if (qudir & DIRM9)
    n += retour_rap_log (ptab0, -9, c, negc, &ptrr);
  if (qudir & DIR10)
    n += retour_rap_log (ptab0, 10, c, negc, &ptrr);
  if (qudir & DIRM10)
    n += retour_rap_log (ptab0, -10, c, negc, &ptrr);

  *ptrr = NULL;


  return n;
}


/*
 * For the last move, we compute the score without updating the board (tableau)
 */

inline int
calcul_rap_log (int *ptab0, int j, int c, int negc)
{
  int *ptab = ptab0 + j;

  if (*ptab == negc)
    {
      int count = 1;
      ptab += j;
      if (*ptab == negc)
	{
	  count++;		/* 2 */
	  ptab += j;
	  if (*ptab == negc)
	    {
	      count++;		/* 3 */
	      ptab += j;
	      if (*ptab == negc)
		{
		  count++;	/* 4 */
		  ptab += j;
		  if (*ptab == negc)
		    {
		      count++;	/* 5 */
		      ptab += j;
		      if (*ptab == negc)
			{
			  count++;	/* 6 */
			  ptab += j;
			}
		    }
		}
	    }
	}
      if (*ptab == c)
	return count;
    }
  return 0;
}

int
calcul_rapide (int x, int c)
{
  int j = 0, n = 0, negc = 3 - c, *ptab, *ptab0;

  const int *i;
  unsigned int qudir;

  ptab0 = &tableau[x];

  qudir = whdir[x];
  if (qudir & DIR1)
    n = calcul_rap_log (ptab0, 1, c, negc);
  if (qudir & DIRM1)
    n += calcul_rap_log (ptab0, -1, c, negc);
  if (qudir & DIR8)
    n += calcul_rap_log (ptab0, 8, c, negc);
  if (qudir & DIRM8)
    n += calcul_rap_log (ptab0, -8, c, negc);
  if (qudir & DIR9)
    n += calcul_rap_log (ptab0, 9, c, negc);
  if (qudir & DIRM9)
    n += calcul_rap_log (ptab0, -9, c, negc);
  if (qudir & DIR10)
    n += calcul_rap_log (ptab0, 10, c, negc);
  if (qudir & DIRM10)
    n += calcul_rap_log (ptab0, -10, c, negc);

  return n;
}


/* Some init, so that all above should not work */


void
init (void)
{


  /* ... */


  for (i = A1; i <= H8; i++)
    {
      whdir[i] = 0;
      if (tableau[i] == 0 && tableau[i + 1] == 0 && tableau[i + 2] == 0)
	whdir[i] |= DIR1;
      if (tableau[i] == 0 && tableau[i - 1] == 0 && tableau[i - 2] == 0)
	whdir[i] |= DIRM1;

      if (tableau[i] == 0 && tableau[i + 8] == 0 && tableau[i + 16] == 0)
	whdir[i] |= DIR8;
      if (tableau[i] == 0 && tableau[i - 8] == 0 && tableau[i - 16] == 0)
	whdir[i] |= DIRM8;

      if (tableau[i] == 0 && tableau[i + 9] == 0 && tableau[i + 18] == 0)
	whdir[i] |= DIR9;
      if (tableau[i] == 0 && tableau[i - 9] == 0 && tableau[i - 18] == 0)
	whdir[i] |= DIRM9;

      if (tableau[i] == 0 && tableau[i + 10] == 0 && tableau[i + 20] == 0)
	whdir[i] |= DIR10;
      if (tableau[i] == 0 && tableau[i - 10] == 0 && tableau[i - 20] == 0)
	whdir[i] |= DIRM10;
    }

  /* ... */


  tete.avant = tete.apres = NULL;
  tete.square = D5;
  queue = &tete;
  for (i = 0; pref[i] != 40; i++)
    {
      if (tableau[pref[i]] == 0)
	{
	  queue->apres = &pointeurs[pref[i]];
	  pointeurs[pref[i]].avant = queue;
	  pointeurs[pref[i]].square = pref[i];
	  pointeurs[pref[i]].apres = NULL;
	  queue = &pointeurs[pref[i]];
	}
    }
}


/*
 * The search by itself
 * c = color, ev = incremental evaluation, passe = last ply pass flag
 */

int
search3 (int alpha, int beta, int c, int n, int ev2, int passe)
{
  int i, j, *x, v, mcb = 0, score, **ptab, killhash = 0;
  struct file_cases *pcases;
  int onpasse = 1;

  score = -30000;


  nodescnt++;
  if (nodescnt++ > etnodes)
    controletemps ();

  bestline[bestindex[n] + 1] = 0;


  for (pcases = tete.apres; pcases != NULL && !timeout; pcases = pcases->apres)
    {
      j = retour_rapide (pcases->square, c, re1[n]);

      if (j)
	{
	  tableau[pcases->square] = c;

	  onpasse = 0;

	  pcases->avant->apres = pcases->apres;
	  if (pcases->apres != NULL)
	    pcases->apres->avant = pcases->avant;

	  if (tete.apres == NULL)
	    v = ev2 + 2 * j + 1;
	  else if (tete.apres->apres == NULL)
	    {
	      int j1;
	      j1 = calcul_rapide (tete.apres->square, 3 - c);
	      if (j1 == 0)
		{
		  j1 = calcul_rapide (tete.apres->square, c);
		  if (j1 == 0)
		    v = ev2 + 2 * j + 1;
		  else
		    v = ev2 + 2 * j + 2 + 2 * j1;
		}
	      else
		v = ev2 + 2 * j - 2 * j1;
	    }
	  else
	    v = -search3 (-beta, -alpha, 3 - c, n + 1, -ev2 - 2 * j - 1, 0);

	  for (ptab = re1[n]; *ptab != NULL; ptab++)
	    **ptab = 3 - c;
	  tableau[pcases->square] = 0;

	  pcases->avant->apres = pcases;
	  if (pcases->apres != NULL)
	    pcases->apres->avant = pcases;

	  if (v > score)
	    {
	      mcb = pcases->square;
	      score = v;
	      if (v > alpha)
		{
		  alpha = v;
		  if (v >= beta)
		    {
		      return (v);
		    }
		  stockline (n, mcb);
		}
	    }
	}
    }
  if (onpasse == 1)
    {
      if (passe == 1)
	return ev2;
      else
	return -search3 (-beta, -alpha, 3 - c, n + 1, -ev2, 1);
    }
  return (score);
}

--LAA05873.770896439/alofi.etca.fr--




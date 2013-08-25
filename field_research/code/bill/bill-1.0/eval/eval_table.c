/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <eval.h>
#include <general.h>

#define OLD_INSTALLED FALSE
#define DEFAULT4
/* singular serach uses 3-comp. */

static eval_t lose ()
{
  user_error ("that evaluation is not installed");
  return (eval_t) 0;		/* to keep lint happy */
}

extern eval_t	evaluate(), eval_out (), eval1 (), eval4 (), evaluate2 (),
  		evaluate3 (), eval_out3 (), evaluate_newedge();
extern eval_t	eval8 (), eval9 (), eval17 (), eval10 (), eval21 (), eval_out2 ();
extern eval_t	eval80 ();
int compute4 (), compute4_newedge();

struct eval_rec All_Evals[MAX_EVALS] =
{
  { /* 0 */
#ifdef DEFAULT4
    "4-component, non-linear with super-smoothing (default)",
    "evaluate",
    evaluate,
    eval_out,
    TRUE,      				/* is gaussian */
    compute4,
#else
    "3-component, non-linear with odd-odd, even-even smoothing (default)",
    "evaluate2",
    evaluate2,
    eval_out2,
    TRUE,			/* is gaussian (non-linear) */
    FALSE,			/* no level above */
#endif
  },
  { /* 1 */
    "4-component, non-linear with disgusting smoothing (disabled)",
    "eval1",
    lose,
    lose,
    TRUE,
    (PFI) lose,/*     compute4,*/
  },
  { /* 2 */
    "4-component, very old hand-tuned linear (disabled)",
    "eval4",
    lose,
    lose,
    FALSE,
    (PFI) lose, /* compute4, */
  },
  { /* 3 */
#if OLD_INSTALLED
    "3-component, non-linear with odd-odd, even-even smoothing",
    "evaluate2",
    evaluate2,
    eval_out2,
#else
    "3-component, non-linear with odd-odd, even-even smoothing (disabled)",
    "evaluate2",
    lose,
    lose,
#endif
    TRUE,
    FALSE,
  },
  { /* 4 */
    "4-component, linear classification (disabled)",
    "eval10",
    lose,
    lose,
    TRUE,
    (PFI) lose, /* compute4 */
  },
  { /* 5 */
    "8-component, extremely noisy classification (disabled)",
    "eval8",
    lose,
    lose,
    TRUE,
    (PFI) lose, /* compute4 */
  },
  { /* 6 */
    "9-component, linear classification (disabled)",
    "eval9",
    lose,
    lose,
    TRUE,
    (PFI) lose, /* compute4, */
  },
  { /* 7 */
    "21-component, linear classified (disabled)",
    "eval21",
    lose,
    lose,
    TRUE,
    (PFI) lose, /* compute4, */
  },
  { /* 8 */
#if OLD_INSTALLED
    "3-component, linear!",
    "evaluate3",
    evaluate3,
    eval_out3,
#else
    "3-component, linear! (disabled)",
    "evaluate3",
    lose,
    lose,
#endif
    TRUE,
    FALSE,
  },
  { /* 9 */
    "non-greedy edge table (in regular 4-component evaluation)",
    "evaluate_newedge",
    evaluate_newedge,
    eval_out,
    TRUE,
    compute4_newedge,
  }    ,
  { /* 10 */
    0,
  },
};

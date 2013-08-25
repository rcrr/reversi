/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* header file for using different evaluations */

#ifndef EVAL_H

#include <pfi.h>
#define MAX_EVALS 11

#define eval_t int

typedef eval_t (*eval_fn_t)();

typedef struct eval_rec
{
  char comment[100],			/* e.g. "linear, 3-component" */
	name[20];			/* e.g. "evaluate2" */
  eval_fn_t eval,			/* the actual function */
	    out;			/* its eval_out procedure */
  int  gaussian;			/* probabilistic? */
  PFI level_above;			/* function to put in features.  NULL if level_above not used */
} eval_rec;

#define EVAL_H
#endif

/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* matrix.h -- define types for matrices using Iliffe vectors
 *************************************************************
 * HISTORY
 * 25-Nov-80  David Smith (drs) at Carnegie-Mellon University
 * Changed virtual base address name to "el" for all data
 * types (Previously vali, vald, ...)  This was possible due to the
 * compiler enhancement which keeps different structure declarations
 * separate.
 *
 * 30-Oct-80  David Smith (drs) at Carnegie-Mellon University
 *	Rewritten for record-style matrices
 *
 */

typedef struct {
	int	lb1, ub1, lb2, ub2;
	char	*mat_sto;
	char	**el;
	}
	cmat;

typedef struct {
	int	lb1, ub1, lb2, ub2;
	char	*mat_sto;
	short	**el;
	}
	smat;

typedef struct {
	int	lb1, ub1, lb2, ub2;
	char	*mat_sto;
	int	**el;
	}
	imat;

typedef struct {
	int	lb1, ub1, lb2, ub2;
	char	*mat_sto;
	int	**el;
	}
	lmat;

typedef struct {
	int	lb1, ub1, lb2, ub2;
	char	*mat_sto;
	float	**el;
	}
	fmat;

typedef struct {
	int	lb1, ub1, lb2, ub2;
	char	*mat_sto;
	double	**el;
	}
	dmat;

cmat	newcmat();
smat	newsmat();
imat	newimat();
lmat	newlmat();
fmat	newfmat();
dmat	newdmat();

#define freemat(m) free((m).mat_sto)

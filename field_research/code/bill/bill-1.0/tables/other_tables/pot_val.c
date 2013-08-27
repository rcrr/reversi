/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <tables.h>

int             Pot_Val[NUM_TABLES][MAX_PIECES] =
{ 
 {  750,  750,  750 },					/* diag3 */
 {  750,  750,  750,  750 },				/* diag4 */
 {  750,  750,  750,  750,  750 },			/* diag5 */
 {  750,  900, 1000, 1000,  900,  750 },		/* diag6 */
 {  800, 1150, 1150, 1150, 1150, 1150,  800 },		/* diag7 */
 { 1000,    0,  750,  750,  750,  750,    0, 1000 },	/* diag8 */
 {  500,  500, 1000, 1000, 1000, 1000,  500,  500 },	/* tb1 */
 {  900, 1150, 1150, 1150, 1150, 1150, 1150,  900 },	/* tb2 */
 { 1100, 1350, 1350, 1350, 1350, 1350, 1350, 1100 },	/* tb3 */
};

/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <node.h>
#include <version.h>
#include <general.h>
#include <hash.h>
#include <move.h>
#include <killer.h>
#include <eval.h>
#include <ab_info.h>
#include <ss_info.h>
#include <stdio.h>

struct hash_entry *Hash_Table[HASH_SIZE];
struct killer_table Killer_Table[2][MAX_PIECES+1];
int    Illegal_Hash = 0,
       Leaf_Nodes = 0,
       Nodes_Searched = 0,
       Save_Leaf = 0,
       Steal = TRUE,
       Hash_Count = 0,
       Node_Count;
Node   Game_Record[MAX_MOVES];	/* contains what Bill saves, mostly for take back */
ab_info_t AB_Info[2];
ss_info_t SS_Info[2];
general_info_t General_Info[2];

/* Variables that control the various printing options */

int        Print_Nodes = TRUE,	/* print total leaf nodes at each iteration? */
  	   Print_Eval = TRUE,	/* print current evaluation? */
  	   Print_Legals = TRUE,	/* print legal moves? */
           Print_Time = TRUE,	/* print time left? */
           Print_Search = -1,	/* print search info? */
           Print_Steal = FALSE,
           Print_Stat = TRUE,	/* print status info at end? */
           Print_Response = TRUE,	/* show predicted response? */
           Print_Board = TRUE,	/* stupid */
	   Print_Depth = TRUE,
	   Print_Best = TRUE,
           Bell = FALSE,
           Single_Move = FALSE;	/* make single move and quit? */

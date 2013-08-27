// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* Merkmale 2.93 */

#ifndef PRECOMP
#include "main.h"
#endif

/*
include "fmo.h"
include "fpo.h"
include "fst.h"
*/
#include "fpatt.h"
#include "newendeval.h"

 
MERKMAL_B *Merkmale[] = {

  &DISC_DIFF_B,

/* Position */

#if 0
  &ADIFF_B, &BDIFF_B, &CDIFF_B, &DDIFF_B, &EDIFF_B, 
  &FDIFF_B, &HDIFF_B, &IDIFF_B, &JDIFF_B, &XDIFF_B,
  &EECDIFF_B, &EEXDIFF_B, 
  &STABDIFF_B, &STDIFF_B, 
  &STABB_B, &STABW_B,
  &STRTESTB_B, 
#endif


  &STRB1B_B,  &STRB2B_B,
  &STRR1B_B, &STRR2B_B, &STRR3B_B, 
  &STRM1B_B, &STRM2B_B, 
  &STRHV1B_B, &STRHV2B_B, &STRHV3B_B, &STRHV4B_B, 
  &STRD1B_B,  &STRD2B_B, &STRD3B_B, &STRD4B_B, &STRD5B_B, &STRD6B_B,
  &STRMOBALLB_B,
  &STRMOBB_B, &STRMOBEBB_B, &STRMOBEWB_B,
  &STRPOTMOBALLB_B,
  &STRPOTMOBB_B, &STRPOTMOBEB_B,

  &LARGE_B,

  &T3X3_B,
  &T4X3_B,

  &PARITY_B, 
  &PARITY2_B, 
  &PARITY1B_B, 
  &OR_BOTH_B,
  &OR_ONE_B,
  &ER_ONE_B,
  &CORNERS_B,
  &CENTER_B,
  &MOBNORM_B,
  &STABILITY_B,
  &NEWSTAB_B,

/* Mobilität */

  &MOB1_B, &MOB2_B, &MOB3_B, &MOB4_B, &MOB5_B, 
  &MOB6_B, &MOB7_B, &MOB8_B, &MOB9_B, &MOB10_B, &MOBSUM_B, &MOBAPP_B, 

#if 0
  &MOBE_B, &MOBX_B, &MOBC_B,
  &MOB_B, &SMOB_B, &BMOB_B, &FMOB_B,
  &POTMOB1_B, &POTMOB2_B, &POTMOB3_B, 
  &POTMOB_B,
#endif

/* All */

  &EVALX_B,
  &NEWENDEVAL_B,

  NULL
};

#define MERKMALS_ANZ (sizeof(Merkmale)/sizeof(MERKMAL_B *)-1)

MERKMAL_B  *MerkmaleDa[MERKMALS_ANZ+1];

int	MerkmalsAnz = MERKMALS_ANZ;


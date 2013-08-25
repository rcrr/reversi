// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef BINFO_H
#define BINFO_H

#include "nnm.h"
#include "nnpatt.h"
#include "nnspatt.h"
#include "nndpatt.h"
#include "nnfeat.h"


class BoardInfo {

public:

  enum { P_NUM_MAX  = 61+24 };  // normal + sparse
  enum { F_NUM_MAX  = 2 };

  typedef int IndType;
  enum { MAX_IND = 531441 };  // = 3^12

  short disc_num, result;
  float val, val2;

  SPFELD board;

  int    f_num, p_num;

  static Features  *p_features;
  static Patterns  *p_patterns;
  static SPatterns *p_spatterns;
  static DPatterns *p_dpatterns;

  Estimate *f_entries[F_NUM_MAX+1];   // pointers to estimates
  Estimate *p_entries[P_NUM_MAX+1];

  short  f_values[F_NUM_MAX];

  int   interval;
  int   start_interval;
  int   interval_num;
  float learn_deltas[WINDOW_MAX_LEN];

  static int patt_index(SPFELD &bo, int *sq_list);
  static void attach_vectors(
                Patterns &patterns, 
		Features &features, 
		SPatterns &spatterns, 
		DPatterns &dpatterns
	      );
  void from_board(SPFELD &bo, int dn);
  float evaluate() const;

};

#endif

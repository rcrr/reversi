// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef evolopt_h
#define evolopt_h

#include "main.h"

class Options {

public:

  static char  *example_file;
  static uint4 org_num;
  static uint4 gene_num;
  static uint4 point_num;
  static real4 p_empty;
  static uint4 training_cycles;
  static real4 learning_rate;
  static real4 best_size;
  static real4 worst_size;
  static real4 new_size;

};

#endif

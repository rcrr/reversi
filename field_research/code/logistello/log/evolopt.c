// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "evolopt.h"

char *Options::example_file    = "examples.sfk";
uint4 Options::org_num         = 1024;
uint4 Options::gene_num        = 256;
uint4 Options::point_num       = 4;
real4 Options::p_empty         = 0.1;
uint4 Options::training_cycles = 50;
real4 Options::learning_rate   = 0.1;
real4 Options::best_size       = 0.2; 
real4 Options::worst_size      = 0.3; 
real4 Options::new_size        = 0.2; // < worst_size

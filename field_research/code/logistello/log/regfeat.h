// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef REGFEAT_H
#define REGFEAT_H

#include "regm.h"
#include "regentry.h"

struct FeatureInfo {

  char *name;
  int (*f)(SPFELD &, int disc_num);
  int bucket_num;
  bool one_phase;
    
}; 


class Feature {

public:

  char *name;
  int (*f)(SPFELD &, int disc_num);
  int bucket_num;
  bool one_phase;
  Entry *entries;

  Feature();
  void init(FeatureInfo &finf);
  void asc_write(FILE *fp);
  void bin_write(FILE *fp);
  bool bin_read(FILE *fp);
  bool bin_write_freq(FILE *fp, int n);

};

class Features : public vector<Feature> {};

#endif

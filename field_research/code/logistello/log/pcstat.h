// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// pcstat class / 1.97

#ifndef PCSTAT_H
#define PCSTAT_H

#include "main.h"
#include "crt.h"


class PCStat {

public:

  struct AppInfo { 

    float inva, b, s;
    bool  defined;

  };

private: 

  enum { MAX_PC_DEPTH = 19, MIN_NUM   = 18, MAX_NUM = 54, NUM = MAX_NUM - MIN_NUM + 1 };

  AppInfo App[NUM][MAX_PC_DEPTH+1][MAX_PC_DEPTH+1];


public:

  bool loaded;

  void init();

  PCStat() { init(); loaded = false; }

  void read(const String &filename);

  inline const AppInfo *get(int disc_num, int d1, int d2) const
  {
    if (disc_num < MIN_NUM) disc_num = MIN_NUM;
    if (disc_num > MAX_NUM) disc_num = MAX_NUM;

    if (d1 < 0 || d1 > MAX_PC_DEPTH) Error("PCStat::get: d1?");
    if (d2 < 0 || d2 > MAX_PC_DEPTH) Error("PCStat::get: d2?");
    if (d1 >= d2) Error("PCStat::get: d1 >= d2");
    if (!App[disc_num-MIN_NUM][d1][d2].defined) {
      fprintf(stderr, "*** PCStat::get: entry (%d %d %d) not defined\n", 
	      disc_num, d1, d2);
      exit(20);
    }

    return &App[disc_num-MIN_NUM][d1][d2];
  }

};

extern String PCStatFile;
extern String EndPCStatFile;

#endif

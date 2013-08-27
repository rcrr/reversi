// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef RATENTRY_H
#define RATENTRY_H

#include "ratm.h"

class Estimate {

public:

  float  y;
  double delta_sum;
  int    n;

#if USE_RARE_FACTOR
  float  rare_factor;
#endif

#if USE_MOMENTUM
  float  last_delta;
#endif

#if FIND_OUTLIERS
  float min1, min2;
  float max1, max2;
#endif

  Estimate() { 
    y = 0.0; delta_sum = 0.0; n = 0; 

#if USE_RARE_FACTOR
    rare_factor = 1.0; 
#endif

#if USE_MOMENTUM
    last_delta = 0.0;
#endif

#if FIND_OUTLIERS
    min1 = min2 =  MAXFLOAT;
    max1 = max2 = -MAXFLOAT;
#endif

  }

  void set(float v) { y = v; }
  
  inline void new_delta(double ds)
  {
    delta_sum += ds;
    n++;

#if FIND_OUTLIERS

    float diff;

    if (ds > max1) {
      max1 = ds;
    }

    if (ds < min1) {
      min1 = ds;
    }

#endif

  }

  inline float update()
  {
    if (n > 0) {
      double d = delta_sum/n;
      
#if USE_RARE_FACTOR
      d /= rare_factor;
#endif

#if USE_MOMENTUM
      d += MOMENTUM_FACTOR * last_delta;
      last_delta = d;
#endif

#if USE_WEIGHT_BOUND

#error "quatsch"

      if (n >= 10) { y += d; return abs(d); }

      float bound = (n-4)*0.66 + 1;  // 4->1, 10->10
      if (bound <= 0) bound = 0.1;
      if (abs(y + d) < bound) { y += d; return abs(d); }

      if (y + d >= 0) { d = bound - y; y = bound; return d; }
      d = y - bound; y = -bound; return d;
#endif

      y += d;
      return abs(d);
    }	
    
    return 0;
  }

};


class Entry {

public:

  enum { DISC_MIN     = 13 };
  enum { DISC_MAX     = 64 };
  enum { INTERVAL_LEN = 4 };
  enum { INTERVAL_NUM = (DISC_MAX+1-DISC_MIN)/INTERVAL_LEN };

  // for allocation 

  static Estimate *first_esti_block;
  static Estimate *esti_block;
  static int esti_left;

  enum { ESTI_BLOCK_LEN = 4096 };

  Entry    *p_map;
  Estimate *esti;
  bool     one_phase;

#if LINEAR_MODEL
  float lm_a, lm_b;
  float lm_last_delta_a, lm_last_delta_b;
#endif

  Entry(bool one_phase=false);

  inline static int inter(int disc_num)
  {
    if      (disc_num < DISC_MIN) disc_num = DISC_MIN;
    else if (disc_num > DISC_MAX) disc_num = DISC_MAX;

    return (disc_num - DISC_MIN) / INTERVAL_LEN;
  }

  inline static int min_num(int inter) { return DISC_MIN + inter*INTERVAL_LEN; }
  inline static int max_num(int inter) { return DISC_MIN + (inter+1)*INTERVAL_LEN - 1; }

  inline int get_n(int inter) const { return esti[inter].n; }

  inline void set_y(int inter, float value) { esti[inter].y = value; }
  inline float get_y(int inter) const { return esti[inter].y; }

#if USE_RARE_FACTOR
  inline void set_rare_factor(int inter, float value) { esti[inter].rare_factor = value; }
  inline float get_rare_factor(int inter) const { return esti[inter].rare_factor; }
#else
  inline void set_rare_factor(int , float ) { Error("set_rare_factor called"); }
  inline float get_rare_factor(int ) const { Error("set_rare_factor called"); return 0; } 
#endif

  inline void new_delta(int inter, double delta) { esti[inter].new_delta(delta); }


  // new: update all entries separately

  float update(float &min_val, float &max_val);
  void  bin_write(FILE *fp, int inter);
  void  bin_read(FILE *fp, int inter);

  void alloc_esti();
  void set(float v);
  void lower_bound(float v);
  void upper_bound(float v);

};

#endif

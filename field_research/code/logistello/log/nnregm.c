// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// table learning by hill climbing (batch-mode) / 2.97
// sparse patterns & update with momentum / 2.98
// multi-threading / 3.98
// regularization / 8.99

#include <pthread.h>
#include "nnregm.h"
#include "nnentry.h"
#include "nnpatt.h"
#include "nnspatt.h"
#include "nnfeat.h"
#include "nnbinfo.h"
#include "timer.h"

#define NEW_TABS false   /* 2x8 adj opp */
#define OLD_TABS false   /* no sparse patterns */

#define USE_THREADS true

#define SHOW_TIME   false


#define REGULARIZATION 1
#define REG_FAC        0.05


#define DECR_DIFF true   // decreases label-error implications

const float SIGN_PENALTY = 0.5;

const float decr_factor[33] = {

  /*  0 */ 1.0,
  /*  2 */ 1.0,
  /*  4 */ 1.0,
  /*  8 */ 0.8,
  /* 10 */ 0.6,
  /* 12 */ 0.4,
  /* 14 */ 0.2,
  /* 16 */ 0.2,
  0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
  0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,
  0.2,0.2,0.2
};


#if EVAL_TEST
#undef USE_THREADS
#define USE_THREADS false
#endif

#ifndef NDEBUG
#undef USE_THREADS
#define USE_THREADS false
#endif

#if USE_THREADS
#define PROC_NUM_MAX 2
#else
#define PROC_NUM_MAX 1
#endif

#if NEW_TABS
#define TAB_SUFFIX  "front.bin"
#else
#define TAB_SUFFIX  "regbin"
#endif

#define DATA_PATH "data/"

static bool f_show_extreme = false;
static bool f_accrej = false;
static bool f_val_out = false;
static float extr_diff_min = 0;
static int   extr_int_min = 0;
static int   extr_int_max = Entry::INTERVAL_NUM-1;

static int proc_num = PROC_NUM_MAX;

// ******************** patterns ********************** XXX

#if NEW_TABS

// new 

static PatternInfo pattern_infos[] = {

 {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0,
    0,0,9,0,0,0,0,10,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "EDGE+2X", 0
  // , { 1,2,3,4,5,6,7,8,0 }
  }

#if 0
  ,

  {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,0,0,0,0,
    0,6,7,8,9,10,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "2x5", 0
   // , { 1,2,3,4,6,7,8,9,0 }
  }
#endif

  ,



  {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,0,0,0,0,0,0,
    0,4,5,6,0,0,0,0,0,0,
    0,7,8,9,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
   }, "3x3", 0
   // , { 1,2,3,4,5,6,0 }
  }

  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV2" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV3" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV4" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,0,0,0,0,0,0,0,0, 
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0, 
    0,0,0,0,0,0,6,0,0,0, 
    0,0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,0,0,8,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D8" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0, 
    0,0,0,0,0,0,6,0,0,0,
    0,0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D7" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0, 
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0,
    0,0,0,0,0,0,6,0,0,0,
    0,0,0,0,0,0,0,0,0,0 
  }, "D6" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0, 
    0,0,0,3,0,0,0,0,0,0, 
    0,0,0,0,4,0,0,0,0,0,
    0,0,0,0,0,5,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D5" }

  ,

 {{
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
 }, "D4" }

#if 0
 ,

 {{
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,2,0,0,
    0,0,3,4,5,6,7,8,0,0,
    0,0,0,9,10,11,12,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
 }, "WALL" }

 ,

 {{
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,
    0,0,0,5,0,2,0,0,0,0,
    0,0,9,0,6,0,3,0,0,0,
   0,0,0,10,0,7,0,4,0,0,
   0,0,0,0,11,0,8,0,0,0,
   0,0,0,0,0,12,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "CHECKERS", 0
  // , { 1,2,3,4,5,6,7,8,0 }
  }
#endif

};

#else

// old

static PatternInfo pattern_infos[] = {

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,6,7,8,0, 
    0,0,9,0,0,0,0,10,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "EDGE+2X", 0
   // , { 1,2,3,4,5,6,7,8,0 }
  }


  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,0,0,0,0, 
    0,6,7,8,9,10,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "2x5", 0
   // , { 1,2,3,4,6,7,8,9,0 } 
  }

  ,

  {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,0,0,0,0,0,0,
    0,4,5,6,0,0,0,0,0,0,
    0,7,8,9,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "3x3", 0
   // , { 1,2,3,4,5,6,0 }
  }


#if 1
  ,
  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV2" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV3" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV4" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,0,0,0,0,0,0,0,0, 
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0, 
    0,0,0,0,0,0,6,0,0,0, 
    0,0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,0,0,8,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D8" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0, 
    0,0,0,0,0,0,6,0,0,0,
    0,0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D7" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0, 
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0,
    0,0,0,0,0,0,6,0,0,0,
    0,0,0,0,0,0,0,0,0,0 
  }, "D6" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0, 
    0,0,0,3,0,0,0,0,0,0, 
    0,0,0,0,4,0,0,0,0,0,
    0,0,0,0,0,5,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D5" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0, 
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D4" }

#endif

};

#endif

const int PATT_NUM = sizeof(pattern_infos)/sizeof(pattern_infos[0]);

static Patterns patterns;


#if OLD_TABS

// no spare patterns

static SPatternInfo spattern_infos[] = {

};

#else


#if NEW_TABS

static SPatternInfo spattern_infos[] = {

  // { "CNT", DATA_PATH"confcnt.24" }
  // { "2x8", DATA_PATH"conf2x8.75.hash" },
  // { "4x4", DATA_PATH"conf4x4.75.hash" }  // takes too much time
  // { "ADJ", DATA_PATH"confadj.75.hash" }
  // { "OPP", DATA_PATH"confopp.75.hash" }
};

#else

static SPatternInfo spattern_infos[] = {

  { "2x8", DATA_PATH"conf2x8.75" },
  { "4x4", DATA_PATH"conf4x4.75" },
};

#endif

#endif  // OLD_TABS


const int SPATT_NUM = sizeof(spattern_infos)/sizeof(spattern_infos[0]);

static SPatterns spatterns;


// *********************** features ************************** YYY


static int feature_par(SPFELD &, int disc_num)
{
  return disc_num & 1;
}

static int feature_center_diff(SPFELD &bo, int)
{
  sint1 *p=bo.p;

  return p[C3]+p[C4]+p[C5]+p[C6]+
         p[D3]+p[D4]+p[D5]+p[D6]+
         p[E3]+p[E4]+p[E5]+p[E6]+
         p[F3]+p[F4]+p[F5]+p[F6];
}

const int MOB_MAX = 15;

static int feature_mob(SPFELD &bo, int)
{
  BRETT brett;

  SfBrett(&bo, &brett);
  
  int md = int(STRMOBALLB(&brett));

  if      (md > +MOB_MAX) md = +MOB_MAX;
  else if (md < -MOB_MAX) md = -MOB_MAX;
  
  return md + MOB_MAX;
}


static FeatureInfo feature_infos[] = {

    { "PARITY", feature_par, 2 }
    //  , { "CENTER", feature_center_diff, 1 }
  //  , { "MOB",    feature_mob, 2*MOB_MAX+1 }

};

const int FEAT_NUM = sizeof(feature_infos)/sizeof(feature_infos[0]);

static Features features;



// *********************** dynamic patterns ******************* DDD


// only 2x4 for now

static DPatternInfo dpattern_infos[] = {

 {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,0,0,0,0,0, 
    0,5,6,7,8,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "FRONT" }

};

const int DPATT_NUM = sizeof(dpattern_infos)/sizeof(dpattern_infos[0]);

static DPatterns dpatterns;



/**************************************************************/


static bool finished = false;

void _abort(void) { exit(20); }

static void ctrlc(int) { finished = true; }


static pthread_t threads[PROC_NUM_MAX];



class Statistic {

public:

  int    num;
  int    last_bound;
  int    training_num[Entry::INTERVAL_NUM], training_correct[Entry::INTERVAL_NUM];
  double delta_sum;
  double delta_sums[Entry::INTERVAL_NUM];
  int    delta_ns[Entry::INTERVAL_NUM];
  Timer  ti; 

  Statistic() { init(); }

  void init()
  {
    int i;

    num = 0; delta_sum = 0; last_bound = 0;

    FOR (i, Entry::INTERVAL_NUM) {
      training_num[i] = training_correct[i] = delta_ns[i] = 0;
      delta_sums[i] = 0.0;
    }

    ti.in_mode(Timer::REAL_TIME);
    ti.stamp();
  }

  void update(Statistic &statistic)
  {
    int i;

    num += statistic.num;
    delta_sum += statistic.delta_sum;

    FOR (i, Entry::INTERVAL_NUM) {
      training_num[i]     += statistic.training_num[i];
      training_correct[i] += statistic.training_correct[i];
      delta_ns[i]         += statistic.delta_ns[i];
      delta_sums[i]       += statistic.delta_sums[i];
    }
  }

} statistic;


static pthread_mutex_t fp_in_lock, extreme_lock; 
static FILE *fp_in = 0;

static pthread_mutex_t update_lock;
static pthread_mutex_t interval_locks[Entry::INTERVAL_NUM];

static int read_boards(SFCODE *buffer, int size)
{
#if USE_THREADS
  pthread_mutex_lock(&fp_in_lock);
#endif

  int num = fSfCodeRead(fp_in, buffer, size);

#if USE_THREADS
  pthread_mutex_unlock(&fp_in_lock);
#endif

  return num;
}


int gcd(int u, int v)
{
  int t;

  while (u > 0) {
    if (u < v) { t = u; u = v; v = t; }
    u = u - v;
  }

  return v;
}



static void *process(void *param)
{
  const int BUFFER_SIZE = 16;  // large => cache trash
  int i, size = 0;
  SFCODE *buffer;
  BoardInfo *infos, **i_list[Entry::INTERVAL_NUM];
  int i_list_len[Entry::INTERVAL_NUM];
  Statistic local_statistic;
  int id = (int)param;
  int step = id+1;

  infos  = new BoardInfo[BUFFER_SIZE];
  buffer = new SFCODE[BUFFER_SIZE];

  FOR (i, Entry::INTERVAL_NUM)
    i_list[i]  = new (BoardInfo *)[BUFFER_SIZE*WINDOW_MAX_LEN];

  printf("process %d created\n", id);

  if (step >= Entry::INTERVAL_NUM) Error("step >= INTERVAL_NUM");
  if (gcd(step, Entry::INTERVAL_NUM) != 1) Error("gcd != 1");
  
  FOREVER {

    Timer start_ti(Timer::REAL_TIME);
    Timer read_ti(Timer::REAL_TIME);
    Timer eval_ti(Timer::REAL_TIME);
    Timer update_ti(Timer::REAL_TIME);

    // read boards

    start_ti.stamp();

    size = read_boards(buffer, BUFFER_SIZE);

    read_ti.stamp();

    if (size <= 0) break;  // no more boards

    int num;

    local_statistic.init();

    FOR (i, Entry::INTERVAL_NUM) i_list_len[i] = 0;

    FOR (num, size) {

      SPFELD    bo;
      BoardInfo &inf = infos[num];
      int num_b, num_w, disc_num;

      SfEntpack(buffer[num], bo, num_b, num_w);   

      disc_num = num_b + num_w;

      if (disc_num < Entry::DISC_MIN || disc_num > Entry::DISC_MAX) continue;

      inf.from_board(bo, disc_num);

      // compute values
  
      int inter0 = Entry::inter(inf.disc_num);

      inf.interval = inter0;

#if EVAL_TEST

      BRETT board;

      SfBrett(&bo, &board);
      float evaluate =  inf.evaluate();
      float evala = EVAL_FUNCTION(&board, BLACK)/10000.0;
      float diff = evaluate-evala;
   
      cout << "." << flush;
   
      if (abs(diff) > 0.01) {
	cout << endl << "res= " << inf.result << " diff=" << diff << " "
	     << "evaluate=" << evaluate << " " 
	     << "evala=" << evala << endl;
      }
   
      continue; // skip update
   
#endif

      int interval_index = 0;

      for (int inter=inter0-WIDTH; inter <= inter0+WIDTH+RIGHT_ADJ; inter++) {
     
	if (inter >= 0 && inter < Entry::INTERVAL_NUM) {
       
	  int num_terms = inf.f_num + inf.p_num;
	  float value = 0;

	  register Estimate **pe = inf.f_entries;
	  int i;

	  if (interval_index == 0) inf.start_interval = inter;

	  i_list[inter][i_list_len[inter]++] = &inf; 

	  FOR (i, FEAT_NUM) {
	    Feature &feat = features[i];
	 
	    if (feat.bucket_num == 1) {
	      value += inf.f_values[i] * (*pe)[inter].y;
	   
#if USE_RARE_FACTOR
	      * (*pe)[inter].rare_factor;
#endif
		  ;
	      if (inf.f_values[i] == 0) num_terms--;
	   
	    } else {
	   
	      value += (*pe)[inter].y
	     
#if USE_RARE_FACTOR
		* (*pe)->feat.entries[inf.feat[i]].get_rare_factor(inter)
#endif
		;
	    }
	  }
       
	  if (inf.p_num > 0) {

	    pe = inf.p_entries+inf.p_num-1;
       
	    do {

	      value += (*pe)[inter].y

#if USE_RARE_FACTOR
		* (*pe)[inter].rare_factor
#endif
		;

	      pe--;

	    } while (pe >= inf.p_entries);
	  }

	  // cout << k << " " << inf.val << " " << value << " " << num_terms << " " << BoardInfo::ind_num << endl;
	  
#if 0	  
	  if (inter == inter0 && value != inf.evaluate()) {
	    cout << ">>>" << inf.val << " " << inf.evaluate() << endl;
	  }
#endif


	  double delta = inf.val - value;

#if DECR_DIFF

	  int index = round(abs(inf.val)*0.5);
	  if (index < 0)  index = 0;
	  if (index > 32) index = 32;
	  float fac = decr_factor[index];

	  delta *= fac;

	  if (inf.disc_num >= 44) {

	    // (almost) correct labels

	    if ((inf.val > 0 && value < inf.val)) {
	      delta += 2.0 * SIGN_PENALTY * delta;
	    }

	    if (inf.val < 0 && value > inf.val) {
	      delta += 2.0 * SIGN_PENALTY * delta;
	    }


	  } else {

	    // classification error possible

	    if ((inf.val > 0 && value < inf.val)) {
	      delta += SIGN_PENALTY * delta;
	    }

	    if (inf.val < 0 && value > inf.val) {
	      delta += SIGN_PENALTY * delta;
	    }

	  }
#endif

	  double learn_delta = LEARN_RATE*(delta/num_terms);
       
	  if (inter == inter0) {

	    // update local statistic      

	    if (f_val_out) {
	      pthread_mutex_lock(&extreme_lock);
	      printf("%d %.2f %.2f\n", inter0, inf.val, value);
	      pthread_mutex_unlock(&extreme_lock);
	    }

	    if (f_show_extreme) {

	      pthread_mutex_lock(&extreme_lock);

	      if (abs(delta) >= extr_diff_min && 
		  inter0 >= extr_int_min && inter0 <= extr_int_max) {

		SfAus(&inf.board, 0, 0);
		printf("#=%d, res=%d, v=%.2f, diff=%.2f\n\n", inf.disc_num, inf.result, value, delta);


		if (f_accrej) {

		  static FILE *fp_rej = 0;
		  if (!fp_rej) {
		    fp_rej = fopen("rej.sfk", "w");
		  }

		  if (!fp_rej) Error("can't open rej.sfk");

		  fSfWrite(fp_rej, &inf.board, 1);
		}


	      } else {
		
		if (f_accrej) {

		  static FILE *fp_acc = 0;

		  if (!fp_acc) {
		    fp_acc = fopen("acc.sfk", "w");
		  }

		  if (!fp_acc) Error("can't open acc.sfk");

		  int res = inf.board.Marke - (MA_DIFF+64);
		  if (res < -64 || res > 64) Error("no diff-label?????");

		  fSfWrite(fp_acc, &inf.board, 1);
		}
	      }

	      pthread_mutex_unlock(&extreme_lock);

	    } 

	    local_statistic.delta_sum += abs(delta);
	    local_statistic.delta_sums[inter] += abs(delta);
	    local_statistic.delta_ns[inter]++;
	    local_statistic.num++;

	    if (inf.result) {
	      if ((inf.result > 0) ^ (value < 0)) 
		local_statistic.training_correct[Entry::inter(inf.disc_num)]++;
	      local_statistic.training_num[Entry::inter(inf.disc_num)]++;
	    }
	  }

	  inf.learn_deltas[interval_index] = learn_delta;
	  interval_index++;
	}
      }

      inf.interval_num = interval_index;
    }

    eval_ti.stamp();   

    // update

    int interval;

#if USE_THREADS
    int updated_intervals;
    bool updated[Entry::INTERVAL_NUM];
    int ret;

    FOR (updated_intervals, Entry::INTERVAL_NUM) 
      updated[updated_intervals] = false;

    for (updated_intervals = 0, interval = step % Entry::INTERVAL_NUM;
	 updated_intervals < Entry::INTERVAL_NUM;
	 updated_intervals++) {

      FOREVER {

	// search for a free update slot

	if (updated[interval] || 
	    (ret=pthread_mutex_trylock(&interval_locks[interval]))) {

#if 0
	  if (updated[interval]) cout << "u" << id << "/" << interval << " " << flush;
	  else                   cout << "f" << id << "/" << interval << "/" << ret << " " << flush;
#endif

	  // gcd(step, INTERVAL_NUM) == 1 => visit all slots

	  interval += step;
	  if (interval >= Entry::INTERVAL_NUM) interval -= Entry::INTERVAL_NUM;

	} else {

#if 0
	  cout << "!" << id << "/" << interval << "/" << ret << " " << flush;
#endif
	  updated[interval] = true;
	  break;
	}
      }

#else

    FOR (interval, Entry::INTERVAL_NUM) {

#endif

      int end = i_list_len[interval];
      BoardInfo **list = i_list[interval];

      FOR (num, end) {

	BoardInfo &inf = *(*list++);

#if 0
	if (interval < inf.start_interval || 
	    interval >= inf.start_interval + inf.interval_num) 
	  Error("interval not included?");
#endif

	Estimate **pe = inf.f_entries;
	double learn_delta = inf.learn_deltas[interval - inf.start_interval];
	int i;
	int valence;

#if DECL_INFL

	int d = abs(interval - inf.interval);
	if      (d == 0) { valence = 3; learn_delta += learn_delta + learn_delta; }
	else if (d == 1) { valence = 2; learn_delta += learn_delta; }
	else if (d >= 2) { valence = 1; }

#else

	valence = 1;

#endif

	FOR (i, inf.f_num) {
	  
	  Feature &feat = features[i];
	  
	  if (feat.bucket_num == 1) {
	    if (inf.f_values[i] != 0) 
	      (*pe)[interval].new_delta(learn_delta/inf.f_values[i], valence);
	  } else {
	    (*pe)[interval].new_delta(learn_delta, valence);
	  }
	  
	  pe++;
	}
	
	if (inf.p_num) {
 
	  pe = inf.p_entries+inf.p_num-1;

	  do {
	    (*pe)[interval].new_delta(learn_delta, valence);
	    pe--;
	  } while (pe >= inf.p_entries);
	}
      }


#if USE_THREADS
      pthread_mutex_unlock(&interval_locks[interval]);
#endif
    }

    update_ti.stamp();

#if USE_THREADS
    pthread_mutex_lock(&update_lock); 
#endif

    statistic.update(local_statistic);

    if (statistic.num >= statistic.last_bound) {
      Timer cu(Timer::REAL_TIME);

      cout << cu.diff(statistic.ti) << " " 
	   << statistic.num << "-> " 
	   << statistic.num/(cu.diff(statistic.ti)+0.001) 
	   << endl;
      statistic.last_bound += 100000;
    }

#if SHOW_TIME
    cout << "read  : " << read_ti.diff(start_ti)  << endl;
    cout << "eval  : " << eval_ti.diff(read_ti)   << endl;
    cout << "update: " << update_ti.diff(eval_ti) << endl << endl;
#endif

#if USE_THREADS
    pthread_mutex_unlock(&update_lock);
#endif


  }

  delete [] buffer;
  delete [] infos;
  FOR (i, Entry::INTERVAL_NUM) delete [] i_list[i];
  return NULL;
}


int main(int argc, char **argv)
{
  FILE *fp=0;
  bool f_freq = false;
  int  freq_n = 0;
  char *file_name = 0;
  int  i, j;

  signal(SIGINT, ctrlc);
  InitSetzen();

  if (argc <= 1) {

  error: ;

    Error("usage: onn (-iter [-freq n] [-extr accrej(0/1) diff imin imax] sfk-file | -show table-file | -prep sfk-file)");
    exit(20);
  }

  int argi = 1;

  if (argi < argc && !strcmp(argv[argi], "-show")) {
   
    PatternInfo pi;
    Pattern pat;
    int k;

    fp=fopen(argv[argi+1], "r");
    if (!fp) Error("can't open file");
    int len = fgetc(fp);

    cout << len << " discs" << endl;

    if (len < 1 || len > 12) Error("len?");

    if (fgetc(fp) != Entry::INTERVAL_NUM) Error("interval num?");
    if (fgetc(fp) != Entry::DISC_MIN)     Error("disc min?");
    if (fgetc(fp) != Entry::INTERVAL_LEN) Error("interval len?");
    fclose(fp);

    pi.one_phase = 0;
    pi.sub_list[0] = 0;
    pi.name = "show";

    FOR (i, 100) pi.sq[i] = 0;
    k = len;

    FOR_SFPOS10(i) {
      pi.sq[i] = k;
      k--;
      if (k <= 0) break;
    }

    fp=fopen(argv[argi+1], "r");
    if (!fp) Error("can't open file");
    pat.init(pi);
    pat.bin_read(fp);
    pat.asc_write(stdout);
    fclose(fp);
    exit(0);
  }

  if (argi < argc && !strcmp(argv[argi], "-prep")) {
   
    // preprocess board file such that collisions are minimized
    // (>= +4 interval offset for adjacent boards)

    argi++;

    if (argi >= argc) goto error;

    FILE *fp_in, *fp_out;
    char name[1000];
    const int BUFFER_MAX_LEN = 1024;
    const int INTER_INC = WINDOW_MAX_LEN;
    int bu_len = 0;
    int next_inter = 0;

    struct {
      SPFELD bo;
      int inter;
    } buffer[BUFFER_MAX_LEN];

    sprintf(name, "%s.prep", argv[argi]);

    fp_in = fopen(argv[argi], "r");
    if (!fp_in) Error("can't open file (r)");

    fp_out = fopen(name, "w");
    if (!fp_out) Error("can't open file (w)");

    FOREVER {
      int num_b, num_w, disc_num;

      while (bu_len > 0) {

	// next_inter in buffer?

	FOR (i, bu_len) if (buffer[i].inter == next_inter) break;

	if (i < bu_len) {
	  fSfWrite(fp_out, &buffer[i].bo, 1);

	  //cout << next_inter << " ";
	
	  next_inter += INTER_INC;
	  if (next_inter >= Entry::INTERVAL_NUM) 
	    next_inter -= Entry::INTERVAL_NUM;

	  buffer[i] = buffer[bu_len-1];
	  bu_len--;
	  
	} else break;

      }

      if (bu_len < BUFFER_MAX_LEN) {

	if (!fSfReadNum(fp_in, &buffer[bu_len].bo, num_b, num_w)) {

	  if (bu_len == 0) break;
	  goto find_next;
	}

	disc_num = num_b + num_w;

	int inter = Entry::inter(disc_num);

	buffer[bu_len].inter = inter;

	if (inter == next_inter) {

	  fSfWrite(fp_out, &buffer[bu_len].bo, 1);

	  //cout << buffer[bu_len].inter << " ";

	  next_inter += INTER_INC;
	  if (next_inter >= Entry::INTERVAL_NUM) 
	    next_inter -= Entry::INTERVAL_NUM;

	} else bu_len++;

      } else {

	// buffer full or end of file: choose closest board so far

      find_next: ;

	int min_i = -1, min_dist = MAXINT;

	FOR (i, bu_len) {
	  int dist = buffer[i].inter - next_inter;
	  if (dist < 0) dist += Entry::INTERVAL_NUM;
	  if (dist < min_dist) { min_dist = dist; min_i = i; }
	}

	next_inter = buffer[min_i].inter + INTER_INC;
	if (next_inter >= Entry::INTERVAL_NUM) 
	  next_inter -= Entry::INTERVAL_NUM;
	
	fSfWrite(fp_out, &buffer[min_i].bo, 1);

	//cout << buffer[min_i].inter << " ";

	buffer[min_i] = buffer[bu_len-1];
	bu_len--;
      }
    }

    fclose(fp_in);
    fclose(fp_out);
    exit(0);
  }


  if (argi >= argc || strcmp(argv[argi], "-iter")) goto error;

  argi++;

  if (argi < argc &&  !strcmp(argv[argi], "-freq")) {

    f_freq = true;
    argi++;
    
    if (argi >= argc) goto error;
    freq_n = atoi(argv[argi]);

    argi++;
  }

  if (argi < argc &&  !strcmp(argv[argi], "-valout")) {

    f_val_out = true;
    argi++;
    
  }

  if (argi < argc &&  !strcmp(argv[argi], "-extr")) {

    f_show_extreme = true;
    argi++;

    if (argi >= argc) goto error;
    f_accrej = atoi(argv[argi]) != 0;
    argi++;

    if (argi >= argc) goto error;
    extr_diff_min = atof(argv[argi]);
    argi++;

    if (argi >= argc) goto error;
    extr_int_min = atoi(argv[argi]);
    argi++;

    if (argi >= argc) goto error;
    extr_int_max = atoi(argv[argi]);
    argi++;

    proc_num = 1;
  }


  if (argi >= argc) goto error;
  
  file_name = argv[argi];

  cout << "file: " << file_name << endl << endl;


  cout << "USE_SPARSE_PATTERNS=" << USE_SPARSE_PATTERNS << endl;
  cout << "USE_MOMENTUM=" << USE_MOMENTUM 
       << " (f=" << MOMENTUM_FACTOR << ")" << endl;
  cout << "USE_RARE_FACTOR=" << USE_RARE_FACTOR 
       << " (max=" << RARE_FACTOR_MAX << " nmax=" << RARE_N_MAX << ")" << endl;
  cout << "IGNORE_RARE=" << IGNORE_RARE 
       << " (max=" << RARE_COUNT << ")" << endl;
  cout << "USE_WEIGHT_BOUND=" << USE_WEIGHT_BOUND << endl;
  cout << "INTERVAL=[" << -WIDTH << "," << WIDTH+RIGHT_ADJ << "]" << endl;
  cout << "LINEAR_MODEL=" << LINEAR_MODEL << endl;
  cout << "DECL_INFL=" << DECL_INFL << endl;
  cout << "LEARN_RATE=" << LEARN_RATE << endl;
  cout << "USE_SIGMOID=" << USE_SIGMOID << endl;
  cout << endl;


  // iteration

  FOR (i, PATT_NUM) {
    Pattern patt;
    patt.init(pattern_infos[i]);
    patterns.push_back(patt);
  }

  FOR (i, FEAT_NUM) {
    Feature feat;
    feat.init(feature_infos[i]);
    features.push_back(feat);
  }

  FOR (i, SPATT_NUM) {
    SPattern spatt;
    spatt.init(spattern_infos[i]);
    spatterns.push_back(spatt);
  }

  FOR (i, DPATT_NUM) {
    DPattern dpatt;
    dpatt.init(dpattern_infos[i]);
    dpatterns.push_back(dpatt);
  }

  BoardInfo::attach_vectors(patterns, features, spatterns, dpatterns);

  vector<BoardInfo> dinfo[Entry::INTERVAL_NUM][RES_NUM];
  bool ok[Entry::INTERVAL_NUM][RES_NUM];

  if (USE_TEST_BOARDS) {

    // read test boards into buckets

    fp = fopen(TEST_FILE_NAME, "r");
    if (!fp) Error("can't open test-file");

    printf("reading test-boards ...\n");
    
    FOR (i, Entry::INTERVAL_NUM) 
      FOR (j, RES_NUM)
        ok[i][j] = false;

    int ok_num;

    for (ok_num = 0; ok_num < Entry::INTERVAL_NUM*RES_NUM; ) {

      SPFELD bo;
      int num_b, num_w;

      if (!fSfReadNum(fp, &bo, num_b, num_w)) break;

      int res = bo.Marke - (MA_DIFF+64);
      if (res < -64 || res > 64) Error("no diff-label");

      if (res & 1) {
	if (res > 0) res++;
	if (res < 0) res--;
      }

      int disc_num = num_b + num_w;

      if (disc_num < Entry::DISC_MIN || disc_num > Entry::DISC_MAX || 
	  res < RES_MIN || res > RES_MAX)  
	continue;

      int res_index = res - RES_MIN;

      res_index /= 2;

      if (ok[Entry::inter(disc_num)][res_index]) continue;

      BoardInfo inf;
    
      inf.from_board(bo, disc_num);
      dinfo[Entry::inter(disc_num)][res_index].push_back(inf);
      
      //SfAus(&bo, 0, 0);
      //cout << disc_num << " " << res << " " << res_index << endl;
      
      if ((int)dinfo[Entry::inter(disc_num)][res_index].size() >= DISCOR_BUCKET_SIZE) {

	// cout << "ok " << Entry::inter(disc_num) << " " << res_index << endl;
	
	ok[Entry::inter(disc_num)][res_index] = true;
	ok_num++;
      }
    }

    fclose(fp);

    if (ok_num < Entry::INTERVAL_NUM*RES_NUM) {
      printf("!!! not enough test-boards");
    }
 
  }

  bool missing = false;

  if (READ_DATA) {

    cout << "\nread config. values" << endl;
  
    FOR (i, PATT_NUM) {

      char name[1000];
      
      sprintf(name, DATA_PATH"patt_%s.%s", patterns[i].name, TAB_SUFFIX);
      cout << name << endl;
      fp=fopen(name,"r");
      if (fp) {
	if (patterns[i].bin_read(fp)) cout << "read error" << endl;
	fclose(fp);
      } else {
	cout << " not found" << endl;
	missing = true;
      }
    }

    FOR (i, DPATT_NUM) {

      char name[1000];
      
      sprintf(name, DATA_PATH"dpatt_%s.%s", dpatterns[i].name, TAB_SUFFIX);
      cout << name << endl;
      fp=fopen(name,"r");
      if (fp) {
	if (dpatterns[i].bin_read(fp)) cout << "read error" << endl;
	fclose(fp);
      } else {
	cout << " not found" << endl;
	missing = true;
      }
    }

    cout << "OK" << endl;
    cout << "\nread feature weights" << endl;
    
    FOR (i, FEAT_NUM) {
      
      char name[1000];
      
      sprintf(name, DATA_PATH"feat_%s.%s", features[i].name, TAB_SUFFIX);
      
      cout << name << endl;
      
      fp=fopen(name,"r");
      if (fp) {
	if (features[i].bin_read(fp)) cout << "read error" << endl;
	fclose(fp);
      } else {
	cout << " not found" << endl;
	missing = true;
      }
    }
    
    cout << "OK" << endl << endl;

#if USE_SPARSE_PATTERNS

    FOR (i, SPATT_NUM) {

      char name[1000];
      
      sprintf(name, DATA_PATH"spatt_%s.%s", spatterns[i].name, TAB_SUFFIX);
      cout << name << endl;
      fp=fopen(name,"r");
      if (fp) {
	if (spatterns[i].bin_read(fp)) cout << "read error" << endl;
	fclose(fp);
      } else {
	cout << " not found" << endl;
	missing = true;
      }
    }

    cout << "OK" << endl;

#endif

  }

#if USE_THREADS
  pthread_mutex_init(&fp_in_lock, NULL);
  pthread_mutex_init(&update_lock, NULL);
  pthread_mutex_init(&extreme_lock, NULL);

  FOR (i, Entry::INTERVAL_NUM) pthread_mutex_init(&interval_locks[i], NULL);
#endif

  for (int iter=1; ; iter++) {

    // go through all examples

    int k;
    
    statistic.init();

    fp_in = fopen(file_name, "r");
    if (!fp_in) Error("can't open board-file");

    if (proc_num > 1) {

      // create processes

      int pr;
      
      FOR (pr, proc_num) {
	
	//printf("create %d\n", i);
	
	if (pthread_create(&threads[pr], NULL, process, (void*)pr))
	  Error("cannot make thread\n");
      }
      
      // join processes
      
      FOR (pr, proc_num) {
	
	void *ret_val;
	
	// printf("join %d\n", i);
	
	if (pthread_join(threads[pr], &ret_val)) {
	  perror("");
	Error("thread join failed\n");
	}
	
	printf("process joined\n");
      }

    } else

      process(NULL);


    // end of pass through all examples

    if (f_show_extreme || f_val_out) exit(0);


    if (iter == 1) {

      // establish pointers to sub-patterns if configuration count is low

      FOR (i, PATT_NUM) {

	Pattern &patt = patterns[i];

	if (patt.p_sub) {

	  cout << "sub-pattern: " << patt.name << flush;

	  int n=0, sub_n=0;

	  for (k=Pot3[patt.len]-1; k >= 0; k--) {

	    if (patt.tab[k].p_map == &patt.tab[k]) {
	      int l, sn = 0;
	      
	      FOR (l, Entry::INTERVAL_NUM) 
		sn += patt.tab[k].esti[l].n;

	      n++;

	      if (sn < N_SUB) {
		int si = patt.sub_index(k);

		// copy values to sub_config

		FOR (l, Entry::INTERVAL_NUM) {

		  if (patt.p_sub->tab[si].p_map->esti[l].y != 0.0 &&
		      patt.p_sub->tab[si].p_map->esti[l].y != patt.tab[k].esti[l].y)
		    Error("different?");

		  patt.p_sub->tab[si].p_map->esti[l].y = 
		    patt.tab[k].esti[l].y;
		}

		// pointer to sub_config

    	        patt.tab[k].p_map = patt.p_sub->tab[si].p_map;
		sub_n++;
	      }

#if 0
	      Pattern::conf_write(stdout, patt.len, k);
	      cout << " " << sn << endl;
#endif

	    }
	  }

	  // pointer shortcut: max. distance is 1

	  for (k=Pot3[patt.len]-1; k >= 0; k--) {

	    Entry *p = patt.tab[k].p_map;

	    while (p != p->p_map) { p = p->p_map; }

	    patt.tab[k].p_map = p;
	  }

	  cout << " " << n-sub_n << "/" << n << endl;

	}
      }


      if (f_freq) {

	// generate freq files

	int n = freq_n;

	if (n <= 0) Error("freq_n <= 0");

	cout << "writing freq-files (n=" << n << ")..."  << flush;

	FOR (i, PATT_NUM) {

	  Pattern &patt = patterns[i];
	  char name[1000];

	  sprintf(name, "patt_%s.freq", patt.name);
	  fp=fopen(name,"w");
	  if (!fp) Error("can't write pattern-freq file");
          patt.bin_write_freq(fp, n);
	  fclose(fp);

	}
	
	FOR (i, FEAT_NUM) {
	  Feature &feat = features[i];
	  char name[1000];

	  sprintf(name, "feat_%s.freq", feat.name);
	  fp=fopen(name,"w");
	  if (!fp) Error("can't write feature-freq file");
          feat.bin_write_freq(fp, n);
	  fclose(fp);
	}

	cout << "OK" << endl;
	exit(0);
      }
 
    }

    // update all values

    double delta_abs_sum=0, max_abs_delta = 0;
    int    delta_n=0;

#if !USE_RARE_FACTOR
    missing = true;
#endif

    if (!(iter == 1 && READ_DATA && !missing)) {

      cout << "update" << endl;

      FOR (i, PATT_NUM) {
	
	Pattern &patt = patterns[i];
	
	for (k=Pot3[patt.len]-1; k >= 0; k--) {
	  if (patt.tab[k].p_map == &patt.tab[k]) {

#if REGULARIZATION
	    patt.tab[k].regul(REG_FAC);
#endif
	    double d = abs(patt.tab[k].update());
	    if (d != 0) {
	      max_abs_delta = max(d, max_abs_delta);
	      delta_abs_sum += d;
	      delta_n++;
	    }
	  }
	}
      }
      
      FOR (i, FEAT_NUM) {
	Feature &feat = features[i];
	
	for (k=feat.bucket_num-1; k >= 0; k--) {
	  
#if REGULARIZATION
	  feat.regul(REG_FAC);
#endif
	  double d = abs(feat.entries[k].update());
	  if (d != 0) {
	    max_abs_delta = max(d, max_abs_delta);
	    delta_abs_sum += d;
	    delta_n++;
	  }
	}
      }
 
#if USE_SPARSE_PATTERNS

      FOR (i, SPATT_NUM) {
	
	SPattern &spatt = spatterns[i];
	
	for (k=spatt.sp.var_num-1; k >= 0; k--) {
	  if (spatt.tab[k].p_map == &spatt.tab[k]) {
	    double d = abs(spatt.tab[k].update());
	    if (d != 0) {
	      max_abs_delta = max(d, max_abs_delta);
	      delta_abs_sum += d;
	      delta_n++;
	    }
	  }
	}
      }
      
#endif

    } else

      cout << "no update after first iteration!" << endl;


    // print statistic

    int i;
    
    printf("%-8d num=%d d=%.3f avg=%.3f max=%.3f \n",
	   iter,
	   statistic.num,
	   statistic.delta_sum / statistic.num,
	   delta_abs_sum / (delta_n+0.0001),
	   max_abs_delta);

    FOR (i, FEAT_NUM) {
      cout << "Feature(" << features[i].bucket_num << ") " << features[i].name << endl;
      FOR (k, features[i].bucket_num) {
	int l;
	cout << setw(2) << k << ": " << flush;
	FOR (l, Entry::INTERVAL_NUM) 
	  printf("%+5.2f ", features[i].entries[k].get_y(l));
	puts("");
      }
    }

#if 0
    patterns[2].asc_write(stdout);


    printf("%f %f %f %f\n", 
	   patterns[2].tab[28795].get_y(0), 
	   patterns[2].tab[0].get_y(0),
	   patterns[2].tab[P12(0,0,0,0,0,0,0,0,0,0,0,0)].get_y(0),
	   patterns[2].tab[P12(0,1,1,1,1,0,1,1,1,1,1,1)].get_y(0));
#endif


#if USE_TEST_BOARDS

    // evaluate test boards

    int test_correct[Entry::INTERVAL_NUM], test_num[Entry::INTERVAL_NUM];
    float disco[Entry::INTERVAL_NUM];
    float d_abs_sum[Entry::INTERVAL_NUM];
    int   d_n[Entry::INTERVAL_NUM];

    FOR (i, Entry::INTERVAL_NUM) {
      test_correct[i] = test_num[i] = 0;
      Values all0, all1;
      d_abs_sum[i] = 0;
      d_n[i] = 0;
      
      FOR (j, RES_NUM)
	FOR (k, dinfo[i][j].size()) {
	BoardInfo &inf = dinfo[i][j][k];
	
	inf.val2 = inf.evaluate();

	//	  cout << inf.val2 << " " << inf.val << endl;
	
	d_abs_sum[i] += abs(inf.val2 - inf.val);
	d_n[i]++;
	
	if (inf.result) {
	  if (inf.result < 0) 
	    all0.push_back(inf.val2);
	  else
	    all1.push_back(inf.val2);
	  
	  if ((inf.val2 > 0) ^ (inf.result < 0)) 
	    test_correct[i]++;
	  test_num[i]++;
	}
      }
      
      disco[i] = discordance(all0, all1);
      
    }


    // compute discordance
    
    cout << "#" << DISCO_STEP << "    test-disco test-err train-err test-d train-d" << flush;
    
#if 0
    FOR (j, RES_NUM-DISCO_STEP/2) printf("%+3d   ", RES_MIN+2*j);
#endif
    puts("");

    FOR (i, Entry::INTERVAL_NUM) {
      
      printf("%2d-%2d   ", Entry::min_num(i), Entry::max_num(i));
      printf("%4.1f      ", 100*disco[i]);      
      printf("%4.1f     ", 100-100*test_correct[i]/(test_num[i]+0.001));
      printf("%4.1f     ", 100-100*statistic.training_correct[i]/(statistic.training_num[i]+0.001));
      printf("%5.3f  ", d_abs_sum[i]/(d_n[i]+0.001));
      printf("%5.3f  ", statistic.delta_sums[i]/(statistic.delta_ns[i]+0.001));

      fflush(stdout);
#if 0      
      FOR (j, RES_NUM-DISCO_STEP/2) {
	
	Values val0, val1;
	
	FOR (k, dinfo[i][j].size()) {
	  val0.push_back(dinfo[i][j][k].val2);
	}

	FOR (k, dinfo[i][j+DISCO_STEP/2].size()) {
	  val1.push_back(dinfo[i][j+DISCO_STEP/2][k].val2);
	}
	
	printf("%5.1f ", 100.0*discordance(val0, val1));
	
	//FOR (k, DISCOR_BUCKET_SIZE) cout << val0[k] << " " << val1[k] << endl;
      }
      
#endif    
      puts("");
    }

    printf("\n");

#else

    cout << "  #   err   diff" << endl;
    FOR (i, Entry::INTERVAL_NUM) {
      cout << setw(2) << Entry::min_num(i) << "-" 
	   << setw(2) << Entry::max_num(i) << " " << flush;
      printf("%4.1f  ", 100-100*statistic.training_correct[i]/(statistic.training_num[i]+0.001));
      printf("%5.3f\n", statistic.delta_sums[i]/(statistic.delta_ns[i]+0.001));
    }

#endif

    cout << endl;

    if (WRITE_DATA) {

      cout << "\nwriting ..." << flush;
  
      FOR (i, PATT_NUM) {

	char name[1000];
      
	sprintf(name, DATA_PATH"patt_%s.%s", patterns[i].name, TAB_SUFFIX);

	fp=fopen(name,"w");
	if (!fp) Error("can't write pattern file");
      
	if (ASC_WRITE)
	  patterns[i].asc_write(fp);
	else
	  patterns[i].bin_write(fp);
	
	fclose(fp);
      }
      
      FOR (i, FEAT_NUM) {
	
	char name[1000];
	
	sprintf(name, DATA_PATH"feat_%s.%s", features[i].name, TAB_SUFFIX);
	
	fp=fopen(name,"w");
	if (!fp) Error("can't write feature file");
	
	if (ASC_WRITE)
	  features[i].asc_write(fp);
	else
	  features[i].bin_write(fp);
	
	fclose(fp);
      }
      
      FOR (i, SPATT_NUM) {

	char name[1000];
      
	sprintf(name, DATA_PATH"spatt_%s.%s", spatterns[i].name, TAB_SUFFIX);

	fp=fopen(name,"w");
	if (!fp) Error("can't write spattern file");
      
	if (ASC_WRITE)
	  spatterns[i].asc_write(fp);
	else
	  spatterns[i].bin_write(fp);
	
	fclose(fp);
      }
      

      FOR (i, DPATT_NUM) {

	char name[1000];
      
	sprintf(name, DATA_PATH"dpatt_%s.%s", dpatterns[i].name, TAB_SUFFIX);

	fp=fopen(name,"w");
	if (!fp) Error("can't write pattern file");
      
	if (ASC_WRITE)
	  dpatterns[i].asc_write(fp);
	else
	  dpatterns[i].bin_write(fp);
	
	fclose(fp);
      }
      
      cout << "OK" << endl << endl;
    }

    if (finished) break;
  }

  return 0;
}

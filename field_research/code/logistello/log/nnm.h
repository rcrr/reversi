// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef NNM_H
#define NNM_H

#include "main.h"
#include "crt.h"
#include "patt.h"
#include "eval.h"
#include "trans.h"
#include "fpatt.h"
#include "disco.h"
#include "sparse.h"

#include <vector>

#define USE_SIGMOID   false

#define LINEAR_MODEL  true
#define FIND_OUTLIERS false
#define OUTLIER_DIFF  0.2

#define DECL_INFL    false /* declining influence of examples to neighbourhood */

#define IGNORE_RARE true
const int RARE_COUNT = 4;

#define EVAL_TEST     false
#define EVAL_FUNCTION EvalK

#define USE_SPARSE_PATTERNS true


#define USE_MOMENTUM          true
const float MOMENTUM_FACTOR = 0.5;  // 0.75 better than 0.5 and 0.9

const float LEARN_RATE = 1.0;  // 3.0 -> divergence


#define USE_WEIGHT_BOUND false


// interpolation is better than using are rare_factor

#define USE_RARE_FACTOR false 

const float RARE_FACTOR_MAX = 1.5;  // was 1
const int   RARE_N_MAX      = 10;   // was 20


#undef DISC_MIN
#undef DISC_MAX

#if 0
const int WIDTH     = 0; 
const int RIGHT_ADJ = 0; // interval = [i-WIDTH ... i+WIDTH+RIGHT_ADJ]
const int WINDOW_MAX_LEN = 2*WIDTH + 1 + RIGHT_ADJ;
#else

// current
const int WIDTH     = 2;  // was 2
const int RIGHT_ADJ = -1;  // interval = [i-WIDTH ... i+WIDTH+RIGHT_ADJ]
const int WINDOW_MAX_LEN = 2*WIDTH + 1 + RIGHT_ADJ;
#endif

// const int N_MIN    = 20; // look for at least that many examples in vicinity
// const int W_MAX    = 3;  // extrapol-interpos size

const int N_SMOOTH = 50; // use delta * min(1,n/N_SMOOTH) if rare_factor is not used

const char FILE_NAME[]      = "test.sfk";  // "1163.sfk"

#define USE_TEST_BOARDS false

const char TEST_FILE_NAME[] = "check.sfk";

const bool READ_BOARDS_INTO_MEMORY = false;
const int INFO_MAXNUM = 25000;


const int N_SUB = 12;  // total frequ. < N_SUB => use sub-pattern // doesn't work

const bool WRITE_DATA = true;
const bool READ_DATA  = true;
const bool ASC_WRITE  = false;

// for discordance computation

const int DISCOR_BUCKET_SIZE = 250;
const int DISCO_STEP = 4;

const int RES_MAX = 20;
const int RES_MIN = -RES_MAX;
const int RES_NUM = (RES_MAX-RES_MIN)/2 + 1;

#define P8(p1,p2,p3,p4,p5,p6,p7,p8) \
(3280 + (3*(3*(3*(3*(3*(3*(3*p1+p2)+p3)+p4)+p5)+p6)+p7)+p8))

#define P10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
(29524 + (3*(3*(3*(3*(3*(3*(3*(3*(3*p1+p2)+p3)+p4)+p5)+p6)+p7)+p8)+p9)+p10))

#define P12(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) \
(265720 + (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*p1+p2)+p3)+p4)+p5)+p6)+p7)+p8)+p9)+p10)+p11)+p12))

#endif

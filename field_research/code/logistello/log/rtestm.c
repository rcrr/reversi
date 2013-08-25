// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"

const int FEAT_N = 8;      // features
const int EXPL_N = 10000;  // examples

const float ALPHA = 2;
const float BETA  = 0.75;

const float LB = 1.0;
const float UB = 4.0;

#define OUT 0


class Entry {

public:
  
  float y;
  float delta_sum;
  int   n;
  float previous_step;
  
  Entry() { y = 0; delta_sum = 0; n = 0; previous_step = 0; }

  void update() {
    if (n) {
      float d = delta_sum/n + BETA * previous_step;
      y += d;
      previous_step = d;
      delta_sum = 0;
      n = 0;
    }
  }
  void lower_bound(float b)   { if (y < b) y = b; }
  void upper_bound(float b)   { if (y > b) y = b; }  
  void set(float b) { y = b; }
  float get() const { return y; }
  void new_delta(float delta) { delta_sum += delta; n++; }

};


Entry ev[FEAT_N], ew[FEAT_N];

float v_0[FEAT_N] = {
  3.0, -1.0, 4.0, -2.0, 3.0, 5.0, 6.0, 10.0
};

float w_0[FEAT_N] = {
  1.0, 1.0, 1.5, 2.0, 1.0, 2.0, 3.0, 1.0
};


int main(int argc, char **argv)
{
  // generate
  
  int i, j;
  int feat[EXPL_N][FEAT_N];
  float y[EXPL_N];

  int k = 0;
  
  for (i=0; k < EXPL_N; i++) {
    
    int c = i;
    bool set = false;

    if (k >= EXPL_N) break;
    
    FOR (j, FEAT_N) {
      feat[k][j] = c & 1;
      if (c & 1) set = true;
      c >>= 1;
    }

    if (!set) continue;

    float v = 0;
    float w = 0;
    
    FOR (j, FEAT_N) {
      v += feat[k][j]*v_0[j];
      w += feat[k][j]*w_0[j];
    }

    y[k] = v/w;

#if OUT    
    FOR (j, FEAT_N) printf("%d ", feat[k][j]);
    printf(" : %f\n", y[k]);
#endif


    k++;
  }
  
  // solve

  // reset parameters

  FOR (j, FEAT_N) ew[j].set(LB);
  
  for (int iter=1;; iter++) {

    printf("i=%d\n", iter);

    float sq_sum = 0;
    
    FOR (i, EXPL_N) {

      float v = 0;
      float w = 0;

      FOR (j, FEAT_N) {
	v += feat[i][j]*ev[j].get();
	w += feat[i][j]*ew[j].get();
      }

      float val = v/w;
      float err = y[i] - val;

      //      printf("%f %f %f\n", v, w, err);

      sq_sum += err*err;
      
      float delta_v = ALPHA * err / w;
      float delta_w = - ALPHA * err*v/(w*w);

      FOR (j, FEAT_N) {
	ev[j].new_delta(delta_v * feat[i][j]);
	ew[j].new_delta(delta_w * feat[i][j]);
      }

    }

    printf("MSE=%f\n", sqrt(sq_sum/EXPL_N));
    
    FOR (j, FEAT_N) { ev[j].update(); ew[j].update(); }

    FOR (j, FEAT_N) {
      printf("%d: %f %f\n", j, ev[j].get(), ew[j].get());
    }

    FOR (j, FEAT_N) { ew[j].lower_bound(LB); ew[j].upper_bound(UB); }

    ew[0].set(LB);

    puts("");
    
    FOR (j, FEAT_N) {
      printf("%d: %f %f\n", j, ev[j].get(), ew[j].get());
    }

    puts("");
  }
  
}

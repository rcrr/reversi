// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "regentry.h"

int Entry::esti_left = 0;
Estimate *Entry::first_esti_block = 0;
Estimate *Entry::esti_block = 0;


Entry::Entry(bool o_p) 
{ 
  int i_n;

  one_phase = o_p;
  if (one_phase) i_n = 1; else i_n = INTERVAL_NUM;
  esti = 0;
  p_map = this;

#if LINEAR_MODEL
  lm_a = lm_b = 0;
  lm_last_delta_a = lm_last_delta_b = 0;
#endif

}



#if LINEAR_MODEL

// y = a * interval + b

#if DECL_INFL

// WINDOW_LEN = 4


// setting A: 22 100 10

const int RELEVANT_N   = 18;  // (was 18) >= 6 examples (*3)
const int REGRESSION_N = 80;  // (was 80) /8 = 1+2+3+2
const int FLAT_N       = 8;   // (was 8)

#else

// WINDOW_LEN = 4

const int RELEVANT_N   = 5;   // (was 6) >= 6 examples (*1)
const int REGRESSION_N = 20;  // (was 50) /4 = 1+1+1+1
const int FLAT_N       = 4;   // (was 4)
const int SINGLE_N     = 20;  // (was 50)
const bool EXTRAPOLATE = false;  // bad idea!

#endif

struct Interval {

  int l, r;
  int n_sum;

};

struct NewY {

  double sum;
  int n;

};


// add F*0.5 * y^2 / n to error sum
// => deriv. delta =  -F * y/n
// => delta_sum delta = -F * y
//
// => estimate smaller if n is small

void Entry::regul(float F, int &n, float &sq_sum)
{
  int i;
  
  FOR (i, INTERVAL_NUM) {
    if (esti[i].n) {

      
      esti[i].delta_sum -= esti[i].y * F;

      n++;
      sq_sum += F*0.5*square(esti[i].y)/esti[i].n;
    }
  }
}



#define VERB false

float Entry::update()
{
  int i;
  double max_abs_d = 0;
  Interval intervals[3*INTERVAL_NUM];
  NewY     newy[INTERVAL_NUM];

  if (one_phase) Error("one-phase not implemented");

#if 0
  cout << endl;
  FOR (i, INTERVAL_NUM) cout << setw(5) << esti[i].n;
  cout << endl;
#endif

#if VERB
cout << endl << endl << " ------------------------------------ " << endl << endl;
#endif

  int index = 0;

  for (i=INTERVAL_NUM-1; i >= 0; i--) {

    if (esti[i].n > 0) {
      
      if (index >= 3*INTERVAL_NUM) Error("index too large");

      Interval &interval = intervals[index];
      int j;
      int n_sum = 0;

      for (j=i; j >= 0; j--) {
	n_sum += esti[j].n;
	if (n_sum >= SINGLE_N) break;
      }

      interval.l = max(0, j);
      interval.r = i;
      interval.n_sum = n_sum;
      index++;

      if (j < 0) { 

	if (n_sum < SINGLE_N) {

#if VERB
cout << "###" << endl;
#endif
	  for (i=interval.r+1; i < INTERVAL_NUM && n_sum < SINGLE_N; i++) {
	    n_sum += esti[i].n;
	  }

	  interval.n_sum = n_sum;
	  interval.r = i-1;
	}

	break;
      }

      if (j < i && esti[j].n >= SINGLE_N) i = j+1; // single interval j is next
      else i = j;                                  // start new interval
    }
  }

#if VERB
FOR (i, INTERVAL_NUM) 
  cout << "(" << i << " " 
       << " y=" << esti[i].y  
       << " n=" << esti[i].n 
       << " ds=" << esti[i].delta_sum 
       << ") " 
       << endl;

FOR (i, index) {
  cout << "int " << i 
       << " l=" << intervals[i].l 
       << " r=" << intervals[i].r
       << " nsum=" << intervals[i].n_sum
       << endl;
}
#endif

  FOR (i, INTERVAL_NUM) { newy[i].sum = 0; newy[i].n = 0; }

  FOR (i, index) {

    Interval &interval = intervals[i];
    
#if VERB
cout << "interval " << i << " [" << interval.l << "," << interval.r << "]" << endl;
#endif

    if (interval.l == interval.r && interval.n_sum >= SINGLE_N) {

      int j = interval.l;

      newy[j].sum += esti[j].y * esti[j].n + esti[j].delta_sum;
      newy[j].n   += esti[j].n;

#if VERB
cout << "single: ds=" << esti[j].y * esti[j].n + esti[j].delta_sum
     << " dn=" << esti[j].n
     << endl;
#endif

    } else {

      // check if regression makes sense

      int num_relevant = 0;
      double sx = 0, sy = 0, sxx = 0, sxy = 0;
      
      for (int j=interval.l; j <= interval.r; j++) {
	int n = esti[j].n;
	double dy_n = n*esti[j].y + esti[j].delta_sum;
	if (n >= RELEVANT_N) num_relevant++;
	sx  += j * n;
	sy  += dy_n; 
	sxx += j * j * n;
	sxy += j * dy_n;
      }

      double a, b;

      if (num_relevant >= 2 && interval.n_sum >= REGRESSION_N) {

	// allow a != 0 only if we have enough examples

	double del = 1.0/(interval.n_sum*sxx-sx*sx);
	a   = (interval.n_sum*sxy-sx*sy)*del;
	b   = (sxx*sy-sx*sxy)*del;

#if VERB
cout << "regr: a=" << a
     << " b=" << b
     << endl;
#endif

      } else if (interval.n_sum <= FLAT_N) {

	a = 0; b = 0; 

#if VERB
cout << "flat: a=" << a
     << " b=" << b
     << endl;
#endif

      } else { 

	a = 0; b = sy / interval.n_sum; 

#if VERB
cout << "horiz: a=" << a
     << " b=" << b
     << endl;
#endif

      }

      for (int j=interval.l; j <= interval.r; j++) {
	int n = esti[j].n;

	if (n > 0 && n < SINGLE_N) {
	  newy[j].sum += a * j + b;
	  newy[j].n++;
#if VERB
cout << "(" << j << ": ds=" <<  a * j + b << ") " << endl;
#endif

	}
      }
    }
  }


#if VERB
cout << "->" << endl;
#endif

  FOR (i, INTERVAL_NUM) {
    
    if (esti[i].n > 0) {

      float new_y = newy[i].sum/newy[i].n

#if USE_MOMENTUM
	+ MOMENTUM_FACTOR * esti[i].last_delta;

      esti[i].last_delta = new_y - esti[i].y
#endif
	;

      float d = abs(new_y - esti[i].y);

      if (d > max_abs_d) max_abs_d = d;

#if VERB
cout << "(" << i << ": " << esti[i].y << " -> " << new_y << ") " << endl;
#endif

      esti[i].y = new_y;
    }
  }


  // linear interpolation for stages without examples
    
  FOR (i, INTERVAL_NUM) {

    if (esti[i].n == 0) {

      // left neighbor
	  
      int l, r;
	
      float yl = 0;

      for (l=i-1; l >= 0; l--)
	if (esti[l].n) { yl = esti[l].y; break; }

      if (l < 0 && !EXTRAPOLATE) { esti[i].y = 0; continue; }

      if (l < 0) l = -1;  // interval 0 can get value != 0

      // right neighbor

      float yr = 0;

      for (r=i+1; r < INTERVAL_NUM; r++)
	if (esti[r].n) { yr = esti[r].y; break; }
	
      if (r >= INTERVAL_NUM && !EXTRAPOLATE) { esti[i].y = 0; continue; }

      if (r >= INTERVAL_NUM) r = INTERVAL_NUM;

      esti[i].y = (i-l) * (yr-yl) / (r-l) + yl;
    }
  }


  // clear delta info

  FOR (i, INTERVAL_NUM) {
    esti[i].delta_sum = 0;
    esti[i].n = 0;
  }
    
  return max_abs_d;
}

#else

float Entry::update()
{
  int i;
  double max_abs_d = 0;

  if (one_phase) Error("one-phase not implemented");

  // update all y's for each stage

#if 0
  cout << endl;
  FOR (i, INTERVAL_NUM) cout << setw(5) << esti[i].n;
  cout << endl;
#endif

#if IGNORE_RARE

  FOR (i, INTERVAL_NUM) {

    if (esti[i].n <= RARE_COUNT) {

      // too few examples => y = 0

      esti[i].y = 0;
      esti[i].delta_sum = 0;
      esti[i].n = 0;
    }
  }

#endif

  FOR (i, INTERVAL_NUM) {
    if (esti[i].n > 0) {
      float d = esti[i].update();
      if (d > max_abs_d) max_abs_d = d;
    }
  }

  // new: linear interpolation for stages without examples
    
  FOR (i, INTERVAL_NUM) {

    if (esti[i].n == 0) {

      // left neighbor
	  
      int l, r;
	
      float yl = 0;

      for (l=i-1; l >= 0; l--)
	if (esti[l].n) { yl = esti[l].y; break; }

      if (l < 0) l = 0;

      // right neighbor

      float yr = 0;

      for (r=i+1; r < INTERVAL_NUM; r++)
	if (esti[r].n) { yr = esti[r].y; break; }
	
      if (r >= INTERVAL_NUM) r = INTERVAL_NUM-1;

      esti[i].y = (i-l) * (yr-yl) / (r-l) + yl;
    }
  }

#if USE_RARE_FACTOR

  // set rare_factor

  float b = (RARE_FACTOR_MAX-1)/(1-1/sqrt(RARE_N_MAX));
  float a = RARE_FACTOR_MAX - b;

  FOR (i, INTERVAL_NUM) {
    if (esti[i].n >= RARE_N_MAX) 
      esti[i].rare_factor = 1.0;
    else if (esti[i].n == 0)         
      esti[i].rare_factor = RARE_FACTOR_MAX;
    else
      esti[i].rare_factor = a + b/sqrt(esti[i].n);

#if 0
    if (esti[i].n > 0) cout << esti[i].n << " " << esti[i].rare_factor << endl;
#endif

  }
    
#endif

  // clear delta info

  FOR (i, INTERVAL_NUM) {
    esti[i].delta_sum = 0;
    esti[i].n = 0;
  }
    
  return max_abs_d;
}

#endif

void Entry::bin_write(FILE *fp, int inter)
{
  int val = int(round(p_map->get_y(inter) * 512));
	
  if (val > +32767) { 
    val = +32767;
    fprintf(stderr, ">"); fflush(stderr); 
  } else if (val < -32767) {
    val = -32767;
    fprintf(stderr, "<%f", p_map->get_y(inter)); fflush(stderr); 
  }
  
  fputc(val & 255, fp);
  fputc(val >> 8,  fp);
}


void Entry::bin_read(FILE *fp, int inter)
{
  short val = fgetc(fp);
  val += fgetc(fp) << 8;
  p_map->set_y(inter, val / 512.0);
}


void Entry::alloc_esti()
{
  int n = 1;

  if (!one_phase) n = INTERVAL_NUM;

  if (esti_left < n) {

    // allocate new esti block

    esti_block = new Estimate[ESTI_BLOCK_LEN];

    if (!first_esti_block) first_esti_block = esti_block;

    esti_left = ESTI_BLOCK_LEN;
    if (!esti_block) Error("alloc_esti: out of memory");

#if 0
    static int count = 0;

    count++;
    cout << count << endl;
#endif

  }

  esti = esti_block;

  esti_block += n;
  esti_left  -= n;
}











#if 0


// old code

  FOR (i, INTERVAL_NUM) {
    int n = esti[i].n;

    if (n >= SINGLE_N) cout << "|"; 
    else if (n >= SINGLE_N/2) cout << "-";
    else cout << "_";

    if (n > 0) {

      double dy_n = n*esti[i].y + esti[i].delta_sum;

#if FIND_OUTLIERS

      if (n >= 3) { // should be >= 3

	float avg = dy_n / n;
	float avg1;

	avg1 = (dy_n - (esti[i].y+esti[i].min1)) / (n-1);

cout << "outlier: avg=" << avg << " n=" << n 
     << " min=" << esti[i].y+esti[i].min1
     << " max=" << esti[i].y+esti[i].max1 
     << " ";


cout << " avg/min = " << avg1 << " ";

	if (abs(avg - avg1) >= OUTLIER_DIFF) {

	  dy_n -= (esti[i].y+esti[i].min1); n--;
	  avg = dy_n/n;

cout << " [-> avg: " << avg << "] " << endl;

	}

	avg1 = (dy_n - (esti[i].y+esti[i].max1)) / (n-1);

cout << " avg/max = " << avg1 << " ";

	if (abs(avg - avg1) >= OUTLIER_DIFF) {
 
	  dy_n -= (esti[i].y+esti[i].max1); n--;

cout << " [-> avg: " << dy_n/n << "] " << endl;

	}

cout << endl;
      }

#endif

      if (n >= RELEVANT_N) num_relevant++;
      num += n;
      sx  += i * n;
      sy  += dy_n; 
      sxx += i * i * n;
      sxy += i * dy_n;
    }
  }  

cout << endl;

  if (!num) return 0;

  double a, b;

  if (num_relevant >= 2 && num >= REGRESSION_N) {  

    // allow a != 0 only if we have enough examples

    double del = 1.0/(num*sxx-sx*sx);
    a   = (num*sxy-sx*sy)*del;
    b   = (sxx*sy-sx*sxy)*del;

#if 0
  if (num > 20) {

    printf("num=%d\n", num);

    FOR (i, Entry::INTERVAL_NUM) {

      if (esti[i].n > 0) {
	double dy_n = esti[i].n*esti[i].y + esti[i].delta_sum;
	printf("%d %.2f\n", i, dy_n/esti[i].n);
      }
    }

    printf("a=%f b=%f\n", a, b);

  }

#endif



  } else {

    if (num <= FLAT_N) { a = 0; b = 0; }
    else               { a = 0; b = sy / num; }

  }


#if 0
  cout << "a=(" << lm_a << "," << a << ") " 
       << "b=(" << lm_b << "," << b << ")" 
       << endl;

  if (a == 0) cout << num << " " << num_relevant << endl;

#endif

  float da = a - lm_a;
  float db = b - lm_b;

#if USE_MOMENTUM

  da += MOMENTUM_FACTOR * lm_last_delta_a;
  db += MOMENTUM_FACTOR * lm_last_delta_b;

  if (lm_a != 0 || lm_b != 0) {
    lm_last_delta_a = da;
    lm_last_delta_b = db;
  }

#endif

  lm_a += da;
  lm_b += db;

  FOR (i, INTERVAL_NUM) {
    if (esti[i].n) {
      float new_y = lm_a * i + lm_b;
      float d = abs(new_y - esti[i].y);

      if (d > max_abs_d) max_abs_d = d;
      esti[i].y = new_y;
    }
  }

#endif

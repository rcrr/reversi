// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// komi tournament / 7.99

// komi4 best

#include <stdio.h>
#include <math.h>

#define MAX_NUM  10000

struct Data {

  float x, y;
  int   result;

} data[MAX_NUM];

inline int round(double x) { return int(rint(x)); }
float komi_round(float x) { return round(x*0.5)*2; }

//----------------------------------------------------

const float A1 = 0.825924;
const float B1 = 0.596778;
const float C1 = 0.463979;

float komi1(float x, float y)
{
  return (A1*x + B1*y + C1);
}

//----------------------------------------------------

const float A2 = 0.744125;
const float B2 = 0.632695;
const float C2 = 0.197316;

float komi2(float x, float y)
{
  if (fabs(x) <= 6) 
    return (A2*x + B2*y + C2);
  else
    return komi1(x,y);
}

//----------------------------------------------------

const float A3 = 0.711834;
const float B3 = 0.478802;

float komi3(float x, float y)
{
  return (A3*(x+y) + B3);
}


//----------------------------------------------------

const float A4 = 0.686704;
const float B4 = 0.204233;

float komi4(float x, float y)
{
  if (fabs(x) <= 6) 
    return (A4*(x+y) + B4);
  else
    return komi3(x,y);
}


//----------------------------------------------------


const float A5 = 0.7089;

float komi5(float x, float y)
{
  return (A5*(x+y));
}

//----------------------------------------------------

const float A6 = 0.686353;

float komi6(float x, float y)
{
  if (fabs(x) <= 6) 
    return (A6*(x+y));
  else
    return komi5(x,y);
}

//----------------------------------------------------

typedef float (*KomiFunc)(float x, float y);

KomiFunc komi_fs[] = {
  komi1,
  komi2,
  komi3,
  komi4,
  komi5,
  komi6
};

const int KOMI_NUM = sizeof(komi_fs)/sizeof(KomiFunc);
float sum_points1[KOMI_NUM], sum_points2[KOMI_NUM];

int main()
{

  // read data

  int i, n;

  for (n=0; n< MAX_NUM; n++) {
    if (scanf("%f %f %d", &data[n].x, &data[n].y, &data[n].result) != 3) break;
  }

  printf("%d lines read\n\n", n);

  // play "tournament"


  for (int r1=0; r1 < 2; r1++) {
    for (int r2=r1; r2 < 2; r2++) {

      for (i=0; i < KOMI_NUM; i++) sum_points1[i] = sum_points2[i] = 0;
      printf("\nr1= %d  r2= %d\n", r1, r2);

      for (int a=0; a < KOMI_NUM; a++) {
	for (int b=0; b < KOMI_NUM; b++) {

	  KomiFunc k1 = komi_fs[a];
	  KomiFunc k2 = komi_fs[b];
      
 	  float points1 = 0;
	  float points2 = 0;
  
	  for (i=0; i < n; i++) {

	    float kom1 = k1(data[i].x, data[i].y);
	    float kom2 = k2(data[i].x, data[i].y);

	    if (r1) kom1 = komi_round(kom1);
	    if (r2) kom2 = komi_round(kom2);
	    
	    if (kom1 == kom2) { points1 += 0.5; points2 += 0.5; }
	    else {

	      if (fabs(kom1 - data[i].result) < fabs(kom2 - data[i].result)) {
		points1 += 1.0;
	      } else {
		points2 += 1.0;
	      }
	    }

	  }
  
	  printf("%d vs. %d: points1= %.1f (%.1f%%) points2= %.1f (%.1f%%)\n",
		 a+1, b+1,
		 points1, 100*points1/n,
		 points2, 100*points2/n);

	  sum_points1[a] += points1;
	  sum_points2[b] += points2;
	}
      }
      for (i=0; i < KOMI_NUM; i++)
	printf("%d %.1f %.1f\n",
	       i+1,
	       100.0*sum_points1[i]/(n*KOMI_NUM),
	       100.0*sum_points2[i]/(n*KOMI_NUM));
    }
  }
}

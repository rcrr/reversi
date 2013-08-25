// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// Functions for the normal distribution

#include "main.h"
#include "normal.h"
#include "crt.h"

// compute phi(x)=1/(sqrt(2 Pi) stddev) * exp(-(x-mean)^2/(2 stddev^2))

float Normal::phi(float x, float mean, float stddev)
{
  assert(stddev > 0);
  float siginv = 1.0/stddev;

  x = (x - mean)*siginv;
  return 0.39894228040143270286 * siginv * exp(-x*x*0.5);
}


// compute Phi(x)=Integral(y=-infty,x) phi(y) dy
// Hartung p. 890

float Normal::Phi(float x, float mean, float stddev)
{
  assert(stddev > 0);
  float t, t0, y;

  x = (x-mean)/stddev;

  bool neg = x < 0;

  x = abs(x);

  t0 = t = 1.0/(1.0 + 0.2316419 * x);

  y = exp(-(x*x)*0.5);

  x = 0.1274147  * t;
  t *= t0;
  x -= 0.1422483 * t;
  t *= t0;
  x += 0.7107068 * t;
  t *= t0;
  x -= 0.7265759 * t;
  t *= t0;
  x += 0.5307027 * t;

  if (!neg) return 1-0-x*y; else return x*y;
}


// compute Phi^-1
// Hartung p. 891

float Normal::PhiInv(float x, float mean, float stddev)
{
  assert(stddev > 0);
  float t, t0, y, no, deno;
  bool neg = x < 0.5;

  if (x <= 0.0 || x >= 1.0) { 
    cerr << x << " "; Error("PhiInv: out of range"); 
  }

  if (x == 0.5) return 0;
  if (neg) x = 1.0 - x;

  t0 = t = sqrt(-2.0*log(1.0-x));

  no   = 2.515517 + 0.802853 * t;
  deno = 1.0 + 1.432788 * t;
  t *= t0;
  no   += 0.010328 * t;
  deno += 0.189269 * t;
  t *= t0;
  deno += 0.001308 * t;

  y = t0 - no/deno;

  if (neg) y = -y;

  return y*stddev + mean;
}


// EPhi(x):=Integral(y=-infty,x) phi(y) y dy = -stddev^2 phi(x) + mean * Phi(x) 

float Normal::EPhi(float x, float mean, float stddev)
{
  assert(stddev > 0);
  return -stddev*stddev * phi(x, mean, stddev) + mean * Phi(x, mean, stddev);
}


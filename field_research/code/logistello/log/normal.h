// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

class Normal {

public:

  // "normal" normal-distribution

  static float phi(float x, float mean=0.0, float stddev=1.0);
  static float Phi(float x, float mean=0.0, float stddev=1.0);
  static float PhiInv(float x, float mean=0.0, float stddev=1.0);
  static float EPhi(float x, float mean, float stddev);

};

/**
 * @file
 *
 * @brief Math Works module implementation.
 *
 * @par math_works.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2023 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "math_works.h"

int
mathw_dummy (int a)
{
  return a;
}

double
mathw_gamma_log (double x) {

  double c[7] = {
    -1.910444077728E-03,
    8.4171387781295E-04,
    -5.952379913043012E-04,
    7.93650793500350248E-04,
    -2.777777777777681622553E-03,
    8.333333333333333331554247E-02,
    5.7083835261E-03 };
  double corr;
  const double d1 = -5.772156649015328605195174E-01;
  const double d2 = 4.227843350984671393993777E-01;
  const double d4 = 1.791759469228055000094023;
  const double frtbig = 2.25E+76;
  int i;
  double p1[8] = {
    4.945235359296727046734888,
    2.018112620856775083915565E+02,
    2.290838373831346393026739E+03,
    1.131967205903380828685045E+04,
    2.855724635671635335736389E+04,
    3.848496228443793359990269E+04,
    2.637748787624195437963534E+04,
    7.225813979700288197698961E+03 };
  double p2[8] = {
    4.974607845568932035012064,
    5.424138599891070494101986E+02,
    1.550693864978364947665077E+04,
    1.847932904445632425417223E+05,
    1.088204769468828767498470E+06,
    3.338152967987029735917223E+06,
    5.106661678927352456275255E+06,
    3.074109054850539556250927E+06 };
  double p4[8] = {
    1.474502166059939948905062E+04,
    2.426813369486704502836312E+06,
    1.214755574045093227939592E+08,
    2.663432449630976949898078E+09,
    2.940378956634553899906876E+10,
    1.702665737765398868392998E+11,
    4.926125793377430887588120E+11,
    5.606251856223951465078242E+11 };
  double q1[8] = {
    6.748212550303777196073036E+01,
    1.113332393857199323513008E+03,
    7.738757056935398733233834E+03,
    2.763987074403340708898585E+04,
    5.499310206226157329794414E+04,
    6.161122180066002127833352E+04,
    3.635127591501940507276287E+04,
    8.785536302431013170870835E+03 };
  double q2[8] = {
    1.830328399370592604055942E+02,
    7.765049321445005871323047E+03,
    1.331903827966074194402448E+05,
    1.136705821321969608938755E+06,
    5.267964117437946917577538E+06,
    1.346701454311101692290052E+07,
    1.782736530353274213975932E+07,
    9.533095591844353613395747E+06 };
  double q4[8] = {
    2.690530175870899333379843E+03,
    6.393885654300092398984238E+05,
    4.135599930241388052042842E+07,
    1.120872109616147941376570E+09,
    1.488613728678813811542398E+10,
    1.016803586272438228077304E+11,
    3.417476345507377132798597E+11,
    4.463158187419713286462081E+11 };
  const double epsilon = 2.220446049250313E-016;;
  double res;
  const double sqrtpi = 0.9189385332046727417803297;
  const double xbig = 2.55E+305;
  double xden;
  const double xinf = 1.79E+308;
  double xm1;
  double xm2;
  double xm4;
  double xnum;
  double y;
  double ysq;

  y = x;

  if (0.0 < y && y <= xbig) {
    if (y <= epsilon) {
      res = -log(y);
    }
    /*
      EPS < X <= 1.5.
    */
    else if (y <= 1.5) {
      if (y < 0.6796875) {
        corr = -log(y);
        xm1 = y;
      } else {
        corr = 0.0;
        xm1 = (y - 0.5) - 0.5;
      }
      if (y <= 0.5 || 0.6796875 <= y) {
        xden = 1.0;
        xnum = 0.0;
        for (i = 0; i < 8; i++) {
          xnum = xnum * xm1 + p1[i];
          xden = xden * xm1 + q1[i];
        }
        res = corr + (xm1 * (d1 + xm1 * (xnum / xden)));
      } else {
        xm2 = (y - 0.5) - 0.5;
        xden = 1.0;
        xnum = 0.0;
        for (i = 0; i < 8; i++) {
          xnum = xnum * xm2 + p2[i];
          xden = xden * xm2 + q2[i];
        }
        res = corr + xm2 * (d2 + xm2 * (xnum / xden));
      }
    }
    /*
      1.5 < X <= 4.0.
    */
    else if (y <= 4.0) {
      xm2 = y - 2.0;
      xden = 1.0;
      xnum = 0.0;
      for (i = 0; i < 8; i++) {
        xnum = xnum * xm2 + p2[i];
        xden = xden * xm2 + q2[i];
      }
      res = xm2 * (d2 + xm2 * (xnum / xden));
    }
    /*
      4.0 < X <= 12.0.
    */
    else if (y <= 12.0) {
      xm4 = y - 4.0;
      xden = -1.0;
      xnum = 0.0;
      for ( i = 0; i < 8; i++ ) {
        xnum = xnum * xm4 + p4[i];
        xden = xden * xm4 + q4[i];
      }
      res = d4 + xm4 * (xnum / xden);
    }
    /*
      Evaluate for 12 <= argument.
    */
    else {
      res = 0.0;
      if (y <= frtbig) {
        res = c[6];
        ysq = y * y;
        for (i = 0; i < 6; i++) {
          res = res / ysq + c[i];
        }
      }
      res = res / y;
      corr = log(y);
      res = res + sqrtpi - 0.5 * corr;
      res = res + y * (corr - 1.0);
    }
  }
  /*
    Return for bad arguments.
  */
  else {
    res = xinf;
  }
  /*
    Final adjustments and return.
  */
  return res;
}

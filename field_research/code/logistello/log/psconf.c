// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "crt.h"
#include "psconf.h"
#include "trans.h"

int Config::glob_res_sum=0;
int Config::glob_n=0;

static int MaxTrans(SPFELD *psf)
{
  int   max=0, j, i, a;
  int   *trj, *trmax;
  sint1 *p=psf->p;

  trmax = TransTab[max];

  for (j=1; j < 8; j++) {

    trj = TransTab[j];

    FOR_SFPOS10 (i) {
      a = p[trj[i]] - p[trmax[i]];
      if (a > 0) { max = j; trmax = TransTab[max]; break; }
      if (a < 0) break;
    }
  }

  return TransInv[max];
}


bool Config::maximal() const
{
  SPFELD bo;
  sint1  *p=bo.p;
  ULL m = 1;
  int x, y;

  FOR (y, 8) {
    FOR (x, 8) {
      int i = y*10+x+11;
      if (!(mask.bits & m))
	p[i] = 0;
      else {
	if (black.bits & m)
	  p[i] = 3;
	else if (white.bits & m)
	  p[i] = 1;
	else
	  p[i] = 2;
      }
      m <<= 1;
    }
  }

  return MaxTrans(&bo) == 0;
}


void Config::transform(Config cons[8]) const
{
  int i;
  SPFELD bo, bos[8];

  cons[0] = *this;
  to_board(bo);
  ::Transform(&bo, bos);
  for (i=1; i < 8; i++) cons[i].from_board(bos[i]);
}


void Config::to_board(SPFELD &bo) const
{
  int x, y;  
  ULL ma = 1LL;
  sint1 *p = bo.p;

  SfGrund(&bo);

  FOR (y, 8) {
    FOR (x, 8) {
      int ind = y * 10 + x + 11;
      if (mask.bits & ma) {
	if (black.bits & ma) 
	  p[ind] = 3;
	else if (white.bits & ma) 
	  p[ind] = 1;
	else
	  p[ind] = 2;
      } else 
	p[ind] = 0;

      ma <<= 1;
    }
  }
}  


void Config::from_board(SPFELD &bo)
{
  int x, y;  
  ULL ma = 1LL;
  sint1 *p = bo.p;

  mask.bits = black.bits = white.bits = 0;
  size = 0;
  n = 0;

  FOR (y, 8) {
    FOR (x, 8) {
      int ind = y * 10 + x + 11;

      switch (p[ind]) {

      case 0:  // nothing
	break;

      case 1:  // white
	size++;
	mask.bits  |= ma;
	white.bits |= ma;
	break;

      case 2:  // empty
	size++;
	mask.bits  |= ma;
	break;

      case 3:  // black
	size++;
	mask.bits  |= ma;
	black.bits |= ma;
	break;

      default: KoorAus(ind); cerr << int(p[ind]) << endl; Error("case?");

      }

      ma <<= 1;
    }
  }
}  

 
ostream &operator << (ostream &os, const Config &conf)
{
  char s[Config::MAX_STR_LEN+1];

  conf.to_string(s);
  os << s;

  return os;
}


void Config::from_line(char *line, int *sq_list)
{
  char *s = line;
  char c1, c2;
  int i = 0;

  n = 0; mean = 0; stddev = 0;
  size = 0;
  i = 0;
  mask.bits = black.bits = white.bits = 0LL;

  FOREVER {

    while (*s == ' ') s++;
    
    if (sscanf(s, "%c%c", &c1, &c2) < 2) break;
    s += 2;

    c1 = toupper(c1);

    cout << c1 << c2 << " ";

    if (c1 < 'A' || c1 > 'H' || c2 < '1' || c2 > '8') break;
    sq_list[i] = (c2-'1')*8+c1-'A';

    mask.bits  |= 1ULL << sq_list[i];
    black.bits |= 1ULL << sq_list[i];
    i++;
  }

  size = i;
  sq_list[i] = -1;
}


int Config::from_string(char *s)
{
  int x, y;
  ULL ma = 1LL;
  char *t = s;
  char c;
  int i;

  size = 0;
  if (sscanf(t, "%d : %d ", &i, &pred) != 2) return -1;

  while (*t && *t != '|') t++;
  if (!*t) return -1;
  t++;

  mask.bits = black.bits = white.bits = 0LL;

  FOR (y, 8) {
    FOR (x, 8) {
      char c = *t++;
      if (c == 'x') {
	size++;
	mask.bits |= ma;
	black.bits |= ma;
      } else if (c == 'o') {
	size++;
	mask.bits |= ma;
	white.bits |= ma;
      } else if (c == '-') {
	size++;
	mask.bits |= ma;
      } else if (c != '\267') 
	return -1;

      ma <<= 1;
    }

    t++;
  }

  n = 0; mean = 0; stddev = 0;

  if (sscanf(t, "%d %c %f %f", &n, &c, &mean, &stddev) != 4) {
    // return -1;
  }

  if (c == '-') mean = -mean;
  return 0;
}
 
void Config::to_string(char *s) const
{
  int x, y;
  ULL ma = 1LL;
  char *t = s;

  sprintf(t, "%d : %d |", size, pred);
  
  t = s+strlen(s);

  FOR (y, 8) {
    FOR (x, 8) {
      if (mask.bits & ma) {
	if (black.bits & ma) 
	  *t++ = 'x';
	else if (white.bits & ma) 
	  *t++ = 'o';
	else
	  *t++ = '-';
      } else 
	*t++ = '\267';

      ma <<= 1;
    }

    *t++ = '|';
  }

  if (mean >= 0)
    sprintf(t, " %d + %.2f %.2f %.2f", n, mean, stddev, mean/stddev);
  else
    sprintf(t, " %d - %.2f %.2f %.2f", n, -mean, stddev, -mean/stddev);
}


int Config::index(int *sq_list)
{
  int sum = 0;

  for (; *sq_list >= 0; sq_list++) {
    int cont;
    ULL m = 1ULL << *sq_list;

    if      (black.bits & m) cont = 2;
    else if (white.bits & m) cont = 0;
    else                     cont = 1;

    sum = 3*sum + cont;
  }

  return sum;
}



bool operator < (const Config &conf1, const Config &conf2)
{
  if (conf1.size < conf2.size) return true;
  if (conf1.size > conf2.size) return false;

  if (conf1.mask.bits  < conf2.mask.bits)  return true;
  if (conf1.mask.bits  > conf2.mask.bits)  return false;
			     
  if (conf1.black.bits < conf2.black.bits) return true;
  if (conf1.black.bits > conf2.black.bits) return false;
			     
  if (conf1.white.bits < conf2.white.bits) return true;
  if (conf1.white.bits > conf2.white.bits) return false;

  return false;
}


bool operator == (const Config &conf1, const Config &conf2)
{
  if (conf1 < conf2) return false;
  if (conf2 < conf1) return false;
  return true;
}


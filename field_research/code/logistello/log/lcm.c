// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// count large configurations / 2.98
// (board-file should be scrambled due to the 
// use of limited statistics)


#include "main.h"
#include "board.h"
#include "sboard.h"
#include "crt.h"
#include "trans.h"
#include "tab.h"


struct Tabs {

  const int MAX_N   = 255;
  const int MAX_SUM = 32767;
 
  typedef uint1 TYPE_OF_N;
  typedef sint2 TYPE_OF_SUM;

  TYPE_OF_N   *tab_n;
  TYPE_OF_SUM *tab_sum;
};


struct PatternInfo {
  char *name;
  int   sq[100];
  int   sq1[100];
  int   sq2[100];
  float coeff;  
};


class Pattern {

public:

  const int MAXLEN = 16;  // < 18

  char  *name;
  float coeff;    // patt_val ~ coeff * (patt1_val + patt2_val)
  int   sq[100];
  int   sq1[100];
  int   sq2[100];
  int   l, l1, l2;
  Tabs  tabs, tabs1, tabs2;
  int   sqlist[MAXLEN+1];
  int   sym_n;
  int   sym_sqlist[8][MAXLEN+1];
  int   sqlist1[MAXLEN+1];
  int   sqlist2[MAXLEN+1];

  void init(PatternInfo &pi);
  int  index(sint1 *p);
  int  sym_index(int t, sint1 *p);
  int  index1(sint1 *p);
  int  index2(sint1 *p);
  void index2list(int s, int *list);
  void write(FILE *fp, int min_num, float min_avg_res);
  void write_config(FILE *fp, int s, int ind1, int ind2, float diff);
};


static PatternInfo pattern_infos[] =
{

#if 0
  { "2x8",
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,5,6,7,8,0,
      0,9,10,11,12,13,14,15,16,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    },

    {
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
    },
    {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,5,4,3,2,1,0,
      0,0,0,0,10,9,8,7,6,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    },

    0.83379

  }
#endif

#if 0
  { "opp",
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,5,6,7,8,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,9,10,11,12,13,14,15,16,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,5,6,7,8,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,5,6,7,8,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    1.2944
  }
#endif

#if 0
  { "adj",
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,5,6,7,8,0,
      0,9,10,0,0,0,0,0,0,0,
      0,11,0,0,0,0,0,0,0,0,
      0,12,0,0,0,0,0,0,0,0,
      0,13,0,0,0,0,0,0,0,0,
      0,14,0,0,0,0,0,0,0,0,
      0,15,0,0,0,0,0,0,0,0,
      0,16,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,5,6,7,8,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,0,0,0,0,0,0,0,0,
      0,2,0,0,0,0,0,0,0,0,
      0,3,0,0,0,0,0,0,0,0,
      0,4,0,0,0,0,0,0,0,0,
      0,5,0,0,0,0,0,0,0,0,
      0,6,0,0,0,0,0,0,0,0,
      0,7,0,0,0,0,0,0,0,0,
      0,8,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    1.1666
  }
#endif

#if 1
  { "4x4",
    {
      0,0,0,0,0,0,0,0,0,0,
      0,1,2,3,4,0,0,0,0,0,
      0,5,6,7,8,0,0,0,0,0,
      0,9,10,11,12,0,0,0,0,0,
      0,13,14,15,16,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    {
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
    }
    ,
    {}
    ,
    1.151127

  }

#endif

#if 0
  { "cnt",
    {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,1,2,3,4,0,0,0,
      0,0,0,5,6,7,8,0,0,0,
      0,0,0,9,10,11,12,0,0,0,
      0,0,0,13,14,15,16,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0
    }
    ,
    {}
    ,
    {}
    ,
    1.0

  }

#endif

};

const int PATTNUM = (sizeof(pattern_infos)/sizeof(pattern_infos[0]));

static Pattern patterns[PATTNUM];


void Pattern::init(PatternInfo &pi)
{
  int i, num;
  SPFELD bo, bos[8];

  name  = pi.name;
  coeff = pi.coeff;

  if (coeff == 0.0) coeff = 1.0;

  // sqlist

  num = 0;

  FOR (i, 100) {

    bo.p[i] = sq[i] = pi.sq[i];

    if (pi.sq[i]) { 

      if (!ZUG(i)) Error("border square in pattern!");

      if (num >= MAXLEN) Error("too many squares!");
      sqlist[num++] = i;
    }
  }

  if (!num) Error("no squares!");

  sqlist[num] = 0;

  cout << "num=" << num << endl;

  tabs.tab_n = new Tabs::TYPE_OF_N[Pot3[num]];
  if (!tabs.tab_n) Error("no memory");

  tabs.tab_sum = new Tabs::TYPE_OF_SUM[Pot3[num]];
  if (!tabs.tab_sum) Error("no memory");

  l = num;

  // determine symmetry

  sym_n = 0;

  Transform(&bo, bos);
  
  for (i=1; i < 8; i++) {
    
    if (sgn_equal(bos[0], bos[i])) {

      // pattern is symmetrical => determine perm-list

      int j;

      FOR (j, num) {
	sym_sqlist[sym_n][j] = Trans[i](sqlist[j]);
      }
      sym_sqlist[sym_n][j] = 0;
      sym_n++;
    }
  }

  printf("pattern:  ");
  FOR (i, num) { KoorAus(sqlist[i]); printf(" "); }
  printf("\n");

  if (sym_n > 0) {
    
    printf("symmetries: ");

    int k;
    FOR (k, sym_n) {
      printf("%d: ", k);
      FOR (i, num) { KoorAus(sym_sqlist[k][i]); printf(" "); }
      printf("\n");
    }
  }

  // sqlist1

  num = 0;

  FOR (i, 100) {

    sq1[i] = pi.sq1[i];

    if (pi.sq1[i]) { 

      if (!ZUG(i)) Error("border square in pattern!");

      if (num >= MAXLEN) Error("too many squares!");
      sqlist1[num++] = i;
    }
  }

  sqlist1[num] = 0;

  cout << "num1=" << num << endl;

  tabs1.tab_n = new Tabs::TYPE_OF_N[Pot3[num]];
  if (!tabs1.tab_n) Error("no memory");

  tabs1.tab_sum = new Tabs::TYPE_OF_SUM[Pot3[num]];
  if (!tabs1.tab_sum) Error("no memory");

  l1 = num;


  // sqlist2

  num = 0;

  FOR (i, 100) {

    sq2[i] = pi.sq2[i];

    if (pi.sq2[i]) { 

      if (!ZUG(i)) Error("border square in pattern!");

      if (num >= MAXLEN) Error("too many squares!");
      sqlist2[num++] = i;
    }
  }

  sqlist2[num] = 0;

  cout << "num2=" << num << endl;

  tabs2.tab_n = new  Tabs::TYPE_OF_N[Pot3[num]];
  if (!tabs2.tab_n) Error("no memory");

  tabs2.tab_sum = new Tabs::TYPE_OF_SUM[Pot3[num]];
  if (!tabs2.tab_sum) Error("no memory");

  l2 = num;
}


int Pattern::index(sint1 *p)
{
  int s = 0, *sql = sqlist;

  while (*sql) s = s + s + s + p[*sql++];
  return s + (Pot3[l]-1)/2; 
}

int Pattern::sym_index(int t, sint1 *p)
{
  int s = 0, *sql = sym_sqlist[t];

  if (!*sql) return -1;

  while (*sql) s = s + s + s + p[*sql++];
  return s + (Pot3[l]-1)/2; 
}

int Pattern::index1(sint1 *p)
{
  int s = 0, *sql = sqlist1;

  while (*sql) s = s + s + s + p[*sql++];
  return s + (Pot3[l1]-1)/2; 
}

int Pattern::index2(sint1 *p)
{
  int s = 0, *sql = sqlist2;

  while (*sql) s = s + s + s + p[*sql++];
  return s + (Pot3[l2]-1)/2; 
}


void Pattern::index2list(int s, int *list)
{
  int i;

  FOR (i, l) {
    list[l-1-i] = s % 3 - 1;
    s /= 3;
  }
}

// F(2) = 0.9, F(oo) = 1, F(x) = 1-F(-x)

static float F(float x)
{
  return 1.0/(1.0+exp(-1.1*x));
}


void Pattern::write(FILE *fp, int min_num, float min_avg_res) 
{
  int  i;

  int conf_num = Pot3[l];
  int num = 0;
  double f_sum = 0;
  int f_num = 0;

  FOR (i, conf_num) {
    if (tabs.tab_n[i] && tabs.tab_n[i] >= min_num) {
      SPFELD sf;
      int list[MAXLEN+1];
      int ind1, ind2;
      int k;

      index2list(i, list);

      FOR (k, l) sf.p[sqlist[k]] = list[k];

      FOR (k, sym_n) {
	if (sym_index(k, sf.p) > i) break;
      }

      if (k < sym_n) continue;  // write only normalized configurations

      if (l1 > 0) ind1 = index1(sf.p); else ind1 = -1;
      if (l2 > 0) ind2 = index2(sf.p); else ind2 = -1;

      float avg = double(tabs.tab_sum[i])/tabs.tab_n[i];
      float avg_oth;

      if (l1 > 0 && l2 > 0) 
	avg_oth = coeff*(double(tabs1.tab_sum[ind1])/tabs1.tab_n[ind1] + 
			 double(tabs2.tab_sum[ind2])/tabs2.tab_n[ind2]);
      else if (l1 > 0)
	avg_oth = coeff*double(tabs1.tab_sum[ind1])/tabs1.tab_n[ind1];
      else if (l2 > 0)
	avg_oth = coeff*double(tabs2.tab_sum[ind2])/tabs2.tab_n[ind2];
      else avg_oth = 255;  // => save for sure
      
      // cout << "diff=" << abs(avg - avg_oth) << endl;
      
      // avg = F(avg);
      // avg_oth = F(avg_oth);

      if (abs(avg_oth) >= 1.0) {
	f_sum += avg/avg_oth;  // simple coeff. estimate
	f_num++;
      }
      
      // omit extreme configurations

      const float t = 10;
      const float t_max = 10;

      /*

	limit:      |
                    |
	      t_max +               +
                    |              /
                    |             /
       min_avg_res  +------------/
                    |
                    |      
                    +-----------+---+---> abs(avg)
                                t   64

	 abs(avg-avg_oth) >= limit => write config

      */

      if (min_avg_res > t_max) min_avg_res = t_max;

      if ((abs(avg) <= t && abs(avg - avg_oth) >= min_avg_res) ||
	  (abs(avg) >  t && abs(avg - avg_oth) >= 
	   min_avg_res+(t_max-min_avg_res)*(abs(avg)-t)/(64-t))) {

	write_config(fp, i, ind1, ind2, avg-avg_oth); num++;
      }
    }
  }
  
  fprintf(fp, "\n%d config(s)\n\n", num);
  fprintf(fp, "coeff *= %f\n", f_sum/f_num);
}


void Pattern::write_config(FILE *fp, int s, int ind1, int ind2, float avg_diff)
{
  int x, y, list[MAXLEN+1];

  index2list(s, list);

  fprintf(fp, "%d : 0 |", l);

  FOR (y, 8) {
    FOR (x, 8) {
      int co = sq[Tab8to10[(y << 3) + x]];

      if (co) {
	co = list[co-1];
	if (co > 0)
          fprintf(fp, "x");
	else if (co < 0)
          fprintf(fp, "o");
	else 
          fprintf(fp, "-");
      } else
	fprintf(fp, "\267");
    }

    fprintf(fp, "|");
  }

  fprintf(fp, " %d %.1f ", tabs.tab_n[s], tabs.tab_sum[s]/(tabs.tab_n[s]+0.001));

  if (ind1 >= 0) 
    fprintf(fp, "(1:%d %.1f) ", tabs1.tab_n[ind1], tabs1.tab_sum[ind1]/(tabs1.tab_n[ind1]+0.001));

  if (ind2 >= 0) 
    fprintf(fp, "(2:%d %.1f) ", tabs2.tab_n[ind2], tabs2.tab_sum[ind2]/(tabs2.tab_n[ind2]+0.001));

  fprintf(fp, "%.2f\n", avg_diff);
}

 
void _abort(void) { exit(1); }


int main(int argc, char **argv)
{
  char name[100];
  int  i, j, k, l, s, ind, Marke, dateinr, StrNr, Anz;
  SPFELD sf;
  FILE  *fp;
  int min_num=-1;
  float min_avg_res = -1;

  InitCrt();

  if (argc != 4) Error("usage: olc sfk-file min_num min_avg_res");

  min_num = atoi(argv[2]);
  if (min_num < 0) Error("min_num?");

  min_avg_res = atof(argv[3]);
  if (min_avg_res < 0) Error("min_avg_res?");

  FOR (i, PATTNUM) patterns[i].init(pattern_infos[i]);

  FOR (i, PATTNUM) {

    printf("%d: \n", i);

    FOR (j, 8) {

      FOR (k, patterns[i].l) { 
	KoorAus(Trans[j](patterns[i].sqlist[k])); printf(",");
      }
      printf("\n");
    }
    printf("\n");
  }


  sprintf(name, "%s", argv[1]);
  
  fp = fopen(name, "r");
  if (!fp) { Error("can't open file\n"); }

  for (i=1; ; i++) {

    j = fSfRead(fp, &sf, 1);

    if (j != 1) break;

    if ((i % 100000) == 0) { printf("%d\n", i); fflush(stdout); }

    SPFELD sft[8];
    int res = sf.Marke;

    if (res < MA_DIFF || res > MA_DIFF+128) Error("no diff-label");
    res -= MA_DIFF+64;

    Transform(&sf, sft);

    FOR (k, 8) {

      FOR (l, PATTNUM) {

	s = patterns[l].index(sft[k].p);

	Tabs &tabs = patterns[l].tabs;

	if (s < 0 || s >= Pot3[patterns[l].l]) Error("s");

	if (tabs.tab_n[s] < Tabs::MAX_N) {
	  tabs.tab_n[s]++;
	  tabs.tab_sum[s] += res;
	}

	if (patterns[l].l1 > 0 ) {

	  s = patterns[l].index1(sft[k].p);

	  Tabs &tabs1 = patterns[l].tabs1;

	  if (s < 0 || s >= Pot3[patterns[l].l1]) Error("s1");

	  if (tabs1.tab_n[s] < Tabs::MAX_N) {
	    tabs1.tab_n[s]++;
	    tabs1.tab_sum[s] += res;
	  }

	}

	if (patterns[l].l2 > 0) {

	  s = patterns[l].index2(sft[k].p);

	  Tabs &tabs2 = patterns[l].tabs2;

	  if (s < 0 || s >= Pot3[patterns[l].l2]) Error("s2");
	  
	  if (tabs2.tab_n[s] < Tabs::MAX_N) {
	    tabs2.tab_n[s]++;
	    tabs2.tab_sum[s] += res;
	  }
	}
      } 
    }

  }
      
  fclose(fp);

  printf("\nfile=%s, min_num=%d\n\n", name, min_num);

  FOR (i, PATTNUM) {
    printf("pattern %d:\n", i);
    patterns[i].write(stdout, min_num, min_avg_res);
  }

  return 0;
}


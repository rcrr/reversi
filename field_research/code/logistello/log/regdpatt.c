// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "regdpatt.h"

static void WriteDisc(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}

DPattern::DPattern() 
{ 
  int i;

  FOR (i, 100) tabs[i] = 0;
  len = 0;
}


void DPattern::init(DPatternInfo &pinf, bool allocate_tabs)
{
  int i, j, num=0;
  SPFELD bo, bos[8];
  bool used[MAX_LEN+1];
  
  name = pinf.name;
  
  cout << "DPattern " << name << endl;
  
  // compute square lists
  
  FOR (i, MAX_LEN) used[i] = false;

  FOR (i, 100) {
    sq[i] = pinf.sq[i];
    if (sq[i] != 0) {
      if (sq[i] <= 0 || sq[i] >= MAX_LEN) 
	Error("DPattern::init: value out of range in pattern-info");
      used[sq[i]-1] = true;
      num++;
    }
    bo.p[i] = sq[i];
  }

  if (sq[A1] == 0) Error("DPattern::init: A1 not occupied");
 
  if (num == 0)       Error("DPattern::init: no squares!");
  if (num >= MAX_LEN) Error("DPattern::init: too many squares!");
  
  FOR (i, num) if (!used[i]) Error("DPattern::init: hole in sequence");
  
  width  = -MAXINT;
  height = -MAXINT;

  FOR (j, 100) {

    if (bo.p[j]) { 
      if (!ZUG(j)) Error("DPattern::init: illegal square in pattern!");

      // cout << int(bos[i].p[j]) << " " << j << endl;
      sq_list[bos[i].p[j]-1] = j;
      
      int x = j % 10 - 1;
      int y = j / 10 - 1;

      if (x+1 > width)  width  = x + 1;
      if (y+1 > height) height = y + 1;
    }
  }

  sq_list[num] = 0;
  
  FOR (j, num) { GKoorAus(sq_list[j]); cout << ","; }
  cout << endl;

  xmax = min(8-width +1, 4) - 1;
  ymax = min(8-height+1, 4) - 1;

  // allocate tables

  int x, y;
  tab_num=0;

  FOR (i, 100) tabi[i] = -1;

  FOR (y, ymax+1) {
    FOR (x, xmax+1) {
      i = (y+1) * 10 + x + 1;
      tabi[i] = tab_num++;
    }
  }
  
  cout << "xmax=" << xmax << " ymax=" << ymax << endl;
  cout << "tab#=" << tab_num << endl;

  if (allocate_tabs) {

    FOR (i, tab_num) {
      tabs[i] = new Entry[Pot3[num]](false);
      if (!tabs[i]) Error("DPattern::init: no memory");
      FOR (j, Pot3[num]) {
	tabs[i][j].alloc_esti();
	tabs[i][j].p_map = &tabs[i][j];
      }
    }
  }

  len = num;
}

// so far only 2x4!


int DPattern::indices(sint1 *p, int trans, int &tab_index)
{
  int i, x, y;
  int *tr = TransTab[trans];
  int xmin, ymin;

#if 0
  SfAus(&bo, 0, 0);
  printf("trans=%d B1->", trans);
  KoorAus(tr[B1]);
  printf("\n");
#endif

  // find maximal disc in upper left quadrant (mod trans)

  // printf("pass1\n");

  FOR (y, 4) {
    FOR (x, 4) {
      int pos = (y+1)*10 + x + 1;
      i = tr[pos];
      // KoorAus(pos); printf("->"); KoorAus(i); printf("\n");
      if (p[i]) goto end1;
    }
  } 

 end1: ;

  ymin = y;

  if (ymin >= 4) {

    // printf("no disc! %d %d\n", xmax, ymax);
    xmin = xmax; ymin = ymax;   

  } else {

    // printf("pass2\n");

    FOR (x, 4) {
      for (y=ymin; y < ymin+height; y++) {
	int pos = (y+1)*10 + x + 1;
	i = tr[pos];
	//KoorAus(pos); printf("->"); KoorAus(i); printf("\n");
	if (p[i]) goto end2;
      }
    }

  end2: ;

    xmin = x;
  }

  int d = ymin*10 + xmin;  // origin displacement

  int index = 0;
  
  FOR (i, len) {
    index = index + index + index + p[tr[sq_list[i]+d]] + 1;

    //    KoorAus(tr[sq_list[i]+d]); printf(" ");
  }

  //  printf("x=%d y=%d index=%d\n", xmin, ymin, index);

  tab_index = tabi[d+11];
  return index;
}


Estimate *DPattern::e_pointer(SPFELD &bo, int trans)
{
  int tab_index;
  int conf_index = indices(bo.p, trans, tab_index);

  assert(tab_index  >= 0 && tab_index < tab_num);
  assert(conf_index >= 0 && conf_index < Pot3[len]);

  return tabs[tab_index][conf_index].p_map->esti;
}




void DPattern::conf_write(FILE *fp, int l, int n)
{
  int i, r, cont[MAX_LEN];
  
  FOR (i, l) {
    r = n % 3;
    if      (r == 0) cont[i] = WHITE;
    else if (r == 1) cont[i] = LEER;
    else             cont[i] = BLACK;
    n /= 3;
  }
  
  FOR (i, l) WriteDisc(fp, cont[l-1-i]);
}


void DPattern::asc_write(FILE *fp)
{
  int i, k, s;
  Entry *tab;
   
  FOR (s, tab_num) {
    tab = tabs[s];
    FOR (k, Pot3[len]) {
      
      DPattern::conf_write(fp, len, k);
      
      FOR (i, Entry::INTERVAL_NUM) 
	fprintf(fp, " %+6.3f", tab[k].p_map->get_y(i));
      
      fprintf(fp, "\n");
    }
  }
}


void DPattern::bin_write(FILE *fp)
{
  int s, k, l;
  Entry *tab;

  fputc(len, fp);
  
  fputc(Entry::INTERVAL_NUM, fp);
  fputc(Entry::DISC_MIN, fp);
  fputc(Entry::INTERVAL_LEN, fp);
  
  FOR (s, tab_num) {
    tab = tabs[s];
    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, Pot3[len]) 
        tab[k].bin_write(fp, l);
  }
}


bool DPattern::bin_read(FILE *fp)
{
  int s, k, l;
  Entry *tab;

  if (fgetc(fp) != len) return true;
  
  if (fgetc(fp) != Entry::INTERVAL_NUM) return true;
  if (fgetc(fp) != Entry::DISC_MIN)     return true;
  if (fgetc(fp) != Entry::INTERVAL_LEN) return true;

  FOR (s, tab_num) {
    tab = tabs[s];
    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, Pot3[len]) 
        tab[k].bin_read(fp, l);
  }
      
  return false;
}


bool DPattern::bin_write_freq(FILE *fp, int n)
{
  int s, k, l;
  Entry *tab;

  if (n <= 0) Error("n <= 0");
  
  fputc(len, fp);
  fputc(Entry::INTERVAL_NUM, fp);
  fputc(Entry::DISC_MIN, fp);
  fputc(Entry::INTERVAL_LEN, fp);

  FOR (s, tab_num) {
    tab = tabs[s];
  
    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, Pot3[len]) {
        int val = (tab[k].p_map->get_n(l) < n) * 512;
        fputc(val & 255, fp);
        fputc(val >> 8,  fp);
      }
  }
  return false;
}



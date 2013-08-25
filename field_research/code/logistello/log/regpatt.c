// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "regpatt.h"

static void WriteDisc(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}

Pattern::Pattern() 
{ 
  tab = 0; len = 0; trans_num = 0; p_sub = 0; one_phase = false; 
}


void Pattern::init(PatternInfo &pinf)
{
  int i, j, num=0;
  SPFELD bo, bos[8];
  bool used[MAX_LEN+1];
  
  name = pinf.name;
  one_phase = pinf.one_phase;
  
  cout << "Pattern " << name << endl;
  
  // compute square lists
  
  FOR (i, MAX_LEN) used[i] = false;
  
  FOR (i, 100) {
    sq[i] = pinf.sq[i];
    if (sq[i] != 0) {
      if (sq[i] <= 0 || sq[i] >= MAX_LEN) 
	Error("Pattern::init: value out of range in pattern-info");
      used[sq[i]-1] = true;
      num++;
    }
    bo.p[i] = sq[i];
  }
  
  if (num == 0)       Error("Pattern::init: no squares!");
  if (num >= MAX_LEN) Error("Pattern::init: too many squares!");
  
  FOR (i, num) if (!used[i]) Error("Pattern::init: hole in sequence");
  
  FOR (j, 8) {
    FOR (i, num) perms[j][i] = i+1;
  }

  Transform(&bo, bos);
  
  trans_num = 0;
  perm_num  = 1;

  FOR (i, 8) {
    
    for (j=0; j < i; j++)
      if (sgn_equal(bos[i], bos[j])) break;
    
    if (j >= i) {
      
      // new transformation => compute sq-list

      FOR (j, 100) {

	if (bos[i].p[j]) { 
	  if (!ZUG(j)) Error("Pattern::init: illegal square in pattern!");
	  
	  //  cout << int(bos[i].p[j]) << " ";
	  sq_lists[trans_num][bos[i].p[j]-1] = j;
	}
      }
      
      sq_lists[trans_num][num] = 0;
      
      FOR (j, num) { GKoorAus(sq_lists[trans_num][j]); cout << "," << flush; }
      cout << endl;
      trans_num++;
      
    } else {

      if (j == 0) {
	
	// pattern is symmetrical => determine perm-list

#if 0	
	printf("t=%d\n", i);
	{ int x,y; 
	  FOR (y, 10) {
	    FOR (x, 10) printf("%2d ", bo.p[y*10+x]);
	    puts("");
	  }
	  puts("");
	  FOR (y, 10) {
	    FOR (x, 10) printf("%2d ", bos[i].p[y*10+x]);
	    puts("");
	  }
	  puts("");
	}
#endif
	
	FOR (j, 100) {
	  if (bo.p[j]) {
	    perms[perm_num][bo.p[j]-1] = bos[i].p[j];
	  }
	}

	perm_num++;
      }
    }
  }

  FOR (i, perm_num) {
    FOR (j, num) cout << perms[i][j] << " ";
    cout << endl;
  }

  cout << endl;

  // allocate table
 
  tab = new Entry[Pot3[num]](one_phase);
  if (!tab) Error("Pattern::init: no memory");
  
  // compute permutations
  
  int allocated = 0;

  FOR (i, Pot3[num]) {
    int list[MAX_LEN+1], j;
    int s = i;
    
    FOR (j, num) {
      list[num-1-j] = s % 3;
      s /= 3;
    }

    int min_s = MAXINT;
    int k;

    FOR (k, perm_num) {
      s = 0;
      FOR (j, num) s = s+s+s+list[perms[k][j]-1];
      if (s < min_s) min_s = s;
    }

    if (min_s == i) {
      tab[min_s].alloc_esti();  // allocate only once!
      allocated++;
    }

    //    if (i == 265020+265720 || i == 265710+265720) { printf("%d %d\n", i-265720, min_s-265720); }

    tab[i].p_map = &tab[min_s];

    /*
      if (!strcmp(name, "CHECKERS"))
      FOR (k, perm_num) printf("i=%d, %d->%d\n", i, ps[k], min_s);
    */
    
  }

#if 1

  printf("%d entries\n", allocated);

  FOR (i, Pot3[num]) {
    if (!tab[i].p_map->esti) Error("0");
  }

#endif


  len = num;
  
  if (pinf.sub_list[0]) {
    
    // init sub-pattern
    
    PatternInfo sub_info;
    
    sub_info.one_phase = 0;
    sub_info.name = "sub";
    sub_info.sub_list[0] = 0;
    
    FOR (i, 100) sub_info.sq[i] = 0;
    
    for (i=0; pinf.sub_list[i]; i++) {
      sub_list[i] = pinf.sub_list[i];
      FOR (j, 100) 
	if (sq[j] == pinf.sub_list[i]) {
	  sub_info.sq[j] = i+1; break;
	}
    }
    sub_list[i] = 0; // end-marker
    
    p_sub = new Pattern;
    p_sub->init(sub_info);
    
  }
}


void Pattern::conf_write(FILE *fp, int l, int n)
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


void Pattern::asc_write(FILE *fp)
{
  int i, k;
  
  FOR (k, Pot3[len]) {
    
    Pattern::conf_write(fp, len, k);
    
    if (one_phase) {
      
      fprintf(fp, " %+6.3f", tab[k].p_map->get_y(0));
      
    } else {
      
      FOR (i, Entry::INTERVAL_NUM) 
	fprintf(fp, " %+6.3f", tab[k].p_map->get_y(i));
      
    }
    
    fprintf(fp, "\n");
  }
}


void Pattern::bin_write(FILE *fp)
{
  int k, l;
  
  fputc(len, fp);
  
  if (one_phase) {
    
    fputc(1,  fp);
    fputc(4,  fp);
    fputc(61, fp);
    
    FOR (k, Pot3[len]) tab[k].bin_write(fp, 0);
    
  } else {
    
    fputc(Entry::INTERVAL_NUM, fp);
    fputc(Entry::DISC_MIN, fp);
    fputc(Entry::INTERVAL_LEN, fp);
    
    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, Pot3[len]) 
        tab[k].bin_write(fp, l);
  }
}


bool Pattern::bin_read(FILE *fp)
{
  int k, l;
  
  if (fgetc(fp) != len) return true;
  
  if (one_phase) {
    
    if (fgetc(fp) != 1)  return true;
    if (fgetc(fp) != 4)  return true;
    if (fgetc(fp) != 61) return true;
    
    FOR (k, Pot3[len]) tab[k].bin_read(fp, 0);
      
  } else {
    
    if (fgetc(fp) != Entry::INTERVAL_NUM) return true;
    if (fgetc(fp) != Entry::DISC_MIN)     return true;
    if (fgetc(fp) != Entry::INTERVAL_LEN) return true;
    
    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, Pot3[len]) 
        tab[k].bin_read(fp, l);
  }
  
  return false;
}


bool Pattern::bin_write_freq(FILE *fp, int n)
{
  int k, l;
  
  if (n <= 0) Error("n <= 0");
  if (one_phase) Error("one phase not implemented");
  
  fputc(len, fp);
  fputc(Entry::INTERVAL_NUM, fp);
  fputc(Entry::DISC_MIN, fp);
  fputc(Entry::INTERVAL_LEN, fp);
  
  FOR (l, Entry::INTERVAL_NUM)
    FOR (k, Pot3[len]) {
    int val = (tab[k].p_map->get_n(l) < n) * 512;
    fputc(val & 255, fp);
    fputc(val >> 8,  fp);
  }
  
  return false;
}


int Pattern::sub_index(int ind)
{
  int i, list[MAX_LEN+1];
  int s = ind;
  
  FOR (i, len) {
    list[len-1-i] = s % 3;
    s /= 3;
  }
  
  s = 0;
  
  for (i=0; sub_list[i]; i++) 
    s = s+s+s+list[sub_list[i]-1];
  
  // cout << name << " " << ind << " -> " << s << endl;
  
  return s;
}

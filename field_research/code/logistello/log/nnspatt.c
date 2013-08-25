// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "nnspatt.h"

SPattern::SPattern() { tab = 0; trans_num = 0; }

void SPattern::init(SPatternInfo &spinf)
{
  int i, j;

  name = spinf.name;

  cout << "SPattern " << name << endl;

  sp.hash_read(spinf.conf_file);

  // compute square lists

  trans_num = sp.dsymm_n;

  cout << "distinct patterns:" << endl;

  FOR (i, sp.dsymm_n) {
 
    cout << i << "  1: " << flush;

    for (j=0; sp.sq_list1[j]; j++) {
      sq_lists1[i][j] = Trans[sp.dsymm[i]](sp.sq_list1[j]);
      KoorAus(sq_lists1[i][j]); fflush(stdout);
      cout << " ";
    }

    sq_lists1[i][j] = 0;

    cout << " 2: " << flush;

    for (j=0; sp.sq_list2[j]; j++) {
      sq_lists2[i][j] = Trans[sp.dsymm[i]](sp.sq_list2[j]);
      KoorAus(sq_lists2[i][j]); fflush(stdout);
      cout << " ";
    }

    sq_lists2[i][j] = 0;

    cout << endl;

  }

  // allocate table

  cout << " new entry[" << sp.var_num << "]" << endl;

  tab = new Entry[sp.var_num];
  if (!tab) Error("SPattern::init: no memory");

  cout << " entries=" << sizeof(Entry[sp.var_num]) << " esti=" << sp.var_num * sizeof(Estimate[Entry::INTERVAL_NUM]) << endl;

  FOR (i, sp.var_num) tab[i].alloc_esti();

}


void SPattern::conf_write(FILE *fp, int l, int n)
{
  Error("SPattern::conf_write not implemented yet");

#if 0

  int i, r, cont[MAX_LEN];
    
  FOR (i, l) {
    r = n % 3;
    if      (r == 0) cont[i] = WHITE;
    else if (r == 1) cont[i] = LEER;
    else             cont[i] = BLACK;
    n /= 3;
  }
    
  FOR (i, l) WriteDisc(fp, cont[l-1-i]);

#endif

}


void SPattern::asc_write(FILE *fp)
{
  Error("SPattern::asc_write not implemented yet");

#if 0
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
#endif
  
}


void SPattern::bin_write(FILE *fp)
{
  int k, l;
  
  fputc(0x55, fp);  // magic
  fputc(0xaa, fp);
  fputc(0x00, fp);
  fputc((sp.var_num+sp.check_sum) % 253, fp); // source checksum
  
  fputc(Entry::INTERVAL_NUM, fp);
  fputc(Entry::DISC_MIN, fp);
  fputc(Entry::INTERVAL_LEN, fp);
  
  FOR (l, Entry::INTERVAL_NUM)
    FOR (k, sp.var_num) 
      tab[k].bin_write(fp, l);
}


bool SPattern::bin_read(FILE *fp)
{
  int k, l;
  
  if (fgetc(fp) != 0x55) return true;
  if (fgetc(fp) != 0xaa) return true;
  if (fgetc(fp) != 0x00) return true;
  if (fgetc(fp) != (((sp.var_num+sp.check_sum) % 253) & 0xff)) ; // !!! return true;
  
  if (fgetc(fp) != Entry::INTERVAL_NUM) return true;
  if (fgetc(fp) != Entry::DISC_MIN)     return true;
  if (fgetc(fp) != Entry::INTERVAL_LEN) return true;
  
  FOR (l, Entry::INTERVAL_NUM)
    FOR (k, sp.var_num) 
      tab[k].bin_read(fp, l);
  
  return false;
}


bool SPattern::bin_write_freq(FILE *fp, int n)
{
  Error("Spattern::bin_write_freq not implemented");
#if 0
  
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
  
#endif

  return false;
  
}


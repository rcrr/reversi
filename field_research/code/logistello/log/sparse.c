// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "main.h"
#include "sparse.h"
#include "psconf.h"
#include <algorithm>

#define NO_COLLISION true
#define MAX_TRY_NUM  150000

static int pow3[11] = {
  1,3,9,27,81,243,729,2187,6561,19683,59049
};


bool operator < (const SparseInfo &si1, const SparseInfo &si2)
{
  return si1.id2 < si2.id2;
}


void SparseList::sort_all()
{
  sort(begin(), end());
}


SparsePattern::SparsePattern()
{
  id1n = 0;
  id2n = 0;
  config_total = 0;
  var_num = 0;
  tab_id1 = 0;
  hash_id1_seeds = 0;
  hash_tab = 0;
  hash_tab2 = 0;
}


SparsePattern::~SparsePattern()
{
  //  if (tab_id1) { delete [] tab_id1; tab_id1 = 0; } don't release, might have been copied
}


static inline int bit_num(ULL n)
{
  int num = 0;

  while (n) { num++; n = n & (n-1); }
  return num;
}


bool SparsePattern::append(int id1, SparseInfo &si)
{
  assert(id1 >= 0 && id1 < id1n);
  assert(si.id2 >= 0 && si.id2 < id2n);

  //  cout << "i1=" << id1 << " i2=" << si.id2 << " -> " << index(id1, si.id2) << endl;

  if (index(id1, si.id2) >= 0) return false; // already here

  tab_id1[id1].push_back(si);
  tab_id1[id1].sort_all();
  return true;  // appended
}

/*

  configuration syntax:

    size(int) : pred(int) ascii-board +|- mean stddev. mean/stddev


  1. id1 pattern (sq-cont. != "\267" => square occurs in pattern)
  2. id2 pattern (-"-)

  3. ... configurations (sorted)

*/


void SparsePattern::read(const char *file_name)
{
  FILE *fp;
  char line[Config::MAX_STR_LEN+2];
  int  i;

  // 1. pass: count configurations

  fp = fopen(file_name, "r");
  if (!fp) Error("can't open config.file");

  int n = 0;

  check_sum = 0;

  FOREVER {
    if (!fgets(line, Config::MAX_STR_LEN, fp)) break;
    n++;

    int l = strlen(line);
    check_sum += l*n;
    while (--l >= 0) check_sum += line[l];
  }

  fclose(fp);

  cout << n << " lines read from " << file_name << endl;

  int config_num = n-2;

  // 2. pass: store configurations

  fp = fopen(file_name, "r");
  if (!fp) Error("can't open config.file");

  // pattern 1

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / patt1");

  conf1.from_line(line, sq_list1);

  cout << "pattern1: ";
  cout << conf1 << endl;

  if (conf1.size > 10) Error("pattern1 too large");

  id1n = pow3[conf1.size];
  tab_id1 = new SparseList[id1n];


  // pattern 2

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / patt1");

  conf2.from_line(line, sq_list2);

  cout << "pattern2: ";
  cout << conf2 << endl;

  if (conf2.size > 10) Error("pattern2 too large");

  id2n = pow3[conf2.size];

  if (id2n > 32767) Error("id2 > 32767: short?");

  // determine symmetries

  Config conf, conft[8];
  int symm_n=0, symm[8];

  conf1.black.bits = conf1.mask.bits;
  conf1.white.bits = 0LL;

  conf2.black.bits = conf2.mask.bits;
  conf2.white.bits = 0LL;

  if (conf1.mask.bits & conf2.mask.bits) {
    printf("patterns not disjoint!\n");
  }

  conf.mask.bits  = conf1.mask.bits  | conf2.mask.bits;
  conf.black.bits = conf1.black.bits | conf2.black.bits;
  conf.size = bit_num(conf.mask.bits);

  conf.transform(conft);

  dsymm_n = 0;

  FOR (i, 8) {
    //    cout << "t=" << i << " " << conft[i] << endl;

    if (conft[i] == conf) {
      symm[symm_n++] = i;
    }
    
    int j;

    for (j=0; j < i; j++) 
      if (conft[i] == conft[j]) break;

    if (j >= i) { // new transformation
      dsymm[dsymm_n++] = i;
    }
  }

  cout << "symmetries: ";
  FOR (i, symm_n) cout << symm[i] << " ";

  cout << endl << "distinct symmetries: ";
  FOR (i, dsymm_n) cout << dsymm[i] << " ";
 
  cout << endl;

  int index = 0;
  int ci;
  config_total = 0;

  FOR (ci, config_num) {
    
    int i1, i2;
    Config conf_in;
    SparseInfo si;

    //    cout << ci << " " << index << endl;

    if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / conf.");
    if (conf_in.from_string(line)) Error("conf. syntax error");

    if (conf.mask.bits !=conf_in.mask.bits)
      Error("config not described by patterns");

    i1 = conf_in.index(sq_list1);
    i2 = conf_in.index(sq_list2);

    //cout << conf_in << endl;
    //cout << "i1_0=" << i1 << " " << "i2_0=" << i2 << endl;

    si.id2 = i2;
    si.index = index;

    //    cout << "first" << endl;
    //    cout << conf_in << endl;

    if (append(i1, si)) {

      // cout << conf_in << endl;

      // new config
    
      config_total++;

      // cout << "new" << endl;

      conf_in.transform(conft);

      for (i=1; i < symm_n; i++) {
	Config &co = conft[symm[i]];
	int j;

	FOR (j, i) 
	  if (co == conft[symm[j]]) break;

	if (j >= i) {

	  i1 = co.index(sq_list1);
	  i2 = co.index(sq_list2);

	  //   cout << "t=" << symm[i] << " i1_t=" << i1 << " " << "i2_t=" << i2 << endl;
	
	  si.id2 = i2;
	  si.index = index;
	
	  //cout << "trans" << endl;
	  //cout << co << endl;

	  if (!append(i1, si)) { 
	    cout << co << endl;
	    Error("config. not new!?");
	  }
	
	  config_total++;
	}

      }

      index++; // new variable
      if (index >= 65536) Error(">= 65536 variables");

    } else { 

      //cout << "already seen" << endl;
    }
  }

  fgets(line, Config::MAX_STR_LEN, fp);

  if (!feof(fp)) Error("file too long");
  fclose(fp);

  cout << "#vars= "<< index
       << " #confs= " << config_total 
       << " cs= " << check_sum
       << endl << endl;

  // STL is lousy: size() takes too long

  FOR (i, id1n) tab_id1[i].my_size = tab_id1[i].size();

  var_num = index;

  // transform sq_lists from 8x8 to 10x10

  for (i=0; sq_list1[i] >= 0; i++)
    sq_list1[i] = Tab8to10[sq_list1[i]];

  sq_list1[i] = 0;

  for (i=0; sq_list2[i] >= 0; i++)
    sq_list2[i] = Tab8to10[sq_list2[i]];

  sq_list2[i] = 0;


  // compute hash increments

  FOR (i, 64) hash_incr[i] = (i+1) * hash_n * 2;  // *2 for (short)

}



void SparsePattern::hash_read(const char *file_name)
{
  FILE *fp;
  char line[Config::MAX_STR_LEN+2];
  int  i;

  // read index patterns

  fp = fopen(file_name, "r");
  if (!fp) Error("can't open hash-file");

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof (first line)");
  if (line[0] != '#') Error("no remark line");

  // pattern 1

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / patt1");

  conf1.from_line(line, sq_list1);

  cout << "pattern1: ";
  cout << conf1 << endl;

  if (conf1.size > 10) Error("pattern1 too large");

  id1n = pow3[conf1.size];

  //  tab_id1 = new SparseList[id1n];
  tab_id1 = 0;

  // pattern 2

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / patt1");

  conf2.from_line(line, sq_list2);

  cout << "pattern2: ";
  cout << conf2 << endl;

  if (conf2.size > 10) Error("pattern2 too large");

  id2n = pow3[conf2.size];

  if (id2n > 32767) Error("id2 > 32767: short?");

  // determine symmetries

  Config conf, conft[8];
  int symm_n=0, symm[8];

  conf1.black.bits = conf1.mask.bits;
  conf1.white.bits = 0LL;

  conf2.black.bits = conf2.mask.bits;
  conf2.white.bits = 0LL;

  if (conf1.mask.bits & conf2.mask.bits) {
    printf("patterns not disjoint!\n");
  }

  conf.mask.bits  = conf1.mask.bits  | conf2.mask.bits;
  conf.black.bits = conf1.black.bits | conf2.black.bits;
  conf.size = bit_num(conf.mask.bits);

  conf.transform(conft);

  dsymm_n = 0;

  FOR (i, 8) {
    //    cout << "t=" << i << " " << conft[i] << endl;

    if (conft[i] == conf) {
      symm[symm_n++] = i;
    }
    
    int j;

    for (j=0; j < i; j++) 
      if (conft[i] == conft[j]) break;

    if (j >= i) { // new transformation
      dsymm[dsymm_n++] = i;
    }
  }

  cout << "symmetries: ";
  FOR (i, symm_n) cout << symm[i] << " ";

  cout << endl << "distinct symmetries: ";
  FOR (i, dsymm_n) cout << dsymm[i] << " ";
 
  cout << endl;

  // read info

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / id1n");
  if (!sscanf(line, "%d", &id1n)) Error("no id1n");
  cout << "id1n=" << id1n << endl;

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / id2n");
  if (!sscanf(line, "%d", &id2n)) Error("no id2n");
  cout << "id2n=" << id2n << endl;

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / hash_n");
  if (!sscanf(line, "%d", &hash_n)) Error("no hash_n");
  cout << "hash_n=" << hash_n << endl;

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / var_num");
  if (!sscanf(line, "%d", &var_num)) Error("no var_num");
  cout << "var_num=" << var_num << endl;

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / config_total");
  if (!sscanf(line, "%d", &config_total)) Error("no config_total");
  cout << "config_num=" << config_total << endl;

  if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature eof / check_sum");
  if (!sscanf(line, "%d", &check_sum)) Error("no checksum");
  cout << "check_sum=" << check_sum << endl;

  // create hash-tab

  hash_id1_seeds = new int[id1n];
  hash_tab = new SparseInfo2[hash_n];

  FOR (i, id1n) hash_id1_seeds[i] = 0;

  int c_num = 0;
  int d1, d2;

  FOREVER {

    int d;

    if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature EOF");
    if (line[0] == '#') continue;
    if (sscanf(line, "%d %d", &d1, &d2) < 1) Error("syntax error (i2)");
    if (d1 < 0) break;

    int id1 = d1;
    int seed = d2;

    if (id1 >= id1n) Error("id1 too large");
    hash_id1_seeds[id1] = seed;

    // read sequence and store info in hash_tab
    
    FOREVER {
      
      int r;

      if (!fgets(line, Config::MAX_STR_LEN, fp)) Error("premature EOF");
      if (line[0] == '#') continue;
      if ((r=sscanf(line, "%d %d", &d1, &d2)) < 1) Error("syntax error (i2)");

      if (d1 < 0) break;
      if (r < 2) Error("only one number in config. line");

      if (d1 >= id2n) Error("id2 too large");

      int id2 = d1;
      int index = d2;

      if (seed+id2 >= hash_n) Error("hash address out of range");

      SparseInfo2 &si = hash_tab[seed+id2];

      if (si.id1 != SparseInfo2::EMPTY) Error("entry occupied");

      si.id1 = id1;
      si.index = index;
      c_num++;
    }
  }

  fclose(fp);

  cout << c_num << " hash_tab entries written" << endl;


  // transform sq_lists from 8x8 to 10x10

  for (i=0; sq_list1[i] >= 0; i++)
    sq_list1[i] = Tab8to10[sq_list1[i]];

  sq_list1[i] = 0;

  for (i=0; sq_list2[i] >= 0; i++)
    sq_list2[i] = Tab8to10[sq_list2[i]];

  sq_list2[i] = 0;

  // compute hash increments

  FOR (i, 64) hash_incr[i] = (i+1) * hash_n * 2;

}


void SparsePattern::tab_write(float *tab, FILE *fp) const
{
  int k;

  fputc(0x55, fp);  // magic
  fputc(0xaa, fp);
  fputc(0x00, fp);
  fputc((var_num+check_sum) % 253, fp); // source checksum

  FOR (k, var_num) {
    int val = int(round(tab[k] * 512));
	
    if (val > +32767) { 
      val = +32767;
      fprintf(stderr, ">"); fflush(stderr); 
    } else if (val < -32767) {
      val = -32767;
      fprintf(stderr, "<"); fflush(stderr); 
    }
    
    fputc(val & 255, fp);
    fputc(val >> 8,  fp);
  }

}


bool SparsePattern::tab_read(FILE *fp, float *tab)
{
  int k;

  if (fgetc(fp) != 0x55) return true;
  if (fgetc(fp) != 0xaa) return true;
  if (fgetc(fp) != 0x00) return true;

  int a = fgetc(fp);
  int b = ((var_num+check_sum) % 253) & 0xff;

  if (a != b) {
    printf("%d %d \a\n", a, b);
    return true;
  }

  FOR (k, var_num) {
    short val = fgetc(fp);
    val += fgetc(fp) << 8;
    tab[k]= val/512.0;
  }
  
  return false;
}


class InfoSort {

public:

  int size;
  int id1;

  friend bool operator < (const InfoSort &is1, const InfoSort &is2);
};


bool operator < (const InfoSort &is1, const InfoSort &is2)
{
  return is1.size > is2.size;
}



// create hash-table:  id1 -> hash_seed[id1]  ha = hash_seed[id1] + id2
// (resolve all collisions)
// write hash-table to cout

void SparsePattern::construct_hash_tab()
{
  int i, coll = 0, conf_n = 0;
  uint4 j, k;

  // find collision free fit : 
  // first fit decreasing (length) gives reasonable results

  vector<InfoSort> isl;

  FOR (i, id1n) {

    InfoSort is;

    is.id1 = i;
    is.size = tab_id1[i].size();

    isl.push_back(is);
  }

  sort(isl.begin(), isl.end());

#if 0

  int nnn = 0;

  FOR (i, id1n) 
    if (isl[i].size > 0) {
      nnn++;
      cout << isl[i].id1 << endl;
      FOR (j , isl[i].size) 
	cout << tab_id1[isl[i].id1][j].id2 << " " 
	     << tab_id1[isl[i].id1][j].index << endl;
      cout << -1 << endl;
    }

  cout << -1 << endl << nnn << endl;
#endif

  hash_n = round((config_total + 2*id2n)*2.2);
  hash_id1_seeds = new int[id1n];

  uint4 max_seed;

  FOREVER {

    const float GROW_FACTOR = 1.05;

    hash_n = round(hash_n * GROW_FACTOR);

    if (hash_tab) delete [] hash_tab;
    hash_tab = new SparseInfo2[hash_n];

    max_seed = 0;

    cout << "size=" << hash_n << endl;

    FOR (i, id1n) hash_id1_seeds[i] = 0;

    FOR (i, id1n) {

      int i1 = isl[i].id1;
      SparseList &sl = tab_id1[i1];

      if (sl.size() == 0) continue;

      cout << "." << flush;

      // 0..id2n-1 is free =>
      // if id1_seed is 0, id2n can be added savely
      // and entry will not be legal
      
      // hash_n-id2n .. hash_n-1 is free =>
      // there is no wrap-around (no check needed)
      
      for (j=id2n; j < hash_n-id2n; j++) {
	
	hash_id1_seeds[i1] = j;
	
	FOR (k, sl.size()) {
	  int hi = hash_addr(i1, sl[k].id2);
	  if (hash_tab[hi].id1 != SparseInfo2::EMPTY) break;
	}
	
	if (k >= sl.size()) break;
	
      }
      
      if (j < hash_n-id2n) {
	
	// fit
	
	hash_id1_seeds[i1] = j;

	if (j > max_seed) max_seed = j;
	
	FOR (j, sl.size()) {
	  int hi = hash_addr(i1, sl[j].id2);
	  hash_tab[hi].id1 = i1;
	  hash_tab[hi].index = sl[j].index;
	  conf_n++;
	}
	
      } else {

	cout << " collision at " << i << endl;
	break;
      }
    }

    cout << "max_seed=" << max_seed << endl;

    if (i >= id1n) break;  // all fit
  }

  
  hash_n = max_seed + id2n;

  cout << id1n << endl;
  cout << id2n << endl;
  cout << hash_n << endl;
  cout << var_num << endl;
  cout << config_total << endl;
  cout << check_sum << endl;

  FOR (i, id1n) {
    
    int i1 = isl[i].id1;
    SparseList &sl = tab_id1[i1];
    
    if (sl.size() > 0) {

      cout << i1 << " " << hash_id1_seeds[i1] << endl;

      FOR (j, sl.size()) {
	cout << sl[j].id2 << " " << sl[j].index << endl;
      }

      cout << -1 << endl;
    }
  }

  cout << -1 << endl;

  cout << config_total << " configurations" << endl;
}




void SparsePattern::hash_speed_adjust()
{
  int i;

  FOR (i, id1n) {
    if (hash_id1_seeds[i]) 
      hash_id1_seeds[i] *= 2;  // corresponds to double-index
  }

  hash_id1_seeds += (id1n-1)/2;  // center seeds
 
  FOR (i, hash_n) 
    if (hash_tab[i].id1 != 0) {
      hash_tab[i].id1 = (hash_tab[i].id1-(id1n-1)/2) * 2;
    }

  hash_tab += (id2n-1)/2;  // center hash-tab
}


void SparsePattern::free_lists()
{
  delete [] tab_id1;
  tab_id1 = 0;
}

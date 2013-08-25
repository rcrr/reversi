// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "nnbinfo.h"

Patterns *BoardInfo::p_patterns;
Features *BoardInfo::p_features;
SPatterns *BoardInfo::p_spatterns;
DPatterns *BoardInfo::p_dpatterns;

inline int BoardInfo::patt_index(SPFELD &bo, int *sq_list)
{
  int n = 0;
  
  while (*sq_list) n = 3*n + bo.p[*sq_list++] + 1;
  
  return n;
}


void BoardInfo::attach_vectors(
  Patterns &patterns, 
  Features &features, 
  SPatterns &spatterns, 
  DPatterns &dpatterns
)
{
  p_patterns  = &patterns;
  p_features  = &features;
  p_spatterns = &spatterns;
  p_dpatterns = &dpatterns;
}

#if USE_SIGMOID

inline static float F(int result)
{
  if (result ==  64) result =  63;
  if (result == -64) result = -63;

  float p = (result+64)/128.0;

  return 32.0 * log(p/(1.0 - p));
}

#else

inline static float F(int result)
{
  return result;
}

#endif



// compute board data

void BoardInfo::from_board(SPFELD &bo, int dn)
{
  int i;
  int m = bo.Marke;
  
  if (m < MA_DIFF || m > MA_DIFF+128) Error("from_board: no diff-label");
  
  m -= (MA_DIFF+64);
  
  // make m even 
  
  if (m & 1) {
    if (m > 0) m++;
    if (m < 0) m--;
  }

  result = m;
  val = F(result);
  disc_num = dn; 

  board = bo;

  //    if (SfAnz(&bo) != dn) Error("corrupt dn"); // !!!

  // typical features

  f_num = (*p_features).size();

  FOR (i, f_num) {

    Feature &feature = (*p_features)[i];

    if (i >= F_NUM_MAX) Error("too many features");
    f_values[i] = feature.f(bo, disc_num);

    if (feature.bucket_num == 1) {

      f_entries[i] = feature.entries[0].esti;

    } else { 

      if (f_values[i] < 0 || f_values[i] >= feature.bucket_num) {
	cerr << feature.name << ": " << flush;
	Error("feature value out of range");
      }

      f_entries[i] = feature.entries[f_values[i]].esti;

    }
  }

  f_entries[f_num] = 0;


  // patterns

  p_num = 0;

  FORU (i, (*p_patterns).size()) {

    int j;
    Pattern &patt = (*p_patterns)[i];

    FOR (j, patt.trans_num) {

      int r = patt_index(bo, patt.sq_lists[j]);
      if (r < 0 || r > MAX_IND) Error("illegal index");

      if (p_num >= P_NUM_MAX) Error("too many updates");
      p_entries[p_num++] = patt.tab[r].p_map->esti;

      // cout << patt_index(bo, patt.sq_lists[j]) << " ";
    }
    // cout << endl;
  }


#if USE_SPARSE_PATTERNS

  // sparse patterns

  FORU (i, (*p_spatterns).size()) {
    int j;
    SPattern &spatt = (*p_spatterns)[i];

    FOR (j, spatt.trans_num) {

      int r1 = patt_index(bo, spatt.sq_lists1[j]);
      if (r1 < 0 || r1 > MAX_IND) Error("illegal sp1 index");

      int r2 = patt_index(bo, spatt.sq_lists2[j]);
      if (r2 < 0 || r2 > MAX_IND) Error("illegal sp2 index");

      int index = spatt.sp.hash_fast_index(r1, r2);

#if 0
      if (index != spatt.sp.index(r1, r2)) {
	cout << i << " " << j << " " << r1 << " " << r2 << " " 
	     << index << " " << spatt.sp.index(r1, r2) << endl;
	Error("!=");
      }
#endif

      if (index >= 0) {

	if (index >= spatt.sp.var_num) {
	  cout << index << " " << spatt.sp.var_num << " " 
	       << r1 << " " << r2 << " " 
	       << BoardInfo::P_NUM_MAX << " " << p_num 
	       << " " << i << endl;
	  Error(">=");
	}

	if (p_num >= BoardInfo::P_NUM_MAX) Error("too many sp updates");
	p_entries[p_num++] = spatt.tab[index].p_map->esti;

      }

      // cout << patt_index(bo, patt.sq_lists[j]) << " ";
    }
    // cout << endl;
  }

#endif

  // dynamic patterns

  FORU (i, (*p_dpatterns).size()) {

    int t;
    DPattern &patt = (*p_dpatterns)[i];

    FOR (t, 8) {
      Estimate *pe = patt.e_pointer(bo, t);
      if (pe && p_num >= P_NUM_MAX) Error("too many updates");

      if (pe) p_entries[p_num++] = pe;
    }
  }


  p_entries[p_num] = 0;  // end-marker

  //cout << sp_ind_num << endl;
}


float BoardInfo::evaluate() const
{
  int i, inter = Entry::inter(disc_num);
  float value = 0;
  
  const Estimate * const *pe = f_entries;

  FOR (i, int((*p_features).size())) {
    Feature &f = (*p_features)[i];
    
    if (f.bucket_num == 1) 
      value += f_values[i] * (*pe)[inter].y;
    else 
      value += (*pe)[inter].y;

    pe++;
  }
  

  if (p_num > 0) {

    pe = p_entries+p_num-1;
       
    do {
      value += (*pe)[inter].y;
      pe--;
    } while (pe >= p_entries);
  }
  
  return value;
}


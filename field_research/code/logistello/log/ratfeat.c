// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "ratfeat.h"

Feature::Feature() { f = 0; bucket_num = 0; entries_v = entries_w = 0; }

void Feature::init(FeatureInfo &finf)
{
  name = finf.name;
  f = finf.f;
  bucket_num = finf.bucket_num;
  one_phase = finf.one_phase;
  
  cout << "Feature(" << bucket_num << ") " << name << endl << endl;
  
  if (bucket_num <= 0) Error("Feature::init: bucket_num <= 0");
  entries_v = new Entry[bucket_num](one_phase);
  if (!entries_v) Error("Feature::init: no mem");

  entries_w = new Entry[bucket_num](one_phase);
  if (!entries_w) Error("Feature::init: no mem");

  int i;

  FOR (i, bucket_num) {
    entries_v[i].alloc_esti();
    entries_w[i].alloc_esti();
  }

  set_weights(1.0);
}



void Feature::set_weights(float v)
{
  int i;

  FOR (i, bucket_num) entries_w[i].set(v);
}


void Feature::weight_lower_bound(float v)
{
  int i;
  
  FOR (i, bucket_num) entries_w[i].lower_bound(v);
}


void Feature::weight_upper_bound(float v)
{
  int i;
  
  FOR (i, bucket_num) entries_w[i].upper_bound(v);
}


void Feature::asc_write(FILE *fp)
{
  int i, k;

  FOR (k, bucket_num) {
    
    fprintf(fp, "%2d ", k);
    FOR (i, Entry::INTERVAL_NUM)
      fprintf(fp, " %+f/%f", entries_v[k].get_y(i), entries_w[k].get_y(i));
    fprintf(fp, "\n");
  }
}


void Feature::bin_write(FILE *fp)
{
  int i, l;
  
  if (one_phase) Error("one phased features not supported yet");
  
  fputc(bucket_num, fp);
  fputc(Entry::INTERVAL_NUM, fp);
  fputc(Entry::DISC_MIN, fp);
  fputc(Entry::INTERVAL_LEN, fp);
  
  FOR (l, Entry::INTERVAL_NUM)
    FOR (i, bucket_num) 
      entries_v[i].bin_write(fp, l);

  FOR (l, Entry::INTERVAL_NUM)
    FOR (i, bucket_num) 
      entries_w[i].bin_write(fp, l);
}

bool Feature::bin_read(FILE *fp)
{
  int k, l;
  
  if (fgetc(fp) != bucket_num) return true;
  if (fgetc(fp) != Entry::INTERVAL_NUM) return true;
  if (fgetc(fp) != Entry::DISC_MIN)     return true;
  if (fgetc(fp) != Entry::INTERVAL_LEN) return true;
  
  FOR (l, Entry::INTERVAL_NUM)
    FOR (k, bucket_num) 
      entries_v[k].bin_read(fp, l);

  FOR (l, Entry::INTERVAL_NUM)
    FOR (k, bucket_num) 
      entries_w[k].bin_read(fp, l);

  return false;
}

bool Feature::bin_write_freq(FILE *fp, int n)
{
  int k, l;
  
  if (n <= 0) Error("n <= 0");
  if (one_phase) Error("one phased features not supported yet");
  
  fputc(bucket_num, fp);
  fputc(Entry::INTERVAL_NUM, fp);
  fputc(Entry::DISC_MIN, fp);
  fputc(Entry::INTERVAL_LEN, fp);
  
  FOR (l, Entry::INTERVAL_NUM) {
    FOR (k, bucket_num) {
      int val = (entries_v[k].get_y(l) < n) * 512;
      fputc(val & 255, fp);
      fputc(val >> 8,  fp);
    }
  }
  
  return false;
}

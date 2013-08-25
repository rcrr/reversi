// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PATTGEN_H
#define PATTGEN_H

class Entry {

public:

  Entry *p_map;
  sint4 n;
  sint4 y;

  Entry() { n = 0; y = 0; p_map = 0; }

  void bin_write(FILE *fp) {
    fwrite(&n, sizeof(n), 1, fp);
    fwrite(&y, sizeof(y), 1, fp);
  }

  void bin_read(FILE *fp) {
    fread(&n, sizeof(n), 1, fp);
    fread(&y, sizeof(y), 1, fp);
  }
   
};


class PatternInfo {

public:
  
  sint4 sq[100];
  char *name;

  sint4 max_trans() const;
  bool  connected() const;

};


class Pattern {

private:
  
  void copy_of(const Pattern &x) {
    init(x.pinf);
  }

  void free() {
    if (tab) { delete [] tab; tab = 0; }
  }

public:

  enum { MAX_LEN = 16 };

  PatternInfo pinf;
  int   sq[100];
  int   perms[8][MAX_LEN+1];
  int   perm_num;
  int   len;
  Entry *tab;
  int   trans_num;
  int   sq_lists[8][MAX_LEN+1];

  Pattern();
  ~Pattern();

  Pattern(const Pattern &x) {
    tab = 0;
    copy_of(x);
  }

  Pattern &operator=(const Pattern &x) {
    if (this == &x) return *this;
    copy_of(x);
    return *this;
  }

  void init(const PatternInfo &pinf);
  void asc_write(FILE *fp);
  void bin_write(FILE *fp);
  bool bin_read(FILE *fp);
  bool bin_write_freq(FILE *fp, int n);

  static void conf_write(FILE *fp, int l, int n);

};


class Patterns : public vector<Pattern> {};

#endif

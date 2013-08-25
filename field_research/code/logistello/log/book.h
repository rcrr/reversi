// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef BOOK_H
#define BOOK_H

#include "main.h"
#include "sboard.h"
#include "move.h"
#include "newgame.h"


class BookNode;


class MoveNode {

public:

  BookNode   *next;
  PackedMove *moves;
  int	     game_num;
  sint1	     value;
  uint1      move_num;

private:

  uint1	     flags;
  enum { ALTERNATIVE=2, PUBLIC_DRAW=4, FINISHED=8 };
  inline void set_flag(int mask, bool f) { if (f) flags |= mask; else f &= ~mask; }
  inline bool get_flag(int mask) const { return (flags & mask) != 0; }

public:

  MoveNode() 
  {
    game_num = 0;
    value = 0;
    move_num = 0;
    moves = 0;
    next  = 0;
    flags = 0;
  }

  inline void set_flags(int fl) { flags = fl; };
  inline int  get_flags() const { return flags; };

  void is_alternative(bool f) { set_flag(ALTERNATIVE, f); }
  bool is_alternative() const { return get_flag(ALTERNATIVE); }

  void is_public_draw(bool f) { set_flag(PUBLIC_DRAW, f); }
  bool is_public_draw() const { return get_flag(PUBLIC_DRAW); }

  void is_finished(bool f) { set_flag(FINISHED, f); }
  bool is_finished() const { return get_flag(FINISHED); }
};


class BookNode {

public:

  uint1   son_num;
  MoveNode *sons;

  BookNode(int son_number = 0);
};



class PosInfo {

public:

  BookNode *p_node;
  int      move_index;
  int      path_index;
  int      trans;
    
  PosInfo() 
  {
    p_node = 0;
    move_index = 0;
    path_index = 0;
    trans = 0;
  }

  bool is_branch() { return move_index < 0; }
};



class Book {

  // private data

private:

  BookNode *p_root;

  NewGame  *games;
  int      game_num;

  PackedMove *move_pool;             // move pool
  int        move_pool_num;

  int    depth_bound;
  int    game_num_bound;

  int    mode;

  int    draw_mode;
  int    public_draw_mode;

  int    black_offset;	             // for draws good or bad
  int    public_black_offset;

  int    priv_disc_max;              // if #discs is less or equal =>
                                     // if there is a private drawing
                                     // successor and the value is 0 then 
                                     // the current node is a private draw

  // path-randomization variables

  bool   path_randomization;         // active?
  bool   rnd_book;                   // is it a random-book?
  int    max_delta;                  // maximum allowed deviation from best value
  int    curr_max_val;               // maximum reachable deviation value
  int    old_max_val;                // old maximum reachable deviation value
  int    min_val;                    // for initialization


  // private helpers

private:

  void init();
  void init_move_node(MoveNode &mn, NewGame &game, int move_index);
  bool append_game(BookNode &bn, NewGame &game, int move_index);
  int  rec_visit(BookNode &bn, int &Val, bool &is_public_draw, bool &is_finished);

  class RandPathInfo {
  public:
    bool determine_path_num;  // do so?
    int  path_num;            // returned path-number
    int  root_player;         // player to move at current root
    int  alpha, beta;         // window
  };

  int  rec_eval(BookNode &bn, int discs, int depth, RandPathInfo &rpi);
  void f_write_alternatives(FILE *fp, BookNode &bn, NewGame &game, int move_num);
  void f_write_prefixes(FILE *fp, BookNode &bn, NewGame &game, int movenum, int n, int m);


  void f_write_good(FILE *fp, BookNode &bn, NewGame &game, int movenum);
  void show_book_node(BookNode &bn, int depth, int maxdepth, char *out);


public:

  // public classes

  class Mode {
    
  public:

    enum {
      RES_FIRST   = 1,
      DEV_FIRST   = 2,
      RES_DEV_MIX = 3
    };

  };

  class DrawMode {
  public:

    enum {
      NORMAL = 0,
      GOOD   = 1,
      BAD    = 2,
      GOOD_FOR_BLACK = 3,
      BAD_FOR_BLACK  = 4
    };
  };


  // public methods

  Book();

  BookNode *get_root() { return p_root; }

  void set_mode(int mo) { assert(mo >= 1 && mo <= 3); mode = mo; }
  int  get_mode() const { return mode; }

  void set_depth_bound(int bound) { depth_bound = bound; }
  int  get_depth_bound() const    { return depth_bound; }

  void set_game_num_bound(int bound) { game_num_bound = bound; }
  int  get_game_num_bound() const    { return game_num_bound; }

  void set_draw_mode(int dm) { 
    assert(dm >= 0 && dm <= 4); 
    draw_mode = dm; 
  }
  int get_draw_mode() const  { return draw_mode; }

  void set_public_draw_mode(int dm) { 
    assert(dm >= 0 && dm <= 4); 
    public_draw_mode = dm; 
  }
  int get_public_draw_mode() const  { return public_draw_mode; }

  void set_path_randomization(bool f) { path_randomization = f; }
  bool get_path_randomization() const { return path_randomization; }

  void set_max_delta(int d)  { max_delta = d; }
  int  get_max_delta() const { return max_delta; }

  void set_priv_disc_max(int d)  { priv_disc_max = d; }
  int  get_priv_disc_max() const { return priv_disc_max; }

  void set_rnd_book(bool f) { rnd_book = f; }
  bool get_rnd_book() const { return rnd_book; }

  void create_from_files(const String &bookfile, const String &altfile, const String &drawsfile);
  bool append_game(NewGame &game);
  void append_alternatives_from_file(char *file);

  int  find_move(NewGame &game, int to_play, int &value, ValType &vtype);
  bool find_position(NewGame &game, PosInfo &posi);

  void f_write_alternatives(FILE *fp);
  void f_write_good(FILE *fp);
  void f_write_prefixes(FILE *fp, int n, int m);
  
  void rec_visit();
  bool show_sub_tree(NewGame &game, int depth);

  int  reset_max();
  int  restore_max();
  int  set_old_max();

  void delete_alt_leaf(NewGame &ngame);


  class BookState {
  public:
    int curr_max_val;
    int old_max_val;
  };

  void save_state(BookState &bs);
  void load_state(BookState &bs);
};


#endif

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef NEWGAME_H
#define NEWGAME_H

#include "game.h"

/* packed representation:  P 0 M (1 byte)
 *                        Player: 0=BLACK
 *                            Move: 1..60  (without center squares)
 */


class PackedMove {

private:
  
  enum { 
    PLAYER_BIT = 0x80, 
    MOVE_MASK  = 0x3f
  };
  uint1 pm;

  static int Code2Move[], Move2Code[];

public:

  PackedMove() { pm = 0; }

  inline void set_raw(int x) { pm = x; }
  inline int get_raw() const { return pm; }

  inline void set(int player, int move) 
  {
    assert(ZUG(move));
    assert(player == BLACK || player == WHITE);
    pm = Move2Code[move] | (player == WHITE ? PLAYER_BIT : 0);
  }

  inline int get_player() const 
  { 
    return (pm & PLAYER_BIT) ? WHITE : BLACK;
  }

  inline int get_move() const 
  {
    return Code2Move[pm & MOVE_MASK];
  }


  friend bool operator == (PackedMove &pm1, PackedMove &pm2)
  {
    return pm1.pm == pm2.pm;
  }

  friend bool operator != (PackedMove &pm1, PackedMove &pm2)
  {
    return !(pm1 == pm2);
  }
};




class NewGame {

private:

  // flags

  enum {
    ALT_BIT        = 4,
    PUBLIC_DRAW_BIT= 8,
    FINISHED_BIT   = 16 // permanent
  };

  uint1 move_num;
  sint1 value;
  PackedMove pa_moves[61];	  // endmarker 0
  uint1 flags;		

  inline void set_flag(int mask, bool f) { if (f) flags |= mask; else flags &= ~mask; }
  inline bool get_flag(int mask) const { return (flags & mask) != 0; }

public:

  NewGame() { 
    move_num = 0; 
    pa_moves[0].set_raw(0); 
    set_flags(0);
    value = 0; 
  }

  inline void set_move_num(int mn) { move_num = mn; }
  inline int  get_move_num() const { return move_num; }

  inline void set_value(int val){ value = val; }
  inline int  get_value() const { return value; }

  inline PackedMove &get_pm(int i)
  {
    assert(i >= 0 && i < 61);
    return pa_moves[i];
  } 

  inline void set_pm(int i, PackedMove &packed_move) 
  {
    assert(i >= 0 && i < 61);
    pa_moves[i] = packed_move;
  } 

  inline void set_flags(int fl) { flags = fl; };
  inline int  get_flags() const { return flags; };

  inline void is_alternative(bool f) { set_flag(ALT_BIT, f); }
  inline bool is_alternative() const { return get_flag(ALT_BIT); }

  inline void is_public_draw(bool f) { set_flag(PUBLIC_DRAW_BIT, f); }
  inline bool is_public_draw() const { return get_flag(PUBLIC_DRAW_BIT); }

  inline void is_finished(bool f) { set_flag(FINISHED_BIT, f); }
  inline bool is_finished() const { return get_flag(FINISHED_BIT); }

  void copy_info(NewGame &dest) {
    dest.value = value;
    dest.flags = flags;
  }

  bool  ok();
  bool  from_old(GAME &old_game);

  void	s_read(char *s);
  void	s_write(char *s);

  bool  s_read_srv(char *s);

  bool	f_read(FILE *fp);
  bool	f_write(FILE *fp);

  bool	f_read_packed(FILE *fp);
  bool	f_write_packed(FILE *fp);

  void	to_tab  (SPFELD &tab);
  bool	from_tab(SPFELD &tab, bool compute_fin = true);

  int	play();  // also sets player bits!
  int	play(int MoveNum, SPFELD &bo);  // also sets player bits!
  void 	unique();
  void	update(SPFELD &nboard, PARTEI ToMove);

  bool  is_proper_prefix(NewGame &prefix, int &next_move);
};


#endif


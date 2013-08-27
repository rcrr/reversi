// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ClientBoard_H
#define ClientBoard_H

#include "ClientTypes.H"
#include "ClientBoardType.H"
#include "ClientMove.H"


class ClientBoard {

private:

  ClientBoard &operator=(const ClientBoard &x) {
    ERR("private operator=");
    return *this;
  }

public:

  static const sint4 BLACK = ClientColor::BLACK,
    WHITE = ClientColor::WHITE,
    EMPTY=2, BORDER=3,
    MAX_BOARD_WIDTH = 14,
    DX = MAX_BOARD_WIDTH+2,
    MAX_BOARD_SIZE = DX * DX,
    MAX_MOVE_NUM = MAX_BOARD_SIZE,

    N = -DX, S = DX, E = 1, W = -1,
    NE = N+E, SE = S+E, NW = N+W, SW = S+W;

protected:
  
  static const sint4 d[8];
  
  ClientPTR<ClientBoardType> type;
  sint4 turn_color;
  mutable sint4 sq[MAX_BOARD_SIZE];

  void toggle_to_move();

public:

  ClientBoard() { turn_color = ClientColor::UNDEF; }
  
  ClientBoard(const ClientBoard &x) { copy_of(x); }

  virtual void copy_of(const ClientBoard &x) {

    type = x.type->clone();
    turn_color = x.turn_color;
    FORS (i, MAX_BOARD_SIZE) sq[i] = x.sq[i];
  }

  void clear();

  static sint4 xy2ind(sint4 x, sint4 y) {
    if (x < 0 || x >= MAX_BOARD_WIDTH ||
        y < 0 || y >= MAX_BOARD_WIDTH)
      ERR("xy2ind: x or y out of range");
    return (y+1) * DX + x + 1;
  }

  static sint4 ind2x(sint4 ind) { return ind % DX - 1; }
  static sint4 ind2y(sint4 ind) { return ind / DX - 1; }

  sint4 width()   const { return type->board_width(); }
  sint4 squares() const { return type->board_squares(); }  

  sint4 cont_num(sint4 co) const {
    sint4 num = 0;
    FORS (i, MAX_BOARD_SIZE)
      if (sq[i] == co) num++;
    return num;
  }
  
  sint4 get_to_move() const { return turn_color; }
  void  set_to_move(sint4 col) { turn_color = col; }
  
  sint4 get_sq_cont(sint4 index) const { return sq[index]; }
  void  set_sq_cont(sint4 index, sint4 cont) { sq[index] = cont; }  

  String to_string() const;

  String read_ggf(istream &is);
  const ClientBoardType &get_type() const { return *type; }

  virtual void write_ggf(ostream &os, bool one_line = false) const;
  
  virtual void  setup() = 0;
  virtual bool  make_move(const ClientMove &mv) = 0;
  virtual bool  is_finished() const = 0;
  virtual sint4 char2cont(char c) const = 0;
  virtual sint1 cont2char(sint4 cont) const = 0;  

  virtual ClientBoard *clone() const = 0;
  virtual ~ClientBoard();
};


#endif


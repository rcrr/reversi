// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ClientMove_H
#define ClientMove_H

#include "ClientTypes.H"

class ClientMove
{

protected:
  
  real4 eval;
  sint8 time;        // usecs
  
  ClientMove &operator=(const ClientMove &x) {
    ERR("private operator=");
  }

public:

  enum { UNDEF = -2, PASS = -1 };

  ClientMove() { eval = 0; time = 0; }

  virtual void copy_of(const ClientMove &x) {
    eval = x.eval;
    time = x.time;
  }
  
  void set_eval(real4 f) { eval = f; }
  real4 get_eval() const { return eval; }

  void set_time(sint8 f) { time = f; }
  sint8 get_time() const { return time; }

  static String coord2str(int c);

  String from_string(const String &s);
  String to_string() const;

  //  virtual void   init(int c1, int c2, int c3) = 0; 
  virtual String coords2str() const = 0;
  virtual String str2coords(String &s) = 0;
  virtual bool   is_valid() const = 0;

  virtual ClientMove *clone() const = 0;
  virtual ~ClientMove() {};
};

#endif


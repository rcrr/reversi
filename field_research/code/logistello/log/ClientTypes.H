// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ClientTypes_H
#define ClientTypes_H

// Client classes
// 9.99 by Michael Buro

#include "ClientIncludes.H"
#include "ClientString.H"

bool int_parse( const String& s, bool Sign, sint8& n );

class HHMMSS
{
public:
  /*    */ HHMMSS() {}
  explicit HHMMSS( sint8 Sec ) : s(Sec) {}

  sint8 sec() const { return s; }

  ostream& print( ostream& os ) const;
  bool     parse( const String& S, bool Min = false );

protected:
  sint8 s;
};

inline ostream& operator << ( ostream& os, const HHMMSS& v ) { return v.print(os); }

//

const sint8 uSec = 1000000;


class USEC
{
public:
  USEC( sint8 USec ) : us(USec) {}

  sint8  sec() const    { return us / uSec; }
  sint8 usec() const    { return us; }
  sint8 uadd( sint8 u ) { return us += u; }
  sint8 uset( sint8 u ) { return us  = u; }
  
  ostream& print( ostream& os ) const;
  bool     parse( const String& S, bool Min = false );
  
protected:
  sint8 us;
};

inline bool operator == (const USEC &u1, const USEC &u2) {
  return u1.usec() == u2.usec();
}

inline ostream& operator << (ostream &os, const USEC &v) { return v.print(os); }

//

class COMMA
{
public: 
  COMMA( sint8 N ) : n(N) {}

  ostream& print( ostream& os ) const;

protected:
  sint8 n;
};

inline ostream& operator << ( ostream& os, const COMMA& v ) { return v.print(os); }


class ClientColor
{
public:
  
  enum { BLACK = 0, WHITE = 1, UNDEF = 2, ALL = 3 };

  static sint4 opponent(sint4 col) {
    assert(col == BLACK || col == WHITE);
    if (col == BLACK) return WHITE; else return BLACK;
  }

  static void write(ostream &os, sint4 col) {
    if      (col == UNDEF) os << '?';
    else if (col == BLACK) os << '*';
    else if (col == WHITE) os << 'O';
    else ERR("illegal color");
  }

  static sint4 read(istream &is) {
    char c;
    is >> c;
    // errstr << char(c) << endl;
    if      (c == '*') return BLACK;
    else if (c == 'O') return WHITE;
    else if (c == '?') return UNDEF;
    else ERR("illegal color");
    return -1;
  }
};


#endif

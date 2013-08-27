//: ClientClock.H (bof) (c) Igor Durdanovic since 1998

#ifndef ClientClock_H
#define ClientClock_H

#include "ClientTypes.H"

class ClientClock
{
public:
  ClientClock();

  bool parse( const String& arg );  // this will read the settings!!

  void  reset();                     // resets clock to settings
  void  start();
  sint8 stop ( sint8 elapsed = -1 ); // in usec; -1 = use wall clock

  bool running() const { return ticking; }
  
  bool in_extension() const { return using_extension; }

  sint8 time_left(bool setting = false) const {
    if (setting) return ini_set.usec();
    else         return now.usec();
  }
  
  ostream& print( ostream& os, bool Setting = false ) const;
  
private:
  bool  using_increment;
  bool  using_extension;
  bool  ticking;
  
  USEC now; // this is the current (left) time
  USEC inc;
  USEC ext;

  USEC ini_set;
  USEC inc_set;
  USEC ext_set;
  
  USEC mark; // in usec

  friend bool operator == ( const ClientClock& c1, const ClientClock& c2 ) {
    bool eq =
      c1.ini_set == c2.ini_set &&
      c1.inc_set == c2.inc_set &&
      c1.ext_set == c2.ext_set;
    return eq;
  }
};


#endif

//: ClientClock.H (eof) (c) Igor Durdanovic since 1998

//: ClientClock.C (bof) (c) Igor Durdanovic since 1999

#include "Client.H"
#include "ClientClock.H"
#include <iomanip.h>

ClientClock::ClientClock()
  :
  using_extension(false),
  ticking(false),
  now(0),
  inc(0),
  ext(0),
  ini_set( 15 * 60 * uSec ),
  inc_set( 0),
  ext_set(  2 * 60 * uSec ),
  mark(0)
{
}

bool ClientClock::parse( const String& s )
{
  String arg, tmp, rst = s;
  String::parse( rst, arg, tmp, '/' ); rst = tmp;
   if (! ini_set.parse( arg, true ) ) return false;

  String::parse( rst, arg, tmp, '/' ); rst = tmp;
  if ( arg.empty() ) return true;
  if (! inc_set.parse( arg ) ) return false;

  String::parse( rst, arg, tmp, '/' ); rst = tmp;
  if ( arg.empty() ) return true;
  if (! ext_set.parse( arg, true ) ) return false;

  return rst.empty();
}

void ClientClock::reset()
{
  ticking = false;
  now.uset( ini_set.usec() );
  inc.uset( inc_set.usec() );
  ext.uset( ext_set.usec() );

  using_extension = false;
}

void ClientClock::start()
{
  if ( ticking ) return;
  
  ticking = true;
  //  mark.uset( System::real_time() );
}

sint8 ClientClock::stop( sint8 elapsed )
{
  if (!ticking ) return -1;

  if ( elapsed < 0 ) {
    ERR("elapsed < 0");
    // elapsed = System::real_time() - mark.usec();
  }

  ticking = false;
  now.uadd( inc.usec() - elapsed );

  if ( now.usec() < 0 && !using_extension ) {
    using_extension = true;
    now.uadd( ext.usec() );
    ext.uset( 0 );
  }

  return elapsed;
}

ostream& ClientClock::print( ostream& os, bool Setting ) const
{
  if ( Setting ) {
    os << setw(5) << HHMMSS( ini_set.sec() ) << '/'
       << setw(5) << HHMMSS( inc_set.sec() ) << '/'
       << setw(5) << HHMMSS( ext_set.sec() );
  } else {
    os << setw(5) << HHMMSS( now.sec() ) << '/'
       << setw(5) << HHMMSS( inc.sec() ) << '/'
       << setw(5) << HHMMSS( ext.sec() );
  }
  return os;
}
  
//: ClientClock.C (eof) (c) Igor Durdanovic since 1999

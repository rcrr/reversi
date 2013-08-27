#include <vector>
#include <algorithm>
#include "ClientString.H"
#include "ClientTypes.H"


bool int_parse( const String& s, bool Sign, sint8& n )
{
  if ( s.empty() ) return false;

  String::const_iterator it = s.begin();
  String::const_iterator hi = s.end();

  bool sign = !Sign;
  for ( ; it != hi; it++ ) {
    if ( ( *it == '+' || *it == '-' ) && ! sign ) { sign = true; continue; }
    if (! isdigit( *it ) ) return false;
  }

  n = atoi( s.c_str() );

  return true;
}

//

ostream& HHMMSS::print( ostream& os ) const 
{
  int w = os.width();
  os << setw(0);

  ostrstream ts;
  const int MM = 60;
  const int HH = 60 * MM;
  const int DD = 24 * HH;
  sint8 r = ( s < 0 ? -s : s );
  int dd = r / DD; r %= DD;
  int hh = r / HH; r %= HH;
  int mm = r / MM; r %= MM;
  int ss = r;
  if ( s < 0 ) ts << '-';
  if ( dd )                       ts << dd << '.';
  if ( dd || hh || w >= 8 )       ts.form("%02d:" , hh );
  if ( dd || hh || mm || w >= 5 ) ts.form("%02d:" , mm );
  /*                           */ ts.form("%02d"  , ss );
  for ( ; w > ts.pcount(); --w ) os << ' ';
  os.write( ts.str(), ts.pcount() );
  ts.rdbuf()->freeze( false );
  return os;
}

bool HHMMSS::parse( const String& S, bool Min )
{
  String s1, s2, s3, tmp;
  String::parse( S,   s1, tmp, ':' );
  String::parse( tmp, s2,  s3, ':' );
  if ( s3.empty() ) {
    if ( s2.empty() ) {
      if (! int_parse( s1, false, s ) ) return false;
      if ( Min ) s *= 60;
      return true;
    }
    sint8 m;
    if (! int_parse( s1, false, m ) ) return false;
    if (! int_parse( s2, false, s ) ) return false;
    s += m * 60;
    return true;
  }
  sint8 h;
  if (! int_parse( s1, false, h ) ) return false;
  sint8 m;
  if (! int_parse( s2, false, m ) ) return false;
  if (! int_parse( s3, false, s ) ) return false;
  s += m * 60 + h * 3600;
  if ( s > 24*60*60 ) return false;
  return true;
}

//

ostream& USEC::print( ostream& os ) const 
{
  sint8 ts = ( us < 0 ? -us : us );
  sint8 sec  = us / uSec;
  sint8 msec = ts % uSec; msec /= 1000;
  int w = os.width() - 4; if ( w < 0 ) w = 0; os << setw(w);
  os << HHMMSS( sec ) << '.';
  os.form("%03lld", msec );
  return os;
}

bool USEC::parse( const String& S, bool Min )
{
  HHMMSS hms;
  String p1, p2;
  String::parse( S, p1, p2, '.' );

  if (! hms.parse( p1, Min ) ) return false;

  if ( p2.empty() ) us = 0; else if (! int_parse( p2, false, us ) ) return false;

  us += hms.sec() * uSec;

  return true;
}

//

ostream& COMMA::print( ostream& os ) const 
{
  char buff[ 128 ]; // any uint8 should fit here
  sint8 t = ( n < 0 ? -n : n );
  sint4 c = 0;
  sint4 i = 127;
  buff[ i ] = 0;
  for ( ;; ) {
    if ( c == 3 )
      { buff[ --i ] = ','; c = 0; }
    buff[ ++c,--i ] = char('0' + (t % 10));
    if ( (t /= 10) == 0 ) break;
  }
  if ( n < 0 ) buff[--i] = '-';
  return os << (buff + i);
}

//

class CPP{ public: CPP (); };

CPP::CPP()
{
  assert( sizeof(sint1) == 1 );
  assert( sizeof(uint1) == 1 );
  assert( sizeof(sint2) == 2 );
  assert( sizeof(uint2) == 2 );
  assert( sizeof(sint4) == 4 );
  assert( sizeof(uint4) == 4 );
  assert( sizeof(sint8) == 8 );
  assert( sizeof(uint8) == 8 );
  assert( sizeof(real4) == 4 );
  assert( sizeof(real8) == 8 );
  assert( sizeof(realC) == 12 );
}

static CPP _cpp_types; // check basic types


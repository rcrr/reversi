// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "ClientString.H"
#include "Client.H"
#include <stdarg.h>

void String::parse( const String& Text, String& Name, String& Rest )
{
  Name.erase();
  Rest.erase();

  String::const_iterator it1 = Text.begin();
  String::const_iterator it2;
  while ( it1 != Text.end() &&  isspace(*it1) ) ++it1; // position at the first non-space
  it2 = it1;
  while ( it2 != Text.end() && !isspace(*it2) ) ++it2; // position at the next space
  for ( ; it1 != it2; ++it1 ) Name += *it1;            // copy to Name

  it1 = it2;
  while ( it1 != Text.end() &&  isspace(*it1) ) ++it1; // position at the first non-space
  it2 = Text.end();
  if ( it1 == it2 ) return;
  --it2;
  while ( it2 != it1 && isspace(*it2) ) --it2;         // position at the last non-space
  ++it2;
  for ( ; it1 != it2; ++it1 ) Rest += *it1;            // copy to Rest 
}

void String::parse( const String& Text, vector<String>& Names )
{
  String rest( Text );
  String name;
  String tmp;

  Names.erase( Names.begin(), Names.end() );

  for ( ;; ) {
    String::parse( rest, name, tmp ); rest = tmp;
    if ( name.empty() ) break;
    Names.push_back( name );
  }
}


void String::parse( const String& Text, String& Name, String& Rest, char Sep )
{
  Name.erase();
  Rest.erase();

  String::const_iterator it1 = Text.begin();
  String::const_iterator it2 = it1;
  while ( it2 != Text.end() && *it2 != Sep ) ++it2;    // position at the first Sep
  for ( ; it1 != it2; ++it1 ) Name += *it1;            // copy to Name

  if ( it1 != Text.end() ) ++it1;
  it2 = Text.end();
  for ( ; it1 != it2; ++it1 ) Rest += *it1;            // copy to Rest 
}

void String::parse( const String& Text, vector<String>& Names, char Sep )
{
  String rest( Text );
  String name;
  String tmp;

  Names.erase( Names.begin(), Names.end() );

  for ( ;; ) {
    String::parse( rest, name, tmp, Sep ); rest = tmp;
    if ( name.empty() ) { if ( rest.empty() ) break; else continue; }
    Names.push_back( name );
  }
}

String& String::pack()
{
  {
    String::iterator it = begin();
    String::iterator hi = end();
    for ( ; it != hi && isspace(*it); ++it );
    erase( begin(), it );
  }
  {
    String::reverse_iterator it = rbegin();
    String::reverse_iterator hi = rend();
    for ( ; it != hi && isspace(*it); ++it );
    erase(size() - (it - rbegin()), it - rbegin() );
  }
  
  return *this;
}

void String::form(const char* fmt, ... )
{
  ostringstream oss;
  
  va_list ap;
  va_start(ap, fmt);
  vform(oss, fmt, ap);
  va_end(ap);

  erase(); append(oss.str());
  // oss.rdbuf()->freeze(false);
}

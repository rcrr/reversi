// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ClientString_H
#define ClientString_H

#include "ClientIncludes.H"
#include <string>
#include <vector>

class String : public string
{
public:
  String() {}
  String( const string& s ) : string(s) {}
  String( ccptr s ) : string(s) {}
  String( ccptr s, sint4 n ) : string(s, n) {}
  String( ostringstream& os ) : string( os.str() ) {
  }

  String& operator = ( ccptr s ) { string::operator = ( s ); return *this; }
  
  String& pack(); // removes leading and trailing spaces
  
  static void parse( const String& Text, String& Name, String& Rest );
  static void parse( const String& Text, vector<String>&      Names );
  static void parse( const String& Text, String& Name, String& Rest, char Sep );
  static void parse( const String& Text, vector<String>&      Names, char Sep );

  void form(const char* fmt, ... );
  
};

#endif


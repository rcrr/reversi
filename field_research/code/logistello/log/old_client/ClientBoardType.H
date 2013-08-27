#ifndef ClientBoardType_H
#define ClientBoardType_H

// Client classes
// 9.99 by Michael Buro

#include "ClientIncludes.H"
#include "ClientString.H"
#include <vector>

class ClientBoardType
{
public:

  enum { TYPE_UNDEF = -1 };
  sint4 TYPE_DEFAULT;
  
protected:

  sint4 bt;
  vector<sint4>  types;
  vector<String> names;

public:

  ClientBoardType() { bt = TYPE_UNDEF; TYPE_DEFAULT = TYPE_UNDEF; }

  friend bool operator == (const ClientBoardType &c1, const ClientBoardType &c2);

  String to_string() const;
  String to_string2() const;
  
  String parse(istream &is);

  sint4 get_type_num() const { return types.size(); }
  sint4 get_type(sint4 i) const { return types[i]; }
  sint4 get_code() const { return bt; }
  
  void set_type_index(sint4 i) { bt = types[i]; }

  sint4 get_type_index() const;

  String get_name(sint4 i) { return names[i]; }
  
  // bool type_ok(int bt);
  
  virtual sint4 board_width() const = 0;
  virtual sint4 board_squares() const = 0;

  virtual ClientBoardType *clone() const = 0;
  virtual ~ClientBoardType() {}
};


#endif


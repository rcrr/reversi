#include "ClientBoardType.H"

bool operator == (const ClientBoardType &c1, const ClientBoardType &c2)
{
  return c1.bt == c2.bt;
}

sint4 ClientBoardType::get_type_index() const
{
  FORU (i, types.size())
    if (types[i] == bt) return i;

  return -1; 
}


String ClientBoardType::to_string() const
{
  String s;
  s.form("%d", bt);
  return s;
}


String ClientBoardType::parse(istream &is)
{
  is >> bt;
  if (!is) return "illegal board type";
  else return "";
}


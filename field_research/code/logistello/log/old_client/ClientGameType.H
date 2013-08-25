#ifndef ClientGameType_H
#define ClientGameType_H

#include "ClientIncludes.H"
#include "ClientString.H"
#include "ClientBoardType.H"

class ClientGameType
{

protected:

  // UPDATE COMPARE & COPY & SAVE & LOAD IF MEMBERS CHANGE
  
  ClientPTR<ClientBoardType> bt;
  sint4 rand_type;      // >= 0 <=> is rand game
  bool  komi_game;
  bool  anti_game;
  sint4 pref_color;     // color request of issuing player
                        // UNDEF => random colors

  String to_string(bool with_komi, real4 komi) const;

public:

  ClientGameType();

  ClientGameType(const ClientGameType &gt) { *this = gt; }

  ClientGameType &operator=(const ClientGameType &x);

  // 100% equal
  
  friend bool operator == (const ClientGameType &c1, const ClientGameType &c2);

  bool equal_up_to_pref(const ClientGameType &t) const;

  // equal type & preferred colors match
  
  bool is_matching(const ClientGameType &t) const;

  String key() const;  // return game type key (rating, files etc)

  String to_string() const;
  String to_string_with_komi(real4 komi) const;

  String parse(istream &is);

  const ClientBoardType &get_board_type() const { return *bt; }
  
  bool  is_rand_game()   const { return rand_type >= 0; }
  sint4 get_rand_type()  const { return rand_type; }
  bool  is_anti_game()   const { return anti_game; }
  bool  is_komi_game()   const { return komi_game; }
  sint4 get_pref_color() const { return pref_color; }

  ostream &save(ostream &os) const { return os.write(ccptr(this), sizeof(*this)); }
  istream &load(istream &is) { return is.read(cptr(this), sizeof(*this)); }

  virtual bool has_komi() const = 0;
  virtual bool has_rand() const = 0;
  virtual bool has_rand_type() const = 0;
  virtual bool has_anti() const = 0;  
  virtual bool has_pref_color() const = 0;    

  virtual ~ClientGameType() {};
};


#endif

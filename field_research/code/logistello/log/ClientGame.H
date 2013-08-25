// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef ClientGame_H
#define ClientGame_H

// Client classes
// 9.99 by Michael Buro

#include "ClientTypes.H"
#include "ClientGameType.H"
#include "ClientMove.H"
#include "ClientBoard.H"
#include "ClientClock.H"

class ClientGame
{
public:
  
  class ClientPlayerInfo
  {
  public:
    
    String     name;
    sint4      color;
    ClientClock clock;
    real8      rating;
    sint4      timeout1;  // first timeout order matters => sint4
    bool       timeout2;  // second timeout (fatal)
    bool       offered;   // player offered a value (result=value+komi (black))
    real8      offered_value; 
    bool       aborted;
    bool       undone;
    bool       adjourned;
    bool       resigned;
    ClientPTR<ClientMove> komi_move;
    
    void reset() {
      color     = ClientColor::UNDEF;
      timeout1  = 0;
      timeout2  = false;
      adjourned = false;
      resigned  = false;
      komi_move = (ClientMove*) 0;
      reset_offers_and_requests();
    }

    void reset_offers_and_requests() {
      offered  = false;
      offered_value = 0;
      aborted  = false;
      undone   = false;
    }
    
    ClientPlayerInfo() { reset(); }
  };

  String id;
  String place;
  String date;
  
  ClientPTR<ClientGameType> type;
  ClientPTR<ClientBoard>    start_pos;
  ClientPTR<ClientBoard>    curr_pos;

  vector< ClientPTR<ClientMove> > moves;

  ClientPlayerInfo    pinf[2];
  bool                komi_defined;
  real4               komi;

  String last_msgs[2];

  const ClientGameType &get_type() const { return *type; }
  
  const String &name_to_move() const;
  sint4 color_index(sint4 color) const;
  sint4 name_index(const String &pl) const;  // maybe ambiguous!
  sint4 index_to_move() const;

  void reset() {
    pinf[0].reset();
    pinf[1].reset();
    komi_defined = false;
    komi = 0;
    moves.erase(moves.begin(), moves.end());
  }

  real8 result() const;   // returns komi adjusted game result for black

  static real8 score(real8 r);  // normalizes r to [0,1]
  real8 score() const;          // normalizes result() to [0,1]
  
  bool play(ostream &os, const String &pl, const ClientMove &mv);
  void replay(); // replay move list

  void game_end(); // generates msgs and calls cb_end()

  void reset_offers_and_requests();

  bool is_finished() const;
  bool is_interrupted() const;
  bool matching_offers() const;
  bool aborted() const;
  bool adjourned() const;
  bool resigned() const;
  bool timeout() const;  

  String err_string();

public:

  ClientGame() { reset(); }

  void write_ggf(ostream &os, bool one_line=false) const;
  String read_ggf(istream &is);
  
  virtual ClientMove *new_ClientMove() const = 0;
  virtual ~ClientGame() {}
};


#endif


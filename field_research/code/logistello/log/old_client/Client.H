// VC/os C++ client (C) Michael Buro & Igor Durdanovic since 1999

// Version: 99-10-22

#ifndef Client_H
#define Client_H

#include <list>
#include <string>
#include <string.h>
#include <iostream.h>
#include <strstream.h>
#include <math.h>
#if !defined(glibc)
#include <stdarg.h>
#endif

#include "ClientIncludes.H"
#include "ClientString.H"
#include "ClientTypes.H"
#include "ClientClock.H"
#include "ClientGame.H"

class Client;


// this client also manages a list of games which is useful
// for pets playing several games simultaneously


class ClientMatchReq
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;
  bool   saved;
  bool   rated;
  ClientPTR<ClientGameType> gt;

  String p1;  // players
  String p2;
  real8  r1;  // ratings
  real8  r2;

  ClientClock c1;
  ClientClock c2;
};


class ClientTell  // others than from /os or super-users
{
public:
  bool read(const Client &client, const vector<String>& V );

  String sender;
  vector<String> msg;
};


class ClientRemote  // super-user message
{
public:

  String sender;
  vector<String> msg;
};


class ClientInternal  // stdin message 
{
public:

  String msg;
};


// whole game 

class ClientJoin
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;      // game id
  bool new_game;  // was not in game list
                  // access game via Client::get_game(id)
private:

  ClientPTR<ClientGame> game;
  friend Client;
};


// incremental game update (move)

class ClientUpdate
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;

private:

  ClientPTR<ClientMove> move;
  friend Client;
};


class ClientAbortReq
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;
};


class ClientScoreReq
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;
};


class ClientUndoReq
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;
};


// game start

class ClientStart
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;
  String p1;
  String p2;
  bool   rated;
};


// game end

class ClientEnd
{
public:
  bool read(const Client &client, const vector<String>& V );

  String id;
  String p1;
  String p2;

  bool aborted;
  bool adjourned;
  real4 result;  // valid iff (!aborted & !adjourned)
  
  bool game_in_list; // was game in list?
  ClientPTR<ClientGame> game;
};


// ------------ low level stuff -----------

class IO_Buff
{
public:
   IO_Buff() : buf(0), len(0), plen(0) {}
  ~IO_Buff();
  
  cptr  begin() { return buf; }
  cptr  end()   { return buf + len; }
  sint4 size()  { return len; }

  cptr  find( char c );

  void  extend( sint4 Len );
  void  update( sint4 Len ) { len += Len; }
  void  erase ( sint4 Len );
  
private:
  cptr   buf;
  sint4  len; // logical  len
  sint4 plen; // physical len
};



class Client
{
public:

  Client(const String &service, const String &host, sint4 port,
	 const String &login, const String &passwd);
  
  virtual ~Client();

  enum TYPE { UNDEF, TELL, REMOTE_CMD, INTERNAL_CMD, MATCH_REQ, START, JOIN,
	      UPDATE, END, ABORT_REQ, SCORE_REQ, UNDO_REQ,
	      SERVICE_ON,  // service reconnected
	      SERVICE_OFF  // service left
  };
  
  const String &login() const { return self; }  // your login

  const String &service_login() const { return service; }
  
  void add_super_user(const String &login);
  void remove_super_user(const String &login);

  void close_input();  // for programs started in bg
  void io();           // read/write messages 

  void send(ostrstream& os);
  void send(const ccptr mssg);
  void send(const ccptr mssg, sint4 len);
  void send(const String &s);

  
  void reset() { type = UNDEF; mssg.erase( mssg.begin(), mssg.end()); }
  TYPE state() const { return type; }

  const ClientGame *get_game(const String &id);
  bool active_games() const { return games.size() > 0; }
  void delete_games() { games.erase(games.begin(), games.end()); }
  
  ClientMatchReq  m_req;      
  ClientStart     start;      
  ClientEnd       end;        
  ClientTell      tell;       
  ClientRemote    remote;     
  ClientInternal  internal;   
  ClientJoin      join;       
  ClientUpdate    update;     
  ClientAbortReq  a_req;      
  ClientScoreReq  s_req;      
  ClientUndoReq   u_req;

private:

  list< ClientPTR<ClientGame> > games;
  list< ClientPTR<ClientGame> >::iterator find_game(const String &id);

  list<String> super_users;
  list<String>::iterator find_super_user(const String &login);

  bool read_service_on (const Client &client, const vector<String>& v );
  bool read_service_off(const Client &client, const vector<String>& v );

  bool input;

  String self;     // client-login
  String service;  // service-login

  IO_Buff   send_buff;   
  IO_Buff   recv_buff;
  IO_Buff   stdio_buff;
  sint4          sock;
  vector<String> mssg;
  TYPE           type; // must be UNDEF in order to read new mssg


public:
  
  virtual ClientGameType *new_ClientGameType() const = 0;
  virtual ClientGame     *new_ClientGame() const = 0;
  virtual ClientMove     *new_ClientMove() const = 0;  
};

#endif

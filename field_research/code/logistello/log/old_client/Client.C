#include "Client.H"

#include <sys/time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/vfs.h>
#include <sys/sysinfo.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <strstream.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <time.h>
#include <stdio.h>

#if defined (glibc)
# define RLIMIT_RESOURCE __rlimit_resource
# define SOCKLEN_T socklen_t
#else
# define RLIMIT_RESOURCE int
# define SOCKLEN_T int
#endif


#define DEBUG 1

list< ClientPTR<ClientGame> >::iterator Client::find_game(const String &id)
{
  list< ClientPTR<ClientGame> >::iterator it = games.begin();
  list< ClientPTR<ClientGame> >::iterator en = games.end();  

  for (; it != en; it++) if ((*it)->id == id) break;
  return it;
}

list<String>::iterator Client::find_super_user(const String &login)
{
  list<String>::iterator it = super_users.begin();
  list<String>::iterator en = super_users.end();  

  for (; it != en; it++) if ((*it) == login) break;
  return it;
}

const ClientGame *Client::get_game(const String &id)
{
  list< ClientPTR<ClientGame> >::iterator it = find_game(id);

  if (it == games.end()) return 0; else return &*(*it);
}

void Client::add_super_user(const String &login)
{
  if (find_super_user(login) != super_users.end()) return;
  super_users.push_back(login);
}

void Client::remove_super_user(const String &login)
{
  list<String>::iterator it = find_super_user(login);
  if (it == super_users.end()) return;
  super_users.erase(it);
}

const sint4 send_size = 4096;
const sint4 recv_size = 4096;

// -- System stuff

class System
{
public:
  static sint4 clock();     // in sec
  static ccptr dtime( sint4 c );

  static void rusage_get( struct rusage& ru );
  static sint8 real_time(); // in micro sec
  static sint8 user_time(); // in micro sec

  static void free   ( vptr Ptr );
  static vptr malloc (           sint4 Len );
  static vptr realloc( vptr Ptr, sint4 Len );

  static sint4 sock_open    ( sint4 Domain, sint4 Type, sint4 Protocol );
  static sint4 sock_connect ( ccptr Host, uint4 Port, bool Exit );
  static bool  sock_read    ( sint4 Sock, IO_Buff& B, uint4 Flag = 0 );
  static bool  sock_write   ( sint4 Sock, IO_Buff& B, uint4 Flag = 0 );
  static void  sock_shutdown( sint4 Sock, sint4 How );
  static void  sock_close   ( sint4 Sock);

  //  static void txtsockopt( sint4 Sock, ostream& os );

  static bool  fd_read ( sint4 Desc, IO_Buff& B );
  static bool  fd_write( sint4 Desc, IO_Buff& B );
  
  static sint4 select_desc( sint4 N, fd_set& Read, fd_set& Write, fd_set& Excp, struct timeval* T );

  static sint4 fcntl( sint4 Desc, sint4 Cmd );
  static sint4 fcntl( sint4 Desc, sint4 Cmd, long Arg );

  static sint4 getsockopt( sint4 Sock, sint4 Level, sint4 Opt, vptr Val, SOCKLEN_T *Len );
  static sint4 setsockopt( sint4 Sock, sint4 Level, sint4 Opt, vptr Val, int  Len );
};

#define PEER_NAME

sint4 System::clock() { return ::time(0); }

ccptr System::dtime( sint4 c )
{
  time_t C = c;
  struct tm* tc = localtime( &C );
  static char stc[1024];
  strftime( stc, 1024, "%a %d %b %Y %H:%M:%S %Z", tc );

  return stc;
}

sint8 System::real_time()
{
  struct timeval tv;

  sint4 ok = gettimeofday( &tv, 0 );

  if ( ok != 0 ) {
    cerr.form("gettimeofday( %p, 0 ) = %d", &tv, ok) << endl;
    ERR("");
    return 0;
  }

  sint8 sec  = tv.tv_sec;
  sint8 usec = tv.tv_usec;

  return sec * uSec + usec;
}

void System::rusage_get( struct rusage& ru )
{
  errno = 0;

  sint4 ok = getrusage( RUSAGE_SELF, &ru );

  if ( ok != 0 ) {
    exit( errno );
  }
}

sint8 System::user_time()
{
  struct rusage ru;

  rusage_get( ru );

  sint8 sec  = ru.ru_utime.tv_sec  + ru.ru_stime.tv_sec;
  sint8 usec = ru.ru_utime.tv_usec + ru.ru_stime.tv_usec;

  return sec * uSec + usec;
}

void System::free( vptr Ptr )
{
  if ( Ptr ) ::free( Ptr );
}

vptr System::malloc( sint4 Len )
{
  if ( Len <= 0 ) return 0;
  void *p = ::malloc( Len );
  if (! p ) {
    ERR("malloc");
    exit( errno );
  }
  return p;
}

vptr System::realloc( vptr Ptr, sint4 Len )
{
  if (! Ptr ) return malloc( Len );
  vptr p = ::realloc( Ptr, Len );
  if (! p ) {
    ERR("realloc");
    exit( errno );
  }
  return p;  
}

sint4 System::sock_open( sint4 Domain, sint4 Type, sint4 Protocol )
{
  errno = 0;

  sint4 desc = socket( Domain, Type, Protocol );

  if ( desc < 0 ) {
    ERR("sock_open");
    exit( errno );
  }

  return desc;
}

sint4 System::sock_connect( ccptr Host, uint4 Port, bool Exit )
{
  sint4 sock = sock_open( AF_INET, SOCK_STREAM, 0 );

  struct sockaddr_in sa;
  sint4                ip;

  bzero( &sa, sizeof(sa) );

  errno = 0;

  ip = inet_addr( Host );

  if ( ip == -1 ) {
    struct hostent* hp = gethostbyname( Host );
    if ( hp == 0 ) {
      ERR("socket_connect");
      if ( Exit ) exit( errno ); else return -1;
    }

    //  sa.sin_family = hp->h_addrtype;
    bcopy( hp->h_addr, cptr(&sa.sin_addr), hp->h_length );
  } else {
    bcopy( &ip, cptr(&sa.sin_addr), sizeof(ip) ); 
  }
  sa.sin_family = AF_INET;
  sa.sin_port   = htons((u_short) Port);

  sint4 ok = connect( sock, (struct sockaddr*) &sa, sizeof(sa) );

  if ( ok < 0 ) {
    cerr << ">>> socket_connect error" << endl;
    sock_close( sock );
    return -1;
  }

  return sock;
}

bool System::sock_read( sint4 Sock, IO_Buff& B, uint4 Flag )
{
  if ( Sock < 0 ) return false; // should never happen ...

  for ( bool first = true ;; first = false ) {
    errno = 0; 

    B.extend( recv_size );
    sint4 no = ::recv( Sock, B.end(), recv_size, Flag );

    if ( no == 0 && first ) return false;
    if ( no <= 0 ) {
      if ( errno == EAGAIN ) return true;
      if ( errno == EINTR  ) return true;
      if ( errno == 0      ) return true; // no error?
      cerr.form("sock_read ::recv( %d, %p, %d, %d ) = %d", Sock, B.begin(), recv_size, Flag, no );
      cerr << endl;
      return false;
    }
    cout.write( B.end(), no );
    B.update( no );
  }
}

bool System::sock_write( sint4 Sock, IO_Buff& B, uint4 Flag )
{
  if ( Sock < 0 ) return false; // should never happen ..

  for ( bool first = true ; B.size() ; first = false ) {
    errno = 0;

    sint4 no = ::send( Sock, B.begin(), B.size(), Flag );

    if ( no == 0 && first ) return false;
    if ( no <= 0 ) {
      if ( errno == EAGAIN ) return true;
      if ( errno == EINTR  ) return true;
      if ( errno == 0      ) return true; // no error?
      cerr.form("sock_write ::send( %d, %p, %d ) = %d", Sock, B.begin(), B.size(), no ) << endl;
      return false;
    }
    B.erase( no );
  }

  return true;
}

void System::sock_shutdown( sint4 Sock, sint4 How )
{
  errno = 0;

  sint4 ok = ::shutdown( Sock, How  );

  if ( ok != 0 ) {
    cerr << "shutdown( " << Sock << ", " << How << " ) = " << ok << endl;
  }
}

void System::sock_close( sint4 Sock )
{
  errno = 0;

  sint4 ok = ::close( Sock );

  if ( ok != 0 ) {
    cerr << "close( " << Sock << " ) = " << ok << endl;;
  }
}

bool System::fd_read( sint4 Desc, IO_Buff& B )
{
  if ( Desc < 0 ) return false; // should never happen ...

  for ( bool first = true ;; first = false ) {
    errno = 0; 

    B.extend( recv_size );
    sint4 no = ::read( Desc, B.end(), recv_size );

    if ( no == 0 && first ) return false;
    if ( no <= 0 ) {
      if ( errno == EAGAIN ) return true;
      if ( errno == EINTR  ) return true;
      if ( errno == 0      ) return true; // no error?
      cerr.form("fd_read ::read( %d, %p, %d ) = %d", Desc, B.begin(), recv_size, no );
      cerr << endl;;
      return false;
    }
    B.update( no );
  }
}

bool System::fd_write( sint4 Desc, IO_Buff& B )
{
  if ( Desc < 0 ) return false; // should never happen ..

  for ( bool first = true ; B.size() ; first = false ) {
    errno = 0;

    sint4 no = ::write( Desc, B.begin(), B.size() );

    if ( no == 0 && first ) return false;
    if ( no <= 0 ) {
      if ( errno == EAGAIN ) return true;
      if ( errno == EINTR  ) return true;
      if ( errno == 0      ) return true; // no error?
      cerr.form("fd_write ::write( %d, %p, %d ) = %d", Desc, B.begin(), B.size(), no ) << endl;
      return false;
    }
    B.erase( no );
  }

  return true;
}

sint4 System::select_desc( sint4 N, fd_set& Read, fd_set& Write, fd_set& Excp, struct timeval* T )
{
  errno = 0;

  sint4 no = ::select( N, &Read, &Write, &Excp, T );

  if ( no < 0 ) {
    if ( errno == EAGAIN ) return no;
    if ( errno == EINTR  ) return no;
    if ( errno == 0      ) return no;

    cerr << "select_desc select( " << N << ", ... ) = " << no << endl;

    exit( errno );
  }

  return no;
}

sint4 System::fcntl( sint4 Desc, sint4 Cmd )
{
  errno = 0;

  sint4 ok = ::fcntl( Desc, Cmd );

  if ( ok == -1 ) {
    cerr << "fcntl( " << Desc << ", " << Cmd << " ) = " << ok << endl;

    exit( errno );
  }

  return ok;
}

sint4 System::fcntl( sint4 Desc, sint4 Cmd, long Arg )
{
  errno = 0;

  sint4 ok = ::fcntl( Desc, Cmd, Arg );

  if ( ok == -1 ) {
    cerr << "fcntl( " << Desc << ", " << Cmd << ", " << Arg << " ) = " << ok << endl;

    exit( errno );
  }

  return ok;
}

sint4 System::getsockopt( sint4 Sock, sint4 Level, sint4 Opt, vptr Val, SOCKLEN_T *Len)
{
  errno = 0;

  sint4 ok = ::getsockopt( Sock, Level, Opt, Val, Len );

  if ( ok != 0 ) {
    cerr << "getsockopt( " << Sock << ", " << Level << ", " << Opt << ", .. , " << *Len << " )" << endl;

    exit( errno );
  }

  return ok;
}

sint4 System::setsockopt( sint4 Sock, sint4 Level, sint4 Opt, vptr Val, int Len )
{
  errno = 0;

  sint4 ok = ::setsockopt( Sock, Level, Opt, Val, Len );

  if ( ok != 0 ) {
    cerr << "setsockopt( " << Sock << ", " << Level << ", " << Opt << ", .. , " << Len << " )" << endl;
    exit( errno );
  }

  return ok;
}


// -- IO_Buffer

IO_Buff::~IO_Buff()
{
  System::free(buf);
  buf = 0;
  len = 0;
  plen = 0;
}

cptr IO_Buff::find( char c )
{
  for ( cptr it = begin(), hi = end(); it != hi; ++it ) if ( c == *it ) return it;
  return 0;
}

void IO_Buff::extend( sint4 Len )
{
  if ( plen < len + Len ) buf = (cptr) System::realloc( buf, plen = len + Len );
}

void IO_Buff::erase( sint4 Len )
{
  for ( cptr to = begin(), from = to + Len, hi = end(); from != hi; ) *to++ = *from++;
  len -= Len; // never release physical space ..
}


bool ClientJoin::read(const Client &client, const vector<String>& v)
{
  vector<String> vec;
  String::parse(v[0], vec);

  if ( vec.size() < 3   ) return false;

  if ( vec[0] != client.service_login()+":" ) return false;
  if ( vec[1] != "join")  return false;

  id = vec[2];
  String s;
  
  for (uint4 i=3; i < vec.size(); i++) {
    s += vec[i];
    if (i < vec.size()-1) s += " ";
  }

  for (uint4 i = 1; i < v.size(); i++) {
    s += v[i];
    s += " ";
  }
	 
  istrstream is(s.c_str(), s.size());
  ostrstream os;

  //  cout << "sgf: " << s << endl;


  game = client.new_ClientGame();
  String es = game->read_sgf(is);
    
  if (!es.empty()) {
    cerr << ">>> corrupt sgf: " << es << endl;
    return false;
  }

  game->id = id;

  
#if DEBUG
  cout << "JOIN" << endl;
  game->write_sgf(cout); cout << endl;
#endif
  
  return true;
}

#if 0
bool VC_Board::my_turn() const
{
  return ( ( p1 == self && ( col1 == turn || col1 == '?' ) ) ||
	   ( p2 == self && ( col2 == turn || col2 == '?' ) ) );
}
#endif


bool ClientMatchReq::read(const Client &client, const vector<String>& v )
{
  vector<String> vec; String::parse( v[0], vec );
  if ( vec.size() < 10  ) return false;
  if ( vec[0] != client.service_login()+":" ) return false;
  if ( vec[1] != "+"    ) return false;
  id = vec[2];
  r1 = atof( vec[3].c_str() );
  p1 = vec[4];
  if (! c1.parse(vec[5]) ) return false;
  saved = false;
  rated = false;
  if (vec[7] == "S") {
    saved = true;
    rated = true;
  } else {
    rated = vec[7] == "R";
  }

  ostrstream oss;
  istrstream iss(vec[6].c_str());

  gt = client.new_ClientGameType();
  
  if (gt->parse(iss) != "") return false;

  r2 = atof( vec[8].c_str() );
  p2 = vec[9];
  if (vec.size() > 10) {

    if (vec[10][0] != '.' && !c2.parse(vec[10])) return false;
    
  } else c2 = c1;
  cout << "MATCH_REQ" << endl;
  return true;
}

bool ClientTell::read(const Client &client, const vector<String>& v)
{
  vector<String> vec;
  String::parse(v[0], vec);

  sender = vec[0];
  sender.erase(vec[0].size()-1, vec[0].size());

  if (sender == client.service_login()) return false;

  msg = v;
  msg[0].erase(0, vec[0].size()+1);
  return true;
}

bool ClientUpdate::read(const Client &client, const vector<String>& v)
{
  vector<String> vec; String::parse(v[0], vec);

  if (vec.size() < 4) return false;

  if (vec[0] != client.service_login()+":") return false;
  if (vec[1] != "update") return false;

  id = vec[2];
  ostrstream oss;
  String es;

  move = client.new_ClientMove();
  
  if ((es=move->from_string(vec[3])) != "") ERR2(es, vec[3]);

#if DEBUG
  cout << ">>> UPDATE" << endl;
  cout << ">>> id: " << id << endl;
  cout << ">>> mv: "; cout << move->to_string(); cout << endl;
#endif

  return true;
}


bool ClientAbortReq::read(const Client &client, const vector<String>& v)
{
  vector<String> vec; String::parse(v[0], vec);

  if (vec.size() < 3) return false;

  if (vec[0] != client.service_login()+":") return false;
  if (vec[1] != "abort") return false;

  id = vec[2];

#if DEBUG
  cout << ">>> ABORT" << endl;
  cout << ">>> id: " << id << endl;
#endif

  return true;
}


bool ClientScoreReq::read(const Client &client, const vector<String>& v )
{
  vector<String> vec; String::parse(v[0], vec);

  if (vec.size() < 3) return false;

  if (vec[0] != client.service_login()+":") return false;
  if (vec[1] != "score") return false;

  id = vec[2];

#if DEBUG
  cout << ">>> SCORE" << endl;
  cout << ">>> id: " << id << endl;
#endif

  return true;
}


bool ClientUndoReq::read(const Client &client, const vector<String>& v )
{
  vector<String> vec; String::parse(v[0], vec);

  if (vec.size() < 3) return false;

  if (vec[0] != client.service_login()+":") return false;
  if (vec[1] != "undo") return false;

  id = vec[2];

#if DEBUG
  cout << ">>> UNDO" << endl;
  cout << ">>> id: " << id << endl;
#endif
  
  return true;
}

bool ClientStart::read(const Client &client, const vector<String>& v )
{
  vector<String> vec; String::parse( v[0], vec );
  if ( vec.size() < 10   ) return false;
  if ( vec[0] != client.service_login()+":" ) return false;
  if ( vec[1] != "start" ) return false;
  id = vec[2];
  if ( vec[3] != "("     ) return false;
  p1 = vec[4];
  if ( vec[5] != "vs."   ) return false;
  p2 = vec[6];
  if ( vec[7] != ")"     ) return false;
  rated = ( vec[8] == "rated" );

#if DEBUG
  cout << ">>> START" << endl;
  cout << ">>> p1: " << p1 << endl;
  cout << ">>> p2: " << p2 << endl;  
  cout << ">>> rated: " << rated << endl;
#endif
  
  return true;
}

bool ClientEnd::read(const Client &client, const vector<String>& v )
{
  vector<String> vec; String::parse(v[0], vec );
  if ( vec.size() < 10   ) return false;
  if ( vec[0] != client.service_login()+":" ) return false;
  if ( vec[1] != "end"   ) return false;
  id = vec[2];
  if ( vec[3] != "("     ) return false;
  p1 = vec[4];
  if ( vec[5] != "vs."   ) return false;
  p2 = vec[6];
  if ( vec[7] != ")"     ) return false;

  aborted = false;
  adjourned = false;
  result = 0;

#if DEBUG
  cout << ">>> END" << endl;
  cout << ">>> p1: " << p1 << endl;
  cout << ">>> p2: " << p2 << endl;  
#endif
  
  return true;
}


Client::Client(const String &s, const String &host,
	       sint4 port, const String &login, const String &passwd)
{
  type = UNDEF;
  input = true;
  
  sint4 flag;

  flag = System::fcntl( 0, F_GETFL ) | O_NONBLOCK; // cin NONBLOCKING!
  System::fcntl( 0, F_SETFL, flag );

  sock = System::sock_connect(host.c_str(), port, false );

  if ( sock < 0 ) { cerr << "sock < 0"; exit(-1); }

  //  System::txtsockopt( sock, cout << "before" << endl );
  flag = System::fcntl( sock, F_GETFL ) | O_NONBLOCK;
  System::fcntl( sock, F_SETFL, flag );

  sint4 val;
  System::setsockopt( sock, SOL_SOCKET, SO_KEEPALIVE, &(val = 1), sizeof(val) );
  System::setsockopt( sock, SOL_SOCKET, SO_REUSEADDR, &(val = 1), sizeof(val) );
  System::setsockopt( sock, SOL_SOCKET, SO_DONTROUTE, &(val = 0), sizeof(val) );
#if 0
  System::setsockopt( sock, SOL_SOCKET, SO_SNDBUF,    &(val = send_size), sizeof(val) );
  System::setsockopt( sock, SOL_SOCKET, SO_RCVBUF,    &(val = recv_size), sizeof(val) );
#endif  
  struct linger ling = { 0, 0 };
  System::setsockopt( sock, SOL_SOCKET, SO_LINGER,    &ling,      sizeof(ling) );
  //  System::txtsockopt( sock, cout << "after" << endl );

  send(login + "\n");
  send(passwd + "\n");

  self = login;
  service = s;

  cout << "SERVICE= " << service_login() << endl;

}

Client::~Client() {}

void Client::send( ostrstream& os )
{
  send( os.str(), os.pcount() );
  os.rdbuf()->freeze( false );
}

void Client::send(const ccptr mssg )
{
  sint4 len = strlen( mssg );
  send( mssg, len );
}
		      
void Client::send(const String &s)
{
  send(s.c_str());
}

void Client::send(const ccptr mssg, sint4 len )
{
  String msg( mssg, len );
  cout << "CLIENT: SEND " << msg;
  
  send_buff.extend( len );
  strncpy( send_buff.end(), mssg, len );
  send_buff.update( len );
}

bool Client::read_service_on(const Client &client, const vector<String>& v )
{
  if ( v[0] != ": + " + client.service_login()) return false;
  cout << "CLIENT: SERVICE_ON" << endl;
  return true;
}

bool Client::read_service_off(const Client &client, const vector<String>& v )
{
  if ( v[0] != ": - " + client.service_login() ) return false;
  cout << "CLIENT: SERVICE_OFF" << endl;
  return true;
}

void Client::close_input()
{
  input = false;
}

void Client::io()
{
  fd_set fd_s; FD_ZERO( &fd_s );
  fd_set fd_r; FD_ZERO( &fd_r );
  fd_set fd_e; FD_ZERO( &fd_e );

  if (send_buff.size() > 0) FD_SET(sock, &fd_s);
  FD_SET(sock, &fd_r);
  FD_SET(sock, &fd_e);

  if (input) FD_SET(0, &fd_r); // cin

  struct timeval tv = { 0, 0 };

  sint4 no = System::select_desc(1024, fd_r, fd_s, fd_e, &tv );

  if (no <= 0 && recv_buff.size() == 0) return;

  if (input && type == UNDEF) {

    if (FD_ISSET(0, &fd_r)) {
      
      System::fd_read(0, stdio_buff);
      String msg(stdio_buff.begin(), stdio_buff.size()-1);
      internal.msg = msg;
      type = INTERNAL_CMD;
#if DEBUG
      cout << ">>> INTERNAL_CMD: " << msg << endl;
#endif
      stdio_buff.erase(stdio_buff.size());
      return;
    }
  }
  
  if (FD_ISSET(sock, &fd_s)) System::sock_write(sock, send_buff);

  if (FD_ISSET(sock, &fd_r)) {
    bool ok = System::sock_read(sock, recv_buff, 0);
    if (!ok) exit(-1);
  }

  if (FD_ISSET(sock, &fd_e)) {
    bool ok = System::sock_read(sock, recv_buff, 1);
    if (!ok) exit(-1);
  }

  if (recv_buff.size() == 0) return;
  
  for ( ; type == UNDEF ; ) {
    cptr it = recv_buff.find( '\n' ); if (it == 0) break;
    sint4 no = it - recv_buff.begin(), mo = no;
    if ( mo > 0 && it[-1] == '\r' )  --mo;

    String line( recv_buff.begin(), mo ); recv_buff.erase( no + 1);
    if (line.empty()) continue;
      
    if (line == "READY" || line == "ALERT") {

      if (mssg.size() == 0) continue;

      // delete bells in first line

      uint4 a;
      if (mssg[0].length() > 0 && (a=mssg[0].find('\a')) < mssg[0].length()) {
	mssg[0].erase(a, a+1); cout << "\a" << endl;
      }
      
      // delete leading "|"s
      
      uint4 i;

      for (i=1; i < mssg.size(); i++) {
	if (mssg[i].length() > 0 && mssg[i][0] == '|') mssg[i].erase(0, 1);
      }


#if DEBUG > 1

      FOR (i, mssg.size()) {
	cout << ">>> READ: " << mssg[i] << endl;
      }
	
#endif
      

      // -------------- JOIN --------------
      
      if (join.read(*this, mssg)) {
	type = JOIN;

	// add game to list

	list< ClientPTR<ClientGame> >::iterator it;
	if ((it=find_game(join.id)) == games.end()) {

	  // append game

#if DEBUG
	  cout << ">>> NEW GAME " << join.game->id << endl;
#endif
	  
	  join.new_game = true;
	  games.push_back(join.game);

	} else {

#if DEBUG
	  cout << ">>> REPLACE GAME " << join.game->id << endl;
#endif
	  join.new_game = false;
	  (*it) = join.game;
	}
	
	continue;
      }

      // -------------- UPDATE -------------
      
      if (update.read(*this, mssg)) {
	type = UPDATE;

	// append move to game
	
	list< ClientPTR<ClientGame> >::iterator it;
	if ((it=find_game(update.id)) == games.end()) {

	  // game not in list -> error

	  type = UNDEF;
	  cerr << "Client: update: game not found" << endl;

	} else {

	  // append move to game

#if DEBUG
	  cout << ">>> UPDATE " << update.id << " ";
	  cout << update.move->to_string(); cout << endl;
#endif
	  
	  ostrstream os;
	  if (!(*it)->play(os, (*it)->name_to_move(), *update.move)) {
	    type = UNDEF;
	    cerr << "Client: update: illegal move" << endl;
	    
	  } else continue;
	}
      }

      // -------------- END -------------

      if (end.read(*this, mssg)) {
	type = END;

	// save game in end class and remove it from list

	list< ClientPTR<ClientGame> >::iterator it;
	if ((it=find_game(end.id)) == games.end()) {

	  // game not in list

	  end.game_in_list = false;

	} else {

	  // in list -> save game and remove it

#if DEBUG
	  cout << ">>> REMOVE GAME " << end.id << endl;
#endif
	  end.game_in_list = true;
	  end.game = (*it);
	  games.erase(it);
	}
	
	continue;
      }

      if (m_req .read(*this, mssg)) { type = MATCH_REQ; continue; }
      if (start .read(*this, mssg)) { type = START;     continue; }
      if (a_req .read(*this, mssg)) { type = ABORT_REQ; continue; }
      if (s_req .read(*this, mssg)) { type = SCORE_REQ; continue; }
      if (u_req .read(*this, mssg)) { type = UNDO_REQ;  continue; }
      if (read_service_on (*this, mssg)) { type = SERVICE_ON;  continue; }
      if (read_service_off(*this, mssg)) { type = SERVICE_OFF; continue; }

      // -------------- NON-SERVICE TELL --------------

      if (tell.read(*this, mssg)) {

	list<String>::iterator it = super_users.begin();
	list<String>::iterator en = super_users.end();  
	
	for (; it != en; it++) if ((*it) == tell.sender) break;

	if (it != en) {

	  // super user tell

	  if (tell.msg.size() == 0) continue;

	  vector<String> vec;
	  String::parse(tell.msg[0], vec);

	  if (vec[0] == "+" && vec.size() >= 2 && vec[1].length() > 0) {

	    // + login : add a super-user
	  
	    add_super_user(vec[1]);
	    String msg = "tell ";
	    msg += tell.sender;
	    msg += " ";
	    msg += vec[1];
	    msg += " added\n";
	    send(msg);
	    reset();

	  } else if (vec[0] == "-" && vec.size() >= 2 && vec[1].length() > 0) {

	    // - login : remove a super-user

	    remove_super_user(vec[1]);
	    String msg = "tell ";
	    msg += tell.sender;
	    msg += " ";
	    msg += vec[1];
	    msg += " removed\n";
	    send(msg);
	    reset();

	  } else if (vec[0] == "?") {

	    // ? : show super-user list
	    
	    String msg = "tell ";
	    msg += tell.sender;
	    msg += " super-user(s): ";

	    list<String>::iterator it = super_users.begin();
	    list<String>::iterator en = super_users.end();  
	    
	    for (; it != en; it++) {
	      msg += (*it);
	      msg += " ";
	    }

	    msg += '\n';
	    send(msg);
	    reset();

	  } else {
	    
	    type = REMOTE_CMD;
	    remote.sender = tell.sender;
	    remote.msg = tell.msg;
	    continue;
	  }
	    
	} else {

	  type = TELL;
	  continue;
	}
      }
      
      // unknown message print out & move on
      mssg.erase(mssg.begin(), mssg.end());

    } else {
      mssg.push_back(line);
    }
  }

}

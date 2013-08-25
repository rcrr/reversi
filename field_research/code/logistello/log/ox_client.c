// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* 
   ox client/server interface
*/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sys/types.h>
#include <sys/time.h> 
#include <sys/resource.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define MY_NAME "black"
#define MY_PASS "pass"


#ifndef NO_PROTO
struct hostent *gethostbyname( char* );
int bind( int, struct sockaddr*, int );
int listen( int, int );
int connect( int, void*, int );
int accept( int, struct sockaddr*, int* );
void bcopy( void*, void*, int );
void bzero( void*, int );
int shutdown( int, int );
int close( int );
void perror( char* );
int inet_addr( char* );
int setsockopt( int, int, int, void*, int );
int socket( int, int, int );
int send( int, char*, int, int );
int recv( int, char*, int, int );
int select( int, fd_set*, fd_set*, fd_set*, struct timeval* );
void usleep( int );
#endif

#define OX_MAIN

#include "ox_client.h"
#include "sig.i"

typedef enum { C_NONE, C_MOVES, C_HISTORY, C_BOARD, C_WAIT } t_client_state;
static t_client_state client_state  = C_NONE;

static int            server_socket = -1;

/* server/client story */

static int f_setsockopt( int s, int cmd, int opt )
{
int ok;

   ok = setsockopt( s, SOL_SOCKET, cmd, &opt, sizeof(opt) );
   if ( ok < 0 ) { 
      perror( "setsockopt" ); 
      return -1;
   }
   return 0;
}

static int f_fcntl( int f, int flags ) 
{ 
int val;

   val  = fcntl(f, F_GETFL, 0);
   val |= flags; 
   if ( fcntl( f, F_SETFL, val ) == -1 ) { 
      perror( "fcntl" ); 
      return -1;
   }
   return 0;
}

static int accept_player( void )
{
struct sockaddr_in sa;
int                s, i;

   i = sizeof(sa);
   s = accept( server_socket, (struct sockaddr *) &sa, &i);

   if ( s == -1 ) { 
      perror( "accept" ); 
      return -1;
   }

   f_setsockopt( s, SO_DEBUG, 0 );
   f_setsockopt( s, SO_REUSEADDR, 1 );
   f_setsockopt( s, SO_KEEPALIVE, 1 );
   f_setsockopt( s, SO_DONTROUTE, 0 );
   f_setsockopt( s, SO_LINGER, 0 );
   f_setsockopt( s, SO_BROADCAST, 0 );
   f_setsockopt( s, SO_OOBINLINE, 0 );
   f_setsockopt( s, SO_USELOOPBACK, 1 );

   f_fcntl( s, O_NONBLOCK );

   i = 0; 
   if ( player[i].socket != -1 ) i = 1;
   if ( player[i].socket != -1 ) {
      shutdown( s, 2 );
      close( s );
   }
   player[i].socket = s;
   player[i].sig    = R_LOGIN;
   return 0;
}

int init_server( int port )              /* 0 = ok, else fail */
{
struct sockaddr_in sa;
int                i;

   server_socket = socket( PF_INET, SOCK_STREAM, 0 );
   if (server_socket < 0) {
     perror( "socket" );
     return -1;
   }

   if ( f_setsockopt( server_socket, SO_DEBUG, 0 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_REUSEADDR, 1 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_KEEPALIVE, 1 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_DONTROUTE, 0 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_LINGER, 0 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_BROADCAST, 0 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_OOBINLINE, 0 ) ) return -1;
   if ( f_setsockopt( server_socket, SO_USELOOPBACK, 1 ) ) return -1;

   bzero((char *) &sa, sizeof(sa)) ;
   sa.sin_family      = AF_INET ;
   sa.sin_addr.s_addr = htonl(INADDR_ANY) ;
   sa.sin_port        = htons(port) ;
   i = bind( server_socket, (struct sockaddr *) &sa, sizeof(sa) );
   if ( i < 0 ) {
      perror( "bind" );
      return -1;
   }

   f_fcntl( server_socket, O_NONBLOCK );

   i = listen(server_socket, 2 );
   if ( i < 0 ) {
      perror( "listen" );
      return -1;
   }
   for ( i = 0; i < 2; i++ ) {
      player[i].socket = -1;
      strcpy( player[i].name, "<none>" );
      player[i].sig    = R_NONE;
      player[i].i.len  = 0;
      player[i].i.buf  = NULL;
      player[i].o.len  = 0;
      player[i].o.buf  = NULL;
   }
   return 0;
}

int init_client( char *host, int port )
{
struct sockaddr_in sa;
struct hostent *hp;
long   addr;

   server_socket = -1;

   client_state  = C_NONE;
   client.socket = -1;
   strcpy( client.name, MY_NAME );
   client.sig    = R_NONE;
   client.i.len  = 0;
   client.i.buf  = NULL;
   client.o.len  = 0;
   client.o.buf  = NULL;

   bzero(&sa, sizeof(sa)) ;
   if ((addr = inet_addr(host)) != -1) {
       bcopy((char*) &addr, (char*) &sa.sin_addr, sizeof(addr)) ; 
       sa.sin_family = AF_INET ;
   } else {
       if ((hp = gethostbyname(host)) == NULL) {
	  fprintf(stdout, "client unknown host %s\n", host) ;
	  return -1;
       }
       bcopy((char*) hp->h_addr, (char*) &sa.sin_addr, hp->h_length) ;
       sa.sin_family = hp->h_addrtype ;
   }

   sa.sin_port = htons((u_short) port);

   client.socket = socket(sa.sin_family, SOCK_STREAM, 0);
   if ( client.socket < 0 ) {
       perror("socket") ;
       return -1;
   }
   if (connect(client.socket, &sa, sizeof(sa)) < 0) {
       perror("connect");
       return -1;
   }  
   send_string( &client, MY_NAME );
   send_string( &client, "\n" );
   send_string( &client, MY_PASS );
   send_string( &client, "\n" );
   return 0;
}

/* game story */

void game_continue( t_game *g )
{
   g->history.no = g->history.current;
}

int  game_backward( t_game *g ) /* 0 = ok, else fail */
{
   if ( g->history.current == 0 ) return -1;
   g->history.current--;
   g->current = g->start;
   if ( replay2current( g ) ) return -1;
   return 0;
}

int  game_forward ( t_game *g )
{
   if ( g->history.current >= g->history.no ) return -1;
   g->history.current++;
   if ( replay2current( g ) ) return -1;
   return 0;
}

static int chk_dir( t_sint1 *b, int c, int oc, int d ) /* 0 - ok, else fail */
{
   if ( *(b+=d) == oc ) {
      while ( *(b+=d) == oc ); if ( *b == c ) return 0;
   }
   return -1;
}

static int chk_move( t_game *g, t_sint1 c, t_sint1 m ) /* 0 - ok, else fail */
{
t_sint1 *p, oc;

   p = &(g->current.pos[m]);
   if ( *p != 0 ) return -1;
   oc = -c;

   if (! chk_dir( p, c, oc,   1 ) ) return 0;
   if (! chk_dir( p, c, oc,  -1 ) ) return 0;
   if (! chk_dir( p, c, oc,   9 ) ) return 0;
   if (! chk_dir( p, c, oc,  -9 ) ) return 0;
   if (! chk_dir( p, c, oc,  10 ) ) return 0;
   if (! chk_dir( p, c, oc, -10 ) ) return 0;
   if (! chk_dir( p, c, oc,  11 ) ) return 0;
   if (! chk_dir( p, c, oc, -11 ) ) return 0;
   return -1;
}

static void play_dir( t_sint1 *b, int c, int oc, int d )
{
   if ( *(b+=d) == oc ) {
      while ( *(b+=d) == oc );
      if ( *b == c ) {
	 b-=d; do *b = c; while ( *(b-=d) == oc );
      }
   }
}

static void play_move( t_game *g, t_sint1 c, t_sint1 m )
{
t_sint1 *p, oc;

   p = &(g->current.pos[m]);
   oc = -c;

   play_dir( p, c, oc,   1 );
   play_dir( p, c, oc,  -1 );
   play_dir( p, c, oc,   9 );
   play_dir( p, c, oc,  -9 );
   play_dir( p, c, oc,  10 );
   play_dir( p, c, oc, -10 );
   play_dir( p, c, oc,  11 );
   play_dir( p, c, oc, -11 );
   g->current.pos[m] = c;
}

static void prt_move( char *s, t_sint1 m );

static int move_list( t_game *g, int c, t_sint1 *move )
{
int i,n;

   for (n = i = 0; i < 100; i++) 
      if (! chk_move( g, c, i ) ) {
	 n++; *(move++) = i;
      }
   return n;
}

static void count_disc( t_game *g )
{
int i, b, w;

   for ( i = 0, b = 0, w = 0; i < d_pos_no; i++ ) 
     switch ( g->current.pos[i] ) {
     case d_black : b++; break;
     case d_white : w++; break;
     default: break;
     }
   g->current.b_disc = b;
   g->current.w_disc = w;
}

static void list_moves( t_game *g )
{
   g->current.b_move = move_list( g, d_black, g->current.b_list );
   g->current.w_move = move_list( g, d_white, g->current.w_list );
}

static void dec_timer( t_game *g, t_real time )
{
   if ( g->history.current & 1 ) g->current.b_time -= time;
   else                          g->current.w_time -= time;
}

static void start2current( t_game *g )
{
  bcopy( &(g->start), &(g->current), sizeof(g->start) );
}

static void current2start( t_game *g )
{
  bcopy( &(g->current), &(g->start), sizeof(g->start) );
}

void init_game( t_game *g, t_real time )
{
#define B d_black
#define W d_white
#define E 0
#define R (-2)

static t_pos i_pos = {
  R,R,R,R,R,R,R,R,R,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,W,B,E,E,E,R,
  R,E,E,E,B,W,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,R,R,R,R,R,R,R,R,R,
};

  bcopy( i_pos, g->current.pos, sizeof(i_pos) );

  g->no = 1;

  g->current.b_time = time;
  g->current.w_time = time;

  count_disc( g );
  list_moves( g );

  current2start( g );

  g->history.no = 
  g->history.current = 0;

  strcpy( g->black, "black" );
  strcpy( g->white, "white" );
}

void clear_game( t_game *g )
{
#define E 0
#define R (-2)

static t_pos i_pos = {
  R,R,R,R,R,R,R,R,R,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,E,E,E,E,E,E,E,E,R,
  R,R,R,R,R,R,R,R,R,R,
};

  bcopy( i_pos, g->current.pos, sizeof(i_pos) );
}

static void add_history( t_game *g, t_move *m )
{
   g->history.list[ g->history.no ] = *m;
   g->history.no ++;
   g->history.current = g->history.no;
}

int new_move( t_game *g, t_move *m ) /* 0 - ok, else fail */
{
int     n;
t_sint1 c;

   c = ( g->history.current & 1 ) ? d_black : d_white;
   dec_timer( g, m->time );

   n = (g->history.current & 1) ? g->current.b_move : g->current.w_move;
   if ( n == 0 && m->yx != d_move_pass ) return -1;
   if ( n != 0 && m->yx == d_move_pass ) return -1;
   if ( n == 0 ) goto UPDATE;

   if ( chk_move( g, c, m->yx ) ) return -1;
       play_move( g, c, m->yx );
   count_disc( g );
   list_moves( g );

 UPDATE: ;

   add_history( g, m );   
   return 0;
}

int replay2current( t_game *g ) /* 0 - ok, else fail */
{
int     i, n, c;
t_move *m;

   start2current( g );
   c = ( g->history.current & 1 ) ? d_black : d_white;
   
   for ( i = 0; i < g->history.current; i++ ) {
      m = &(g->history.list[i]);
      
      dec_timer( g, m->time );

      n = (g->history.current & 1) ? g->current.b_move : g->current.w_move;
      if (! ( n == 0 && m->yx == d_move_pass ) ) return -1;
      if ( n == 0 ) goto UPDATE;

      if ( chk_move( g, c, m->yx ) ) return -1;
	  play_move( g, c, m->yx );
      count_disc( g );
      list_moves( g );

    UPDATE: ;

      add_history( g, m );   
   }
   return 0;
}

int  game_load    ( t_game *g, char *file );
int  game_save    ( t_game *g, char *file );

/* i/o story */

static void *f_realloc( void *p, int n )
{
void *np;

   if ( p == NULL ) np = malloc ( n );
   else             np = realloc( p, n );
   if ( np == NULL ) {
      perror( "(ma)realloc" );
      exit(1);
   }
   return np;
}

static void f_bufcat_CrLf2Lf( t_buff *b, char *s )
{
char *p1, *p2;
int   n;

   n = strlen( s );
   b->buf = f_realloc( b->buf, b->len + n + 1 );
   p1 = &(b->buf[b->len]);
   p2 = s; 
   for (n = 0; (*p1 = *p2) ;) if ( *p2++ != '\r' ) { p1++; n++; }
   b->len += n;
}

static void f_bufcat_Lf2CrLf( t_buff *b, char *s )
{
char *p1, *p2;
int   n;

   n = strlen( s );
   b->buf = f_realloc( b->buf, b->len + 2*n + 1 );
   p1 = &(b->buf[b->len]);
   p2 = s; 
   for (n = 0; (*p1++ = *p2) ; n++) if ( *p2++ == '\n' ) { *p1++ = '\r'; n++; }
   b->len += n;
}

static void f_bufdel( t_buff *b, int n )
{
   if ( b->len != n ) {
      bcopy( &(b->buf[n]), b->buf, b->len - n + 1 );
      b->len -= n;
   } else { 
     free( b->buf ); 
     b->buf = NULL; 
     b->len = 0;
   }
}

char *get_line( t_io *u )
{
int   i;
char *p, *c;

   if ( u->i.len == 0 ) return NULL;

   for ( i = 0, p = u->i.buf; *p != '\n' && *p != 0; i++, p++ );

   if ( *p == 0 ) return NULL;

   c = f_realloc( NULL, i+1 );
   bcopy( u->i.buf, c, i );
   c[i] = 0;
   f_bufdel( &(u->i), i+1 );

   return c;
}

void send_string ( t_io *u, char *s )
{
   if ( u->socket == -1 ) return;
   f_bufcat_Lf2CrLf( &(u->o), s );
}

void send_move  ( t_io *u, t_move *m )
{
char b[80], yx[8];;

   prt_move( yx, m->yx );

   send_string( u, "\n" );
   sprintf( b, "%s %+6.1f %+7.2f\n", yx, m->time, m->value );
   send_string( u, b );
   send_string( u, "\n" );
}

void send_start  ( t_io *u, t_game *g )
{
char b[80];

   send_string( u, "\n" );
   sprintf( b, "(start of game %d, %s vs. %s)\n", g->no,g->black,g->white);   
   send_string( u, b );
   send_string( u, "\n" );
}

void send_end  ( t_io *u, t_game *g )
{
char b[80];

   send_string( u, "\n" );
   sprintf( b, "(end of game %d, %s vs. %s)\n", g->no,g->black,g->white);   
   send_string( u, b );
   send_string( u, "\n" );
}

void send_moves  ( t_io *u, t_game *g )
{
char b[80];
int  m[100], y,x, yx;


   send_string( u, "\n" );
   sprintf( b, "(moves of game %d, %s vs. %s)\n", g->no, g->black, g->white ); 
   send_string( u, b );
   send_string( u, "\n" );
   send_string( u, "-> ##\n" );
   send_string( u, "\n" );

   for ( x = 0; x < 100; x++ )           m[x] = 0;
   for ( x = 0; x < g->history.no; x++ ) m[g->history.list[x].yx] = x+1;

   send_string( u, "   A  B  C  D  E  F  G  H\n" );
   for ( y = 0; y < 8; y++ ) {
      sprintf( b, "%d |", y+1 ); send_string( u, b );
      for ( x = 0; x < 8; x++ ) {
         yx = (y+1)*10+(x+1);
	 switch( m[ yx ] ) {
	 case 0 :  
	    switch ( g->start.pos[ yx ] ) {
	    case d_black : send_string( u, "##|" ); break;
	    case d_white : send_string( u, "()|" ); break;
	    default      : send_string( u, "  |" ); break;
	    }
	    break;
	 default : sprintf( b, "%2d|", m[yx] );
	           send_string( u, b );
	 }
      }
      sprintf( b, "%2d\n", y+1 ); send_string( u, b );
   }
   send_string( u, "   A  B  C  D  E  F  G  H\n" );
}

static void prt_move( char *s, t_sint1 m )
{
   if (m == d_move_pass) sprintf( s, "pa" );
   else sprintf( s, "%c%c", m % 10 + 'a' - 1, m / 10 + '0' ); 
}

void send_history( t_io *u, t_game *g )
{
char    b[80], yx[8];
int     i;
t_move *m;

   send_string( u, "\n" );
   sprintf( b, "(history of game %d, %s vs. %s)\n", g->no, g->black, g->white ); 
   send_string( u, b );
   send_string( u, "\n" );

   sprintf( b, "%d moves played\n", g->history.no );
   send_string( u, b );
   send_string( u, "\n" );
   for ( i = 0; i < g->history.no; i++ ) {
      m = &(g->history.list[i]);
      prt_move( yx, m->yx );
      sprintf( b, "%2d. %s (%6.1f) %7.2f", i+1, yx, m->time, m->value );
      if ( i & 1 ) send_string( u, "\t\t" );
      send_string( u, b );
      if ( i & 1 ) send_string( u, "\n" );
   }
   if ( i & 1 ) send_string( u, "\n" );
}

static void prt_time( char *b, t_real tt )
{
int t;

   t = nint(tt);
   if ( t < 0 ) sprintf(b,"--:--:--"); else
   sprintf(b,"%02d:%02d:%02d",t/(60*60), (t%(60*60)/60), t%60);
}

void send_board  ( t_io *u, t_game *g )
{
char    b[80], tb[9], tw[9], ms[8], mm[32];
int     y,x,yx;
t_move *m;

   send_string( u, "\n" );
   sprintf( b, "(board of game %d, %s vs. %s)\n", g->no, g->black, g->white ); 
   send_string( u, b );
   send_string( u, "\n" );

   send_string( u, "   A  B  C  D  E  F  G  H\n" );
   for ( y = 0; y < 8; y++ ) {
      sprintf( b, "%d |", y+1 ); send_string( u, b );
      for ( x = 0; x < 8; x++ ) {
         yx = (y+1)*10+(x+1);
	 switch ( g->current.pos[ yx ] ) {
	 case d_black : send_string( u, "##|" ); break;
	 case d_white : send_string( u, "()|" ); break;
	 default      : send_string( u, "  |" ); break;
	 }
      }
      sprintf( b, "%2d", y+1 ); send_string( u, b );
      switch ( y+1 ) {
      case 1: send_string( u, "                  (Black)         (White)" );
	      break;
      case 2: prt_time( tb, g->start.b_time );
	      prt_time( tw, g->start.w_time );
	      sprintf( b, "      Start:      %s        %s", tb, tw );
	      send_string( u, b );
              break;
      case 3: prt_time( tb, g->current.b_time );
	      prt_time( tw, g->current.w_time );
	      sprintf( b, "      Clock:      %s        %s", tb, tw );
	      send_string( u, b );
              break;
      case 4: sprintf( b, "     Disc(s):        %2d              %2d", 
		       g->current.b_disc, g->current.w_disc );
	      send_string( u, b );
	      break;
      case 5: sprintf( b, "     Move(s):        %2d              %2d",
		       g->current.b_move, g->current.w_move );
	      send_string( u, b );
	      break;
      case 6: break;
      case 7: if ( g->history.no == 0 ) break;
	      m = &(g->history.list[g->history.no - 1]);
              prt_move( ms, m->yx );
	      sprintf( mm, "%2d. %s (%6.1f) %7.2f   ", 
		      g->history.no, ms, m->time, m->value );
	      sprintf( b, "     (%s) %s", 
		       (g->history.current & 1) ? "White" : "Black", mm);
	      send_string( u, b );
	      break;
      case 8:
	      sprintf( b, "     (%s) %2d.", 
		       (g->history.current & 1) ? "Black" : "White", 
		       g->history.no+1 );
	      send_string( u, b );
	      break;
      }
      send_string( u, "\n" );
   }
   send_string( u, "   A  B  C  D  E  F  G  H\n" );
}

void send_game   ( t_io *u, t_game *g )
{
   send_moves  ( u, g );
   send_history( u, g ); 
   send_board  ( u, g );
}

/* decoder story */

static void parse_moves_line( int y, char *l, t_game *g )
{
int     x, yx, n;
t_move *m;

   if ( y == 0 ) {
      g->history.no = 
      g->history.current = 0;
      clear_game( g );
   }
   for ( x = 0; x < 8; x++, l += 3 ) {
      yx = (y+1) * 10 + (x+1);
      if ( *l == '(' ) { g->current.pos[yx] = d_white; continue; }
      if ( *l == '#' ) { g->current.pos[yx] = d_black; continue; }
      if ( l[0] == ' ' )
	 if ( l[1] == ' ' ) continue;
         else n = l[1] - '0';
      else n = 10 * (l[0] - '0') + (l[1] - '0');
      m = &(g->history.list[n-1]);
      m->yx    = yx;
      m->time  = 0.0;
      m->value = 0.0;
      if ( g->history.no < n ) {
	 g->history.no = 
	 g->history.current = n;
      }
   }
}

void recv_move ( t_io *u, char *l, t_move *m ) 
{
int yx;

   while ( *l == ' ' ) l++; if (! *l ) return;
   if ( *l >= 'A' && *l <= 'H' ) yx = *l - 'A' + 1; else
   if ( *l >= 'a' && *l <= 'h' ) yx = *l - 'a' + 1; else return;
   l++;
   if ( *l >= '1' && *l <= '8' ) {
      yx += 10 * (*l - '0');
      m->yx    = yx;
      m->time  = 0.0;
      m->value = 0.0;
      u->sig = R_MOVE;
      client_state = C_WAIT;
   } else return;

   while ( *(++l) == ' ' ); if (! *l ) return;
   m->value = atof( l ); 
   if ( m->value < -999.9 ) m->value = -999.9;
   if ( m->value >  999.9 ) m->value =  999.9;

   while ( *(++l) != ' ' ) l++; if (! *l ) return;
   while ( *(++l) == ' ' ) l++; if (! *l ) return;
   m->time = atof( l ); if ( m->time < 0.0 ) m->time = 0.0;
}

void recv_start( t_io *u, char *l )
{
static char *PATTERN = "(start of game ";

int     no;
t_name  black, white;

  if (! strncmp(l, PATTERN, strlen(PATTERN)) ) {
     sscanf( &(l[15]), "%d, %s vs. %s)", &no, black, white );
     if (! strcmp( u->name, black ) ||
	 ! strcmp( u->name, white ) ) {
	u->sig = R_START;
	client_state = C_WAIT;
	game.no = no;
	strcpy( game.black, black );
	strcpy( game.white, white );
     }
  }
}

void recv_end( t_io *u, char *l )
{
static char *PATTERN = "(end of game ";

int     no;
t_name  black, white;

  if (! strncmp(l, PATTERN, strlen(PATTERN)) ) {
     sscanf( &(l[13]), "%d, %s vs. %s)", &no, black, white );
     if (! strcmp( u->name, black ) ||
	 ! strcmp( u->name, white ) ) {
	u->sig = R_END;
	client_state = C_WAIT;
	game.no = no;
	strcpy( game.black, black );
	strcpy( game.white, white );
     }
  }
}

void recv_moves( t_io *u, char *l, t_game *g )
{
static char *PATTERN = "(moves of game ";
static int   line = 0;

int     no;
t_name  black, white;

   switch ( line ) {
   case 0 :
     if (! strncmp(l, PATTERN, strlen(PATTERN)) ) {
        sscanf( &(l[15]), "%d, %s vs. %s)", &no, black, white );
        if (! strcmp( u->name, black ) ||
	    ! strcmp( u->name, white ) ) {
	   client_state = C_MOVES;
	   line = 1;
	   g->no = no;
	   strcpy( g->black, black );
	   strcpy( g->white, white );
	}
     }
     break;
   case 1 :
   case 2 :
   case 3 :
   case 4 : line++; break;
   case 5 : 
   case 6 : 
   case 7 : 
   case 8 : 
   case 9 : 
   case 10: 
   case 11: 
   case 12: line++; parse_moves_line( line - 6, &(l[3]), g ); break;
   case 13: client_state = C_HISTORY; 
            line = 0; 
	    g->current.b_time = 0;
	    g->current.w_time = 0;
            count_disc( g );
            list_moves( g );
            current2start( g ); 
            break;
   }
}

void recv_history( t_io *u, char *l, t_game *g )
{
static char *PATTERN = "(history of game ";
static int   line = 0;

int     no, y;
t_name  black, white;
char    x;
float   time, value;
t_move *m;

   switch ( line ) {
   case 0 :
     if (! strncmp(l, PATTERN, strlen(PATTERN)) ) {
        sscanf( &(l[17]), "%d, %s vs. %s)", &no, black, white );
        if (! strcmp( u->name, black ) ||
	    ! strcmp( u->name, white ) ) {
	   client_state = C_HISTORY;
	   line = 1;
	   g->no = no;
	   strcpy( g->black, black );
	   strcpy( g->white, white );
	}
     }
     break;
   case 1 : line++; break;
   case 2 : line++;
            sscanf( l, "%d", &no );
            g->history.no = no;
	    g->history.current = 0;
            break;
   case 3 : line++; break;
   case 4 : ODD: ; 
            if ( g->history.current >= g->history.no ) {
               client_state = C_BOARD;
	       line = 0;
	       break;
            }
            sscanf( l, "%d. %c%d (%6f) %7f", &no, &x,&y,&time,&value);
            g->history.current = no;
            m = &(g->history.list[no-1]);
            if ( x == 'p' ) m->yx = d_move_pass;
            else            m->yx = y * 10 + x - 'a' + 1;
            m->time  = time;
            m->value = value;
            if ( no & 1 ) { l = l + 25; goto ODD; }
            break;
   }
}
 
static void parse_board_line( int y, char *l, t_game *g )
{
int x, yx, h,m,s;

   if ( y == 0 ) clear_game( g );
   for ( x = 0; x < 8; x++, l += 3 ) {
      yx = (y+1) * 10 + (x+1);
      if ( *l == '(' ) g->current.pos[yx] = d_white; else
      if ( *l == '#' ) g->current.pos[yx] = d_black; else
	               g->current.pos[yx] = 0;
   }
   switch ( y ) {
   case 2 : sscanf( &(l[20]), "%d:%d:%d", &h, &m, &s );
            g->start.b_time = h * 3600.0 + m * 60.0 + s;
            sscanf( &(l[36]), "%d:%d:%d", &h, &m, &s );
            g->start.w_time = h * 3600.0 + m * 60.0 + s;
   case 3 : sscanf( &(l[20]), "%d:%d:%d", &h, &m, &s );
            g->current.b_time = h * 3600.0 + m * 60.0 + s;
            sscanf( &(l[36]), "%d:%d:%d", &h, &m, &s );
            g->current.w_time = h * 3600.0 + m * 60.0 + s;
   default: break;
   }
}
 
void recv_board( t_io *u, char *l, t_game *g )
{
static char *PATTERN = "(board of game ";
static int   line = 0;

int     no;
t_name  black, white;

   switch ( line ) {
   case 0 :
     if (! strncmp(l, PATTERN, strlen(PATTERN)) ) {
        sscanf( &(l[15]), "%d, %s vs. %s)", &no, black, white );
        if (! strcmp( u->name, black ) ||
	    ! strcmp( u->name, white ) ) {
	   client_state = C_BOARD;
	   line = 1;
	   g->no = no;
	   strcpy( g->black, black );
	   strcpy( g->white, white );
	}
     }
     break;
   case 1 : line++; break;
   case 2 : line++; break;
   case 3 : 
   case 4 : 
   case 5 : 
   case 6 : 
   case 7 : 
   case 8 : 
   case 9 : 
   case 10: line++; parse_board_line( line - 4, &(l[3]), g ); break;
   case 11: client_state = C_WAIT;
            u->sig = R_GAME;
            line = 0; 
            count_disc( g );
            list_moves( g );
            break;
   }
}

void recv_mssg( t_io *u, char *l )
{
   switch ( client_state ) {
   case C_NONE    : recv_move   ( u, l, &move ); 
                    recv_start  ( u, l );
                    recv_end    ( u, l );
   case C_MOVES   : recv_moves  ( u, l, &game ); break;
   case C_HISTORY : recv_history( u, l, &game ); break;
   case C_BOARD   : recv_board  ( u, l, &game ); break;
   default        : printf("illegal client_state %d\n", client_state );
                    exit(1);
   }
}

/* low level i/o */

static void close_io( t_io *io )
{
   shutdown( io->socket, 2 );
   close( io->socket );
   io->socket = -1;
   io->sig    = R_NONE;
   if ( io->i.buf != NULL ) free( io->i.buf );
   if ( io->o.buf != NULL ) free( io->o.buf );
   io->i.len = 0;
   io->i.buf = NULL;
   io->o.len = 0;
   io->o.buf = NULL;
}

static int read_io( t_io *io )
{
int        n;
char       b[16384];

   n  = recv( io->socket, b, 16384, 0 );
   if ( n <= 0 ) {
      perror( "recv" );
      close_io( io );
      return 0;
   }

   b[n] = 0;
   f_bufcat_CrLf2Lf( &(io->i), b );
   return 0;
}

static int write_io( t_io *io )
{
int n;

   if ( io->socket == -1 ) return -1;

   n = send( io->socket, io->o.buf, io->o.len, 0 );
   if ( n <= 0 ) {
      perror( "send" ); 
      close_io( io );
      return 0;
   }
   f_bufdel( &(io->o), n );
   return 0;
}

static void client_io( void )
{
char *l;

   for (;; ) {
      if ( client.sig != R_NONE ) return;
      l = get_line( &client );
      if ( l == NULL ) return;
      recv_mssg( &client, l );
      if ( client_state == C_NONE ) printf( "%s\n", l );
      if ( client_state == C_WAIT ) client_state = C_NONE;
      free( l );
   }
}

static void server_io( void )
{
int    i;
char  *l;

   for ( i = 0; i < 2; i++ ) {
      for (;;) {
	 l = get_line( &(player[i]) );
	 if ( player[i].sig == R_LOGIN ) {
	    if ( l != NULL ) {
	       strncpy( player[i].name, l, 8 );
	       player[i].name[0] = 0;
	       player[i].sig = R_NONE;
	    }
	 }
	 if ( l == NULL ) goto NEXT;
	 recv_mssg( &(player[i]), l );
	 if ( player[i].sig != R_NONE ) return;
	 free(l);
      }
    NEXT: ;
   }
}


int socket_io( void )
{
struct timeval timeout;
fd_set         r_fd, w_fd;
int            i, max;

 AGAIN: ;

   FD_ZERO( &r_fd );
   FD_ZERO( &w_fd );

   timeout.tv_sec  = 0;  
   timeout.tv_usec = 0;

   max = -1;

   if ( server_socket != -1 ) { /* we are server */
      FD_SET( server_socket, &r_fd );
      max = server_socket;

      for ( i = 0; i < 2; i++ ) {
	 if ( player[i].socket == -1 ) continue;
	 FD_SET( player[i].socket, &r_fd );
	 if ( max < player[i].socket ) max = player[i].socket;
	 if ( player[i].o.len != 0 ) {
	    FD_SET( player[i].socket, &w_fd );
	    if ( max < player[i].socket ) max = player[i].socket;
	 }
      }
   } else 
   if ( client.socket != -1 ) { /* we are client */
      FD_SET( client.socket, &r_fd );
      if ( max < client.socket ) max = client.socket;
      if ( client.o.len ) {
	 FD_SET( client.socket, &w_fd );
         if ( max < client.socket ) max = client.socket;
      }
   } else 
   return 0;
   
   i = select( max+1, &r_fd, &w_fd, NULL, &timeout );
   if ( i == 0 ) return 0;
   if ( i <  0 ) {
      if ( errno == EINTR ) { errno = 0; goto AGAIN; }
      perror( "select" ); 
      return -1;
   }
   
   if ( server_socket != -1 ) { /* we are server */
      if ( FD_ISSET( server_socket, &r_fd ) ) 
	 if ( accept_player() ) return -1;

      for ( i = 0; i < 2; i++ ) {
	 if ( player[i].socket == -1 ) continue;
	 if ( FD_ISSET( player[i].socket, &r_fd ) ) 
	    if ( read_io( &(player[i]) ) ) return -1;
	 if ( player[i].socket == -1 ) continue;
	 if ( player[i].o.len != 0 ) {
	    if ( FD_ISSET( player[i].socket, &w_fd ) )
	       if ( write_io( &(player[i]) ) ) return -1;
	 }
      }
      server_io();
   } else 
   if ( client.socket != -1 ) { /* we are client */
      if ( FD_ISSET( client.socket, &r_fd ) ) 
	 if ( read_io( &client ) ) return -1;
      if ( client.socket != -1 ) 
      if ( client.o.len ) {
	 if ( FD_ISSET( client.socket, &w_fd ) )
	    if ( write_io( &client ) ) return -1;
      }
      client_io();
   } 
   return 0;
}


static void play_sim( void )
{
t_move m;

    if ( game.history.current & 1 ) {
       if (! game.current.b_move ) m.yx = d_move_pass;
       else m.yx = game.current.b_list[ random() % game.current.b_move ];
    } else {
       if (! game.current.w_move ) m.yx    = d_move_pass;
       else m.yx = game.current.w_list[ random() % game.current.w_move ];
    }
    m.value = (float) (game.current.b_disc - game.current.w_disc) /
              (float) (game.current.b_disc + game.current.w_disc);
    m.time  = game.current.w_time / (62-game.history.no);
    new_move( &game, &m );
    send_game ( &(player[0]), &game );
    send_game ( &(player[1]), &game );
}

void main( int ac, char **av )
{
  sig_init_list();
  sig_init(SIGPIPE);
  sig_init(SIGURG);
  sig_init(SIGIO);

  if ( ac < 2 ) { if ( init_server(5001) ) exit(1); }
  else          { if ( init_client( av[1], atoi(av[2]) ) ) exit(1); }

 AGAIN: ;
  if ( ac < 2 ) {
     init_game( &game, 15*60.0 );

     send_start( &(player[0]), &game );
     send_game ( &(player[0]), &(game) );

     send_start( &(player[1]), &game );
     send_game ( &(player[1]), &(game) );

     for (;;) {
       usleep(100000);
       if (! game.current.b_move && 
           ! game.current.w_move ) {
	  send_end( &(player[0]), &game );
	  send_end( &(player[1]), &game );
	  goto AGAIN;
       }
       if ( socket_io() ) exit(1);
       if ( player[0].sig != R_NONE ) {
	  printf("%s: some signal received ... %d\n",
		  player[0].name, player[0].sig );
	  player[0].sig = R_NONE;
       }
       if ( player[1].sig != R_NONE ) {
	  printf("%s: some signal received ... %d\n",
		  player[1].name, player[1].sig );
	  player[1].sig = R_NONE;
       }
       play_sim();
     }
  } else {
     for (;;) {
       sleep(1);
       if ( socket_io() ) exit(1);
       if ( client.sig != R_NONE ) {
	  printf("ox: some signal received ... %d\n",client.sig );
	  client.sig = R_NONE;
       }
     }    
  }
}

/*
  ox [ port | -ios host port ] 
*/


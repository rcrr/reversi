// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* -----------------------------------------------------------------------
   client.c
   -------------------
   (C) Igor Durdanovic
   ----------------------------------------------------------------------- */

#include "main.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <malloc.h>

#define SIGNAL_HANDLER
#define CLIENT

#include "client.h"

#ifdef SIGNAL_HANDLER
#include "sig.i"
#endif

//#define toupper(C) (((C) >= 'a' && (C) <= 'z')? (C) - 'a' + 'A': (C))

#define B MY_BLACK
#define W MY_WHITE
#define N MY_EMPTY
#define R MY_EDGE

static int v_init_board[100] = 
{ 
  R, R, R, R, R, R, R, R, R, R,
  R, N, N, N, N, N, N, N, N, R,
  R, N, N, N, N, N, N, N, N, R,
  R, N, N, N, N, N, N, N, N, R,
  R, N, N, N, W, B, N, N, N, R,
  R, N, N, N, B, W, N, N, N, R,
  R, N, N, N, N, N, N, N, N, R,
  R, N, N, N, N, N, N, N, N, R,
  R, N, N, N, N, N, N, N, N, R,
  R, R, R, R, R, R, R, R, R, R 
};

#undef R
#undef N
#undef W
#undef B

typedef enum { 
  EMPTY, GET_BOARD, GET_MATCH, GET_MOVES, GET_WHO 
} t_client_state;

#define BUFF_LEN 8192
#define LINE_LEN 1024

static t_client_state v_client_state;
static int  ios_socket;

static char ios_buff[ BUFF_LEN ];
static int  ios_buff_idx;
static int  ios_buff_len;

static char ios_line[ LINE_LEN ];
static int  ios_line_disp;
static int  ios_line_idx;
static char ios_line_ready;

static int  ios_analyse;
static int  ios_input;
static int  ios_echo;

static int  ios_prompt;
static int  ios_login_now;
static char ios_login[9];

static void clean_line( void );

static void write_ios( char *m )
{
int l = strlen(m), 
    i = 0,
    n;
   
   for (; l > 0; ) {
      n = write( ios_socket, &(m[i]), l );
      if ( n <= 0 ) {
	 fprintf(stdout,"client write to <ios> failed\n");
	 exit(1);
      }
      i += n;
      l -= n;
   }
}

void ios_put_str( char *s )
{
   write_ios( s );
}

static void read_stdin( void )
{
int    n;
fd_set readfds;
char   buf[8192];  
struct timeval timeout;

   if (! ios_input  ) return;
   if (! ios_prompt ) return;

 READ: ;
   timeout.tv_sec  = 0;
   timeout.tv_usec = 0;

   FD_ZERO( &readfds );
   FD_SET ( 0, &readfds );
   n = select( 1, &readfds, NULL, NULL, &timeout );
   if (n < 0 && errno != EINTR) { perror("select"); exit(1); }
   if (n == 0) return;
   if (! FD_ISSET( 0, &readfds ) ) return;

   fgets( buf, 8192, stdin );

   if ( ios_login_now ) {
   int i;
      strncpy( ios_login, buf, 8 );
      for (i = 0; i < 8; i++) if ( ios_login[i] == '\n' ||
				   ios_login[i] == '\r' ) break;
      ios_login[i] = '\0';
      ios_login_now = 0;
      goto SEND;
   }

   /* internal stuff */
   if (! strncmp( buf, "analyse", strlen("analyse") ) &&
       ! ios_game_now ) { 
      ios_analyse = 1 - ios_analyse;
      fprintf(stdout,"[client]: analyse: %s\n", ios_analyse ? "ON" : "OFF");
      sprintf(buf,"\n");
   }
   if (! strncmp( buf, "input", strlen("input") ) ) {
       ios_input = 1 - ios_input;
       fprintf(stdout,"[client]: input %s\n", ios_input ? "ON" : "OFF");
       sprintf(buf,"\n");
    }
    if (! strncmp( buf, "echo", strlen("echo") ) ) {
       ios_echo = 1 - ios_echo;
       fprintf(stdout,"[client]: echo %s\n", ios_echo ? "ON" : "OFF");
       sprintf(buf,"\n");
    }

 SEND: ;
    write_ios( buf );
    goto READ;
}

static void read_ios( void )
{
  int    n;
  char   c;
  fd_set readfds;
  struct timeval timeout;

   read_stdin();

   if ( ios_line_ready ) /* there is already a line */
     return;

 READ: ;
   timeout.tv_sec  = 0;
   timeout.tv_usec = 0;

   FD_ZERO( &readfds );
   if ( ios_input ) FD_SET ( 0, &readfds );
   FD_SET ( ios_socket, &readfds );
   n = select( ios_socket+1, &readfds, NULL, NULL, &timeout );
   if (n < 0 && errno != EINTR) { perror("select"); exit(1); }
   if (n == 0 ) if ( ios_buff_len == 0) return; else goto LINE;
   if (! FD_ISSET( ios_socket, &readfds) ) 
     if ( ios_buff_len == 0) return; else goto LINE;

   n = read(ios_socket, &(ios_buff[ ios_buff_len ]), BUFF_LEN-ios_buff_len-1);
   if (n <= 0) {
      fprintf(stdout,"client <ios> closed connection\n");
      exit(0);
   }
   ios_buff_len += n;

 LINE: ;
   for (;;) {
      c = ios_line[ ios_line_idx++ ] = ios_buff[ ios_buff_idx++ ];
      ios_buff_len--;
      if ( c == '\r' ) ios_line_idx--;
      if ( c == '\n' ) { 
      READY: ;
	 ios_line[ --ios_line_idx ] = '\0';
	 ios_line_ready = 1;
         for ( n = 0; n < ios_buff_len; n++ )
	   ios_buff[n] = ios_buff[n + ios_buff_idx];
	 ios_buff_idx = 0;
	 return;
      }
      if ( ios_line_idx >= LINE_LEN ) {
	 fprintf(stdout,"client got line longer then %d chars\n",LINE_LEN);
	 ios_line_idx--;
         goto READY;
      }
      if ( ios_buff_len <= 0 ) {
	 ios_line[ ios_line_idx ] = '\0';
	 ios_buff_len = 0;
	 ios_buff_idx = 0;
	 goto READ;
      }
   }
}

static void clean_line( void )
{
  ios_line_ready = 0;
  ios_line_idx   = 0;
  ios_line_disp  = 0;
  ios_line[0]    = 0;
}

static void echo_line( void )
{
  if ( ! ios_echo ) return;
  if ( ios_line_idx <= ios_line_disp ) return;

  if ( ios_line_ready ) puts(&(ios_line[ ios_line_disp ]));
  else                 fputs(&(ios_line[ ios_line_disp ]),stdout); 
  fflush( stdout );

  ios_line_disp = ios_line_idx;
}

/* ---------------------------------------------------------------------- */


static void parse_MATCH( void )
{
  static int ml = 0;
  char dummy[16], color[16], komi;
  int i;
  float val; 

  v_ios.u.MATCH.stored = 0;
  v_ios.u.MATCH.id = -1;
  v_ios.u.MATCH.komi_on = false;
  v_ios.u.MATCH.rand_val = -1;


 AGAIN: ;
  read_ios();
  echo_line(); /* USER: leave comments out if you want echo */
  if (! ios_line_ready ) return;

  switch ( ml ) { 
  case 0 :
    if ( ios_line[13] == 's' ) {
      v_ios.u.MATCH.stored = 1;
      sscanf( &ios_line[21], "%s %s (%d)", dummy, v_ios.u.MATCH.op_name, &v_ios.u.MATCH.id);
      clean_line();
      v_ios.type = IOS_MATCH;
      v_client_state = EMPTY;
      return;
    }
    for ( i = 18; ios_line[i] != ')'; i++ ); i++;

    int a;

    if ((a=sscanf(&ios_line[i], "%d %d %d %s %s %d %d %d %s %c%f (%d)", 
	       &v_ios.u.MATCH.my_time,
	       &v_ios.u.MATCH.my_inc,
	       &v_ios.u.MATCH.my_def,
	       dummy,
	       v_ios.u.MATCH.op_name,
	       &v_ios.u.MATCH.op_time,
	       &v_ios.u.MATCH.op_inc,
	       &v_ios.u.MATCH.op_def,
	       color,
	       &komi,
               &val,
	       &v_ios.u.MATCH.id)) != 12) {
      
      printf("***** %d matches '%s'\n", a, &ios_line[i]);
      exit(20);
    }

    v_ios.u.MATCH.my_color  = (color[0] == 'w' ? MY_BLACK : MY_WHITE );
    v_ios.u.MATCH.rated     = (ios_line[13] == 'r');
    v_ios.u.MATCH.rnd_color = (color[5] == '*');
    if ( komi == 'K' ) {
      v_ios.u.MATCH.komi_on   = (color[5] == '?');
      v_ios.u.MATCH.komi_val  = val;
    } else if ( komi == 'R' ) {
      v_ios.u.MATCH.rand_val  = (int)(val+0.5);
    }


    ml = 1;
    clean_line();
    goto AGAIN;
  case 1 :
    sscanf( &(ios_line[15]), "%d .. %d %d %d .. %d",
	    &v_ios.u.MATCH.max_win,
	    &v_ios.u.MATCH.min_win,
	    &v_ios.u.MATCH.draw,
	    &v_ios.u.MATCH.min_loss,
	    &v_ios.u.MATCH.max_loss );
    ml = 0;
    clean_line();
    v_ios.type = IOS_MATCH;
    v_client_state = EMPTY;
    return;
  }
  printf("client/server bug %s.%d\n",__FILE__,__LINE__);
  exit(0);
}

/* -------------------- */

static void parse_komi( void )
{
  char ios[16];
  char board[16];
  char of[16];
  char game[16];
  int  no;
  char black[16];
  char white[16];

  sscanf(ios_line, "%s %s %s %s %d, (%s vs. %s) K%f R%d",
	 ios, board, of, game, &no, black, white, &v_ios.u.BOARD.komi, &v_ios.u.BOARD.rand);
}

static void parse_color( void )
{ 
   if ( ios_line[12] == IOS_BLACK ) v_ios.u.BOARD.turn_color = MY_BLACK;
   else                             v_ios.u.BOARD.turn_color = MY_WHITE;
}

static void parse_board_line( int y )
{
int i, x;

   for ( x = 1, i = 31; i < 46; i += 2, x++ ) 
     switch ( ios_line[i] ) {
       case IOS_BLACK : v_ios.u.BOARD.board[y*10+x] = MY_BLACK; break;
       case IOS_WHITE : v_ios.u.BOARD.board[y*10+x] = MY_WHITE; break;
       case IOS_EMPTY : v_ios.u.BOARD.board[y*10+x] = MY_EMPTY; break;
       default : fprintf(stdout,"client BOARD: %s\n", ios_line); 
	         exit(1);
     }
}

static void parse_time( void )
{
int time, def, inc;
   time = ((ios_line[11] - '0') * 10 + (ios_line[12] - '0')) * 60 * 60 +
          ((ios_line[14] - '0') * 10 + (ios_line[15] - '0')) * 60 +
          ((ios_line[17] - '0') * 10 + (ios_line[18] - '0'));
   time -= 10;
   def = atoi( &ios_line[20] );
   inc = atoi( &ios_line[24] );
   if ( v_ios.u.BOARD.turn_color == MY_WHITE ) {
     v_ios.u.BOARD.time_w = time; 
     v_ios.u.BOARD.def_w = def; 
     v_ios.u.BOARD.inc_w = inc;
   } else {
     v_ios.u.BOARD.time_b = time;
     v_ios.u.BOARD.def_b = def; 
     v_ios.u.BOARD.inc_b = inc;
   }
   time = ((ios_line[59] - '0') * 10 + (ios_line[60] - '0')) * 60 * 60 +
          ((ios_line[62] - '0') * 10 + (ios_line[63] - '0')) * 60 +
          ((ios_line[65] - '0') * 10 + (ios_line[66] - '0'));
   time -= 10;
   def = atoi( &ios_line[68] );
   inc = atoi( &ios_line[72] );
   if ( v_ios.u.BOARD.turn_color == MY_WHITE ) {
     v_ios.u.BOARD.time_b = time; 
     v_ios.u.BOARD.def_b = def; 
     v_ios.u.BOARD.inc_b = inc;
   } else {
     v_ios.u.BOARD.time_w = time;
     v_ios.u.BOARD.def_w = def; 
     v_ios.u.BOARD.inc_w = inc;
   }
}

static void parse_move( void )
{
   if ( toupper(ios_line[57]) == 'P' ) { 
      v_ios.u.BOARD.last_move = MY_PASS; return; 
   }
   v_ios.u.BOARD.last_move = toupper(ios_line[57]) - 'A' + 1 +
                               10 * (ios_line[58]  - '0');
   v_ios.u.BOARD.last_move_no = (ios_line[52] - '0') * 10 +
                                (ios_line[53] - '0');
}

static void parse_turn( void )
{ 
   if (ios_line[0] == 'Y') v_ios.u.BOARD.turn_my = 1;
   else                    v_ios.u.BOARD.turn_my = 0;
}

static void parse_value( void )
{
   v_ios.u.BOARD.last_value = atof( &(ios_line[70]) );
}


/* <ios> BOARD of GAME   1, (ooo vs. viper) */

static void parse_BOARD( void )
{
  static int bl = 0;

  parse_komi();

 AGAIN: ;
   read_ios();
   echo_line(); /* USER: leave comments out if you want echo */
   if (! ios_line_ready ) return;
   switch ( bl ) {
   case 0 : bl++;
     sprintf(v_ios.u.BOARD.who, "%s", &(ios_line[25]) );
     v_ios.u.BOARD.who[strlen(v_ios.u.BOARD.who)-1] = 0;
     memcpy(v_ios.u.BOARD.board, v_init_board, sizeof( v_init_board ));
     v_ios.u.BOARD.no = atoi(&(ios_line[19]));
     break;
   case 1 : bl++; break;
   case 2 : bl++; parse_color(); break;
   case 3 : bl++; parse_board_line( 1 ); break;
   case 4 : bl++; parse_board_line( 2 ); parse_time(); break;
   case 5 : 
   case 6 : 
   case 7 : 
   case 8 : 
   case 9 : 
   case 10: bl++; parse_board_line( bl - 3 ); break;
   case 11: parse_move(); parse_turn(); parse_value(); bl = 0; 
     clean_line();
     if ( strstr( v_ios.u.BOARD.who, ios_login ) || ios_analyse ) 
          v_ios.type = IOS_BOARD; 
     else v_ios.type = IOS_LOOK;   
     if ( ios_analyse ) v_ios.u.BOARD.turn_my = 1;
     v_client_state = EMPTY;
     return;
   }
   clean_line();
   goto AGAIN;
}

/* -------------------- */

static void f_row( int n, char *b )
{
  register char *c = &(b[3]);
  register int   i, k;
   
  for (i = 1; i < 9; i++, c += 3) {
    if ( c[0] =='(' || c[0] == '#' ) continue;
    if ( c[0] == ' ')
      if (c[1] == ' ') continue; else k = c[1]-'0';
    else k = 10*(c[0]-'0') + c[1]-'0';
    if (v_ios.u.MOVES.no < k) v_ios.u.MOVES.no = k;
    k--;
    v_ios.u.MOVES.moves[k] = n+i;
  }
}

static void parse_move_line( int n )
{
  int i;

  if ( n == 1 ) {
    v_ios.u.MOVES.no = 0;
    for ( i = 0; i < 60; i++ ) v_ios.u.MOVES.moves[i] = 0;
  }
  f_row( n*10, ios_line );
}

static void parse_MOVES( void )
{
  static int ml = 0;

 AGAIN: ;
  read_ios();
  echo_line(); /* USER: leave comments out if you want echo */
  if (! ios_line_ready ) return;
  switch ( ml ) {
  case 0 : 
  case 1 : 
  case 2 : 
  case 3 : ml++; clean_line(); break;
  case 4 : 
  case 5 : 
  case 6 : 
  case 7 : 
  case 8 : 
  case 9 : 
  case 10: 
  case 11: ml++; parse_move_line( ml - 4 ); clean_line(); break;
  case 12:
  case 13: ml++; clean_line(); break;
  case 14: ml=0; 
    clean_line();
    v_ios.type = IOS_MOVES;
    v_client_state = EMPTY;
    return;
  }
  goto AGAIN;
}




/* ---------------------------------------------------------------------- */

static int parse_who_entry( void )
{
  int i;

  if (ios_line[9] != '[') 
    return 1;

  for( i = 0; ios_line[i] != ' '; i++ ) 
    v_ios.u.WHO.list[ v_ios.u.WHO.no ].name[i] = ios_line[i];
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].name[i] = 0;

  v_ios.u.WHO.list[ v_ios.u.WHO.no ].blitz      = atoi(&(ios_line[10]));
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].standard   = atoi(&(ios_line[17]));
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].registered = (ios_line[15] == '+' ? 1 : 0);
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].playing    = (ios_line[25] == '-' ? 0 : 1);
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].open       = (ios_line[27] == '-' ? 0 : 1);
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].rated      = (ios_line[27] == 'r' || 
                                                   ios_line[27] == 'R' ? 1 : 0);
  v_ios.u.WHO.list[ v_ios.u.WHO.no ].trust      = (ios_line[27] == 'R' || 
						   ios_line[27] == 'U' ? 1 : 0);  
  v_ios.u.WHO.no++;
  return 0;
} 

static void parse_WHO( void )
{
  static int ml = 0;

 AGAIN: ;
  read_ios();
  echo_line(); /* USER: leave comments out if you want echo */
  if (! ios_line_ready ) return;
  switch ( ml ) {
  case 0 : ml++; v_ios.u.WHO.no = 0; break;
  case 1 : ml++;  break;
  case 2 : if (! parse_who_entry() ) break;
    ml = 0; 
    clean_line();
    v_client_state = EMPTY;
    v_ios.type = IOS_WHO;
    return;
  }
  clean_line();
  goto AGAIN;
}

/* -------------------- */

static void parse_CREATE( void )
{
  int i;

  echo_line();; /* USER: leave comments out if you want echo */

  for (i = 0; ios_line[i++] != '('; );
  for (; ios_line[i++] != '('; );
  v_ios.u.CREATE.rated = (ios_line[i] == 'r');
  v_ios.type = IOS_GAME_CREATE;
  ios_game_now = 1;
  ios_analyse  = 0;
  clean_line();
  v_client_state = EMPTY;
}

/* -------------------- */

static void parse_END( void )
{
  echo_line(); /* USER: leave comments out if you want echo */

  v_ios.type = IOS_GAME_END;
  ios_game_now = 0;
  clean_line();
  v_client_state = EMPTY;
}

/* -------------------- */

static void parse_PBOARD( void )
{
  char black[9], white[9], color, turn;
  int  time;
  ios_board *g = &v_ios.u.BOARD;
  int i, y, x, yx;

  memcpy(v_ios.u.BOARD.board, v_init_board, sizeof( v_init_board ));

  sscanf( &(ios_line[8]), "%3d %8s %8s %4d %4d %3d %3d %3d %3d %c %c %2d %2d %f %4d",
	  &g->no, black, white,
	  &g->time_b, &g->time_w,
          &g->def_b, &g->def_w,
          &g->inc_b, &g->inc_w,
	  &color, &turn,  
          &g->last_move_no, &g->last_move, &g->last_value, &time );
  sprintf( g->who, "(%s vs. %s)", black, white);
  if ( g->last_move < 0 ) g->last_move = MY_PASS;
  g->turn_my = (turn == 'Y');
  g->turn_color = (color == IOS_BLACK ? MY_BLACK : MY_WHITE );
  i = 78;
  for ( y = 1; y < 9; y++)
    for ( x = 1; x < 9; x++) {
      yx = x + y*10;
      switch (ios_line[i++]) {
      case IOS_WHITE : g->board[yx] = MY_WHITE; break;
      case IOS_BLACK : g->board[yx] = MY_BLACK; break;
      case IOS_EMPTY : g->board[yx] = MY_EMPTY; break;
      default : printf("[client]: <ios> message corrupt\n"); exit(1);
      }
    }
  clean_line();
  if ( strstr( v_ios.u.BOARD.who, ios_login ) || ios_analyse ) 
    v_ios.type = IOS_BOARD; 
  else v_ios.type = IOS_LOOK;   
  if ( ios_analyse ) v_ios.u.BOARD.turn_my = 1;
}

/* ====================================================================== */

static void put_who( void )
{
char buf[90];

  sprintf(buf,"who\n");
  write_ios( buf );
}

static void put_move( void )
{
  char buf[90];
  char yx[4];

  if (v_ios.u.MOVE.yx == MY_PASS) sprintf(yx,"ps"); 
  else sprintf(yx,"%c%c",v_ios.u.MOVE.yx % 10 + 'A' - 1, 
	       v_ios.u.MOVE.yx / 10 + '0');

  sprintf(buf,"PM %s %+5.2f %8.2f\n",yx, v_ios.u.MOVE.value,v_ios.u.MOVE.time);
  write_ios( buf );
}

static void put_accept( void )
{
  char buf[90];

  sprintf(buf, "accept %s\n", v_ios.u.ACCEPT.name );
  write_ios( buf );
}

static void put_decline( void )
{
  char buf[1024];

  sprintf(buf, "decline %s\n", v_ios.u.DECLINE.name );
  write_ios( buf );
  if (v_ios.u.DECLINE.reason[0] ) {
    sprintf(buf, "tell %s %s\n", v_ios.u.DECLINE.name, v_ios.u.DECLINE.reason );
    write_ios( buf );
  }
}

static void put_match( void )
{
  char buf[90];
  char komi[16];

  sprintf(komi, "komi%+f", v_ios.u.MATCH.komi_val);

  sprintf(buf,"match %s %s %d %d %d %s\n",
          (v_ios.u.MATCH.komi_on ? komi : "" ),
	  v_ios.u.MATCH.op_name,
          v_ios.u.MATCH.my_time,
	  v_ios.u.MATCH.my_inc,
	  v_ios.u.MATCH.my_def,
          v_ios.u.MATCH.rnd_color ? "" :
	  (v_ios.u.MATCH.my_color == MY_WHITE ? "white" : 
	   (v_ios.u.MATCH.my_color == MY_BLACK ? "black" : "" )) );
  write_ios( buf );
}

/* ---------------------------------------------------------------------- */
/* public functions */

void ios_connect( char *host, int port )
{
  struct sockaddr_in sa;
  struct hostent *hp;
  int   addr;

#ifdef SIGNAL_HANDLER
  sig_init(SIGINT); 
#endif

  bzero(&sa, sizeof(sa)) ;
  if ((addr = inet_addr(host)) != -1) {
    bcopy(&addr, (char *) &sa.sin_addr, sizeof(addr)) ; 
    sa.sin_family = AF_INET ;
  } else {
    if ((hp = gethostbyname(host)) == NULL) {
      fprintf(stdout, "client unknown host %s\n", host) ;
      exit(1) ;
    }
    bcopy(hp->h_addr, (char *) &sa.sin_addr, hp->h_length) ;
    sa.sin_family = hp->h_addrtype ;
  }

#ifdef NDEBUG
  sa.sin_port = htons((u_short) port);
#else
  sa.sin_port = htons((u_short) port);
  //   cerr << "htons called" << endl;
  //   exit(20);
#endif

  ios_socket = socket(sa.sin_family, SOCK_STREAM, 0);
  if ( ios_socket < 0 ) {
    perror("client socket") ;
    exit(errno) ;
  }
  if (connect(ios_socket, (struct sockaddr*) &sa, sizeof(sa)) < 0) {
    close(ios_socket);
    perror("client connect");
    exit(errno);
  }
  ios_buff_idx  = 0;
  ios_buff_len  = 0;

  ios_line[0]    = 0;
  ios_line_disp  = 0;
  ios_line_idx   = 0;
  ios_line_ready = 0;

  ios_prompt    = 0;
  ios_login_now = 0;
  ios_login[0]  = 0;

  ios_game_now  = 0;

  ios_analyse   = 0;
  ios_input     = 1;
  ios_echo      = 1;

  v_client_state = EMPTY;
  v_ios.type     = IOS_NONE;
}

extern int shutdown(int, int);

void ios_disconnect( void )
{
  shutdown(ios_socket, 2);
  close(ios_socket);
}

void ios_logon( char *login, char *passw, int echo, int input )
{
  char mm[90];

  sprintf(mm,"%s\n",login);
  strcpy(ios_login,login);
  write_ios( mm );
  if (passw != NULL) {
    sprintf(mm,"%s\n",passw);
    write_ios( mm );
  }  
  ios_echo  = echo;
  ios_input = input;
  ios_prompt = 1;
}

void ios_get( void )
{
 READ: ;
 read_ios();
 if (! ios_line_ready ) {
   if (! strcmp(ios_line, "login: ") ) {
     ios_login_now = 1;
     ios_prompt    = 1;
     echo_line();;
     clean_line();
   } else echo_line();;
   return;
 }
 CHECK: ;
 switch ( v_client_state ) {
 case GET_MATCH : parse_MATCH(); return;
 case GET_BOARD : parse_BOARD(); return;
 case GET_MOVES : parse_MOVES(); return;
 case GET_WHO   : parse_WHO  (); return;
 default        : break;
 }

 if (strncmp( ios_line, "<ios> MATCH", strlen("<ios> MATCH"))==0) {
   v_client_state = GET_MATCH;
   goto CHECK;
 }
 if (strncmp( ios_line, "<ios> [GAME", strlen("<ios> [GAME"))==0 &&
     strstr( ios_line, "created" ) &&
     strstr( ios_line, ios_login ) ) {
   parse_CREATE();
   return;
 }
  
 if (strncmp( ios_line, "<ios> [GAME", strlen("<ios> [GAME"))==0 &&
     strstr( ios_line, "ended" ) &&
     strstr( ios_line, ios_login ) ) {
   parse_END();
   return;
 }
 if (strncmp( ios_line, "<ios> BOARD", strlen("<ios> BOARD"))==0 )
   if (  strstr( ios_line, ios_login ) || ! ios_game_now ) {
     v_client_state = GET_BOARD;
     goto CHECK;
   }
 if (strncmp( ios_line, "<ios> PB", strlen("<ios> PB"))==0 ) 
   if (  strstr( ios_line, ios_login ) || ! ios_game_now ) {
     parse_PBOARD();
     return;
   } 
 if (strncmp( ios_line, "<ios> MOVES", strlen("<ios> MOVES"))==0 )
   if (  strstr( ios_line, ios_login ) ) {
     v_client_state = GET_MOVES;
     goto CHECK;
   }

 if (strncmp( ios_line, 
	      "login    blitz  stand.  G  M  idle  on for from host",
	      strlen("login    blitz  stand.  G  M  idle  on for from host"))
     ==0) {
   v_client_state = GET_WHO;
   goto CHECK;
 }

 //ECHO: ;

 echo_line();;
 clean_line();
 goto READ;
}

void ios_put( void )
{
  switch ( v_ios.type ) {
  case IOS_MOVE    : if ( ios_analyse ) return; else put_move(); break;
  case IOS_WHO     : put_who();     break;
  case IOS_MATCH   : put_match();   break;
  case IOS_ACCEPT  : put_accept();  break;
  case IOS_DECLINE : put_decline(); break;
  default          : printf("\nERROR: %d not supported in ios_put(), yet\n", 
			    v_ios.type); 
  exit(1);
  }
  v_ios.type = IOS_NONE;
}


/*
   third part utilities provided through "client_util.i"
*/

#include "client_util.i"

/* -----------------------------------------------------------------------
   (eof) client.c
   ----------------------------------------------------------------------- */

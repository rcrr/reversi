/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* -----------------------------------------------------------------------
   client.c
   -------------------
   (C) Igr Durdanovic
   ----------------------------------------------------------------------- */

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
#include <sig.i>
#endif

#define toupper(C) (((C) >= 'a' && (C) <= 'z')? (C) - 'a' + 'A': (C))

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

typedef enum { EMPTY, GET_BOARD, GET_MATCH, GET_MOVES, GET_WHO } t_client_state;

static v_client_state;

static int  ios_socket;
static char ios_buff[16384];
static char ios_line[ 8192];
static int  ios_buff_idx;
static int  ios_buff_len;
static int  ios_line_disp;
static int  ios_line_idx;
static char ios_line_eol;
static char ios_login[9];
static int  ios_login_now;
static int  ios_analyse;
static int  ios_input;
static int  ios_echo;
static int  ios_echo_board;

static void write_ios( char *m )
{
int l = strlen(m), 
    i = 0,
    n;
   
   for (; l > 0; ) {
      n = write( ios_socket, &(m[i]), l );
      if ( n <= 0 ) {
	 fprintf(stderr, "client write to <ios> failed\n");
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

static void read_ios( void )
{
int    n;
char   c;
fd_set readfds;
struct timeval timeout;
int count=0;

   if ( ios_line_eol ) return;

   if ( ios_buff_len && 
        ios_buff_len > ios_buff_idx ) goto PARSE;

 CHECK: ;
   timeout.tv_sec  = 0;
   timeout.tv_usec = 0;

   FD_ZERO( &readfds );
   if ( ios_input ) FD_SET ( 0, &readfds );
   FD_SET ( ios_socket, &readfds );
   n = select( ios_socket+1, &readfds, NULL, NULL, &timeout );
   if (n < 0 && errno != EINTR) { perror("select"); exit(1); }
   if (n == 0) return;
   if ( ios_input ) if ( FD_ISSET( 0, &readfds ) ) {
   char buf[8192];  
      fgets( buf, 8192, stdin );
#ifndef OLIOS     
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
#endif
      if (! strncmp( buf, "echo", strlen("echo") ) ) {
         ios_echo = 1 - ios_echo;
	 fprintf(stdout,"[client]: echo %s\n", ios_echo ? "ON" : "OFF");
         sprintf(buf,"\n");
      }
      if (! strncmp( buf, "board", strlen("board") ) ) {
         ios_echo_board = 1 - ios_echo_board;
	 fprintf(stdout,"[client]: board %s\n", ios_echo_board ? "ON" : "OFF");
         sprintf(buf,"\n");
      }
      write_ios( buf );

      if ( ios_login_now ) {
      int i;
	 strncpy( ios_login, buf, 8 );
         for (i = 0; i < 8; i++) if ( ios_login[i] == '\n' ||
				      ios_login[i] == '\r' ) break;
	 ios_login[i] = '\0';
 	 ios_login_now = 0;
      }
   }
   if (! FD_ISSET( ios_socket, &readfds) ) return;

READ:

   n = read(ios_socket, &(ios_buff[ ios_buff_len ]), 16384 - ios_buff_len);

   if (n <= 0) {
      fprintf(stdout,"client <ios> closed connection\n");
      exit(0);
   }

#if 0
printf("\n %d %d vvvvvvvvvvvvvvvvvvvvvvvvvv\n", count++, ios_buff_len);
if (0)
{int i;
for (i=ios_buff_len; i < ios_buff_len+n; i++) printf("%2d %c", ios_buff[i], ios_buff[i]);
}
printf("\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
#endif

   ios_buff_len += n;



 PARSE: ;
   for (;;) {
      c = ios_line[ ios_line_idx++ ] = ios_buff[ ios_buff_idx++ ];
      if ( c == '\r' ) ios_line_idx--;
      if ( c == '\n' ) { 
	 ios_line[ ios_line_idx ] = '\0';
	 ios_line_eol = 1;
	 return;
      }
      if ( ios_line_idx >= 8192 ) {
	 fprintf(stdout,"client got line longer then 8192 chars\n");
	 close( ios_socket );
	 exit(1);
      }
      if ( ios_buff_idx >= ios_buff_len ) {
	 ios_line[ ios_line_idx ] = '\0';
	 ios_buff_len = 
	 ios_buff_idx = 0;
         goto CHECK;
      }
   }

/* remaining data ... */ 

   timeout.tv_sec  = 1;
   timeout.tv_usec = 0;

printf("a\n");

   FD_ZERO( &readfds );
   FD_SET ( ios_socket, &readfds );
   n = select( ios_socket+1, &readfds, NULL, NULL, &timeout );

printf("b\n");
   if (n < 0 && errno != EINTR) { perror("select"); exit(1); }
   if (! FD_ISSET( ios_socket, &readfds) ) { perror("no data"); exit(1); }

   goto READ;

}

static void echo_line( void )
{
int ok = ios_echo;

   if ( v_client_state == GET_BOARD ) ok &= ios_echo_board;
   if ( ios_line_idx <= ios_line_disp ) ok = 0;

   if ( ios_line_eol ) {
      if ( ok ) printf("%s",&(ios_line[ ios_line_disp ]));
      ios_line_disp = 0;
      ios_line_idx  = 0;
      ios_line_eol  = 0;
   } else {

      if ( ok ) printf("%s",&(ios_line[ ios_line_disp ]));
      ios_line_disp = ios_line_idx;
   }
   if ( ok ) fflush( stdout );
}

static int readln_ios( void )
{
int nl;

   read_ios(); 
   nl = ios_line_eol;
   echo_line();
   return nl;
}

/* ---------------------------------------------------------------------- */
/* BOARD parsing story */

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

static int parse_board( void )
{
static int bl = 0;

 AGAIN: ;
   if (! readln_ios() ) return 0;
   switch ( bl ) {
   case 0 : bl++; break;
   case 1 : bl++; parse_color(); break;
   case 2 : bl++; parse_board_line( 1 ); break;
   case 3 : bl++; parse_board_line( 2 ); parse_time(); break;
   case 4 : bl++; parse_board_line( 3 ); break;
   case 5 : bl++; parse_board_line( 4 ); break;
   case 6 : bl++; parse_board_line( 5 ); break;
   case 7 : bl++; parse_board_line( 6 ); break;
   case 8 : bl++; parse_board_line( 7 ); break;
   case 9 : bl++; parse_board_line( 8 ); break;
   case 10: parse_move(); parse_turn(); parse_value(); bl = 0; return 1;
   }
   goto AGAIN;
}

static void parse_pboard( void )
{
char black[9], white[9], color, turn;
int  time;
ios_board *g = &v_ios.u.BOARD;
int i, y, x, yx;

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
}

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

static int parse_moves( void )
{
static int ml = 0;

 AGAIN: ;
   if (! readln_ios() ) return 0;
   switch ( ml ) {
   case 0 : ml++; break;
   case 1 : ml++; break;
   case 2 : ml++; break;
   case 3 : ml++; parse_move_line( 1 ); break;
   case 4 : ml++; parse_move_line( 2 ); parse_time(); break;
   case 5 : ml++; parse_move_line( 3 ); break;
   case 6 : ml++; parse_move_line( 4 ); break;
   case 7 : ml++; parse_move_line( 5 ); break;
   case 8 : ml++; parse_move_line( 6 ); break;
   case 9 : ml++; parse_move_line( 7 ); break;
   case 10: ml++; parse_move_line( 8 ); break;
   case 11: ml++; break;
   case 12: ml= 0; return 1; break;
   }
   goto AGAIN;
}

static int parse_match( void )
{
static int ml = 0;
char dummy[16], color[16];
int i; 

 AGAIN: ;
  switch ( ml ) { 
  case 0 :
    if ( ios_line[13] == 's' ) {
      v_ios.u.MATCH.stored = 1;
      sscanf( &ios_line[21], "%s %s", dummy, v_ios.u.MATCH.op_name );
      return 1;
    }
    /* look for close paren, to know where to start parsing rest of line */
    for (i=18; i<30; i++)
      if (ios_line[i] == ')')
	break;
    if (i==30)			/* didn't find close paren */
      i == 20;			/* try something */
    /* parse rest of line (timings, color, opponent) */
    sscanf( &ios_line[i+1], "%d %d %d %s %s %d %d %d %s", 
	    &v_ios.u.MATCH.my_time,
	    &v_ios.u.MATCH.my_inc,
	    &v_ios.u.MATCH.my_def,
            dummy,
            v_ios.u.MATCH.op_name,
	    &v_ios.u.MATCH.op_time,
	    &v_ios.u.MATCH.op_inc,
	    &v_ios.u.MATCH.op_def,
	    color );
    v_ios.u.MATCH.my_color  = ( color[0] == 'w' ? MY_BLACK : MY_WHITE );
    v_ios.u.MATCH.rated     = (ios_line[13] == 'r');
    v_ios.u.MATCH.rnd_color = (color[5] == '*');
    ml = 1;
    return 0;
  case 1 :
    sscanf( &(ios_line[15]), "%d .. %d %d %d .. %d",
	    &v_ios.u.MATCH.max_win,
	    &v_ios.u.MATCH.min_win,
	    &v_ios.u.MATCH.draw,
	    &v_ios.u.MATCH.min_loss,
	    &v_ios.u.MATCH.max_loss );
    ml = 0;
    return 1;
  }
  printf("bug\n");
  exit(0);
}

static void parse_create( void )
{
int i;

  for (i = 0; ios_line[i++] != '('; );
  for (; ios_line[i++] != '('; );
  v_ios.u.CREATE.rated = (ios_line[i] == 'r');
}

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

static int parse_who( void )
{
static int ml = 0;

 AGAIN: ;
  if (! readln_ios() ) return 0;
  switch ( ml ) {
    case 0 : ml++; v_ios.u.WHO.no = 0; break;
    case 1 : if ( parse_who_entry() ) { ml = 0; return 1; } break;
  }
  goto AGAIN;
  return 0;
}

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

  sprintf(buf,"match %s %d %d %d %s\n",
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
long   addr;

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

   sa.sin_port = htons((u_short) port);

   ios_socket = socket(sa.sin_family, SOCK_STREAM, 0);
   if ( ios_socket < 0 ) {
       perror("client socket") ;
       exit(errno) ;
   }
   if (connect(ios_socket, &sa, sizeof(sa)) < 0) {
       close(ios_socket);
       perror("client connect");
       exit(errno);
   }  
   ios_login[0]  = 0;
   ios_login_now = 0;
   ios_line_idx  = 0;
   ios_line_eol  = 0;
   ios_buff_idx  = 0;
   ios_buff_len  = 0;
   ios_game_now  = 0;
   ios_analyse   = 0;
   ios_input     = 1;
   ios_echo      = 1;
   ios_echo_board     = 0;
   v_client_state = EMPTY;
}

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
}


void ios_get( void )
{
  char s0[50], s1[50], s2[50], s3[50], s4[50], s5[50], s6[50], s7[50], s8[50];
  int nl;

   do {

      read_ios();

      nl = ios_line_eol;

      if (! nl ) {
	if (! strcmp(ios_line, "login: ") ) {

	  ios_login_now = 1;
          echo_line();
          ios_line[0] = 0;

	} else if (!strcmp(ios_line, "password: ") ) {
        
          echo_line();
          ios_line[0] = 0;

	} else {

#if 1
#if 0
printf("\n >>>%s<<<\n", ios_line);
#endif
          if (strlen(ios_line) >= 2 && index(ios_line, '%')) {

	    char *s=index(ios_line, '%');


/* extract login id */

#if 0
printf("\n###%s###\n", ios_line);
#endif

	    *s = 0;

#if 0
	    if (ios_login[0] && strcmp(ios_login, ios_line))
	      fprintf(stderr, "\nids differ '%s' '%s'\n", ios_line, ios_login);
#endif

	    strcpy(ios_login, ios_line);

	    *s = '%';
	    
#if 0
printf(":::'%s' login=%s\n", ios_line, ios_login);
#endif
	    echo_line();
            ios_line_disp = 0;
            ios_line_idx  = 0;
            ios_line_eol  = 0;
            ios_line[0]   = 0;
	    
          }
#else 
	    echo_line();
            ios_line[0] = 0;
	    
#endif
        }

 	return;
      }

      if (strncmp( ios_line, "<ios> MATCH", strlen("<ios> MATCH"))==0) {
	v_client_state = GET_MATCH;
      }


      if (strncmp( ios_line, "<ios> [GAME", strlen("<ios> [GAME"))==0) {


/* GAME stuff
 *
 *
 * <ios> [GAME 2 (fly vs. mic) (rated) ended
 * 1     2     3 4    5   6    7       8
 *
 */

        sscanf(ios_line, "%s %s %s %s %s %s %s %s", 
          s1, s2, s3, s4, s5, s6, s7, s8);

        strcpy(s0, s4+1);
        strcpy(s4, s0);
        s6[strlen(s6)-1] = 0;

        if (!strcmp(s4, ios_login) || !strcmp(s6, ios_login)) {

	  if (strstr( s8, "created")) {
	    ios_game_now = 1;
	    ios_analyse  = 0;
	    v_ios.type   = IOS_GAME_CREATE;
            parse_create();
            echo_line();
	    return;
          }

          if (strstr( s8, "ended")) {
	    ios_game_now = 0;
	    v_ios.type = IOS_GAME_END;
            echo_line();
	    return;
	  }
        }
      }




      if (strncmp( ios_line, "<ios> BOARD", strlen("<ios> BOARD"))==0 ) {


/* BOARD stuff
 *
 *
 * <ios> BOARD of GAME   4, (scorpion vs. mic) 
 * 1     2     3  4      5  6         7   8
 *
 */

        sscanf(ios_line, "%s %s %s %s %s %s %s %s", 
          s1, s2, s3, s4, s5, s6, s7, s8);


        strcpy(s0, s6+1);
        strcpy(s6, s0);
        s8[strlen(s8)-1] = 0;

	if ((!strcmp(s6, ios_login) || !strcmp(s8, ios_login)) ||
	  !ios_game_now ) {
	  v_client_state = GET_BOARD;
	  sprintf(v_ios.u.BOARD.who, "%s", &(ios_line[25]) );
	  v_ios.u.BOARD.who[strlen(v_ios.u.BOARD.who)-1] = 0;
	  memcpy(v_ios.u.BOARD.board, v_init_board, sizeof( v_init_board ));
	  echo_line();
        } else goto ECHO;
      }





      if (strncmp( ios_line, "<ios> PB", strlen("<ios> PB"))==0 ) {

/* PB stuff
 *
 *
 * <ios> PB   0 bbbbbbbb aaaaaaaa  ... 
 * 1     2    3 4        5 
 *
 */

        sscanf(ios_line, "%s %s %s %s %s %s %s %s", 
          s1, s2, s3, s4, s5, s6, s7, s8);

	if ((!strcmp(s4, ios_login) || !strcmp(s5, ios_login)) ||
	     ! ios_game_now ) {
	    memcpy(v_ios.u.BOARD.board, v_init_board, sizeof( v_init_board ));
	    parse_pboard();
	    if ( strstr( v_ios.u.BOARD.who, ios_login ) ||
		 ios_analyse ) v_ios.type = IOS_BOARD; 
	    else               v_ios.type = IOS_LOOK;   
	    if ( ios_analyse ) v_ios.u.BOARD.turn_my = 1;
	    v_client_state = EMPTY;
	    ios_line_eol = 0;
	    ios_line[0] = 0;
	    ios_line_idx  = 0;
	    return;
         } else goto ECHO;
      }


      if (strncmp( ios_line, "<ios> MOVES", strlen("<ios> MOVES"))==0 ) {

/* MOVES stuff
 *
 *
 * <ios> MOVES of GAME   2, (mic vs. scorpion)
 * 1     2     3  4      5  6    7   8
 *
 */
        strcpy(s0, s6+1);
        strcpy(s6, s0);
        s8[strlen(s8)-1] = 0;


	if (!strcmp(s6, ios_login) || !strcmp(s8, ios_login)) {
	  v_client_state = GET_MOVES;
	  echo_line();
        } else goto ECHO;
      }


#ifndef OLIOS
   
      if (strncmp( ios_line, 
                   "login    blitz  stand.  G  M  idle  on for from host",
            strlen("login    blitz  stand.  G  M  idle  on for from host"))
            ==0) {
        v_client_state = GET_WHO;
        echo_line();
      }

#endif

      if ( v_client_state == GET_BOARD ) {
	 if ( parse_board() ) {
	    if ( strstr( v_ios.u.BOARD.who, ios_login ) ||
		 ios_analyse ) v_ios.type = IOS_BOARD; 
	    else               v_ios.type = IOS_LOOK;   
	    if ( ios_analyse ) v_ios.u.BOARD.turn_my = 1;
	    v_client_state = EMPTY;
         }
	 return;
      }

      if ( v_client_state == GET_MATCH ) {
	 if ( parse_match() ) {
	    v_ios.type = IOS_MATCH;
	    v_client_state = EMPTY;
         }
         echo_line();
      }

      if ( v_client_state == GET_MOVES ) {
	 if ( parse_moves() ) {
	    v_ios.type = IOS_MOVES;
	    v_client_state = EMPTY;
         }
         return;
      }

      if ( v_client_state == GET_WHO ) {
	 if ( parse_who() ) {
	    v_ios.type = IOS_WHO;
	    v_client_state = EMPTY;
         }
         return;
      }


    ECHO: ;
      echo_line();
   } while ( ios_buff_len );   
}

void ios_put( void )
{
   switch ( v_ios.type ) {
   case IOS_MOVE    : if ( ios_analyse ) return; else put_move(); break;
   case IOS_WHO     : put_who();     break;
   case IOS_MATCH   : put_match();   break;
   case IOS_ACCEPT  : put_accept();  break;
   case IOS_DECLINE : put_decline(); break;
   default          : printf("\nERROR: %d not supported in ios_put(), yet\n", v_ios.type); exit(1);
   }
   v_ios.type = IOS_NONE;
}


/*
   third part utilities provided through "client_util.i"
*/

#include <client_util.i>

/* -----------------------------------------------------------------------
   (eof) client.c
   ----------------------------------------------------------------------- */

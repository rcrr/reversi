// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// socket communication for log / 12.97

#include "main.h"
#include "crt.h"
#include "goodies.h"
#include "socketcom.h"
#include <arpa/inet.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <malloc.h>
#include <math.h>
#include <netdb.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <values.h>


#define BUFF_LEN 8192
#define LINE_LEN 1024

static int  my_socket;

static char buff[ BUFF_LEN ];
static int  buff_idx;
static int  buff_len;

static char line[ LINE_LEN ];
static int  line_disp;
static int  line_idx;
static char line_ready;


static void socket_write(const char *m)
{
  int l = strlen(m), i = 0, n;
   
  for (; l > 0; ) {
    n = write( my_socket, &(m[i]), l );
    if ( n <= 0 ) {
      fprintf(stdout,"client write to server failed\n");
      exit(1);
    }
    i += n;
    l -= n;
  }
}

static void socket_read(bool block=false)
{
  int    n;
  char   c;
  fd_set readfds;
  struct timeval timeout;

  if (line_ready) /* there is already a line */
    return;

 READ: ;

  FD_ZERO(&readfds);
  FD_SET (my_socket, &readfds);

  if (block) {
    timeout.tv_sec  = 1;
    timeout.tv_usec = 0;
    n = select(my_socket+1, &readfds, NULL, NULL, &timeout);
  } else {
    timeout.tv_sec  = 0;
    timeout.tv_usec = 0;
    n = select(my_socket+1, &readfds, NULL, NULL, &timeout);
  }
  if (n < 0 /* && errno != EINTR */) { perror("select"); exit(1); }
  if (n == 0 ) if ( buff_len == 0) return; else goto LINE;
  if (! FD_ISSET( my_socket, &readfds) ) 
    if ( buff_len == 0) return; else goto LINE;
  
  n = read(my_socket, &(buff[ buff_len ]), BUFF_LEN-buff_len-1);
  if (n <= 0) {
    fprintf(stdout,"client: server closed connection\n");
    exit(0);
  }
  buff_len += n;
  
 LINE: ;
  for (;;) {
    c = line[ line_idx++ ] = buff[ buff_idx++ ];
    buff_len--;
    if ( c == '\r' ) line_idx--;
    if ( c == '\n' ) { 
    READY: ;
    line[ --line_idx ] = '\0';
    line_ready = 1;
    for ( n = 0; n < buff_len; n++ )
      buff[n] = buff[n + buff_idx];
    buff_idx = 0;
    return;
    }
    if ( line_idx >= LINE_LEN ) {
      fprintf(stdout,"client got line longer then %d chars\n", LINE_LEN);
      line_idx--;
      goto READY;
    }
    if ( buff_len <= 0 ) {
      line[ line_idx ] = '\0';
      buff_len = 0;
      buff_idx = 0;
      goto READ;
    }
  }
}

static void clean_line( void )
{
  line_ready = 0;
  line_idx   = 0;
  line_disp  = 0;
  line[0]    = 0;
}


void socket_connect(const String &host, int port, const String &cid)
{
  struct sockaddr_in sa;
  struct hostent *hp;
  int   addr;

  bzero(&sa, sizeof(sa)) ;
  if ((addr = inet_addr(host.c_str())) != -1) {
    bcopy(&addr, (char *) &sa.sin_addr, sizeof(addr)) ; 
    sa.sin_family = AF_INET ;
  } else {
    if ((hp = gethostbyname(host.c_str())) == NULL) {
      cout << "client unknown host " + host << endl;
      exit(1) ;
    }
    bcopy(hp->h_addr, (char *) &sa.sin_addr, hp->h_length) ;
    sa.sin_family = hp->h_addrtype ;
  }

  sa.sin_port = htons((u_short) port);

  my_socket = socket(sa.sin_family, SOCK_STREAM, 0);
  if ( my_socket < 0 ) {
    perror("client socket") ;
    exit(errno) ;
  }

  if (connect(my_socket, (struct sockaddr*) &sa, sizeof(sa)) < 0) {
    close(my_socket);
    perror("client connect");
    exit(errno);
  }  

  buff_idx  = 0;
  buff_len  = 0;
  
  line[0]    = 0;
  line_disp  = 0;
  line_idx   = 0;
  line_ready = 0;

  SocketSyncSend(cid.c_str());
}


void socket_disconnect()
{
  shutdown(my_socket, 2);
  close(my_socket);
}



//**************************************************//



static bool output = 1;


bool SocketSyncSend(const char *str)
{
  int i;
  int l = strlen(str);

  printf("SocketSyncSend: <%s>\n", str);
  FOR (i, l-1) if (str[i] == '\n') Error("corrupt message");
  socket_write(str);
  if (str[l-1] != '\n') socket_write("\n");
  return true;
}


void SocketSend(const char *s)
{
  FOREVER {

    if (SocketSyncSend(s)) break;

    WARTEN(2);
    printf("w"); fflush(stdout); 
  }
}  


// check for message from pipe ch, return true <=> message has arrived

bool SocketReceive(char *str, bool block)
{
  str[0] = 0;
  if (output) { /* printf(">>> SocketReceive: %s\n", ch); */ printf("."); fflush(stdout); }

#if 0
  do {
    socket_read(block);
  } while (block && !line_ready);
#else
  socket_read(block);
#endif

  if (!line_ready) return false;

  strcpy(str, line);
  if (output && 0) printf(">>> -> '%s'\n", str);

  clean_line();
  return true;
}


#if 0

// wait for message (at most max_time * 0.1 seconds)
// return true <=> message has arrived

bool SocketSyncReceive(char *s, int max_time)
{
  int r, t=0;

  do { 
    r = SocketReceive(s); 

    if (!r) { 
      WARTEN(1); 
      t++;
      if (output) printf(">>> trying\n");
    }
  } while (!r && (max_time == 0 || t < max_time));

  return r != 0;  
}

#endif


/********************************************************/


bool SocketSyncSendCLEAR()
{
  char s[MSG_MAX];

  // printf("SEND SIG_CLEAR\n");

  sprintf(s, "%d", SIG_CLEAR);
  return SocketSyncSend(s);
}  



bool SocketSyncSendEXIT()
{
  char s[MSG_MAX];


  // printf("SEND SIG_EXIT\n");

  sprintf(s, "%d", SIG_EXIT);
  return SocketSyncSend(s);
}  



bool SocketSyncSendBREAK()
{
  char s[MSG_MAX];


  // printf("SEND SIG_BREAK\n");

  sprintf(s, "%d", SIG_BREAK);
  return SocketSyncSend(s);
}  



bool SocketSyncSendBOARD(PARTEI Partei, int Sek, SFPOS LetzterZug, SPFELD *psf)
{
  char s[MSG_MAX], s1[MSG_MAX];
  SFPOS p;


  // printf("SEND SIG_BOARD\n");

  sprintf(s, "%d %d %d %d ", SIG_BOARD, Partei, Sek, LetzterZug);

  FOR_SFPOS10 (p) {

    sprintf(s1, " %d", psf->p[p]);
    strcat(s, s1);

  }


  return SocketSyncSend(s);
}



bool SocketSyncSendGAME(int player, int time, GAME *pgame, bool to_move)
{
  char s[MSG_MAX];

  // printf("SEND SIG_GAME\n");

  sprintf(s, "%d %d %d %d ", SIG_GAME, player, time, to_move != 0);

  sWriteGame(&s[strlen(s)], pgame);
 
  return SocketSyncSend(s);
}



void SocketSendMOVE(SFPOS move, float move_time, float value)
{
  char s[MSG_MAX], t[10];

  // printf("SEND SIG_MOVE %d %d\n", Zug, MoveTime);

  sKoorAus(t, move);
  sprintf(s, "%d %s %.2f %.2f", SIG_MOVE, t, move_time, value);
  SocketSend(s);
}  

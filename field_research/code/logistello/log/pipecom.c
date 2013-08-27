// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// pipe communication for log / 12.97

#include "main.h"
#include "pipecom.h"
#include "crt.h"
#include "goodies.h"
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

static bool output = 1;


bool PipeSyncSend(char *ch, char *str)
{
  int i, p;
  int l = strlen(str);

  printf("PipeSyncSend: <%s>\n", str);

  FOR (i, l-1) if (str[i] == '\n') Error("corrupt message");

  if (str[l-1] != '\n') {
    str[l++] = '\n';
    str[l] = 0;
  }


  if ((p=open(ch, O_WRONLY)) < 0) Error("Pipe not writable"); 

  if (strlen(str)+5 > MSG_MAX) Error("msg too long");

  if (output) { printf(">>> PipeSyncSend: %s %s\n", ch, str); errno = 0; }


  for (i=0; l > i ; ) {
    int n = write(p, str + i, l - i);
    if ( n < 0 ) {
      if ( errno == EINTR ) continue;
      perror( "write" );
      Error("wrote < 0");
    }
    i += n;
  }

  close(p);
  return 1;
}


void PipeSend(char *ch, char *s)
{
  FOREVER {

    if (PipeSyncSend(ch, s)) break;

    WARTEN(2);
    printf("w"); fflush(stdout); 
  }
}  


int pipe_read(int p, char *str, int l, int poll)
{
  fd_set fd_recv;
  struct timeval timeout = { 0, 0 };
  int i, no, n;

again:;

  FD_ZERO( &fd_recv );
  FD_SET( p, &fd_recv );

  if (poll) {
    no = select(p+1, &fd_recv, 0, 0, &timeout );
    if (no == 0) return 0;

  } else {
    no = select(p+1, &fd_recv, 0, 0, 0);
  }

  if ( no <= 0 ) {
    if ( errno == EINTR ) return 0;
    perror( "select" );
    Error("select < 0" );
  }

  for (i=0;;) {

    if (i+1 >= l) Error("too long");

    n = read(p, str + i, 1);

    printf("."); fflush(stdout);

    if ( n < 0 ) {
      if ( errno == EINTR ) continue;
      perror( "read" );
      Error("read < 0" );
    }

    if (n == 0) goto again;

    i += n;

    if (n > 0 && str[i-1] == '\n') return i;
  }

  return 0;
}


// check for message from pipe ch, return true <=> message has arrived

bool PipeReceive(char *ch, char *str)
{
  int p, ret;
  str[0] = 0;
  if (output) { /* printf(">>> PipeReceive: %s\n", ch); */ printf("."); fflush(stdout); }
  if ((p=open(ch, O_RDONLY | O_NONBLOCK)) < 0) Error("Pipe not readable"); 

  ret = pipe_read(p, str, MSG_MAX, 1);

  if (output && 0) printf(">>> -> '%s'\n", str);
  close(p);
  return ret;
}


// wait for message (at most max_time * 0.1 seconds)
// return true <=> message has arrived

bool PipeSyncReceive(char *ch, char *s, int max_time)
{
  int r, t=0;

  do { 
    r = PipeReceive(ch, s); 

    if (!r) { 
      WARTEN(1); 
      t++;
      if (output) printf(">>> trying\n");
    }
  } while (!r && (max_time == 0 || t < max_time));

  return r != 0;  
}



/********************************************************/


bool PipeSyncSendCLEAR(char *ch)
{
  char s[MSG_MAX];

  // printf("SEND SIG_CLEAR\n");

  sprintf(s, "%d", SIG_CLEAR);
  return PipeSyncSend(ch, s);
}  



bool PipeSyncSendEXIT(char *ch)
{
  char s[MSG_MAX];


  // printf("SEND SIG_EXIT\n");

  sprintf(s, "%d", SIG_EXIT);
  return PipeSyncSend(ch, s);
}  



bool PipeSyncSendBREAK(char *ch)
{
  char s[MSG_MAX];


  // printf("SEND SIG_BREAK\n");

  sprintf(s, "%d", SIG_BREAK);
  return PipeSyncSend(ch, s);
}  



bool PipeSyncSendBOARD(char *ch, PARTEI Partei, int Sek, SFPOS LetzterZug, SPFELD *psf)
{
  char s[MSG_MAX], s1[MSG_MAX];
  SFPOS p;


  // printf("SEND SIG_BOARD\n");

  sprintf(s, "%d %d %d %d ", SIG_BOARD, Partei, Sek, LetzterZug);

  FOR_SFPOS10 (p) {

    sprintf(s1, " %d", psf->p[p]);
    strcat(s, s1);

  }


  return PipeSyncSend(ch, s);
}



bool PipeSyncSendGAME(char *channel, int player, int time, GAME *pgame, bool to_move)
{
  char s[MSG_MAX];

  // printf("SEND SIG_GAME\n");

  sprintf(s, "%d %d %d %d ", SIG_GAME, player, time, to_move != 0);

  sWriteGame(&s[strlen(s)], pgame);
 
  return PipeSyncSend(channel, s);
}



void PipeSendMOVE(char *ch, SFPOS Zug, int MoveTime)
{
  char s[MSG_MAX];


  // printf("SEND SIG_MOVE %d %d\n", Zug, MoveTime);

  sprintf(s, "%d %d %d", SIG_MOVE, Zug, MoveTime);
  PipeSend(ch, s);
}  

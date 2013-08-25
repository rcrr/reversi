// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* communicate with log via stdio, 6.95 */

#include "main.h"
#include <sys/time.h>

#include "playm.h"
#include "pmove.h"
#include "eval.h"
#include "sboard.h"

#include "goodies.h"
#include "filecom.h"
#include "crt.h"
#include "playgm.h"
#include "lib.h"
#include "fpatt.h"
#include "hash.h"

#define BPIP true

char to_log[100], from_log[100];



void _abort(void) {}

int CharsToMove(char c1, char c2)
{
  int d1, d2;

  c1 = toupper(c1);
  c2 = toupper(c2);

  if (c1 == 'P' && (c2 == 'A' || c2 == 'S')) return -1;

  d1 = c1 - 'A' + 1;
  d2 = c2 - '1' + 1;

  if (d1 < 1 || d1 > 8 || d2 < 1 || d2 > 8) return -2;

  return d2 * 10 + d1;
}


#define SMAX 1000


int main(int argc, char **argv)
{
  char c1, c2, s[SMAX], *channel = "aaa";
  int  time, player = BLACK, in_game = false, movetime, log_move = false,
       move, prefix = false, argi, oldmove;
  SFPOS movelist[100];
  SPFELD board;

  if (argc < 2) {

error:

    Error("call: otextio [-c channel/aaa] [-t time] [-m] [game]");
  }

  argi = 1;

  if (argv[argi] && !strcmp(argv[argi], "-c")) {

    channel = argv[argi+1];

    if (!channel) Error("channel?");

    argi += 2;
  }

  if (argv[argi] && !strcmp(argv[argi], "-t")) {

    int time;

    time = atoi(argv[argi+1]);

    if (!time <= 10) Error("time <= 10?");

    argi += 2;
  }
  
  if (argv[argi] && !strcmp(argv[argi], "-m")) argi++;

  SfGrund(&board); player = BLACK; time = -1;

  while (argv[argi]) {

    prefix = true;

    c1 = argv[argi][0];
    c2 = argv[argi][1];

    move = CharsToMove(c1, c2);

    if (move < -1) Error("corrupt move");

    if (SfMoeglZuege(&board, player, movelist)) {

      if (move < 0) Error("pass not allowed");

      if (!SfSetzen(&board, player, move)) {
        fSfAus(stderr, &board, player, -1);
        fprintf(stderr, "%d: ", move); 
        Error("illegal move");
      }

    } else if (move >= 0) Error("illegal move");

    player = GEGNER(player);

    argi++;    
  }
  

  sprintf(to_log,   "%s"TO_PRAEFIX,   channel);
  sprintf(from_log, "%s"FROM_PRAEFIX, channel);

  Empf(to_log, s);		/* cancel old messages */
  Empf(from_log, s);		

  FOREVER {

#if 0

    int n;
    fd_set readfds;
    struct timeval timeout;


readline:

    timeout.tv_sec  = 0;
    timeout.tv_usec = 0;

    FD_ZERO( &readfds );
    FD_SET ( 0, &readfds );
    n = select(1, &readfds, NULL, NULL, &timeout );
    if (n < 0 && errno != EINTR) { perror("select"); exit(1); }

    if (n && FD_ISSET( 0, &readfds ) )

#endif

    {

/* read command from stdin */

      fgets(s, SMAX, stdin);

      if (sscanf(s, "setup %d", &time) == 1) { 

        if (!prefix) { SfGrund(&board); player = BLACK; prefix = false; }

        oldmove = -2;
        in_game = false; log_move = false;
        if (time <= 0) Error("time <= 0");

        FOREVER {
          if (SyncSendBREAK(to_log)) break;
          SLEEP(10);
        }        

        FOREVER {
          if (SyncSendCLEAR(to_log)) break;
          SLEEP(10);
        }        
 
      } else if (sscanf(s, "force %c%c", &c1, &c2) == 2) { 

        if (time < 0) Error("setup first");
        if (in_game)  Error("game is started already");

        move = CharsToMove(c1, c2);

        if (move < -1) Error("corrupt move");

        goto make_move;

      } else if (!strncmp(s, "quit", strlen("quit"))) {
  
        exit(0);

      } else if (!strncmp(s, "q", strlen("q"))) {

        printf("restart\n");
        
      } else if (!strncmp(s, "go", strlen("go"))) {

        if (time < 0) Error("setup first");

        in_game = true;
        goto send_board;

      } else if (!strncmp(s, "exit", strlen("exit"))) {
  
        exit(0);

      } else {

        in_game = true;

        c1 = s[0];
        c2 = s[1];

        move = CharsToMove(c1, c2);
 
        if (move < -1) {

	  fprintf(stderr, ">>> %s \n", s);
	  Error("unknown command");
	}
/*
fprintf(stderr, "move: %c %c\n", c1, c2); 
*/

make_move:

        if (SfMoeglZuege(&board, player, movelist)) {

          if (move < 0) Error("pass not allowed");

          if (!SfSetzen(&board, player, move)) {
            fSfAus(stderr, &board, player, -1);
            fprintf(stderr, "%d: ", move); 
            Error("illegal move");
          }

        } else if (move >= 0) Error("illegal move");

/*
fprintf(stderr, "old %d move %d\n", oldmove, move); 

        if (oldmove < 0 && move < 0) { 

          printf("B: %d - %d, W: %d - %d\n", SfAnzBLACK(&board), 0, SfAnzWHITE(&board), 0);
          continue;
        }
*/

        oldmove = move;

        player = GEGNER(player);

SfAus(&board, 0, 0);
fSteinAus(stdout, player); printf(" to move\n\n");
fflush(stdout);

        if (!in_game || log_move) { log_move = false; continue; }

send_board:

#if 0
printf("Send board\n");
#else
        FOREVER {

          if (SyncSendBOARD(to_log, player, time, -1, &board)) break;

          SLEEP(10);
        }

#endif


/* wait for move */

        FOREVER {

	  if (Empf(from_log, s)) {

/* message from log */

	    char *p;
	    int n;

	    if (!ParseNumber(s, &n, &p)   || n != SIG_MOVE ||
	    !ParseNumber(p, &move, &p) || (!ZUG(move) && move != ZUG_PASSEN)) {

	      Error("log message corrupt");

	    }

            if (move == 0) move = -1;

	    if (!ParseNumber(p, &movetime, &p)) movetime = 0;

	    time -= movetime;

	    log_move = true;

/*
fprintf(stderr, "log move: "); fKoorAus(stderr, move); fprintf(stderr, "\n"); 
*/

printf("My move: "); KoorAus(move); 

#if BPIP

printf("\nSeconds used: %d\n", movetime);

#else

	    printf(" %d\n", movetime);

#endif

	    fflush(stdout);

	    goto make_move;

	  }

          SLEEP(10);
	}

      }

    }

  }

  return 0;
}



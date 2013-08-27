// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// dispatcher for NEC LOG-server / 11.97
//
//  version 1: cgi

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

#include "main.h"
#include "crt.h"
#include "game.h"
#include "filecom.h"



#define DIR     "/tmp/log/"
#define LOCK    "lock."
#define KILL    "kill."
#define CHANNEL "ch."

#define LOG_N  4


void _abort() { }

// return  1: got lock on file i
// return  0: file i not available
// retunr -1: file i doesn't exist

int lock(char *name, int &fd)
{
  if ((fd = open(name, O_RDWR)) == -1) return -1;  // file doesn't exist

  struct flock fl = { F_WRLCK, SEEK_SET, 0,       0,     0 };
    
  fl.l_pid = getpid();

  if (fcntl(fd, F_SETLK, &fl) == -1) {
    close(fd);
    return 0;   // file not available
  }

  return 1;     // got lock
}


void unlock(int fd)
{
                    /* l_type   l_whence  l_start  l_len  l_pid   */
  struct flock fl = { F_UNLCK, SEEK_SET, 0,       0,     0 };
  fl.l_pid = getpid();

  if (fcntl(fd, F_SETLK, &fl) == -1) Error("unlock");
}


int main(int argc, char **argc)
{
  if (argc != 2) {
    Error("odisp move-sequence/level/Log's-color");
  }

  char name[1000];
  int i, fd;

  ZEIT start_time, submit_time, response_time; 

  RealeZeit(&start_time);

  FOREVER {

    FOR (i, LOG_N) {
      
      sprintf(name, "%s%s%d", DIR, LOCK, i+1);

      int ret = lock(name, fd);

      // printf("%s -> %d\n", name, ret);

      if (ret == 1) break;
    }
    
    if (i < LOG_N) {

      // idle log found

      printf("send job to %d\n", i+1);

      RealeZeit(&submit_time);

#if TEST

      // just wait (test)

      sleep(1);

#else

      char to_log[1000];
      char from_log[1000];

      sprintf(to_log,   "%s%s%d.To.",   DIR, CHANNEL, i+1);
      sprintf(from_log, "%s%s%d.From.", DIR, CHANNEL, i+1);

      // send board

      GAME game;

      game.MoveNum = 0;
      game.DiscDiffBW = 0;
      game.Flags = 0;
      
      game.Moves[game.MoveNum++] = SMOVE_GEN(D3, BLACK);
      game.Moves[game.MoveNum++] = SMOVE_GEN(C5, WHITE);
      game.Moves[game.MoveNum] = 0;

      //      while (!SyncSendCLEAR(to_log));
      while (!SyncSendGAME(to_log, BLACK, -7, &game, 1));
       

      // wait for move

      int move, n;
      char msg[1000], *p;

      FOREVER {

	if (Empf(from_log, msg)) {

	  if (!ParseNumber(msg, &n, &p) || n != SIG_MOVE ||
	      !ParseNumber(p, &move, &p) || (!ZUG(move) && move != ZUG_PASSEN)) {
	    
	    Error("corrupt move message");
	  }

	  break;

	} else usleep(100000);

      }

      RealeZeit(&response_time);

      printf("\n-> "); KoorAus(move); 
      printf("\nsubmit_delay=%.2f\nresponse_delay=%.2f\n\n", 
	     ZeitDiff(&submit_time, &start_time), 
	     ZeitDiff(&response_time, &submit_time));
      
#endif


      unlock(fd);
      exit(0);

    }

    printf("."); fflush(stdout);
    usleep(100000);
  }

  Error("here?");
  return 0;
}


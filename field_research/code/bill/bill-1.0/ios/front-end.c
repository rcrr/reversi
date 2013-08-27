/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* Front end for Bill to connect to IOS.  It gets moves from IOS, and
   feeds them to Bill, and gets moves from Bill and gives them to IOS.
   It starts up Bill anew, for each new game */

#include "ios.h"
#include "interface.h"
#include "client.h"
#include "/home/sanjoy/include/quit.h"

#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/resource.h>
#include <ctype.h>
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>

#define TRUE 1
#define FALSE 0

#define PROGRAM_DIED -2

void wait_for_challenge (void);
void clean_up ();
Child spawn_program (ios_match match_info, int b_trust, int w_trust);
void spawn_child (char *cmd, Child *data);
void wait_and_get ();
void send_move_to_program (int move);
int get_move (float *value);
void kill_child (Child *data);
void generic_signal_handler (int);
void sigchld_handler ();
void play_games ();
int match_okay (ios_match match_info);

extern int errno;

char Buffer[20001];		/* where output from program goes */
int Bytes;			/* how many bytes in buffer */
Child Program;			/* for communicating with Othello program */
int Use_book = TRUE;

int main (int argc, char **argv)
{
  int i;
  char ios_login[20] = IOS_LOGIN;
  char ios_password[20] = IOS_PASSWORD;

  for (i=1; i<argc; i++)
  {
    if (argv[i][0] == '-')
      switch (argv[i][1])
      {
      case 'l':			/* -l <login name> */
	if (argv[++i])
	  strcpy (ios_login, argv[i]);
	else
	  quit ("Usage: %s [-l <login name>] [-p <password>]", argv[0]);
	break;
      case 'p':
	if (argv[++i])
	  strcpy (ios_password, argv[i]);
	else
	  quit ("Usage: %s [-l <login name>] [-p <password>]", argv[0]);
	break;
      case 'B':			/* don't use book */
      case 'b':			/* do use book */
	Use_book = argv[i][1] == 'b';
	break;
      default:
	quit ("Unrecognized option: %s", argv[i]);
      }
    else
      quit ("Usage: %s [-l <login name>] [-p <password>]", argv[0]);
  }

  ios_connect (IOS_HOST, IOS_PORT);
  ios_logon (ios_login, ios_password, TRUE, TRUE);

  signal (SIGCHLD, sigchld_handler);
  signal (SIGTERM, generic_signal_handler);
  signal (SIGHUP, generic_signal_handler);
  /* ignore interrupts, but let them go through to the child process */
  signal (SIGINT, SIG_IGN);

  play_games ();

  clean_up ();
  exit(0);
}

/* disconnect from IOS and make the child process exit (or kill it) */

void clean_up ()
{
  ios_disconnect ();
  kill_child (&Program);
}  

/* wait for a challenge, accept it if it has the right opponent and 
   timings, and then return the info about the match */

void start_match ()
{
  ios_match match_info;

  do
  {
    wait_and_get ();
    if (v_ios.type != IOS_NONE && v_ios.type != IOS_MATCH)
      fprintf (stderr, "%d\n", v_ios.type);
    if (v_ios.type == IOS_MATCH) /* someone challenged me */
    {
      match_info = v_ios.u.MATCH;
      fprintf (stderr, "Match offer: %s %d %d %d / %d %d %d\n",
	       match_info.op_name,
	       match_info.my_time, match_info.my_inc, match_info.my_def,
	       match_info.op_time, match_info.op_inc, match_info.op_def);
      /* accept match but only if from sanjoy (for now)
         and only if timings are right, and unrated. */
      if (match_okay (match_info))
      {
	fprintf (stderr, "Acceping offer.\n");
	strcpy (v_ios.u.ACCEPT.name, v_ios.u.MATCH.op_name);
	/* wait, so other end can process match */
	/*	sleep(1); */
	v_ios.type = IOS_ACCEPT;
	ios_put ();
      }
      else
      {
	strcpy (v_ios.u.DECLINE.name, v_ios.u.MATCH.op_name);
	strcpy (v_ios.u.DECLINE.reason,	"Sorry, I can't play.  Email sanjoy@hope.caltech.edu for more information.");
	v_ios.type = IOS_DECLINE;
	fprintf (stderr, "Declining offer\n");
	ios_put ();
      }
    }
  }
  while (v_ios.type != IOS_GAME_CREATE);
  if (v_ios.type != IOS_GAME_CREATE)
    fprintf (stderr, "Didn't create game.  Got %d\n", v_ios.type);
  v_ios.type = IOS_NONE;
  /* spawn Othello program */
  Program = spawn_program (match_info,
			   v_ios.u.CREATE.b_trust, v_ios.u.CREATE.w_trust);
  Bytes = 0;			/* clear buffer for storing program output */
}

/* play games, spawning a new Othello program for each one */

void play_games ()
{
 restart:
  start_match ();
  /* we've accepted, IOS has started game, and othello program is running */

  while (v_ios.type != IOS_GAME_END)
  {
    wait_and_get ();
    if (v_ios.type != IOS_NONE && v_ios.type != IOS_BOARD)
      fprintf (stderr, "Type: %d\n", v_ios.type);
    if (v_ios.type == IOS_BOARD)
    {
      fprintf (stderr, "Move: %d  Turn_no: %d  My_turn: %d  Turn_color: %d\n",
	       v_ios.u.BOARD.last_move,
	       v_ios.u.BOARD.last_move_no,
	       v_ios.u.BOARD.turn_my,
	       v_ios.u.BOARD.turn_color);
      if (v_ios.u.BOARD.turn_my == TRUE) /* my (=Bill's) turn? */
      {
	float value;		/* Bill's evaluation of it's position */
	struct timeval before, after; /* for timing Bill's think */

	gettimeofday (&before, 0);
	if (v_ios.u.BOARD.last_move_no > 0)
	  send_move_to_program (v_ios.u.BOARD.last_move);
	v_ios.type = IOS_NONE;

	/* get program's response, and send to IOS */
	v_ios.u.MOVE.yx = get_move (&value);
	gettimeofday (&after, 0);
	if (v_ios.u.MOVE.yx == PROGRAM_DIED)
	  break;
	v_ios.type = IOS_MOVE;
	v_ios.u.MOVE.time = (after.tv_sec - before.tv_sec)
	  + (after.tv_usec - before.tv_usec)/1000000.0;
	v_ios.u.MOVE.value = value;
	fprintf (stderr, "Took %.2f sec\n", v_ios.u.MOVE.time);

	ios_put ();
      }
    }
  }
  /* now game is over, kill Bill if it isn't finished */
  kill_child (&Program);
  goto restart;			/* play another game */
}

/* convert move to alphanumeric othello notation, and send to program */

void send_move_to_program (int move)
{
  char human[] = "a1\n";
  int n;

  if (move == MY_PASS)
  {
    fprintf (stderr, "Sending PASS to program.\n");
    strcpy (human, "--\n");
  }
  else
  {
    human[0] = 'a' - 1 + (move % 10);
    human[1] = move/10 + '0';
    fprintf (stderr, "Sending %c%c to program.\n", human[0], human[1]);
  }
  if ((n = write (Program.to, human, 3)) != 3)
    fprintf (stderr, "Only wrote %d bytes from `%s'", n, human);
  if (n == -1)
    perror ("front-end: write");
}

/* start up othello program (hardwired to start Bill properly) */

Child spawn_program (ios_match match_info, int b_trust, int w_trust)
{
  char cmd[1000];
  int prog_is_black = match_info.my_color == MY_BLACK;
  Child child;
  int my_time, op_time;

  if (match_info.op_time < 0 || match_info.op_time > 10000 ||
      match_info.my_time < 0 || match_info.my_time > 10000 ||
      match_info.my_color != MY_BLACK && 
      match_info.my_color != MY_WHITE)
    fprintf (stderr, "spawn_program(): Got garbage match_info.\n");
  my_time = match_info.my_time + match_info.my_inc/2;
  op_time = match_info.op_time + match_info.op_inc/2;
  /* subtract 1 minute from times for untrusted players, for IOS latency */
  sprintf (cmd, "%s -%c -%c -t %d %d",
	   OTHELLO_PROGRAM,
	   Use_book ? 'u' : 'U',
	   prog_is_black ? 'w' : 'b',
	   (prog_is_black ? my_time : op_time) - (b_trust ? 0 : 1),
	   (prog_is_black ? op_time : my_time) - (w_trust ? 0 : 1));
  spawn_child (cmd, &child);
  return child;
}

/* get move from the othello program */

int get_move (float *value)
{
  char temp_buffer[10000];
  int n;
  char *found;
  fd_set readfds;
  struct timeval timeout;
  int temp;

 reread:
  FD_ZERO (&readfds);
  FD_SET (Program.from, &readfds);
  timeout.tv_sec = 0;
  timeout.tv_usec = 100000;
  n = select (Program.from+1, &readfds, (fd_set *) 0, (fd_set *) 0, &timeout);
  if (n == -1)			/* error */
  {
    perror ("front-end: select");
    if (errno == EBADF)		/* Program.from no longer a valid fd */
    {
      ios_put_str ("abort\n");
      kill_child (&Program);	/* just in case */
      return PROGRAM_DIED;
    }
  }
  else if (n == 0)		/* timed out */
  {
    v_ios.type = IOS_NONE;
    ios_get ();
    if (v_ios.type != IOS_NONE)
      fprintf (stderr, "Got type %d while waiting for program\n", v_ios.type);
    if (v_ios.type == IOS_GAME_END)
    {
      v_ios.type = IOS_NONE;
      kill_child (&Program);
      return PROGRAM_DIED;
    }
    goto reread;
  }

  n = read (Program.from, temp_buffer, 10000);
  if (n == -1)
    perror ("front-end: read");
  if (n == 0 || n==-1 && errno == EBADF)	/* got EOF, program died */
  {
    fprintf (stderr, "Program died\n");
    ios_put_str ("abort\n");
    kill_child (&Program);
    return PROGRAM_DIED;
  }

  /* clean up buffer: search from end for newline */
  for (temp = Bytes - 1; temp >= 0 && Buffer[temp] != '\n'; temp--)
    ;
  if (temp >= 0)		/* save incomplete line, if any */
  {
    Bytes -= temp + 1;
    bcopy (Buffer + temp + 1, Buffer, Bytes);
  }
  if (Bytes + n > 20000)
  {
    fprintf (stderr, "Ran out of room in buffer.  Tossing old stuff\n");
    Bytes = 0;
  }
  bcopy (temp_buffer, Buffer + Bytes, n);
  Bytes += n;
  Buffer[Bytes] = '\0';
  fprintf (stderr, "%s", Buffer); 

  *value = 0;
  if (found = strstr (Buffer, VALUE_TAG))
  {
    found += strlen (VALUE_TAG)+1; /* move past intro "Value:" */
    if (sscanf (found, "%f", value) != 1)
      fprintf (stderr, "Couldn't parse value in: %s\n", found);
  }
  if (found = strstr (Buffer, MOVE_TAG))
  {
    char move[100];

    found += strlen (MOVE_TAG) + 1; /* to point to the move */
    sscanf (found, "%s", move);
    if (isupper(move[0]))
      move[0] = tolower(move[0]);
    return (move[0] - 'a' + 1) + 10 * (move[1] - '0');
  }
  else if (found = strstr (Buffer, PASS_TAG)) /* Bill passed */
    return MY_PASS;
  else				/* didn't find any move, read some more */
    goto reread;
}

/* spawn off child process, making its stdin controlled by us, and
   stdout readable by us.  This code stolen from GNU xboard, xboard.c

   A bug recently found: the spawned process has a copy of the
   parent's open file descriptors.  So the first spawn goes ok, and
   opens up some fds in the parent to talk to the new child.  But a
   concurrent second spawn then gives a copy of those fd's to the
   child it spawns.  Which means that just the parent closing the .to
   fd to the child won't help, the other child would have to as well.
   So we have to make sure the newly created doesn't get those fds.
   Maybe by setting them to be not copied on exec. */

void spawn_child (char *cmd, Child *data)
{
  int i, pid;
  char *argv[64], *p;
  int to_prog[2], from_prog[2];
  char localcmd[1000];

  fprintf (stderr, "Spawning `%s'\n", cmd);
  strncpy (localcmd, cmd, 1000);
    
  i = 0;
  p = localcmd;
  for (;;)
  {
    argv[i++] = p;
    p = strchr(p, ' ');
    if (p == NULL) break;
    *p++ = 0;
  }
  argv[i] = NULL;

  signal (SIGPIPE, SIG_IGN);
  if (pipe (from_prog) != 0)
    pquit ("Couldn't make `from pipe' when spawning `%s'", cmd);
  if (pipe (to_prog) != 0)
    pquit ("Couldn't make `to pipe' when spawning `%s'", cmd);
  if ((pid = fork()) == 0)	/* child */
  {
    struct rlimit limit_info = {RLIM_INFINITY, RLIM_INFINITY};

    /* restore default signal behavior */
    signal (SIGINT, SIG_DFL);
    signal (SIGTERM, SIG_DFL);
    signal (SIGPIPE, SIG_DFL);
    signal (SIGCHLD, SIG_DFL);

    /* unlimit coredumpsize, in case we need to trace Bill */
    if (setrlimit (RLIMIT_CORE, &limit_info))
      perror ("front-end: setrlimit(RLIMIT_CORE,)");

    dup2(to_prog[0], 0);
    dup2(from_prog[1], 1);
    close(to_prog[0]);
    close(to_prog[1]);
    close(from_prog[0]);
    close(from_prog[1]);
/*    dup2(1, fileno(stderr));*/ /* force stderr to the pipe */

    execvp(argv[0], argv);

    perror(argv[0]);
    exit(1);
  }
  /* Parent process */
  close(to_prog[0]);
  close(from_prog[1]);
  
  data->pid = pid;
  data->from = from_prog[0];
  data->to = to_prog[1];

  /* set the close on exec flags, so sibling processes don't get copies */
  if (fcntl (data->from, F_SETFD, 1) == -1)
    perror ("fcntl(data->from)");
  if (fcntl (data->to, F_SETFD, 1) == -1)
    perror ("fcntl(data->to)");
}

void wait_and_get ()
{
  v_ios.type = IOS_NONE;
  usleep (100000);
  ios_get ();
}

void kill_child (Child *data)
{
  /* closing the descriptors may kill it */
  close (data->to);
  close (data->from);
  if (!data->died && kill (data->pid, SIGTERM))
    fprintf (stderr, "Couldn't send TERM to child %d\n", data->pid);
}
    
/* clean up and exit */

void generic_signal_handler (int sig)
{
  fprintf (stderr, "Caught signal %d, cleaning up and exiting...\n", sig);
  clean_up ();
  exit (1);			/* we're done */
}

/* SIGCHLD handler */

void sigchld_handler ()
{
  int pid = wait (0);

  if (pid == Program.pid)
    Program.died = TRUE;
  else if (pid == -1)
  {
    if (errno == ECHILD)	/* no children */
      fprintf (stderr, "SIGCHLD handler (child): all children are gone\n");
    else
      perror ("wait");
  }
  if (pid != -1)
    fprintf (stderr, "Process %d died (Othello program is %d)\n",
	     pid, Program.pid);

  signal (SIGCHLD, sigchld_handler);	/* reinstall signal handler */
}

/* check if opponent on list of okay players to play against */

int name_ok (char *name)
{
  char okay[][20] =
  { "billtest", "sanjoy", "ant", "ant+", "patzer", "OO7", "scorp+", "scorpion",    "obaby", "matreyek", "OO1", ""};
  int i;

  return TRUE;

  for (i=0; okay[i][0] != '\0'; i++)
    if (strcmp (okay[i], name) == 0)
      return TRUE;
  return FALSE;
}

/* return TRUE iff the match offer is acceptable */

int match_okay (ios_match match_info)
{
  return
    name_ok (v_ios.u.MATCH.op_name) 
    && v_ios.u.MATCH.my_time >= 2 && v_ios.u.MATCH.op_time >= 2
    && v_ios.u.MATCH.stored == 0;
}

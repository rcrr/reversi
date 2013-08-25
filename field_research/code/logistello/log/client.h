// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* -----------------------------------------------------------------------
   client.h
   -------------------
   (C) Igor Durdanovic
   ----------------------------------------------------------------------- */

#define IOS_BLACK ('*')
#define IOS_WHITE ('O')
#define IOS_EMPTY ('-')

/* redefine constants to fit your program, or write your function */
/* to remap offered representation into yours                     */

#define MY_BLACK (+1)
#define MY_WHITE (-1)
#define MY_EMPTY ( 0)
#define MY_EDGE  (-2)
#define MY_PASS  ( 0)

typedef enum { 
  IOS_NONE, 
  IOS_MATCH,      /* get & put */
  IOS_BOARD,      /* get */
  IOS_MOVES,      /* get */
  IOS_GAME_END,   /* get */
  IOS_GAME_CREATE,/* get */
  IOS_LOOK,       /* get */
  IOS_WHO,        /* get & put */
  IOS_MOVE,       /* put */
  IOS_ACCEPT,     /* put */
  IOS_DECLINE     /* put */
} ios_type;

/* 
   About the meaning of fields in structure, read help on <ios>
   they roughly corresponds to terms used there 
*/

typedef struct {
  int stored;
  int rated;
  int   komi_on;
  float komi_val;
  int   rand_val;
  int my_time, op_time;
  int my_inc,  op_inc;
  int my_def,  op_def;
  int my_color;
  int rnd_color;
  int id;
  char op_name[9];
  int max_win, min_win, draw, min_loss, max_loss;
} ios_match;

typedef struct {
   char black[9], white[9];
   int  b_trust,  w_trust;
   int rated;
} ios_create;

typedef struct {
   char name[9];
   int  blitz, standard;
   int  registered, playing, open, rated, trust;
} t_who;

typedef struct {
  int   no;
  t_who list[256];
} ios_who;

typedef struct {
   char  who[64];
   int   no;     /* game no */
   int   board[100];
   int   time_w,
         time_b;
   int   def_w,  /* default time */
         def_b;
   int   inc_w,  /* increment time */
         inc_b;
   int   turn_color;
   int   turn_my;
   int   last_move;
   int   last_move_no;
   float last_value;
   float komi;
   int   rand;
} ios_board;

typedef struct {
   int no;
   int moves[60];
} ios_moves;

typedef struct {
   int   yx;
   float time;
   float value;
} ios_move;

typedef struct {
  char name[9];
} ios_accept;

typedef struct {
  char name[9];
  char reason[512];
} ios_decline;

typedef struct {
  ios_type type;
  struct {
    ios_board   BOARD;
    ios_moves   MOVES;
    ios_match   MATCH;
    ios_create  CREATE;
    ios_who     WHO;
    ios_move    MOVE;
    ios_accept  ACCEPT;
    ios_decline DECLINE;
  } u;
} t_ios;

/* this is your connection with module */

#ifdef CLIENT
       t_ios v_ios;
       int   ios_game_now;  /* if you are playing now */
#else
extern t_ios v_ios;
extern int   ios_game_now;
#endif

/*
   internal client commands:

   "board"   - on/off received <ios> board 
   "analyse" - will send all received boards as if own game
               but will not send any moves made for those
	       boards (auto-off when own game played)
   "echo"    - stdout from client on/off
   "input"   - stdin  from client off (no on!!)
*/

/* 
   function to connect to <ios> - simple telnet ... 
*/
void ios_connect( char *host, int port );


/* 
   function to disconnect from <ios> but to stay alive!
*/
void ios_disconnect( void );


/* 
   if you want auto-login to be done ... 
   echo  - enable/disable echo of things received by client to stdout
   input - enable/disable reading from stdin (useful for bg mode)
*/
void ios_logon( char *login, char *passw, int echo, int input );


/*
   This function recognizes messages (exactly which is listed in ios_type)
   coming from <ios>, parses them and sets v_ios.type to correct type
   when the message is complete. 

   IMPORTANT, each time you copied message from one of v_ios.u.XXX you
   have to set v_ios.type to IOS_NONE to signalize your readiness
   to accept another message as it comes.

   Additionally, stdin is read and sent to <ios> to enable you to
   communcate as human with <ios>. Therefore you should call it
   roughly 5-10 times per sec. (more will probably slow down your
   program and less will cause delay effect).
*/
void ios_get( void );


/*
   This function is clean output interface. Fill in v_ios.u.XXX and
   set v_ios.type to corresponding XXX type! then call ios_put().
   The v_ios.type will be set back to IOS_NONE.
*/   
void ios_put( void );


/* 
   This is low-level put instruction which enables you to send anything
   to <ios> however it is highly recomended to use ios_put() interface.
   In case you wrote nice routine you think can be useful, send it to me
   to be included in next release of client.
*/
void ios_put_str( char *s );




/* 
   third part utilities provided through client_util.i
*/

/*
   You have to issue "who" command. Upon reception of IOS_WHO event,
   you may call following function to randomly challenge fitting
   opponent.
   if min_rating < 0 then min_rating += my_rating !
*/
void ios_challenge( int time, int inc, int def, 
		    int color, int rnd_color,
                    int min_rating, int max_rating,
                    int must_rated, int best);

/* -----------------------------------------------------------------------
   (eof) client.h
   ----------------------------------------------------------------------- */

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* 
   ox client/server interface
*/

#define d_pos_no   (100)
#define d_move_no  (100)
#define d_mov_no    (32)

#define d_move_pass (-1)
#define d_black     ( 1)
#define d_white     (-1)
#define d_empty     ( 0)
#define d_edge      (-2)

typedef enum { R_NONE, R_LOGIN, R_GAME, R_MOVE, R_START, R_END } t_sig;

typedef signed char t_sint1;
typedef float       t_real;
typedef char        t_name[9];

typedef t_sint1 t_pos[ d_pos_no ];
typedef t_sint1 t_mov[ d_mov_no ];

typedef struct {
  t_pos    pos;
  t_sint1  b_disc, w_disc,  /* disc(s) no */
           b_move, w_move;  /* move(s) no */
  t_mov    b_list, w_list;  /* move(s) itself */
  t_real   b_time, w_time;  /* time left */
} t_position;

typedef struct {
  t_sint1 yx;
  t_real  value, time;  
} t_move; 

typedef struct {
  t_sint1 no, current; 
  t_move  list[ d_move_no ];
} t_history;

typedef struct {
  t_sint1    no;   /* game no */
  t_position start, current;
  t_history  history;
  t_name     black, white;
} t_game;

typedef struct {
  int   len;
  char *buf;
} t_buff;

typedef struct {
  int    socket;
  t_name name;
  t_sig  sig;
  t_buff i,o;
} t_io;


int init_server( int port );              /* 0 - ok, else need exit ! */
int init_client( char *host, int port );  /* 0 - ok, else need exit ! */

void game_continue( t_game *g );
int  game_backward( t_game *g );            /* 0 - ok, else error */
int  game_forward ( t_game *g );            /* 0 - ok, else error */

void init_game( t_game *g, t_real time );

int new_move      ( t_game *g, t_move *m ); /* 0 - ok, else illegal move */
int replay2current( t_game *g );            /* 0 - ok, else error */

int  game_load    ( t_game *g, char *file );  /* 0 - ok, else error */
int  game_save    ( t_game *g, char *file );  /* 0 - ok, else error */

void send_string ( t_io *u, char *s );

void send_move   ( t_io *u, t_move *m );
void send_start  ( t_io *u, t_game *g );
void send_end    ( t_io *u, t_game *g );
void send_moves  ( t_io *u, t_game *g ); 
void send_history( t_io *u, t_game *g ); 
void send_board  ( t_io *u, t_game *g );
void send_game   ( t_io *u, t_game *g );

void recv_move   ( t_io *u, char *l, t_move *m );
void recv_start  ( t_io *u, char *l );
void recv_end    ( t_io *u, char *l );
void recv_moves  ( t_io *u, char *l, t_game *g );
void recv_history( t_io *u, char *l, t_game *g );
void recv_board  ( t_io *u, char *l, t_game *g );
void recv_mssg   ( t_io *u, char *l );

int socket_io( void );  /* 0 = ok, else need exit ! */

#ifdef OX_MAIN
   t_game     game;               /* ... game */
   t_move     move;               /* ... move */
   t_io       client;
   t_io       player[2];
#else
   extern t_game     game;
   extern t_move     move;
   extern t_io       client;
   extern t_io       player[2];
#endif

/*
  ox [ port | -ios host port ] 
*/


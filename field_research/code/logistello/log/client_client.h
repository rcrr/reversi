// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/*======================================================================

   	File:	client.h

	Name:	(C) Igor Durdanovich
	
	Date:	Modified Mar 96
	
========================================================================*/

#define	CLIENT_BUFFER_SIZE 	65536
#define CLIENT_LINE_SIZE 	4096
#define	CLIENT_CHAT_SIZE 	1024
#define	CLIENT_NAME_SIZE	9
#define	CLIENT_HOST_SIZE	80
#define	CLIENT_LIST_SIZE	256
#define	CLIENT_DATE_SIZE	11
#define	CLIENT_TIME_SIZE	6
#define	CLIENT_DURATION_SIZE	7
#define	CLIENT_REMOTE_SIZE	10
#define	CLIENT_RANK_SIZE	2048
#define	CLIENT_HISTORY_SIZE	50

#define	CLIENT_PASS	-1

typedef enum { 
  	CLIENT_NONE,	

	CLIENT_MATCH,	CLIENT_MATCH_TO,
	CLIENT_CREATE,	
	CLIENT_MOVES,	CLIENT_PMOVES,	CLIENT_LMOVES,
	CLIENT_BOARD,	CLIENT_PBOARD,	CLIENT_LOOK,  	
	CLIENT_MOVE,	CLIENT_ABORT,	CLIENT_BREAK,	CLIENT_END,  	

	CLIENT_ACCEPT,  CLIENT_DECLINE,   

	CLIENT_WHO,  	CLIENT_PLAYERS,	CLIENT_GAMES,	CLIENT_PENDING,

	CLIENT_FINGER,	CLIENT_SET,	CLIENT_KILL,
	CLIENT_TOP,	CLIENT_RANK,	CLIENT_ASSESS,	
	CLIENT_RESIGN,	CLIENT_STORED,	CLIENT_MORETIME,
	CLIENT_OBSERVE,	CLIENT_WHOOBS,	

	CLIENT_FILTER,	CLIENT_YELL,	CLIENT_TELL,
	CLIENT_SAY,	CLIENT_KIBITZ,	CLIENT_WHISPER,	
	CLIENT_LISTEN,	CLIENT_WHOLIST,	

	CLIENT_PIO,	CLIENT_IN,	CLIENT_OUT,

	CLIENT_UPSTATE,	
	CLIENT_LHISTORY,CLIENT_GHISTORY,
	CLIENT_AUTOMAIL,
	CLIENT_MAIL,	CLIENT_HELP,	

	CLIENT_EXIT,

	CLIENT_TIMEOUT,	CLIENT_DISCONNECT,
	CLIENT_OKAY,	CLIENT_DEFAULT,

	CLIENT_BLACK,	CLIENT_WHITE,	CLIENT_EMPTY,	CLIENT_SPARE,
	CLIENT_KOMI,	CLIENT_RAND,	CLIENT_BRONSTEIN,

	CLIENT_HUMAN,	CLIENT_CYBORG,	CLIENT_COMPUTER,

	CLIENT_IDLE,	CLIENT_PLAY,	CLIENT_TOOT,	CLIENT_BUSY,

	CLIENT_REMOTE,		CLIENT_INTERNAL,
	CLIENT_OPERATOR,	CLIENT_SUPERVISOR,
} client_type;

typedef struct {
	client_type	type;
	client_type	special;
  	char 	user[CLIENT_NAME_SIZE];
   	int 	stored;
   	int 	rated;
	int	from;
   	int 	my_time,op_time;
   	int 	my_inc,op_inc;
   	int 	my_def,op_def;
   	client_type	my_color;
   	int 	random_color;
	float	komi;
	int	rand;
	int	game_id;
    	int 	max_win,min_win,draw,min_loss,max_loss;
} t_match;

typedef	struct {
	int	no;
} t_pending;


typedef struct {
   	char 	black[CLIENT_NAME_SIZE],white[CLIENT_NAME_SIZE];
   	int 	rated;
	float	komi;
	int	rand;
} t_create;


typedef struct {
   	char 	black[CLIENT_NAME_SIZE],white[CLIENT_NAME_SIZE];
   	int 	rated;
	int	black_disks,white_disks,disks_diff;
	float	komi;
	int	rand;
	client_type	end;
   	char 	result[CLIENT_CHAT_SIZE];
} t_end;


typedef struct {
  	int   	no;
 	char	sort;
   	char 	user[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
   	int  	blitz_rating[CLIENT_LIST_SIZE];
	int	standard_rating[CLIENT_LIST_SIZE];
   	int  	registered[CLIENT_LIST_SIZE];
	int	playing[CLIENT_LIST_SIZE];
	int	game_id[CLIENT_LIST_SIZE];
	int	open[CLIENT_LIST_SIZE];
	int	rated[CLIENT_LIST_SIZE];
	int	trust[CLIENT_LIST_SIZE];
	char	idle[CLIENT_LIST_SIZE][CLIENT_DURATION_SIZE];
	char	onfor[CLIENT_LIST_SIZE][CLIENT_DURATION_SIZE];
	char	host[CLIENT_LIST_SIZE][CLIENT_HOST_SIZE];
} t_who;


typedef struct {
  	int   	no;
 	char	sort;
   	char 	user[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
   	int  	blitz_rating[CLIENT_LIST_SIZE];
	int	standard_rating[CLIENT_LIST_SIZE];
   	int  	registered[CLIENT_LIST_SIZE];
	int	rated[CLIENT_LIST_SIZE];
	int	trust[CLIENT_LIST_SIZE];
	char	idle[CLIENT_LIST_SIZE][CLIENT_DURATION_SIZE];
	char	onfor[CLIENT_LIST_SIZE][CLIENT_DURATION_SIZE];
	char	host[CLIENT_LIST_SIZE][CLIENT_HOST_SIZE];
} t_players;


typedef struct {
  	int   	no;
  	int	game_no[CLIENT_LIST_SIZE];
	int	rated[CLIENT_LIST_SIZE];
	int	disks[CLIENT_LIST_SIZE];
	int	black_rating[CLIENT_LIST_SIZE];
	char	black_user[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
	int	black_time[CLIENT_LIST_SIZE];
	int	black_inc[CLIENT_LIST_SIZE];
	int	black_def[CLIENT_LIST_SIZE];
	int	white_rating[CLIENT_LIST_SIZE];
	char	white_user[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
	int	white_time[CLIENT_LIST_SIZE];
	int	white_inc[CLIENT_LIST_SIZE];
	int	white_def[CLIENT_LIST_SIZE];
	float	komi[CLIENT_LIST_SIZE];
	int	rand[CLIENT_LIST_SIZE];
} t_games;


typedef struct {
	int	game_id;
  	char  	black_user[CLIENT_NAME_SIZE],white_user[CLIENT_NAME_SIZE];
	float	komi;
	int	rand;
   	int   	black_rating,white_rating;
   	int   	black_disks,white_disks;
   	client_type   	board[100];
	int   	moves;
	int   	move[100];
   	int   	white_time,black_time;
   	int   	white_inc,black_inc;
   	int   	white_def,black_def;
   	client_type   	turn_color;
   	int   	turn_my;
   	int   	last_move;
   	int   	last_move_no;
   	float 	last_eval;
} t_board;


typedef struct {
	int	game_id;
	client_type	start_color;
	char	black_user[CLIENT_NAME_SIZE];
	char	white_user[CLIENT_NAME_SIZE];
   	int 	no;
	client_type	what[100];
  	int 	turn[100];
	int	black_disks;
	int	white_disks;
	float	komi;
	int	rand;
} t_moves;


typedef struct {
   	int   	move;
   	float 	time;
   	float 	eval;
} t_move;


typedef struct {
  	char 	user[CLIENT_NAME_SIZE];
	int	game_id;
	client_type	color;
} t_accept;


typedef struct {
  	char 	user[CLIENT_NAME_SIZE];
  	char 	reason[CLIENT_CHAT_SIZE];
} t_decline;


typedef	struct	{
	char	user[CLIENT_NAME_SIZE];
	char	name[CLIENT_CHAT_SIZE];
	char	email[CLIENT_CHAT_SIZE];
	int	registered;
	char	lastio_date[CLIENT_DATE_SIZE],lastio_time[CLIENT_TIME_SIZE];
	char	registered_date[CLIENT_DATE_SIZE];
	char	registered_time[CLIENT_TIME_SIZE];
	int	blitz_rating,blitz_win,blitz_loss,blitz_draw;
	float	blitz_avg;
	int	blitz_total,blitz_need;
	int	standard_rating,standard_win,standard_loss,standard_draw;
	float	standard_avg;
	int	standard_total,standard_need;
	int	pio,mail,trust,open,bell,unreg;
	int	gio,omail,rated,finger,yell,kibitz;
} t_finger;


typedef	struct {
	char	name[CLIENT_CHAT_SIZE];
	char	pwold[CLIENT_NAME_SIZE],pwnew[CLIENT_NAME_SIZE];
	int	pio,mail,trust,open,bell,unreg;
	int	gio,omail,rated,finger,yell,kibitz;	
	char	info[256];
} t_set;

	
typedef	struct {
	char	user[CLIENT_NAME_SIZE];
	char	pass[CLIENT_NAME_SIZE];
} t_kill;


typedef	struct	{
	int	no;
	char	user[CLIENT_NAME_SIZE];
	int	blitz_offset;
	char	blitz_user[CLIENT_RANK_SIZE][CLIENT_NAME_SIZE];
	int	blitz_rank[CLIENT_RANK_SIZE];
	int	blitz_rating[CLIENT_RANK_SIZE];
	int	standard_offset;
	char	standard_user[CLIENT_RANK_SIZE][CLIENT_NAME_SIZE];
	int	standard_rank[CLIENT_RANK_SIZE];
	int	standard_rating[CLIENT_RANK_SIZE];
} t_rank;


typedef	struct	{
	char	user1[CLIENT_NAME_SIZE],user2[CLIENT_NAME_SIZE];
	int	blitz_win,blitz_draw,blitz_loss;
	int	standard_win,standard_draw,standard_loss;
} t_assess;


typedef	struct	{
	char	user[CLIENT_NAME_SIZE];
	char	reason[CLIENT_CHAT_SIZE];
} t_abort;


typedef	struct	{
	char	reason[CLIENT_CHAT_SIZE];
} t_resign;


typedef	struct	{
	int	time;
} t_moretime;


typedef	struct	{
	int	game;
	char	player[CLIENT_NAME_SIZE];
} t_observe;


typedef	struct	{
	int	game;
	int	no;
	char	user[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
} t_whoobs;


typedef	struct	{
	char	user[CLIENT_NAME_SIZE];
	char	no;
	char	message[CLIENT_CHAT_SIZE];
} t_yell;


typedef	struct	{
	char	user[CLIENT_NAME_SIZE];
	int	channel;
	char	message[CLIENT_CHAT_SIZE];
} t_tell;


typedef	struct	{
	char	message[CLIENT_CHAT_SIZE];
} t_say;


typedef	struct	{
	char	user[CLIENT_NAME_SIZE];
	int	game;
	char	message[CLIENT_CHAT_SIZE];
} t_kibitz;


typedef	struct	{
	char	user[CLIENT_NAME_SIZE];
	int	game;
	char	message[CLIENT_CHAT_SIZE];
} t_whisper;



typedef	struct	{
	client_type	in_out;
	char	user[CLIENT_NAME_SIZE];
	int	blitz_rating;
	int	standard_rating;
} t_pio;


typedef	struct	{
	int	channel;
} t_listen;


typedef	struct	{
	int	channel;
	int	no;
	char	user[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
} t_wholist;


typedef	struct	{
	char	start_date[CLIENT_DATE_SIZE],start_time[CLIENT_TIME_SIZE];
	char	today_date[CLIENT_DATE_SIZE],today_time[CLIENT_TIME_SIZE];
	int	registered_users;
	int	http_hits;
	int	input_now,input_max;
	int	output_now,output_max;
	int	users_now,users_max;
	int	games_now,games_max;
	int	blitz_rating;
	float	blitz_average,blitz_stddev;
	int	standard_rating;
	float	standard_average,standard_stddev;
} t_upstate;


typedef	struct	{
	int	no;
	int	timestamp[CLIENT_HISTORY_SIZE];
	int	rated[CLIENT_HISTORY_SIZE];
	int	disk_diff[CLIENT_HISTORY_SIZE];
	client_type	end[CLIENT_HISTORY_SIZE];
	int	black_rating[CLIENT_HISTORY_SIZE];
	char	black_user[CLIENT_HISTORY_SIZE][CLIENT_NAME_SIZE];
	int	black_time[CLIENT_HISTORY_SIZE];
	int	black_inc[CLIENT_HISTORY_SIZE];
	int	black_def[CLIENT_HISTORY_SIZE];
	int	white_rating[CLIENT_HISTORY_SIZE];
	char	white_user[CLIENT_HISTORY_SIZE][CLIENT_NAME_SIZE];
	int	white_time[CLIENT_HISTORY_SIZE];
	int	white_inc[CLIENT_HISTORY_SIZE];
	int	white_def[CLIENT_HISTORY_SIZE];
	float	komi[CLIENT_HISTORY_SIZE];
	int	rand[CLIENT_HISTORY_SIZE];
} t_ghistory;


typedef struct 	{
	int	no;
	char	date[CLIENT_HISTORY_SIZE][CLIENT_DATE_SIZE];
	char	time[CLIENT_HISTORY_SIZE][CLIENT_DATE_SIZE];
	client_type	in_out[CLIENT_HISTORY_SIZE];
	char	user[CLIENT_HISTORY_SIZE][CLIENT_NAME_SIZE];
	int	blitz_rating[CLIENT_HISTORY_SIZE];
	int	standard_rating[CLIENT_HISTORY_SIZE];
	char	host[CLIENT_HISTORY_SIZE][CLIENT_HOST_SIZE];
} t_lhistory;


typedef struct 	{
	char	file[CLIENT_CHAT_SIZE];
} t_mail;


typedef struct 	{
	char	file[CLIENT_CHAT_SIZE];
} t_help;


typedef struct 	{
	int	dummy;
} t_exit;


typedef	struct	{
	int	no;
	char	list[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
	client_type	color[CLIENT_LIST_SIZE];
} t_stored;


typedef struct 	{
	int	no;
	char	user[CLIENT_NAME_SIZE];
	char	list[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
} t_filter;


typedef struct 	{
	int	no;
	char	user1[CLIENT_NAME_SIZE];
	char	user2[CLIENT_NAME_SIZE];
	char	list1[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
	char	list2[CLIENT_LIST_SIZE][CLIENT_NAME_SIZE];
} t_automail;


typedef struct 	{
	int	dummy;
} t_break;


typedef	struct {
	int	blitz_old,blitz_now;
	int	stand_old,stand_now;
} t_about;


typedef struct {
	int	users;
	char	user[CLIENT_REMOTE_SIZE][CLIENT_NAME_SIZE];
	client_type	status[CLIENT_REMOTE_SIZE];
	int	from;
	char	text[CLIENT_CHAT_SIZE];
} t_remote;


typedef struct {
	char	text[CLIENT_CHAT_SIZE];
} t_internal;


typedef struct {
	char 	login[CLIENT_NAME_SIZE];
	char	password[CLIENT_NAME_SIZE];
	char	host[CLIENT_HOST_SIZE];
	int	port;
	client_type	state;
  	client_type 	put_type;
  	client_type 	get_type;
	int	socket;
	char 	buffer[CLIENT_BUFFER_SIZE];
	int  	buffer_index;
	int  	buffer_size;
	char 	line[CLIENT_LINE_SIZE];
	int  	line_display;
	int  	line_index;
	char 	line_ready;
	int  	analyse;
	int  	input;
	int  	echo;
	int  	prompt;
	int  	login_now;
	int	game_now;
	int	connect;
} t_client;    	

extern t_match          v_match;
extern t_create         v_create;
extern t_end	        v_end;
extern t_who	        v_who;
extern t_players	v_players;
extern t_games	        v_games;
extern t_board	        v_board;
extern t_moves	        v_moves;
extern t_move	        v_move;
extern t_accept	        v_accept;
extern t_decline	v_decline;
extern t_finger	        v_finger;
extern t_set	        v_set;
extern t_kill	        v_kill;
extern t_rank	        v_rank;
extern t_assess	        v_assess;
extern t_abort	        v_abort;
extern t_resign	        v_resign;
extern t_moretime	v_moretime;
extern t_observe	v_observe;
extern t_whoobs	        v_whoobs;
extern t_yell 	        v_yell;
extern t_tell	        v_tell;
extern t_say       	v_say;
extern t_kibitz	        v_kibitz;
extern t_whisper	v_whisper;
extern t_pio	        v_pio;
extern t_listen	        v_listen;
extern t_wholist	v_wholist;
extern t_upstate	v_upstate;
extern t_ghistory	v_ghistory;
extern t_lhistory	v_lhistory;
extern t_mail	        v_mail;
extern t_help	        v_help;
extern t_exit	        v_exit;
extern t_stored	        v_stored;
extern t_filter	        v_filter;
extern t_automail	v_automail;
extern t_break	        v_break;
extern t_about		v_about;
extern t_remote	        v_remote;
extern t_internal	v_internal;
extern t_client 	v_client;

void	client_INIT(void);
void 	client_CONNECT(void);
void	client_LOGON(void);
void 	client_DISCONNECT(void);
void	client_GET(void);
void 	client_PUT(void);
void 	client_PUT_STRING(char *s);
void	client_REMOTE(char *name,client_type status);

void	client_ASK(void);

void	client_NEW_GAME(void);
void	client_MAKE_MOVE(int move,client_type who);
int	client_TEST_MOVE(int move,client_type who);
void	client_TEST_BOARD(void);
void	client_TEST_MOVES(void);

/*  third party utilities */

typedef struct {
	int	dummy;
} t_challenge;

extern t_challenge	v_challenge;

void 	client_CHALLENGE(void);
int 	client_ACCEPT(void);


/*======================================================================*/



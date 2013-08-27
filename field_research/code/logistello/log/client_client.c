// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// changes:  static int -> int,  %i -> %d

/*======================================================================

   	File:	client.c

	Name:	(C) Igor Durdanovich 
	Mods:	Ieuan Willis and David J Summers
	
	Date:	Mar 96
	
========================================================================*/

/*
   Some System 5 machines don't have bcopy and bzero defined this below hack
   should cure the problem .... on other machines memcpy and memset aren't 
   defined ! it which case the below creates problems. Anyway if your compiler
   complines about memcpy and memset not being defined try deleting the
   following two lines ... they are non needed except on some screewy 
   machines ! DJS (zardoz)
*/

#define bcopy(A,B,C)	memcpy((B),(A),(C))
#define bzero(A,B)	memset((A),(int)0,(B))

#define SIGNAL_HANDLER
#define CLIENT

#include "client_header.h"

#ifdef SIGNAL_HANDLER
#include "client_sig.i"
#endif


#define toupper(C)(((C)>='a'&&(C)<='z')?(C)-'a'+'A':(C))

#define B CLIENT_BLACK
#define W CLIENT_WHITE
#define N CLIENT_EMPTY
#define R CLIENT_SPARE

static int v_init_board[100]={ 
 	 R,R,R,R,R,R,R,R,R,R,
 	 R,N,N,N,N,N,N,N,N,R,
 	 R,N,N,N,N,N,N,N,N,R,
 	 R,N,N,N,N,N,N,N,N,R,
 	 R,N,N,N,N,N,N,N,N,R,
 	 R,N,N,N,N,N,N,N,N,R,
	 R,N,N,N,N,N,N,N,N,R,
	 R,N,N,N,N,N,N,N,N,R,
	 R,N,N,N,N,N,N,N,N,R,
	 R,R,R,R,R,R,R,R,R,R };

static int v_test_board[100];

#undef R
#undef N
#undef W
#undef B


static void 	clean_line(void);

static void	echo_string(char *s)
{	
	int	i,l;
l=strlen(s);
for(i=0;i<l;i++)
	{
	putchar(s[i]);
	};
}

static void 	write_client(char *m)
{
	int	l=strlen(m),
	    	i=0,
		n,k;
for(;l>0;) 
	{
     	n=write(v_client.socket,&(m[i]),l);
      	if(n<=0) 
		{
	 	fprintf(stdout,"client write to <ios> failed\n");
	 	exit(1);
      		};
      	i+=n;
      	l-=n;
   	};
}

void read_stdin(void)
{
  int    	n;
  fd_set	readfds;
  struct timeval	timeout;
  if(!v_client.input) return;
  /*if(!v_client.prompt) return;*/
 READ: ;
  timeout.tv_sec=0;
  timeout.tv_usec=0;
  FD_ZERO(&readfds);
  FD_SET(0,&readfds);
  n=select(1,&readfds,NULL,NULL,&timeout);
  if(n<0&&errno!=EINTR)
    { 
      perror("select");
      exit(1);
    };
  if(n==0)
    return;
  if(!FD_ISSET(0,&readfds))
    return;
  fgets(v_internal.text,CLIENT_LINE_SIZE,stdin);
  n=strlen(v_internal.text);
  v_internal.text[n-1]='\0';
  printf("client <ios> internal command recieved.\n");
  v_client.state=CLIENT_NONE;
  v_client.get_type=CLIENT_INTERNAL;
}

static void read_client(void)
{
  int    	n;
  char	c;
  fd_set 	readfds;
  struct timeval 	timeout;
  if(v_client.line_ready) 	/* there is already a line */
    return;
 READ: ;
  timeout.tv_sec=0;
  timeout.tv_usec=0;
  FD_ZERO(&readfds);
  if(v_client.input) 
    FD_SET(0,&readfds);
  FD_SET(v_client.socket,&readfds);
  n=select(v_client.socket+1,&readfds,NULL,NULL,&timeout);
  if(n<0&&errno!=EINTR) 
    { 
      perror("select"); 
      exit(1);
    };
  if(n==0) 
    if(v_client.buffer_size==0) 
      return; 
    else 
      goto LINE;
  if(!FD_ISSET(v_client.socket,&readfds)) 
    if(v_client.buffer_size==0) 
      return; 
    else goto LINE;
  n=read(v_client.socket,&(v_client.buffer[v_client.buffer_size]),
	 CLIENT_BUFFER_SIZE-v_client.buffer_size-1);
  if(n<=0) 
    {
      fprintf(stdout,"client <ios> closed connection\n");
      exit(0);
    };
  v_client.buffer_size+=n;
 LINE: ;
  for(;;) 
    {
      c=v_client.line[v_client.line_index++]=v_client.buffer[v_client.buffer_index++];
      v_client.buffer_size--;
      if(c=='\r') 
	{
	  v_client.line_index--; /* Carriage Return Delete ?*/
	};
      if(c=='\n') 
	{ 
	READY: ;
	v_client.line[--v_client.line_index]='\0';
	v_client.line_ready=1;
	for(n=0; n < v_client.buffer_size; n++)
	  v_client.buffer[n]=v_client.buffer[n+v_client.buffer_index];
	v_client.buffer_index=0;
	return;
	};
      if(v_client.line_index >= CLIENT_LINE_SIZE) 
	{
	  fprintf(stdout,"client got line longer then %d chars\n",CLIENT_LINE_SIZE);
	  v_client.line_index--;
	  goto READY;
	};	
      if(v_client.buffer_size<=0) 
	{
	  v_client.line[v_client.line_index]='\0';
	  v_client.buffer_size=0;
	  v_client.buffer_index=0;
	  goto READ;
	};
    };
}

static void clean_line(void)
{
  v_client.line_ready=0;
  v_client.line_index=0;
  v_client.line_display=0;
  v_client.line[0]=0;
}

static void echo_line(void)
{
  char	string[256];
  
  if(!v_client.echo) 
    return;
  if(v_client.line_index<=v_client.line_display) 
    return; /* was commented out */
  if(v_client.line_ready) 
    {
      sprintf(string,"%s\n",
	      &(v_client.line[v_client.line_display]));
      echo_string(string);
    }
  else        
    {
      sprintf(string,"%s",
	      &(v_client.line[v_client.line_display]));
      echo_string(string);
    };
  v_client.line_display=v_client.line_index;
}


/* ================================================================== 

	Parse CLIENT information for client_get()

   ==================================================================*/

static void get_MATCH(void)
{
  static	int bl=0;
  char 	from[16],color[16],type[16],game[16],spare[16];
  int i;

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;

  switch(bl) 
    { 
    case 0:
      bl++;
      if(v_client.line[13]=='s') 
	{
	  bl=0;
	  v_match.stored=1;
	  sscanf(&v_client.line[21],"%s %s %s",
		 from,v_match.user,game);
	  if(strcmp(from,"FROM")==0)
	    v_match.from=1;
	  else
	    v_match.from=0;
	  v_match.game_id=atoi(&game[1]);
	  clean_line();
	  v_client.get_type=CLIENT_MATCH;
	  v_client.state=CLIENT_NONE;
	  return;
	};
      for(i=18; v_client.line[i] != ')'; i++); 
      i++;
      sscanf(&v_client.line[i],"%d %d %d %s %s %d %d %d %s %s %s %s",
	     &v_match.my_time,
	     &v_match.my_inc,
	     &v_match.my_def,
	     from,
	     v_match.user,
	     &v_match.op_time,
	     &v_match.op_inc,
	     &v_match.op_def,
	     color,type,game,spare);
      if(strcmp(from,"FROM")==0)
	v_match.from=1;
      else
	v_match.from=0;
      if(strcmp(color,"white*")==0)
	{
	  v_match.my_color=CLIENT_WHITE;
	  v_match.random_color=1;
	};
      if(strcmp(color,"white")==0)
	{
	  v_match.my_color=CLIENT_WHITE;
	  v_match.random_color=0;
	};
      if(strcmp(color,"black*")==0)
	{
	  v_match.my_color=CLIENT_BLACK;
	  v_match.random_color=1;
	};
      if(strcmp(color,"black")==0)
	{
	  v_match.my_color=CLIENT_WHITE;
	  v_match.random_color=1;
	};
      if(strcmp(color,"color?")==0)
	{
	  v_match.my_color=CLIENT_KOMI;
	  v_match.random_color=0;
	};
      v_match.komi=0.0;
      v_match.rand=0;
      if(type[0]=='K')
	{
	  v_match.komi=atof(&type[1]);
	};
      if(type[0]=='R')
	{
	  v_match.rand=atoi(&type[1]);
	};
      if(game[0]=='(')
	{
	  v_match.game_id=atoi(&game[1]);
	}
      else
	{
	  v_match.game_id=atoi(&spare[1]);
	  v_match.special=CLIENT_NONE;
	  if(strcmp(game,"bronstein")==0)
	    {
	      v_match.special=CLIENT_BRONSTEIN;
	    };
	};
      v_match.rated=(v_client.line[13]=='r');
      v_match.stored=0;
      clean_line();
      break; 
    case 1:
      bl=0;
      sscanf(&(v_client.line[15]),"%d .. %d %d %d .. %d",
	     &v_match.max_win,
	     &v_match.min_win,
	     &v_match.draw,
	     &v_match.min_loss,
	     &v_match.max_loss);
      clean_line();
      v_client.get_type=CLIENT_MATCH;
      v_client.state=CLIENT_NONE;
      return;
    };
  goto	AGAIN;
}

static void get_MATCH_TO(void)
{
}

static void get_BOARD_color(void)
{ 
  if(v_client.line[12]=='*') 
    v_board.turn_color=CLIENT_BLACK;
  if(v_client.line[12]=='O')                        
    v_board.turn_color=CLIENT_WHITE;
}

static void get_BOARD_line(int y)
{
  int i,x;
  for(x=1,i=31;i<46;i+=2,x++) 
    switch(v_client.line[i]) 
      {
      case '*':
	v_board.board[y*10+x]=CLIENT_BLACK; 
	break;
      case 'O':
	v_board.board[y*10+x]=CLIENT_WHITE; 
	break;
      case '-':
	v_board.board[y*10+x]=CLIENT_EMPTY; 
	break;
      default:
	fprintf(stdout,"client BOARD: %s\n",v_client.line); 
	exit(1);
      };
}

static void get_BOARD_time(void)
{
  int time,def,inc;
  time=((v_client.line[11]-'0')*10+(v_client.line[12]-'0'))*60*60+
    ((v_client.line[14]-'0')*10+(v_client.line[15]-'0'))*60+
    ((v_client.line[17]-'0')*10+(v_client.line[18]-'0'));
  inc=atoi(&v_client.line[20]);
  def=atoi(&v_client.line[24]);
  if(v_board.turn_color==CLIENT_WHITE) 
    {
      v_board.white_time=time; 
      v_board.white_def=def; 
      v_board.white_inc=inc;
    } 
  else 
    {
      v_board.black_time=time;
      v_board.black_def=def; 
      v_board.black_inc=inc;
    };
  time=((v_client.line[59]-'0')*10+(v_client.line[60]-'0'))*60*60+
    ((v_client.line[62]-'0')*10+(v_client.line[63]-'0'))*60+
    ((v_client.line[65]-'0')*10+(v_client.line[66]-'0'));
  inc=atoi(&v_client.line[68]);
  def=atoi(&v_client.line[72]);
  if(v_board.turn_color==CLIENT_WHITE) 
    {
      v_board.black_time=time; 
      v_board.black_def=def; 
      v_board.black_inc=inc;
    }
  else 
    {
      v_board.white_time=time;
      v_board.white_def=def; 
      v_board.white_inc=inc;
    };
}

static void get_BOARD_last(void)
{
  int 	i;
  v_board.last_move_no=(v_client.line[52]-'0')*10+(v_client.line[53]-'0');
  if(toupper(v_client.line[57])=='P') 
    { 
      v_board.last_move=CLIENT_PASS; 
      return; 
    }
  else
    {
      v_board.last_move=
	toupper(v_client.line[57])-'A'+1
	+10*(v_client.line[58]-'0');
    };
}

static void get_BOARD_turn(void)
{ 
  if(v_client.line[0]=='Y') 
    v_board.turn_my=1;
  else
    v_board.turn_my=0;
}

static void get_BOARD_value(void)
{
  v_board.last_eval=atof(&(v_client.line[70]));
}

static void get_BOARD_players(void)
{
  if(v_board.turn_color==CLIENT_WHITE)
    {
      sscanf(&v_client.line[59],"%8s",v_board.black_user);
      sscanf(&v_client.line[11],"%8s",v_board.white_user);
      v_board.black_rating=atoi(&v_client.line[52]);
      v_board.white_rating=atoi(&v_client.line[4]);
    }
  else
    {
      sscanf(&v_client.line[11],"%8s",v_board.black_user);
      sscanf(&v_client.line[59],"%8s",v_board.white_user);
      v_board.black_rating=atoi(&v_client.line[4]);
      v_board.white_rating=atoi(&v_client.line[52]);
    };
}

static void get_BOARD_disks(void)
{
  if(v_board.turn_color==CLIENT_WHITE)
    {
      v_board.black_disks=atoi(&v_client.line[59]);
      v_board.white_disks=atoi(&v_client.line[11]);
    }
  else
    {
      v_board.black_disks=atoi(&v_client.line[11]);
      v_board.white_disks=atoi(&v_client.line[59]);
    };
}

static void get_BOARD_moves(void)
{
  v_board.moves=atoi(&v_client.line[11]);
}

static void get_BOARD_move(int idx,int off)
{
  char	c1,c2;
  if((idx+off)<v_board.moves)
    {
      sscanf(&v_client.line[off*3+3],"%c%c",&c1,&c2);
      v_board.move[idx+off]=(toupper(c1)-'A'+1)+10*(c2-'0');
    };
}

static void get_BOARD_whom(void)
{     
  int	i;
  char	s1[16],s2[16],s3[16],komi[16],rand[16];
  sscanf(&v_client.line[20],"%d",&v_board.game_id);
  sscanf(&v_client.line[25],"%s %s %s %s %s",s1,s2,s3,komi,rand);

  v_board.komi=atof(&komi[1]);  // !!!! was [1]
  v_board.rand=atoi(&rand[1]);
}

static void get_BOARD(void)
{
  int bl=0;
  int off;

 AGAIN: ;

  read_client();
  echo_line();  /*    USER: leave comments out if you want echo */
  if(!v_client.line_ready) 
    return;
  switch(bl) 
    {
    case 0:
      bl++;
      get_BOARD_whom();
      memcpy(v_board.board,v_init_board,sizeof(v_init_board));
      break;
    case 1:
      bl++; 
      break;
    case 2:
      bl++; 
      get_BOARD_color(); 
      break;
    case 3:
      bl++; 
      get_BOARD_line(1); 
      get_BOARD_players();
      break;
    case 4:
      bl++; 
      get_BOARD_line(2); 
      get_BOARD_time(); 
      break;
    case 5:
      bl++; 
      get_BOARD_line(3); 
      get_BOARD_disks();
      break;
    case 6:
      bl++; 
      get_BOARD_line(4); 
      get_BOARD_moves();
      break;
    case 7:
      bl++; 
      get_BOARD_line(5); 
      for(off=0;off<8;off++)
	get_BOARD_move(0,off);
      break;
    case 8:
      bl++; 
      get_BOARD_line(6); 
      for(off=0;off<8;off++)
	get_BOARD_move(8,off);
      break;
    case 9:
      bl++; 
      get_BOARD_line(7); 
      for(off=0;off<8;off++)
	get_BOARD_move(16,off);
      break;
    case 10: 
      bl++; 
      get_BOARD_line(8); 
      for(off=0;off<8;off++)
	get_BOARD_move(24,off);
      break;
    case 11: 
      bl++;
      get_BOARD_last(); 
      get_BOARD_turn(); 
      get_BOARD_value(); 
      bl=0; 
      clean_line();
      v_client.get_type=CLIENT_BOARD; 
      v_client.state=CLIENT_NONE;
      v_client.game_now=1;
      if (v_client.analyse) v_board.turn_my = 1; // analyse!!! 
      printf(">>>>>> TURN_MY=%d\n", v_board.turn_my);
      return;
    }
  clean_line();
  goto AGAIN;
}

static void get_MOVES_whom(void)
{     
  int	i,n;
  char	name[16];
  i=20;
  v_moves.game_id=atoi(&(v_client.line[i]));
  i=26;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(name,v_moves.black_user);
  i+=5;
  for(;v_client.line[i]!='\0';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n-1]='\0';
  strcpy(name,v_moves.white_user);
}

static void get_MOVES_start()
{
  if(v_client.line[3]=='(')
    v_moves.start_color=CLIENT_WHITE;
  if(v_client.line[3]=='#')
    v_moves.start_color=CLIENT_BLACK;
}

static void get_MOVES_line(int n)
{
  int	i,c;
  i=4;
  for(c=1;c<9;c++)
    {
      if(v_client.line[i]=='#')
	{
	  v_moves.what[n*10+c]=CLIENT_BLACK;
	}
      else
	{
	  if(v_client.line[i]==')')
	    {
	      v_moves.what[n*10+c]=CLIENT_WHITE;
	    }
	  else
	    {
	      if(v_client.line[i]==' ')
		{
		  v_moves.what[n*10+c]=CLIENT_NONE;
		}
	      else
		{
		  v_moves.what[n*10+c]=CLIENT_MOVE;
		  v_moves.turn[n*10+c]=
		    atoi(&(v_client.line[i-1]));
		  v_moves.no++;
		};
	    };
	};
      i+=3;
    };
}

static void get_MOVES_last(void)
{     
  int	i;
  v_moves.black_disks=atoi(&(v_client.line[8]));
  v_moves.white_disks=atoi(&(v_client.line[11]));
  v_moves.komi=atof(&(v_client.line[15]));
  v_moves.rand=atoi(&(v_client.line[22]));
}

static void get_MOVES(void)
{
  int 	ml=0;
  int	i;

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml) 
    {
    case 0:
      ml++;
      get_MOVES_whom();
      v_moves.no=0;
      for(i=0;i<100;i++)
	{
	  v_moves.what[i]=CLIENT_SPARE;
	  v_moves.turn[i]=0;
	};
      clean_line();
      break;
    case 1:
      get_MOVES_start();
      ml++;
      clean_line();
      break;
    case 2:
    case 3:
      ml++; 
      clean_line(); 
      break;
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10: 
    case 11: 
      ml++; 
      get_MOVES_line(ml-4); 
      clean_line(); 
      break;
    case 12:
    case 13: 
      ml++; 
      clean_line(); 
      break;
    case 14: 
      get_MOVES_last();
      ml=0; 
      clean_line();
      v_client.get_type=CLIENT_MOVES;
      v_client.state=CLIENT_NONE;
      return;
    };
  goto AGAIN;
}

static void get_PMOVES()
{
  int	i,sq,x,y,c;
  char	c1,c2,s[256];
  v_moves.game_id=atoi(&(v_client.line[9]));
  sscanf(&(v_client.line[13]),"%s",v_moves.black_user);
  sscanf(&(v_client.line[22]),"%s",v_moves.white_user);
  v_moves.komi=atof(&(v_client.line[31]));
  v_moves.rand=atoi(&(v_client.line[38]));
  v_moves.no=0;
  for(i=0;i<100;i++) {
    v_moves.what[i]=CLIENT_SPARE;
    v_moves.turn[i]=0;
  };
  i=41;sq=0;c=0;
  while(v_client.line[i]!='\0')
    {
      y=(sq/8)+1;x=(sq%8)+1;
      c1=v_client.line[i];c2=v_client.line[i+1];
      if(c2=='#')
	{
	  v_moves.what[y*10+x]=CLIENT_BLACK;
	  c++;
	}
      else
	{
	  if(c2==')')
	    {
	      v_moves.what[y*10+x]=CLIENT_WHITE;
	      c++;
	    }
	  else
	    {
	      if(c2==' ')
		{
		  v_moves.what[y*10+x]=CLIENT_NONE;
		}
	      else
		{
		  v_moves.what[y*10+x]=CLIENT_PLAY;
		  v_moves.turn[y*10+x]=(c2-'0');
		  if(c1!=' ')
		    v_moves.turn[y*10+x]+=(c1-'0')*10;
		  v_moves.no++;
		};
	    };
	};
      sq++;
      i+=2;
    };
  client_TEST_MOVES();
  sprintf(s,"<ios> PMOVES of GAME %3i, (%s vs. %s)\n",
	  v_moves.game_id,
	  v_moves.black_user,v_moves.white_user);
  echo_string(s);
  if(c&1)
    v_moves.start_color=CLIENT_WHITE;
  else
    v_moves.start_color=CLIENT_BLACK;
  switch(v_moves.start_color)
    {
    case CLIENT_BLACK:
      sprintf(s,"-> ##\n");
      echo_string(s);
      break;
    case CLIENT_WHITE:
      sprintf(s,"-> ()\n");
      echo_string(s);
      break;
    };
  sprintf(s,"\n   A  B  C  D  E  F  G  H\n");
  echo_string(s);
  for(y=1;y<9;y++)
    {
      sprintf(s,"%d ",y);
      echo_string(s);
      for(x=1;x<9;x++)
	{
	  switch(v_moves.what[y*10+x])
	    {
	    case CLIENT_NONE:
	      sprintf(s,"|  ");
	      echo_string(s);
	      break;
	    case CLIENT_BLACK:
	      sprintf(s,"|##");
	      echo_string(s);
	      break;
	    case CLIENT_WHITE:
	      sprintf(s,"|()");
	      echo_string(s);
	      break;
	    case CLIENT_PLAY:
	      sprintf(s,"|%2i",v_moves.turn[y*10+x]);
	      echo_string(s);
	      break;
	    };
	};
      sprintf(s,"| %d\n",y);
      echo_string(s);
    };
  sprintf(s,"   A  B  C  D  E  F  G  H\n\n");
  echo_string(s);
  sprintf(s,"##:() - %2i:%2i K%+5.2f R%d\n",
	  v_moves.black_disks,v_moves.white_disks,
	  v_moves.komi,v_moves.rand);
  echo_string(s);
  clean_line();
  v_client.get_type=CLIENT_MOVES;
  v_client.state=CLIENT_NONE;
}

static void get_LMOVES(void)
{
  clean_line();
  v_client.get_type=CLIENT_NONE;
  v_client.state=CLIENT_NONE;
}

static void get_CREATE(void)
{
  int 	i,n;
  char	name[80];
  echo_line();  
  for(i=0; v_client.line[i]!='(';i++)
    {
    };
  n=0;i++;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(v_create.black,name);
  i+=5;
  n=0;
  for(;v_client.line[i]!=')';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]=v_client.line[i];
  i++;
  n++;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n-1]='\0';
  strcpy(v_create.white,name);
  for(;v_client.line[i]==' ';i++)
    {
    };
  v_create.rated=(v_client.line[i]=='r');
  for(;v_client.line[i]!='K';i++)
    {
    };
  i++;
  v_create.komi=atof(&(v_client.line[i]));
  for(;v_client.line[i]=='R';i++)
    {
    };
  i++;
  v_create.komi=atoi(&(v_client.line[i]));
  v_client.get_type=CLIENT_CREATE;
  //  v_client.analyse=0;  // deleted !!!
  clean_line();
  v_client.state=CLIENT_NONE;
  v_client.game_now=1;
}

static void get_END(void)
{
  int	i,n;
  char	name[80];
  echo_line(); 
  for(i=0;v_client.line[i]!='(';i++)
    {
    };
  i++;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(v_end.black,name);
  i+=5;n=0;
  for(;v_client.line[i]!=')';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]=v_client.line[i];
  i++;
  n++;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n-1]='\0';
  strcpy(v_end.white,name);
  v_end.rated=(v_client.line[i]=='r');
  for(;v_client.line[i]!='-';i++)
    {
    };
  i++;n=0;
  for(;v_client.line[i]!=']';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  if(strstr(name,"wins on time")!=NULL)
    v_end.end=CLIENT_TIMEOUT;
  if(strstr(name,"mutual abort")!=NULL)
    v_end.end=CLIENT_ABORT;
  if(strstr(name,"disconnected")!=NULL)
    v_end.end=CLIENT_DISCONNECT;
  if(strstr(name,"resigned")!=NULL)
    v_end.end=CLIENT_RESIGN;
  if(strstr(name,"ok")!=NULL)
    {
      v_end.end=CLIENT_OKAY;
      sscanf(name,"%d : %d = %d K%f R%d",
	     &v_end.black_disks,
	     &v_end.white_disks,
	     &v_end.disks_diff,
	     &v_end.komi,
	     &v_end.rand);
    };
  if(strstr(name,"def")!=NULL)
    {
      v_end.end=CLIENT_DEFAULT;
      sscanf(name,"%d : %d = %d K%f R%d def]",
	     &v_end.black_disks,
	     &v_end.white_disks,
	     &v_end.disks_diff,
	     &v_end.komi,
	     &v_end.rand);
    };
  v_client.get_type=CLIENT_END;
  clean_line();
  v_client.state=CLIENT_NONE;
  v_client.game_now=0;
}

char cell(int i)
{
  char	c;
  switch(v_board.board[i])
    {
    case(CLIENT_BLACK):
      c='*';
      break;
    case(CLIENT_WHITE):
      c='O';
      break;
    default:
      c='-';
      break;
    };
  return c;
}

void row(int i)
{
  char	s[255];
  sprintf(s,"%1i %c %c %c %c %c %c %c %c %1i",
	  i,cell(i*10+1),cell(i*10+2),cell(i*10+3),cell(i*10+4),
	  cell(i*10+5),cell(i*10+6),cell(i*10+7),cell(i*10+8),i);
  echo_string(s);
}

void play(int i)
{
  char	s[255];
  if(i>=v_board.moves)
    echo_string("   ");
  else
    {
      sprintf(s,"%c%c ",
	      (v_board.move[i]%10)-1+'A',
	      v_board.move[i]/10+'0');
      echo_string(s);
    };
}

void last(int i)
{
  char	s[255];
  if(i==CLIENT_PASS)
    echo_string("PA");
  else
    {
      sprintf(s,"%c%c",(i%10)-1+'A',i/10+'0');
      echo_string(s);
    };
}

static void get_PBOARD(void)
{
  char	s[255];
  int  	time;
  int 	i,y,x,yx;
  int	t;
  /*echo_line();*/

  memcpy(v_board.board,v_init_board,sizeof(v_init_board));
  v_board.game_id=atoi(&(v_client.line[9]));
  sscanf(&(v_client.line[13]),"%s",v_board.black_user);
  sscanf(&(v_client.line[22]),"%s",v_board.white_user);
  v_board.black_time=atoi(&(v_client.line[31]));
  v_board.white_time=atoi(&(v_client.line[36]));
  v_board.black_def=atoi(&(v_client.line[41]));
  v_board.white_def=atoi(&(v_client.line[45]));
  v_board.black_inc=atoi(&(v_client.line[49]));
  v_board.white_inc=atoi(&(v_client.line[53]));
  if(v_client.line[57]=='O')
    v_board.turn_color=CLIENT_WHITE;
  if(v_client.line[57]=='*')
    v_board.turn_color=CLIENT_BLACK;
  if(v_client.line[59]=='Y')
    v_board.turn_my=1;
  else
    v_board.turn_my=0;
  v_board.last_move_no=atoi(&(v_client.line[61]));
  v_board.last_move=atoi(&(v_client.line[64]));
  v_board.last_eval=atof(&(v_client.line[67]));
  v_board.black_disks=0;
  v_board.white_disks=0;
  i=78;
  for(y=1;y<9;y++)
    for(x=1;x<9;x++) 
      {
	yx=x+y*10;
	switch(v_client.line[i++]) 
	  {
	  case 'O':
	    v_board.board[yx]=CLIENT_WHITE; 
	    v_board.white_disks++; 
	    break;
	  case '*':
	    v_board.board[yx]=CLIENT_BLACK; 
	    v_board.black_disks++; 
	    break;
	  case '-':
	    v_board.board[yx]=CLIENT_EMPTY; 
	    break;
	  default:
	    printf("[client]: <ios> message corrupt\n"); 	
	    exit(1);
	  };
      };

  v_board.komi=atof(&(v_client.line[143]));  // !!! was [144]
  v_board.rand=atoi(&(v_client.line[150]));
  v_board.black_rating=atoi(&(v_client.line[153]));
  v_board.white_rating=atoi(&(v_client.line[158]));
  client_TEST_BOARD();
  v_client.get_type=CLIENT_BOARD; 

  if (v_client.analyse) v_board.turn_my = 1; // analyse !!!
  printf(">>>>>> TURN_MY=%d\n", v_board.turn_my);

  sprintf(s,"<ios> PB of GAME %3i (%s vs. %s) K%+5.2f R%d\n",
	  v_board.game_id,v_board.black_user,v_board.white_user,
	  v_board.komi,v_board.rand);
  echo_string(s);
  if(v_board.turn_color==CLIENT_BLACK)
    {
      sprintf(s,"  BLACK   (*)                 A B C D E F G H     WHITE   (O)\n");
      echo_string(s);
      sprintf(s,"  [%4i]  %-8s          ",
	      v_board.black_rating,v_board.black_user);
      echo_string(s);
      row(1);
      sprintf(s,"   [%4i]  %-8s\n",
	      v_board.white_rating,v_board.white_user);
      echo_string(s);
      sprintf(s,"  time  : %02i:%02i:%02i %3i %3i  ",
	      v_board.black_time/3600,(v_board.black_time/60)%60,
	      v_board.black_time%60,v_board.black_inc,v_board.black_def);
      echo_string(s);
      row(2);
      sprintf(s,"   time  : %02i:%02i:%02i %3i %3i\n",
	      v_board.white_time/3600,(v_board.white_time/60)%60,
	      v_board.white_time%60,v_board.white_inc,v_board.white_def);
      echo_string(s);
      sprintf(s,"  discs : %2i                ",v_board.black_disks);
      echo_string(s);
      row(3);
      sprintf(s,"   discs : %2i\n",v_board.white_disks);
      echo_string(s);
      sprintf(s,"  moves : %2i                ",v_board.moves);
      echo_string(s);
      row(4);
      echo_string("\n");
      echo_string("  ");
      play( 0);play( 1);play( 2);play( 3);
      play( 4);play( 5);play( 6);play( 7);
      echo_string("  ");	
      row(5);
      sprintf(s,"   total : %2i\n",v_board.black_disks+v_board.white_disks);
      echo_string(s);
      echo_string("  ");
      play( 8);play( 9);play(10);play(11);
      play(12);play(13);play(14);play(15);	
      echo_string("  ");	
      row(6);
      sprintf(s,"   empty : %2i\n",64-v_board.black_disks-v_board.white_disks);
      echo_string(s);
      echo_string("  ");
      play(16);play(17);play(18);play(19);
      play(20);play(21);play(22);play(23);
      echo_string("  ");		
      row(7);
      if(v_match.stored)
	sprintf(s,"   Stored Game\n");
      else
	sprintf(s,"   %+i .. %+i %+i %+i .. %+i\n",
		v_match.max_win,v_match.min_win,
		v_match.draw,
		v_match.min_loss,v_match.max_loss);
      echo_string(s);
      echo_string("  ");
      play(24);play(25);play(26);play(27);
      play(28);play(29);play(30);play(31);
      echo_string("  ");	
      row(8);
      echo_string("\n");
      if(v_client.get_type==CLIENT_BOARD)
	{
	  if(v_board.turn_my)
	    echo_string("YOUR TURN      ");
	  else
	    echo_string("OPPONENT'S TURN");
	}
      else
	echo_string("               ");
      sprintf(s,"               A B C D E F G H     [%02i]: ",
	      v_board.last_move_no);
      echo_string(s);
      last(v_board.last_move);
      sprintf(s,"  %02i:%02i:%02i   ",time/3600,(time/60)%60,time%60);
      echo_string(s);
      sprintf(s,"%+5.2f\n",v_board.last_eval);
      echo_string(s);
    }
  else
    {
      sprintf(s,"  WHITE   (O)                 A B C D E F G H     BLACK   (*)\n");
      echo_string(s);
      sprintf(s,"  [%4i]  %-8s          ",
	      v_board.white_rating,v_board.white_user);
      echo_string(s);
      row(1);
      sprintf(s,"   [%4i]  %-8s\n",
	      v_board.black_rating,v_board.black_user);
      echo_string(s);
      sprintf(s,"  time  : %02i:%02i:%02i %3i %3i  ",
	      v_board.white_time/3600,(v_board.white_time/60)%60,
	      v_board.white_time%60,v_board.white_inc,v_board.white_def);
      echo_string(s);
      row(2);
      sprintf(s,"   time  : %02i:%02i:%02i %3i %3i\n",
	      v_board.black_time/3600,(v_board.black_time/60)%60,
	      v_board.black_time%60,v_board.black_inc,v_board.black_def);
      echo_string(s);
      sprintf(s,"  discs : %2i                ",v_board.white_disks);
      echo_string(s);
      row(3);
      sprintf(s,"   discs : %2i\n",v_board.black_disks);
      echo_string(s);
      sprintf(s,"  moves : %2i                ",v_board.moves);
      echo_string(s);
      row(4);
      echo_string("\n");
      echo_string("  ");
      play( 0);play( 1);play( 2);play( 3);
      play( 4);play( 5);play( 6);play( 7);
      echo_string("  ");	
      row(5);
      sprintf(s,"   total : %2i\n",v_board.black_disks+v_board.white_disks);
      echo_string(s);
      echo_string("  ");
      play( 8);play( 9);play(10);play(11);
      play(12);play(13);play(14);play(15);	
      echo_string("  ");	
      row(6);
      sprintf(s,"   empty : %2i\n",64-v_board.black_disks-v_board.white_disks);
      echo_string(s);
      echo_string("  ");
      play(16);play(17);play(18);play(19);
      play(20);play(21);play(22);play(23);
      echo_string("  ");		
      row(7);
      if(v_match.stored)
	sprintf(s,"   Stored Game\n");
      else
	sprintf(s,"   %+i .. %+i %+i %+i .. %+i\n",
		v_match.max_win,v_match.min_win,
		v_match.draw,
		v_match.min_loss,v_match.max_loss);
      echo_string(s);
      echo_string("  ");
      play(24);play(25);play(26);play(27);
      play(28);play(29);play(30);play(31);
      echo_string("  ");	
      row(8);
      echo_string("\n");
      if(v_client.get_type==CLIENT_BOARD)
	{
	  if(v_board.turn_my)
	    echo_string("YOUR TURN      ");
	  else
	    echo_string("OPPONENT'S TURN");
	}
      else
	echo_string("               ");
      sprintf(s,"               A B C D E F G H     [%02i]: ",
	      v_board.last_move_no);
      echo_string(s);
      last(v_board.last_move);
      sprintf(s,"  %02i:%02i:%02i   ",time/3600,(time/60)%60,time%60);
      echo_string(s);
      sprintf(s,"%+5.2f\n",v_board.last_eval);
      echo_string(s);	
    };
  clean_line();
  v_client.state=CLIENT_NONE;
  v_client.game_now=1;
}

static void get_WHO_line(int tst)
{
  int i;
  for(i=0;v_client.line[i]!=' ';i++)
    v_who.user[tst][i]=v_client.line[i];
  v_who.user[tst][i]=0;
  v_who.blitz_rating[tst]=atoi(&(v_client.line[10]));
  v_who.standard_rating[tst]=atoi(&(v_client.line[17]));
  v_who.registered[tst]=(v_client.line[15]=='+'?1:0);
  if(v_client.line[25]=='-')
    {
      v_who.playing[tst]=0;
    }
  else
    {
      v_who.playing[tst]=1;
      v_who.game_id[tst]=atoi(&(v_client.line[23]));
    };
  v_who.open[tst]=(v_client.line[27]=='-'?0:1);
  v_who.rated[tst]=(v_client.line[27]=='r'||v_client.line[27]=='R'?1:0);
  v_who.trust[tst]=(v_client.line[27]=='R'||v_client.line[27]=='U'?1:0);  
  sscanf(&(v_client.line[29]),"%s",&(v_who.idle[tst][0]));
  sscanf(&(v_client.line[36]),"%s",&(v_who.onfor[tst][0]));
  sscanf(&(v_client.line[43]),"%s",&(v_who.host[tst][0]));
} 

static void get_WHO(void)
{
  int 	ml=0,tst=0;

 AGAIN:;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_who.sort=v_client.line[4];
      v_who.no=atoi(&(v_client.line[7]));
      clean_line();
      tst=0;
      break;
    case 1:
    case 2:
      ml++;
      clean_line();
      break;
    case 3:
      get_WHO_line(tst);
      clean_line();
      tst++;
      if(tst==v_who.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_WHO;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_PLAYERS_line(int tst)
{
  int i;
  for(i=0;v_client.line[i]!=' ';i++)
    v_players.user[tst][i]=v_client.line[i];
  v_players.user[tst][i]=0;
  v_players.blitz_rating[tst]=atoi(&(v_client.line[10]));
  v_players.standard_rating[tst]=atoi(&(v_client.line[17]));
  v_players.registered[tst]=(v_client.line[15]=='+'?1:0);
  v_players.rated[tst]=(v_client.line[23]=='r'||v_client.line[23]=='R'?1:0);
  v_players.trust[tst]=(v_client.line[23]=='R'||v_client.line[23]=='U'?1:0);  
  sscanf(&(v_client.line[25]),"%s",&(v_players.idle[tst][0]));
  sscanf(&(v_client.line[32]),"%s",&(v_players.onfor[tst][0]));
  sscanf(&(v_client.line[39]),"%s",&(v_players.host[tst][0]));
} 

static void get_PLAYERS(void)
{
  int 	ml=0,tst=0;

 AGAIN:;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_players.sort=v_client.line[8];
      v_players.no=atoi(&(v_client.line[11]));
      clean_line();
      tst=0;
      break;
    case 1:
    case 2:
      ml++;
      clean_line();
      break;
    case 3:
      get_PLAYERS_line(tst);
      clean_line();
      tst++;
      if(tst==v_players.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_WHO;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_FINGER(void)
{
  int 	ml=0;
  int	i,n;

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      i=7;n=0;
      for(;v_client.line[i]!=' ';i++)
	{
	  v_finger.user[n]=v_client.line[i];
	  n++;
	};
      v_finger.user[n]='\0';
      for(i=0;v_client.line[i]!='(';i++)
	{
	};
      i++;
      v_finger.registered=(v_client.line[i]=='r');
      clean_line();
      break;
    case 1:
      v_finger.finger=0;
      if(v_client.line[0]=='n')
	{
	  ml=2;
	  sprintf(v_finger.name,&(v_client.line[7]));
	  v_finger.finger=1;
	  clean_line();
	}
      else
	{
	  ml=3;
	  sprintf(v_finger.name,"");
	  sprintf(v_finger.email,"");
	  v_finger.finger=0;
	};
      break;
    case 2:
      ml++;
      sprintf(v_finger.email,&(v_client.line[7]));
      clean_line();
      break;
    case 3:
      ml++;
      clean_line();
      break;
    case 4:
      ml++;
      sscanf(&v_client.line[13],"%10s",v_finger.lastio_date);
      sscanf(&v_client.line[24],"%5s",v_finger.lastio_time);
      clean_line();
      break;
    case 5:
      ml++;
      sscanf(&v_client.line[13],"%10s",v_finger.registered_date);
      sscanf(&v_client.line[24],"%5s",v_finger.registered_time);
      clean_line();
      break;
    case 6:
    case 7:
    case 8:
      ml++;
      clean_line();
      break;
    case 9:
      ml++;
      v_finger.blitz_rating=atoi(&v_client.line[11]);
      v_finger.blitz_win=atoi(&v_client.line[17]);
      v_finger.blitz_loss=atoi(&v_client.line[23]);
      v_finger.blitz_draw=atoi(&v_client.line[29]);
      v_finger.blitz_avg=atoi(&v_client.line[35]);
      v_finger.blitz_total=atoi(&v_client.line[43]);
      v_finger.blitz_need=atoi(&v_client.line[51]);
      clean_line();
      break;
    case 10:
      ml++;
      v_finger.standard_rating=atoi(&v_client.line[11]);
      v_finger.standard_win=atoi(&v_client.line[17]);
      v_finger.standard_loss=atoi(&v_client.line[23]);
      v_finger.standard_draw=atoi(&v_client.line[29]);
      v_finger.standard_avg=atoi(&v_client.line[35]);
      v_finger.standard_total=atoi(&v_client.line[43]);
      v_finger.standard_need=atoi(&v_client.line[51]);
      clean_line();
      break;
    case 11:
      ml++;
      clean_line();
      break;
    case 12:
      ml++;
      v_finger.pio=atoi(&v_client.line[4]);
      v_finger.mail=atoi(&v_client.line[12]);
      v_finger.trust=atoi(&v_client.line[20]);
      v_finger.open=atoi(&v_client.line[29]);
      v_finger.bell=atoi(&v_client.line[36]);
      v_finger.unreg=atoi(&v_client.line[45]);
      clean_line();
      break;
    case 13:
      ml=0;
      v_finger.gio=atoi(&v_client.line[4]);
      v_finger.omail=atoi(&v_client.line[12]);
      v_finger.rated=atoi(&v_client.line[20]);
      v_finger.finger=atoi(&v_client.line[29]);
      v_finger.yell=atoi(&v_client.line[36]);
      v_finger.kibitz=atoi(&v_client.line[45]);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_FINGER;
      clean_line();
      return;
    };
  goto AGAIN;
}

static void get_RANK_line(void)
{
  if(v_client.line[1]!=' ')
    v_rank.blitz_offset=v_rank.no;
  v_rank.blitz_rank[v_rank.no]=atoi(&(v_client.line[7]));
  v_rank.blitz_rating[v_rank.no]=atoi(&(v_client.line[11]));
  sscanf(&(v_client.line[16]),"%s",&(v_rank.blitz_user[v_rank.no][0]));
  if(v_client.line[36]!=' ')
    v_rank.standard_offset=v_rank.no;
  v_rank.standard_rank[v_rank.no]=atoi(&(v_client.line[42]));
  v_rank.standard_rating[v_rank.no]=atoi(&(v_client.line[46]));
  sscanf(&(v_client.line[51]),"%s",&(v_rank.standard_user[v_rank.no][0]));
  v_rank.no++;
}

static void get_RANK(void)
{
  int 	i,ml=0;
  char	prompt[11];
  sprintf(prompt,"%s%% ",v_client.login);

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_rank.no=0;
      v_rank.blitz_offset=v_rank.standard_offset=-1;
      clean_line();
      break;
    case 1:
    case 2:
    case 3:
      ml++;
      clean_line();
      break;
    case 4:
      if(strncmp(v_client.line,prompt,strlen(prompt))==0)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  if(v_rank.blitz_offset>=0)
	    v_client.get_type=CLIENT_RANK;
	  else
	    v_client.get_type=CLIENT_TOP;
	  return;
	}
      get_RANK_line();
      clean_line();
      break;
    };
  goto AGAIN;
}

static void get_ASSESS(void)
{
  int 	ml=0;

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
    case 1:
      ml++;
      clean_line();
      break;
    case 2:
      ml++;
      v_assess.blitz_win=atoi(&v_client.line[20]);
      clean_line();
      break;
    case 3:
      ml++;
      v_assess.blitz_draw=atoi(&v_client.line[20]);
      clean_line();
      break;
    case 4:
      ml++;
      v_assess.blitz_loss=atoi(&v_client.line[20]);
      clean_line();
      break;
    case 5:
    case 6:
    case 7:
      ml++;
      clean_line();
      break;
    case 8:
      ml++;
      v_assess.standard_win=atoi(&v_client.line[20]);
      clean_line();
      break;
    case 9:
      ml++;
      v_assess.standard_draw=atoi(&v_client.line[20]);
      clean_line();
      break;
    case 10:
      ml=0;
      v_assess.standard_loss=atoi(&v_client.line[20]);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_ASSESS;
      clean_line();
      return;
    };
  goto AGAIN;
}

static void get_ABORT(void)
{
  if(strncmp(v_client.line,v_match.user,strlen(v_match.user))==0)
    {
      sprintf(v_abort.user,v_match.user);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_ABORT;
      clean_line();
    }
  else
    {
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_NONE;
    };
  return;
}

static void get_STORED_line(int tst)
{
  int 	i,n;
  char	name[9];
  i=0;n=0;
  for(;v_client.line[i]!='.';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  if(strcmp(name,v_client.login)!=0)
    {
      strcpy(&(v_stored.list[tst][0]),name);
      v_stored.color[tst]=CLIENT_WHITE;
      return;
    };
  i++;n=0;
  for(;v_client.line[i]!='\0';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  if(strcmp(name,v_client.login)!=0)
    {
      strcpy(&(v_stored.list[tst][0]),name);
      v_stored.color[tst]=CLIENT_BLACK;
    };
}

static void get_STORED(void)
{
  int 	ml=0,tst=0;
  char	name[9];

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_stored.no=atoi(&(v_client.line[8]));
      clean_line();
      tst=0;
      break;
    case 1:
      get_STORED_line(tst);
      clean_line();
      tst++;
      if(tst==v_stored.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_STORED;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_FILTER_line(int tst)
{
  int 	i,n;
  char	name[9];
  i=0;n=0;
  for(;v_client.line[i]!='\0';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_filter.list[tst][0]),name);
}

static void get_FILTER(void)
{
  int 	ml=0,tst=0;
  char	name[9];

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_filter.no=atoi(&(v_client.line[8]));
      clean_line();
      tst=0;
      break;
    case 1:
      get_FILTER_line(tst);
      clean_line();
      tst++;
      if(tst==v_filter.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_FILTER;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_AUTOMAIL_line(int tst)
{
  int 	i,n;
  char	name[9];
  i=0;n=0;
  for(;v_client.line[i]!='.';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  i++;
  strcpy(&(v_automail.list1[tst][0]),name);
  for(;v_client.line[i]!='\0';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_automail.list2[tst][0]),name);
}

static void get_AUTOMAIL(void)
{
  int 	ml=0,tst=0;
  char	name[9];

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_automail.no=atoi(&(v_client.line[10]));
      clean_line();
      tst=0;
      break;
    case 1:
      get_AUTOMAIL_line(tst);
      clean_line();
      tst++;
      if(tst==v_automail.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_AUTOMAIL;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_LHISTORY_line(int tst)
{
  int 	i,n;
  char	name[255];
  i=0;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_lhistory.date[tst][0]),name);
  i++;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_lhistory.time[tst][0]),name);
  i++;
  switch(v_client.line[i])
    {
    case 'I':
      v_lhistory.in_out[tst]=CLIENT_IN;
      break;
    case 'O':
      v_lhistory.in_out[tst]=CLIENT_OUT;
      break;
    };
  i+=4;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_lhistory.user[tst][0]),name);
  for(;v_client.line[i]!='[';i++)
    {
    };
  i++;
  v_lhistory.blitz_rating[tst]=atoi(&(v_client.line[i]));
  i+=7;
  v_lhistory.standard_rating[tst]=atoi(&(v_client.line[i]));
  i+=6;n=0;
  for(;v_client.line[i]!='\0';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_lhistory.host[tst][0]),name);
}

static void get_LHISTORY(void)
{
  int 	ml=0,tst=0;
  char	name[9];

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_lhistory.no=atoi(&(v_client.line[10]));
      clean_line();
      tst=0;
      break;
    case 1:
    case 2:
      ml++;
      clean_line();
      break;
    case 3:
      get_LHISTORY_line(tst);
      clean_line();
      tst++;
      if(tst==v_lhistory.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_LHISTORY;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_GHISTORY_line(int tst)
{
  int 	i,n;
  char	name[9];
  i=0;
  v_ghistory.timestamp[tst]=atoi(&(v_client.line[i]));
  i+=10;
  switch(v_client.line[i])
    {
    case 'r':
      v_ghistory.rated[tst]=1;
      break;
    case 'u':
      v_ghistory.rated[tst]=0;
      break;
    };
  i++;
  v_ghistory.disk_diff[tst]=atoi(&(v_client.line[i]));
  i+=3;
  switch(v_client.line[i])
    {
    case 'r':
      v_ghistory.end[tst]=CLIENT_RESIGN;
      break;
    case 'd':
      v_ghistory.end[tst]=CLIENT_DISCONNECT;
      break;
    case 't':
    case 'T':
      v_ghistory.end[tst]=CLIENT_TIMEOUT;
      break;
    case 'a':
      v_ghistory.end[tst]=CLIENT_ABORT;
      break;
    case 'e':
      v_ghistory.end[tst]=CLIENT_OKAY;
      break;
    };
  i=17;
  v_ghistory.black_rating[tst]=atoi(&(v_client.line[i]));
  i=22;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_ghistory.black_user[tst][0]),name);
  i=31;
  v_ghistory.black_time[tst]=atoi(&(v_client.line[i]));
  i=35;
  v_ghistory.black_inc[tst]=atoi(&(v_client.line[i]));
  i=39;
  v_ghistory.black_def[tst]=atoi(&(v_client.line[i]));
  i=43;
  v_ghistory.white_rating[tst]=atoi(&(v_client.line[i]));
  i=48;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_ghistory.white_user[tst][0]),name);
  i=57;
  v_ghistory.white_time[tst]=atoi(&(v_client.line[i]));
  i=61;
  v_ghistory.white_inc[tst]=atoi(&(v_client.line[i]));
  i=65;
  v_ghistory.white_def[tst]=atoi(&(v_client.line[i]));
  i=69;
  v_ghistory.komi[tst]=atoi(&(v_client.line[i]));
  i=76;
  v_ghistory.rand[tst]=atoi(&(v_client.line[i]));
}

static void get_GHISTORY(void)
{
  int 	ml=0,tst=0;
  char	name[9];

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_ghistory.no=atoi(&(v_client.line[10]));
      clean_line();
      tst=0;
      break;
    case 1:
    case 2:
      ml++;
      clean_line();
      break;
    case 3:
      get_GHISTORY_line(tst);
      clean_line();
      tst++;
      if(tst==v_ghistory.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_GHISTORY;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_GAMES_line(int tst)
{
  int 	i,n;
  char	name[9];
  i=0;
  v_games.game_no[tst]=atoi(&(v_client.line[i]));
  i=4;
  switch(v_client.line[i])
    {
    case 'u':
      v_games.rated[tst]=0;
      break;
    case 'r':
      v_games.rated[tst]=1;
      break;
    };
  i=6;
  v_games.disks[tst]=atoi(&(v_client.line[i]));
  i=11;
  v_games.black_rating[tst]=atoi(&(v_client.line[i]));

  i=16;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_games.black_user[tst][0]),name);
  i=25;
  v_games.black_time[tst]=atoi(&(v_client.line[i]));
  i=29;
  v_games.black_inc[tst]=atoi(&(v_client.line[i]));
  i=33;
  v_games.black_def[tst]=atoi(&(v_client.line[i]));
  i=38;
  v_games.white_rating[tst]=atoi(&(v_client.line[i]));
  i=43;n=0;
  for(;v_client.line[i]!=' ';i++)
    {
      name[n]=v_client.line[i];
      n++;
    };
  name[n]='\0';
  strcpy(&(v_games.black_user[tst][0]),name);
  i=52;
  v_games.white_time[tst]=atoi(&(v_client.line[i]));
  i=56;
  v_games.white_inc[tst]=atoi(&(v_client.line[i]));
  i=60;
  v_games.white_def[tst]=atoi(&(v_client.line[i]));
  i=64;
  v_games.komi[tst]=atof(&(v_client.line[i]));
  i=71;
  v_games.rand[tst]=atoi(&(v_client.line[i]));
}

static void get_GAMES(void)
{
  int 	ml=0,tst=0;
  char	name[9];

 AGAIN: ;

  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_games.no=atoi(&(v_client.line[10]));
      clean_line();
      tst=0;
      break;
    case 1:
    case 2:
      ml++;
      clean_line();
      break;
    case 3:
      get_GAMES_line(tst);
      clean_line();
      tst++;
      if(tst==v_games.no)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_GAMES;
	  return;
	}
      break;
    };
  goto AGAIN;
}

static void get_WHOOBS_line(void)
{
  strcpy(v_whoobs.user[v_whoobs.no],v_client.line);
  v_whoobs.no++;
}

static void get_WHOOBS(void)
{
  int 	ml=0;
  char	prompt[11];
  sprintf(prompt,"%s%% ",v_client.login);
 AGAIN: ;
  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_whoobs.no=0;
      clean_line();
      break;
    case 1:
      if(strncmp(v_client.line,prompt,strlen(prompt))==0)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_WHOOBS;
	  return;
	}
      get_WHOOBS_line();
      clean_line();
      break;
    };
  goto AGAIN;
}

static void get_WHOLIST_line(void)
{
  strcpy(v_wholist.user[v_wholist.no],v_client.line);
  v_wholist.no++;
}

static void get_WHOLIST(void)
{
  int 	ml=0;
  char	prompt[11];
  sprintf(prompt,"%s%% ",v_client.login);
 AGAIN: ;
  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      v_wholist.no=0;
      clean_line();
      break;
    case 1:
      if(strncmp(v_client.line,prompt,strlen(prompt))==0)
	{
	  ml=0;
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_WHOLIST;
	  return;
	};	
      get_WHOLIST_line();
      clean_line();
      break;
    };
  goto AGAIN;
}

static void get_UPSTATE(void)
{
  int 	ml=0;
 AGAIN: ;
  read_client();
  echo_line();  
  if(!v_client.line_ready) 
    return;
  switch(ml)
    {
    case 0:
      ml++;
      sscanf(&v_client.line[7],"%10s",v_upstate.start_date);
      sscanf(&v_client.line[18],"%5s",v_upstate.start_time);
      clean_line();
      break;
    case 1:
      ml++;
      sscanf(&v_client.line[7],"%10s",v_upstate.today_date);
      sscanf(&v_client.line[18],"%5s",v_upstate.today_time);
      clean_line();
      break;
    case 2:
      ml++;
      clean_line();
      break;
    case 3:
      ml++;
      v_upstate.registered_users=atoi(&v_client.line[0]);
      clean_line();
      break;
    case 4:
      ml++;
      clean_line();
      break;
    case 5:
      ml++;
      v_upstate.http_hits=atoi(&v_client.line[27]);
      clean_line();
      break;
    case 6:
      ml++;
      v_upstate.input_now=atoi(&v_client.line[27]);
      v_upstate.input_max=atoi(&v_client.line[38]);
      clean_line();
      break;
    case 7:
      ml++;
      v_upstate.output_now=atoi(&v_client.line[27]);
      v_upstate.output_max=atoi(&v_client.line[38]);
      clean_line();
      break;
    case 8:
      ml++;
      v_upstate.users_now=atoi(&v_client.line[27]);
      v_upstate.users_max=atoi(&v_client.line[38]);
      clean_line();
      break;
    case 9:
      ml++;
      v_upstate.games_now=atoi(&v_client.line[27]);
      v_upstate.games_max=atoi(&v_client.line[38]);
      clean_line();
      break;
    case 10:
      ml++;
      clean_line();
      break;
    case 11:
      ml++;
      v_upstate.blitz_rating=atoi(&v_client.line[20]);
      v_upstate.blitz_average=atof(&v_client.line[36]);
      v_upstate.blitz_stddev=atof(&v_client.line[54]);
      clean_line();
      break;
    case 12:
      ml=0;
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_UPSTATE;
      v_upstate.standard_rating=atoi(&v_client.line[20]);
      v_upstate.standard_average=atof(&v_client.line[36]);
      v_upstate.standard_stddev=atof(&v_client.line[54]);
      clean_line();
      return;
    };
  goto AGAIN;
}

static void get_THE_REST(void)
{
  int	i,n,m,l,s,b,c,g;
  char	name[100],mess[255],temp[512];
  l=strlen(v_client.line);
  echo_line();
  if((strncmp(v_client.line,"<ios> [",7)==0)&&
     (v_client.line[l-1]==']'))
    {
      i=7;n=0;
      while(v_client.line[i]!=' ')
	{
	  name[n]=v_client.line[i];
	  i++;n++;
	};
      name[n]='\0';
      i++;
      if(v_client.line[i]=='(')
	{
	  i++;
	  b=atoi(&v_client.line[i]);
	  while(v_client.line[i]!='/')
	    i++;
	  i++;
	  s=atoi(&v_client.line[i]);
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_NONE;
	  clean_line();
	  return;
	}
      else
	{
	  v_client.state=CLIENT_NONE;
	  v_client.get_type=CLIENT_NONE;
	  clean_line();
	  return;
	};
    };


  i=0;n=0;
  while(v_client.line[i]!=' ')
    {
      if(v_client.line[i]=='\0')
	goto SKIP;
      name[n]=v_client.line[i];
      i++;n++;
    };
  name[n]='\0';
  if(n<2) goto SKIP;
  i++;m=0;
  while(v_client.line[i]!='\0')
    {
      mess[m]=v_client.line[i];
      i++;m++;
    };
  mess[m]='\0';
  n--;
  if(name[n]==':')
    {
      name[n]='\0';
      strcpy(v_tell.user,name);
      v_tell.channel=-1;
      strcpy(v_tell.message,mess);
      for(i=0;i<v_remote.users;i++)
	{
	  if(strcmp(v_remote.user[i],v_tell.user)==0)
	    {
	      v_client.state=CLIENT_NONE;
	      v_client.get_type=CLIENT_REMOTE;
	      v_remote.from=i;
	      strcpy(v_remote.text,v_tell.message);
	      clean_line();
	      if(v_remote.status[i]==CLIENT_OPERATOR)
		printf("client <ios> operator command from %s recieved.\n"
		       ,v_remote.user[i]);
	      if(v_remote.status[i]==CLIENT_SUPERVISOR)
		printf("client <ios> supervisor command from %s recieved.\n"
		       ,v_remote.user[i]);
	      return;
	    };
	};    	
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_TELL;
      clean_line();
      return;
    };
  if(name[n]==']')
    {
      while(name[n]!='[')
	{
	  n--;
	  if(n<2) goto SKIP;
	};
      c=atoi(&name[n+1]);
      name[n]='\0';
      strcpy(v_tell.user,name);
      v_tell.channel=c;
      strcpy(v_tell.message,mess);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_TELL;
      clean_line();
      return;
    };
  if(name[n]==')')
    {
      while(name[n]!='(')
	{
	  n--;
	  if(n<2) goto SKIP;
	};
      g=atoi(&name[n+1]);
      name[n]='\0';
      strcpy(v_kibitz.user,name);
      v_kibitz.game=g;
      strcpy(v_kibitz.message,mess);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_KIBITZ;
      clean_line();
      return;
    };
  if(name[n]=='>')
    {
      while(name[n]!='<')
	{
	  n--;
	  if(n<2) goto SKIP;
	};
      g=atoi(&name[n+1]);
      name[n]='\0';
      printf("CLIENT whisper %s %d %s\n",name,g,mess);
      strcpy(v_whisper.user,name);
      v_whisper.game=g;
      strcpy(v_whisper.message,mess);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_WHISPER;
      clean_line();
      return;
    };
  if(isdigit(name[n]))
    {
      while(name[n]!='!')
	{
	  n--;
	  if(n<2) goto SKIP;
	};
      g=atoi(&name[n+1]);
      name[n]='\0';
      strcpy(v_yell.user,name);
      v_yell.no=g;
      strcpy(v_yell.message,mess);
      v_client.state=CLIENT_NONE;
      v_client.get_type=CLIENT_YELL;
      clean_line();
      return;
    };
 SKIP:;
  v_client.state=CLIENT_NONE;
  v_client.get_type=CLIENT_NONE;
  clean_line();
}

/* ================================================================== 

	Send CLIENT information for client_put()

   ==================================================================*/

static void put_NULL(void)
{
  char	buf[90];
  sprintf(buf,"tell %s",v_client.login);
  write_client(buf);
}

static void put_MOVE(void)
{
  char buf[90];
  char yx[4];
  if(v_move.move==CLIENT_PASS) 
    sprintf(yx,"ps"); 
  else 
    sprintf(yx,"%c%c",(v_move.move%10)+'A'-1,(v_move.move/10)+'0');
  sprintf(buf,"PM %s %+5.2f %8.2f\n",yx,v_move.eval,v_move.time);
  write_client(buf);
}

static void put_MATCH(void)
{
  char buf[90];
  sprintf(buf,"match ");
  write_client(buf);
  if(v_match.type==CLIENT_KOMI)
    {
      sprintf(buf,"komi%f ",v_match.komi);
      write_client(buf);
    };
  if(v_match.type==CLIENT_RAND)
    {
      sprintf(buf,"rand%d ",v_match.rand);
      write_client(buf);
    };
  if(v_match.special==CLIENT_BRONSTEIN)
    {
      sprintf(buf,"bronstein ");
      write_client(buf);
    };
  sprintf(buf,"%d %d %d %s %d %d %d ",
	  v_match.op_time,v_match.op_inc,v_match.op_def,
	  v_match.user,
	  v_match.my_time,v_match.my_inc,v_match.my_def);
  write_client(buf);
  if(v_match.type==CLIENT_BLACK)
    {
      sprintf(buf,"black");
      write_client(buf);
    };
  if(v_match.type==WHITE)
    {
      sprintf(buf,"white");
      write_client(buf);
    };
  sprintf(buf,"\n");
  write_client(buf);
}

static void put_ACCEPT(void)
{
  char buf[90];
  char	color;
  color=' ';
  if(v_accept.color==CLIENT_BLACK)
    color='b';
  if(v_accept.color==CLIENT_WHITE)
    color='w';
  sprintf(buf,"accept %s %d %c\n",
	  v_accept.user,v_accept.game_id,color);
  write_client(buf);
}

static void put_DECLINE(void)
{
  char buf[1024];
  sprintf(buf,"decline %s\n",v_decline.user);
  write_client(buf);
  if(v_decline.reason[0]) 
    {
      sprintf(buf,"tell %s %s\n",v_decline.user,v_decline.reason);
      write_client(buf);
    };
}

static void put_WHO(void)
{
  char buf[90];
  sprintf(buf,"who %c\n",v_who.sort);
  write_client(buf);
}

static void put_PLAYERS(void)
{
  char buf[90];
  sprintf(buf,"players %c\n",v_players.sort);
  write_client(buf);
}

static void put_FINGER(void)
{
  char buf[90];
  sprintf(buf,"finger\n");
  write_client(buf);
}

static void put_BOARD(void)
{
  char buf[90];
  sprintf(buf,"board %d\n",v_board.game_id);
  write_client(buf);
}

static void put_MOVES(void)
{
  char buf[90];
  sprintf(buf,"moves %d\n",v_board.game_id);
  write_client(buf);
}

static void put_PMOVES(void)
{
  char buf[90];
  sprintf(buf,"pmoves %d\n",v_board.game_id);
  write_client(buf);
}

static void put_LMOVES(void)
{
  char buf[90];
  sprintf(buf,"lmoves %d\n",v_board.game_id);
  write_client(buf);
}

static void put_SET(void)
{
  char buf[90];
  sprintf(buf,"name %s\n",v_set.name);
  write_client(buf);
  sprintf(buf,"pw %s %s\n",v_set.pwold,v_set.pwnew);
  write_client(buf);
  sprintf(buf,"set pio %d\n",v_set.pio);
  write_client(buf);
  sprintf(buf,"set gio %d\n",v_set.gio);
  write_client(buf);
  sprintf(buf,"set open %d\n",v_set.open);
  write_client(buf);
  sprintf(buf,"set mail %d\n",v_set.mail);
  write_client(buf);
  sprintf(buf,"set omail %d\n",v_set.omail);
  write_client(buf);
  sprintf(buf,"set trust %d\n",v_set.trust);
  write_client(buf);
  sprintf(buf,"set rated %d\n",v_set.rated);
  write_client(buf);
  sprintf(buf,"set finger %d\n",v_set.finger);
  write_client(buf);
  sprintf(buf,"set bell %d\n",v_set.bell);
  write_client(buf);
  sprintf(buf,"set yell %d\n",v_set.yell);
  write_client(buf);
  sprintf(buf,"set kibitz %d\n",v_set.kibitz);
  write_client(buf);
  sprintf(buf,"set unreg %d\n",v_set.unreg);
  write_client(buf);
  sprintf(buf,"set info %s\n",v_set.info);
  write_client(buf);
}

static void put_KILL(void)
{
  char buf[90];
  sprintf(buf,"kill %s %s\n",v_kill.user,v_kill.pass);
  write_client(buf);
}

static void put_TOP(void)
{
  char buf[90];
  sprintf(buf,"top %d\n",v_rank.no);
  write_client(buf);
}

static void put_RANK(void)
{
  char buf[90];
  sprintf(buf,"rank %s\n",v_rank.user);
  write_client(buf);
}

static void put_ASSESS(void)
{
  char buf[90];
  sprintf(buf,"assess %s %s\n",v_assess.user1,v_assess.user2);
  write_client(buf);
}

static void put_GAMES(void)
{
  char buf[90];
  sprintf(buf,"games\n");
  write_client(buf);
}

static void put_ABORT(void)
{
  char buf[90];
  if(v_client.game_now)
    {
      sprintf(buf,"abort\n");
      write_client(buf);
      if(strlen(v_abort.reason)!=0)
	{
	  sprintf(buf,"say %s\n",v_abort.reason);
	  write_client(buf);
	};
    };
}

static void put_RESIGN(void)
{
  char buf[90];
  sprintf(buf,"resign\n");
  write_client(buf);
}

static void put_STORED(void)
{
  char buf[90];
  sprintf(buf,"stored\n");
  write_client(buf);
}

static void put_MORETIME(void)
{
  char buf[90];
  sprintf(buf,"moretime %d\n",v_moretime.time);
  write_client(buf);
}

static void put_OBSERVE(void)
{
  char buf[90];
  sprintf(buf,"observe\n");
  write_client(buf);
}

static void put_WHOOBS(void)
{
  char buf[90];
  sprintf(buf,"whoobs %d\n",v_whoobs.game);
  write_client(buf);
}

static void put_YELL(void)
{
  char buf[90];
  sprintf(buf,"yell %s\n",v_yell.message);
  write_client(buf);
}

static void put_TELL(void)
{
  char buf[90];
  sprintf(buf,"tell %s %s\n",v_tell.user,v_tell.message);
  write_client(buf);
}

static void put_SAY(void)
{
  char buf[90];
  sprintf(buf,"say %s\n",v_say.message);
  write_client(buf);
}

static void put_KIBITZ(void)
{
  char buf[90];
  sprintf(buf,"kibitz %d %s\n",v_kibitz.game,v_kibitz.message);
  write_client(buf);
}

static void put_WHISPER(void)
{
  char buf[90];
  sprintf(buf,"whisper %d %s\n",v_whisper.game,v_whisper.message);
  write_client(buf);
}

static void put_LISTEN(void)
{
  char buf[90];
  sprintf(buf,"listen\n");
  write_client(buf);
}

static void put_WHOLIST(void)
{
  char buf[90];
  sprintf(buf,"wholist %d\n",v_wholist.channel);
  write_client(buf);
}

static void put_UPSTATE(void)
{
  char buf[90];
  sprintf(buf,"upstate\n");
  write_client(buf);
}

static void put_LHISTORY(void)
{
  char buf[90];
  sprintf(buf,"lhistory\n");
  write_client(buf);
}

static void put_GHISTORY(void)
{
  char buf[90];
  sprintf(buf,"ghistory\n");
  write_client(buf);
}

static void put_MAIL(void)
{
  char buf[90];
  sprintf(buf,"mail %s\n",v_mail.file);
  write_client(buf);
}

static void put_HELP(void)
{
  char buf[90];
  sprintf(buf,"help %s\n",v_help.file);
  write_client(buf);
}

static void put_EXIT(void)
{
  char buf[90];
  sprintf(buf,"exit\n");
  write_client(buf);
}

static void put_FILTER(void)
{
  char buf[90];
  sprintf(buf,"filter %s\n",v_filter.user);
  write_client(buf);
}

static void put_AUTOMAIL(void)
{
  char buf[90];
  sprintf(buf,"automail %s %s\n",v_automail.user1,v_automail.user2);
  write_client(buf);
}

static void put_BREAK(void)
{
  char buf[90];
  sprintf(buf,"break\n");
  write_client(buf);
}




/* ==================
        DATA
   ==================*/

t_match         v_match;
t_create        v_create;
t_end	        v_end;
t_who	        v_who;
t_players	v_players;
t_games	        v_games;
t_board	        v_board;
t_moves	        v_moves;
t_move	        v_move;
t_accept	v_accept;
t_decline	v_decline;
t_finger	v_finger;
t_set	        v_set;
t_kill	        v_kill;
t_rank	        v_rank;
t_assess	v_assess;
t_abort	        v_abort;
t_resign	v_resign;
t_moretime	v_moretime;
t_observe	v_observe;
t_whoobs	v_whoobs;
t_yell 	        v_yell;
t_tell	        v_tell;
t_say       	v_say;
t_kibitz	v_kibitz;
t_whisper	v_whisper;
t_pio	        v_pio;
t_listen	v_listen;
t_wholist	v_wholist;
t_upstate	v_upstate;
t_ghistory	v_ghistory;
t_lhistory	v_lhistory;
t_mail	        v_mail;
t_help	        v_help;
t_exit	        v_exit;
t_stored	v_stored;
t_filter	v_filter;
t_automail	v_automail;
t_break	        v_break;
t_about		v_about;
t_remote	v_remote;
t_internal	v_internal;
t_client 	v_client;

/* ================================================================== 

	Functions to connect and communicate to ios

   ==================================================================*/

void client_INIT(void)
{
  int	i;
  v_remote.users=0;
  v_about.blitz_old=v_about.stand_old=9999;
  for(i=0;i<100;i++)
    {
      v_board.board[i]=v_init_board[i];
      v_moves.what[i]=v_init_board[i];
    };
  v_client.connect=0;
  v_client.login_now=0;
  v_client.prompt=0;
  v_client.echo=1;
  v_client.input=1;
}

void client_CONNECT(void)
{
  struct 	sockaddr_in 	sa;
  struct 	hostent 	*hp;
  int   	addr;
  int 	attempts=0;
  int	sleeper=0;
  if(v_client.connect==1)
    {
      fprintf(stdout,"client <ios> already connected.\n");
      return;
    };
  printf("client <ios> connect.\n");
  v_client.connect=1;
  fprintf(stderr,"client ");
#ifdef SIGNAL_HANDLER
  sig_init(SIGINT); 
#endif

 AGAIN:
  if(sleeper==20) 
    sleeper=0;
  bzero(&sa,sizeof(sa));
  if((addr=inet_addr(v_client.host))!=-1) 
    {
      bcopy(&addr,(char *)&sa.sin_addr,sizeof(addr)); 
      sa.sin_family=AF_INET;
    } 
  else 
    {
      if((hp=gethostbyname(v_client.host))==NULL) 
	{
	  fprintf(stdout,"client unknown host %s\n",v_client.host);
	  attempts++;sleeper++;
	  fprintf(stdout,"client <ios> attempt %d sleep for %d\n",
		  attempts,sleeper);
	  sleep(sleeper);
	  goto AGAIN;
	};
      bcopy(hp->h_addr,(char *)&sa.sin_addr,hp->h_length);
      sa.sin_family=hp->h_addrtype ;
    };
  sa.sin_port=htons((u_short) v_client.port);
  v_client.socket=socket(sa.sin_family,SOCK_STREAM,0);
  if(v_client.socket<0) 
    {
      perror("client socket");
      attempts++;sleeper++;
      fprintf(stdout,"client <ios> attempt %d sleep for %d\n",
	      attempts,sleeper);
      sleep(sleeper);
      goto AGAIN;
    };
  if(connect(v_client.socket,(struct sockaddr *)&sa,sizeof(sa))<0) 
    {
      close(v_client.socket);
      perror("client connect");
      attempts++;sleeper++;
      fprintf(stdout,"client <ios> attempt %d sleep for %d\n",
	      attempts,sleeper);
      sleep(sleeper);
      goto AGAIN;
    };
 CONTINUE:  
  v_client.buffer_index=0;
  v_client.buffer_size=0;
  v_client.line[0]=0;
  v_client.line_display=0;
  v_client.line_index=0;
  v_client.line_ready=0;
  v_client.prompt=0;
  v_client.login_now=0;
  v_client.login[0]=0;
  v_client.game_now=0;
  v_client.analyse=0;
  v_client.input=1;
  v_client.echo=1;
  v_client.state=CLIENT_NONE;
  v_client.get_type=v_client.put_type=CLIENT_NONE;
}

void client_DISCONNECT(void)
{
  if(v_client.connect==0)
    {
      fprintf(stdout,"client <ios> already disconnected.\n");
      return;
    };
  fprintf(stdout,"client <ios> disconnect.\n");
  v_client.connect=0;
  shutdown(v_client.socket,2);
  close(v_client.socket);
}

void client_LOGON(void)
{
  char mm[90];
  fprintf(stdout,"client <ios> logon (%s).\n", v_client.login);
  sprintf(mm,"%s\n",v_client.login);
  write_client(mm);
  if(v_client.password!=NULL) {
    sprintf(mm,"%s\n",v_client.password);
    write_client(mm);
  };  
  v_client.prompt=1;
}

void client_GET(void)
{
  char	s[256];
  int i;
  int	l1,l2;

 LOCAL: ;

  read_stdin();
  if(v_client.get_type==CLIENT_INTERNAL)
    return;
  if(v_client.connect==0)
    {
      return;
    };

 READ: ;

  read_client();
  if(!v_client.line_ready) 
    {
      if(!strcmp(v_client.line,"login: ")) 
	{
	  v_client.login_now=1;
	  v_client.prompt=1;
	  echo_line();
	  clean_line();
	} 
      else 
	echo_line();     
      return;
    }

 CHECK: ;

  switch(v_client.state) 
    {
    case CLIENT_MATCH:
      get_MATCH(); 
      return;
    case CLIENT_MATCH_TO:
      get_MATCH_TO(); 
      return;
    case CLIENT_BOARD:
      get_BOARD(); 
      return;
    case CLIENT_PBOARD:
      get_PBOARD(); 
      return;
    case CLIENT_CREATE:
      get_CREATE(); 
      return;
    case CLIENT_END:
      get_END(); 
      return;
    case CLIENT_MOVES:
      get_MOVES(); 
      return;
    case CLIENT_PMOVES:
      get_PMOVES(); 
      return;
    case CLIENT_LMOVES:
      get_LMOVES(); 
      return;
    case CLIENT_WHO:
      get_WHO(); 
      return; 	
    case CLIENT_PLAYERS:
      get_PLAYERS(); 
      return;
    case CLIENT_FINGER:
      get_FINGER(); 
      return;
    case CLIENT_RANK:
      get_RANK(); 
      return;
    case CLIENT_ASSESS:
      get_ASSESS(); 
      return;
    case CLIENT_GAMES:
      get_GAMES(); 
      return;
    case CLIENT_ABORT:
      get_ABORT(); 
      return;
    case CLIENT_STORED:
      get_STORED(); 
      return;
    case CLIENT_FILTER:
      get_FILTER(); 
      return;
    case CLIENT_AUTOMAIL:
      get_AUTOMAIL(); 
      return;
    case CLIENT_LHISTORY:
      get_LHISTORY(); 
      return;
    case CLIENT_GHISTORY:
      get_GHISTORY(); 
    case CLIENT_WHOOBS:
      get_WHOOBS(); 
      return;
    case CLIENT_WHOLIST:
      get_WHOLIST(); 
      return;
    case CLIENT_UPSTATE:
      get_UPSTATE(); 
      return;
    default:
      break;
    }
  if(strncmp(v_client.line,"<ios> MATCH",11)==0) 
    {
      v_client.state=CLIENT_MATCH;
      goto CHECK;
    };
  if(strncmp(v_client.line,"% <ios> MATCH",13)==0) 
    {
      v_client.state=CLIENT_MATCH_TO;
      goto CHECK;
    };
  if(strncmp(v_client.line,"<ios> [GAME",11)==0&&
     strstr(v_client.line,"created") &&strstr(v_client.line,v_client.login)) 
    {
      v_client.state=CLIENT_CREATE;
      goto CHECK;
    }
  if(strncmp(v_client.line,"<ios> [GAME",11)==0&&
     strstr(v_client.line,"ended")&&strstr(v_client.line,v_client.login)) 
    {
      v_client.state=CLIENT_END;
      goto CHECK;
    }
  if(strncmp(v_client.line,"<ios> BOARD",11)==0)
    if( strstr(v_client.line,v_client.login) ||!v_client.game_now || v_client.analyse) // !!! 
      {
	v_client.state=CLIENT_BOARD;
	goto CHECK;
      };
  if(strncmp(v_client.line,"<ios> PB",8)==0) 
    if( strstr(v_client.line,v_client.login)||!v_client.game_now || v_client.analyse) // !!!
      {
	v_client.state=CLIENT_PBOARD;
	goto CHECK;
      }; 
  if(strncmp(v_client.line,"<ios> MOVES",11)==0)
    if( strstr(v_client.line,v_client.login)) 
      {
	v_client.state=CLIENT_MOVES;
	goto CHECK;
      };
  if(strncmp(v_client.line,"<ios> PM",8)==0)
    {
      v_client.state=CLIENT_PMOVES;
      goto CHECK;
    };
  if(strncmp(v_client.line,"<ios> LM",8)==0)
    {
      v_client.state=CLIENT_LMOVES;
      goto CHECK;
    };
  if(strncmp(v_client.line,
	     "LIST of BEST players on <ios>",29)==0) 
    {
      v_client.state=CLIENT_RANK;
      goto CHECK;
    };
  if((strncmp(v_client.line,"login: ",7)==0)&&(strlen(v_client.line)>7))
    {
      v_client.state=CLIENT_FINGER;
      goto CHECK;
    };
  if((strstr(v_client.line,": asks for abort"))&&(v_client.game_now))
    {
      v_client.state=CLIENT_ABORT;
      goto CHECK;
    };
  if(strncmp(v_client.line,"   Blitz",8)==0)
    {
      v_client.state=CLIENT_ASSESS;
      goto CHECK;
    };
  if(strncmp(v_client.line,"games:",6)==0) 
    {
      v_client.state=CLIENT_GAMES;
      goto CHECK;
    };
  if((strncmp(v_client.line,"who ",4)==0)&&(v_client.line[5]=':')) 
    {
      v_client.state=CLIENT_WHO;
      goto CHECK;
    };
  if((strncmp(v_client.line,"players ",8)==0)&&(v_client.line[9]=':')) 
    {
      v_client.state=CLIENT_PLAYERS;
      goto CHECK;
    };
  if(strncmp(v_client.line,"stored:",7)==0) 
    {
      v_client.state=CLIENT_STORED;
      goto CHECK;
    };
  if(strncmp(v_client.line,"filter:",7)==0) 
    {
      v_client.state=CLIENT_FILTER;
      goto CHECK;
    };
  if(strncmp(v_client.line,"automail:",9)==0) 
    {
      v_client.state=CLIENT_AUTOMAIL;
      goto CHECK;
    };
  if(strncmp(v_client.line,"GAME observed by:",17)==0) 
    {
      v_client.state=CLIENT_WHOOBS;
      goto CHECK;
    };
  if(strncmp(v_client.line,"CHANNEL listened by:",20)==0) 
    {
      v_client.state=CLIENT_WHOLIST;
      goto CHECK;
    };
  if(strncmp(v_client.line,"Start:",6)==0) 
    {
      v_client.state=CLIENT_UPSTATE;
      goto CHECK;
    };
  if(strncmp(v_client.line,"lhistory:",9)==0) 
    {
      v_client.state=CLIENT_LHISTORY;
      goto CHECK;
    };
  if(strncmp(v_client.line,"ghistory:",9)==0) 
    {
      v_client.state=CLIENT_GHISTORY;
      goto CHECK;
    };

  get_THE_REST();

  goto READ;
}

void client_PUT_STRING(char *s)
{
  if(v_client.connect==1)
    write_client(s);
  v_client.state=CLIENT_NONE;
  v_client.put_type=CLIENT_NONE;
}

void client_PUT(void)
{
  if(v_client.connect==0)
    {
      v_client.state=CLIENT_NONE;
      v_client.put_type=CLIENT_NONE;
      return;
    };
  switch(v_client.put_type) 
    {
    case CLIENT_MOVE:
      if(v_client.analyse) 
	return; 
      else 
	put_MOVE(); 
      break;
    case CLIENT_MATCH:
      put_MATCH();   
      break;
    case CLIENT_ACCEPT:
      put_ACCEPT();  
      break;
    case CLIENT_DECLINE:
      put_DECLINE(); 
      break;
    case CLIENT_WHO:
      put_WHO();
      break;
    case CLIENT_PLAYERS:
      put_PLAYERS();
      break;
    case CLIENT_MOVES:
      put_MOVES();
      break;
    case CLIENT_PMOVES:
      put_PMOVES();
      break;
    case CLIENT_LMOVES:
      put_LMOVES();
      break;
    case CLIENT_FINGER:
      put_FINGER();
      break;
    case CLIENT_SET:
      put_SET();
      break;
    case CLIENT_KILL:
      put_KILL();
      break;
    case CLIENT_TOP:
      put_TOP();
      break;
    case CLIENT_RANK:
      put_RANK();
      break;
    case CLIENT_ASSESS:
      put_ASSESS();
      break;
    case CLIENT_GAMES:
      put_GAMES();
      break;
    case CLIENT_ABORT:
      put_ABORT();
      break;
    case CLIENT_RESIGN:
      put_RESIGN();
      break;
    case CLIENT_STORED:
      put_STORED();
      break;
    case CLIENT_MORETIME:
      put_MORETIME();
      break;
    case CLIENT_OBSERVE:
      put_OBSERVE();
      break;
    case CLIENT_WHOOBS:
      put_WHOOBS();
      break;
    case CLIENT_YELL:
      put_YELL();
      break;
    case CLIENT_TELL:
      put_TELL();
      break;
    case CLIENT_SAY:
      put_SAY();
      break;
    case CLIENT_KIBITZ:
      put_KIBITZ();
      break;
    case CLIENT_WHISPER:
      put_WHISPER();
      break;
    case CLIENT_LISTEN:
      put_LISTEN();
      break;
    case CLIENT_WHOLIST:
      put_WHOLIST();
      break;
    case CLIENT_UPSTATE:
      put_UPSTATE();
      break;
    case CLIENT_LHISTORY:
      put_LHISTORY();
      break;
    case CLIENT_GHISTORY:
      put_GHISTORY();
      break;
    case CLIENT_MAIL:
      put_MAIL();
      break;
    case CLIENT_HELP:
      put_HELP();
      break;
    case CLIENT_EXIT:
      put_EXIT();
      break;
    case CLIENT_FILTER:
      put_FILTER();
      break;
    case CLIENT_AUTOMAIL:
      put_AUTOMAIL();
      break;
    case CLIENT_BREAK:
      put_BREAK();
      break;
    default:
      printf("\nERROR: %d not supported in client_put(),yet\n",
	     v_client.put_type); 
      exit(1);
    };
  v_client.state=CLIENT_NONE;
  v_client.put_type=CLIENT_NONE;
}

void client_ASK(void)
{
  client_type	t;
  t=v_client.put_type;
  client_PUT();
  v_client.get_type=CLIENT_NONE;
  while(v_client.get_type!=t)
    client_GET();
}

void client_REMOTE(char *name,client_type status)
{
  char	s[256];
  switch(status)
    {
    case(CLIENT_OPERATOR):
      fprintf(stdout,"client <ios> Operator %s added.\n",name);
      break;
    case(CLIENT_SUPERVISOR):
      fprintf(stdout,"client <ios> Supervisor %s added.\n",name);
      break;
    };
  strcpy(v_remote.user[v_remote.users],name);
  v_remote.status[v_remote.users]=status;
  v_remote.users++;
}

/* ================================================================== 

	CLIENT Game Logic functions

   ==================================================================*/

void client_NEW_GAME(void)
{
  memcpy(v_test_board,v_init_board,sizeof(v_init_board));
}

static int scans[8]={-11,-10,-9,-1,1,9,10,11};

int client_TEST_MOVE(int move,client_type who)
{
  int	me,op;
  int	flip,mask,dirn,sq,n;
  switch(who)
    {
    case CLIENT_WHITE:
      me=CLIENT_WHITE;
      op=CLIENT_BLACK;
      break;
    case CLIENT_BLACK:
      op=CLIENT_WHITE;
      me=CLIENT_BLACK;
      break;
    };
  mask=0;
  if(v_test_board[move]==CLIENT_NONE)
    {
      for(dirn=0;dirn<8;dirn++)
	{
	  flip=0;
	  sq=move+scans[dirn];
	  while(v_test_board[sq]==op)
	    {
	      sq+=scans[dirn];
	      flip=1;
	    };
	  if(v_test_board[sq]!=me)
	    {
	      flip=0;	
	    };
	  mask+=(flip<<dirn);
	};
    };
  return mask;
}

void client_MAKE_MOVE(int move,client_type who)
{
  int	me,op;
  int	flip,dirn,sq;
  switch(who)
    {
    case CLIENT_WHITE:
      me=CLIENT_WHITE;
      op=CLIENT_BLACK;
      break;
    case CLIENT_BLACK:
      op=CLIENT_WHITE;
      me=CLIENT_BLACK;
      break;
    };
  flip=client_TEST_MOVE(move,who);
  for(dirn=0;dirn<8;dirn++)
    {
      if(flip&(1<<dirn))
	{
	  sq=move+scans[dirn];
	  while(v_test_board[sq]==op)
	    {
	      v_test_board[sq]=me;
	      sq+=scans[dirn];
	    };
	};
    };
  v_test_board[move]=me;
}

void client_TEST_MOVES(void)
{
  int	f,i,n;
  client_type	w;
  for(i=0;i<100;i++)
    {
      if(v_moves.what[i]==CLIENT_PLAY)
	v_test_board[i]=CLIENT_NONE;
      else
	v_test_board[i]=v_moves.what[i];
    };
  w=v_moves.start_color;
  for(n=1;n<=v_moves.no;n++)
    {
      i=0;
      while(v_moves.turn[i]!=n)
	{
	  i++;
	};
      f=client_TEST_MOVE(i,w);
      if(f)
	{
	  client_MAKE_MOVE(i,w);
	};
      switch(w)
	{
	case CLIENT_BLACK:
	  w=CLIENT_WHITE;
	  break;
	case CLIENT_WHITE:
	  w=CLIENT_BLACK;
	  break;
	};
    };
  v_moves.black_disks=0;
  v_moves.white_disks=0;
  for(i=0;i<100;i++)
    {
      switch(v_test_board[i])
	{
	case CLIENT_BLACK:
	  v_moves.black_disks++;
	  break;
	case CLIENT_WHITE:
	  v_moves.white_disks++;
	};
    };
}

void client_TEST_BOARD(void)
{
  int	i,f;
  for(i=0;i<100;i++)
    {
      v_test_board[i]=v_board.board[i];
    };
  v_board.moves=0;	
  for(i=0;i<100;i++)
    {
      f=client_TEST_MOVE(i,v_board.turn_color);
      if(f)
	{
	  v_board.move[v_board.moves]=i;
	  v_board.moves++;
	};
    };
}

void client_PRINT(char *s)
{
  echo_string(s);
}

/*======================================================================*/
/*
 * 	Third Party Utilities:
 *
 * 	Mark Brockington (Mod)	client_CHALLENGE(...)
 *	Igor Durdanovich (Mod)	client_RATING(...)
 *
 */

void client_RATING(int r,int orb,int orw,int *nrb, int *nrw )
{
  float vb, vw, nb, nw;
  float rb, rw, K, w;

  w=0.0;
  if(r<0) w=-1.0;
  if(r>0) w=1.0;

  K=32+32*fabs(r/64.0);


  if(rb==0) rb=1600;
  if(rw==0) rw=1600;

  rb=(float) orb;
  rw=(float) orw;

  vb=(1+w)/2;
  nb= rb+K*(vb-1/(1+pow(10,(rw-rb)/400))); 

  vw=(1-w)/2;
  nw= rw+K*(vw-1/(1+pow(10,(rb-rw)/400))); 

  *nrb=(int)(nb+0.5);
  *nrw=(int)(nw+0.5);
}

/*static int 	blitz_compare(t_who_line *p1,t_who_line *p2)
{
return p2->blitz - p1->blitz;
}

static int 	standard_compare(t_who_line *p1,t_who_line *p2)
{
return p2->standard - p1->standard;
}

typedef int(*t_qcmp)(const void *,const void *);*/

t_challenge	v_challenge;

void client_CHALLENGE(void)
{
  /*
    int	cx,rating,qu,blitz,low_rating,high_rating;
    int  	player[256];
    printf("Challenge \n");
    blitz=(time+inc/2<15);
    if(blitz)
    qsort(v_who.list,v_who.no,
    sizeof(v_who.list[0]),(t_qcmp)blitz_compare);
    else
    qsort(v_who.list,v_who.no,
    sizeof(v_who.list[0]),(t_qcmp)standard_compare);
    if(relative)
    {
    cx=0;
    while(strcmp(v_who.list[cx].name,v_client.login)) 
    cx++;
    if(blitz)
    {
    low_rating=v_who.list[cx].blitz+min_rating;
    high_rating=v_who.list[cx].blitz+max_rating;
    }
    else
    {
    low_rating=v_who.list[cx].standard+min_rating;
    high_rating=v_who.list[cx].standard+max_rating;
    };
    }
    else
    {
    low_rating=min_rating;
    high_rating=max_rating;
    };
    if(average)
    {
    cx=0;
    while(strcmp(v_who.list[cx].name,v_client.login)) 
    cx++;
    if(blitz)
    {
    if(average<100)
    {
    low_rating=v_who.list[cx].blitz+min_rating;
    high_rating=v_who.list[cx].blitz+max_rating;
    }
    else
    {
    {
    low_rating=2*v_who.list[cx].blitz+min_rating-average;
    high_rating=2*v_who.list[cx].blitz+max_rating-average;
    }
    };
    }
    else
    {
    if(average<100)
    {
    low_rating=v_who.list[cx].standard+min_rating;
    high_rating=v_who.list[cx].standard+max_rating;
    }
    else
    {
    low_rating=2*v_who.list[cx].standard+min_rating-average;
    high_rating=2*v_who.list[cx].standard+max_rating-average;
    };
    };
    };
    printf("Rating Range [%d ... %d]\n",low_rating,high_rating);
    qu=0;
    for(cx=0;cx<v_who.no;cx++) 
    {
    rating=(blitz?v_who.list[cx].blitz:v_who.list[cx].standard);
    if(rating>=low_rating&&rating<=high_rating&&
    !v_who.list[cx].playing&&
    v_who.list[cx].open &&
    v_who.list[cx].rated>=must_rated)
    {	
    if(strcmp(v_who.list[cx].name,v_client.login))
    {
    player[qu++]=cx;
    printf("Opponent %8s Blitz %4d Standard %4d\n",
    v_who.list[cx].name,
    v_who.list[cx].blitz,
    v_who.list[cx].standard);
    }
    else
    {
    v_about.blitz_now=v_who.list[cx].blitz;
    v_about.stand_now=v_who.list[cx].standard;
    printf("<< Your Rating >> Blitz %d Standard %d\n",
    v_about.blitz_now,v_about.stand_now);
    };
    };
    };
    printf("%s challenge %d players.\n",(blitz)?"Blitz":"Standard",qu);
    if(qu>0) 
    {
    cx=player[best?0:rand()%qu];
    v_client.put_type=CLIENT_MATCH;
    v_match.op_time=v_match.my_time=time;
    v_match.op_inc=v_match.my_inc=inc;
    v_match.op_def=v_match.my_def=def;
    v_match.my_color=color;
    v_match.rnd_color=rnd_color;
    strcpy(v_match.user,v_who.list[cx].name);
    client_PUT();
    printf("Challenge to %s\n",v_match.user);
    };
  */
} 

int	client_ACCEPT(void)
{
}


/*======================================================================*/


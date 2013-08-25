// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "client_header.h"

void	othello_SLEEP(char *t)
{
command_EXECUTE(COMMAND_PRI,"disconnect");
sleep(atoi(t));
command_EXECUTE(COMMAND_PRI,"connect");
}

void	othello_CONNECT(char *t)
{
strcpy(v_client.host,command_GET("ioshost",1));
v_client.port=command_INTEGER("iosport",1);
client_CONNECT();
strcpy(v_client.login,command_GET("ioslogin",1));
strcpy(v_client.password,command_GET("iospassword",1));
v_client.echo=command_INTEGER("iosecho",1);
v_client.input=command_INTEGER("iosinput",1);
client_LOGON();
}

void	othello_DISCONNECT(char *t)
{
client_DISCONNECT();
}

void	othello_INIT(void)
{
client_INIT();
client_REMOTE("ieuan",CLIENT_SUPERVISOR);
client_REMOTE("Buffalo",CLIENT_OPERATOR);
client_REMOTE("unic",CLIENT_OPERATOR);
client_REMOTE("jancgp",CLIENT_OPERATOR);

command_INIT();
command_PUB("ioshost          external.nj.nec.com",NULL);
command_PUB("iosport 5000",NULL);
command_PUB("ioslogin Demon",NULL);
command_PRI("iospassword",NULL);
command_PUB("iosecho 1",NULL);
command_PUB("iosinput 1",NULL);
command_PUB("connect  fsjhd ghj       fghgsfhd ",othello_CONNECT);
command_PUB("disconnect",othello_DISCONNECT);
command_PUB("sleep",othello_SLEEP);
command_DUMP();
event_INIT();
}

void	othello_ARGS(char *arg)
{
command_EXECUTE(COMMAND_ALL,arg);
}

void    othello_MATCH(void)
{
v_client.get_type=CLIENT_NONE;
strcpy(v_accept.user,v_match.user);
strcpy(v_decline.user,v_match.user);
v_accept.game_id=v_match.game_id;
if(v_match.stored)
	{
	v_client.put_type=CLIENT_ACCEPT;
	client_PUT();
	return;
	};
if(v_match.my_time==0)
	{
	strcpy(v_decline.reason,"Sorry >=1 minute only!\n");	
	v_client.put_type=CLIENT_DECLINE;
	client_PUT();
	return;
	};
if(v_match.my_color==CLIENT_KOMI)
	{
	/*if(v_match.rand>0)
		{
		strcpy(v_decline.reason,"Sorry No RAND games!\n");	
		v_client.put_type=CLIENT_DECLINE;
		client_PUT();
		return;
		};*/
	if(v_match.komi>=0)
		{
		v_accept.color=CLIENT_WHITE;
		v_client.put_type=CLIENT_ACCEPT;
		client_PUT();
		return;
		};
	if(v_match.komi>=-1)
		{
		v_accept.color=CLIENT_WHITE;
		v_client.put_type=CLIENT_ACCEPT;
		client_PUT();
		return;
		};
	if((rand()%100)>=50)
		{
		v_accept.color=CLIENT_WHITE;
		v_client.put_type=CLIENT_ACCEPT;
		client_PUT();
		return;
		}
	else
		{
		v_accept.color=CLIENT_WHITE;
		v_client.put_type=CLIENT_ACCEPT;
		client_PUT();
		return;
		};
	};
if(v_match.random_color==0)
	{
	strcpy(v_decline.reason,"Sorry No RAND games!\n");	
	v_client.put_type=CLIENT_DECLINE;
	client_PUT();
	return;
	};
v_accept.color=CLIENT_NONE;
v_client.put_type=CLIENT_ACCEPT;
client_PUT();
}

void    othello_CREATE(void)
{
v_client.get_type=CLIENT_NONE;
}

void    othello_MOVES(void)
{
v_client.get_type=CLIENT_NONE;
}

void    othello_BOARD(void)
{
v_client.get_type=CLIENT_NONE;
if(v_board.turn_my)
	{
	if(v_board.moves==0)
		v_move.move=CLIENT_PASS;
	else
		v_move.move=v_board.move[rand()%v_board.moves];
	v_move.eval=0.0;
	v_client.put_type=CLIENT_MOVE;
	client_PUT();
	event_JUMP(CLIENT_PLAY);
	}
else
	{
	event_JUMP(CLIENT_TOOT);
	};
}

void    othello_ABORT(void)
{
}

void    othello_END(void)
{
v_client.get_type=CLIENT_NONE;
event_JUMP(CLIENT_IDLE);
}

void	othello_NORMAL(void)
{
switch(v_client.get_type)
	{
case CLIENT_INTERNAL:
	command_EXECUTE(COMMAND_ALL,v_internal.text);
	break;
case CLIENT_REMOTE:
	command_EXECUTE(v_remote.status[v_remote.from],v_remote.text);
	sprintf(v_tell.message,"'%s' Done",v_remote.text);
	strcpy(v_tell.user,v_remote.user[v_remote.from]);
	v_tell.channel=-1;
	v_client.put_type=CLIENT_TELL;
	client_PUT();
	break;
	};
}

main(int argc,char **argv)
{	
	int	i;
othello_INIT();
for(i=1;i<argc;i++)
	othello_ARGS(argv[i]);
event_LOOP();
}





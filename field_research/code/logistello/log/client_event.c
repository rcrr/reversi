// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "client_header.h"

t_event v_event;
jmp_buf event_jump_buffer;

void    event_INIT(void)
{
v_event.state=CLIENT_IDLE;
v_event.timeout=1.0;
timer_SET(&v_event.timer,TIMER_REAL);
}

void    event_JUMP(client_type state)
{
v_event.state=state;
longjmp(event_jump_buffer,1);
}

void    event_NORMAL(void)
{
othello_NORMAL();
v_client.get_type=CLIENT_NONE;
}

void    event_HANDLER(void)
{
event_DUMP();
switch(v_event.state)
        {
case(CLIENT_IDLE):
        client_GET();
        switch(v_client.get_type)
                {
        case(CLIENT_MATCH):
                othello_MATCH();
                break;
        case(CLIENT_CREATE):
                othello_CREATE();
                break;
	case(CLIENT_BOARD):
                othello_BOARD();
                break;
       	default:
                event_NORMAL();
                break;
                };
        break;
case(CLIENT_PLAY):
        client_GET();
        switch(v_client.get_type)
                {
        case(CLIENT_MOVES):
                othello_MOVES();
                break;
        case(CLIENT_BOARD):
                othello_BOARD();
                break;
        case(CLIENT_ABORT):
                othello_ABORT();
                break;
        case(CLIENT_END):
                othello_END();
                break;
        default:
                event_NORMAL();
                break;
                };
        break;
case(CLIENT_TOOT):
        client_GET();
        switch(v_client.get_type)
                {
        case(CLIENT_MOVES):
                othello_MOVES();
                break;
        case(CLIENT_BOARD):
                othello_BOARD();
                break;
         case(CLIENT_ABORT):
                othello_ABORT();
                break;
       case(CLIENT_END):
                othello_END();
                break;
        default:
                event_NORMAL();
                break;
                };
        break;
case(CLIENT_BUSY):
        event_NORMAL();
        break;
        };
}

void    event_TEST(void)
{
if(timer_GET(&v_event.timer)>v_event.timeout)
        {
        event_DUMP();
        timer_SET(&v_event.timer,TIMER_REAL);
        event_HANDLER();
        };
}

void    event_LOOP(void)
{
setjmp(event_jump_buffer);
while(1)
        {
        usleep(100000);
	event_DUMP();
        timer_SET(&v_event.timer,TIMER_REAL);
        event_HANDLER();
        }
}

void	event_DUMP(void)
{
/*switch(v_event.state)
	{
case CLIENT_IDLE:
	printf("EVENT is IDLE\n");
	break;
case CLIENT_PLAY:
	printf("EVENT is PLAY\n");
	break;
case CLIENT_TOOT:
	printf("EVENT is TOOT\n");
	break;
case CLIENT_BUSY:
	printf("EVENT is BUSY\n");
	break;
	};*/
}

// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "client_header.h"

#include <sys/resource.h>

static	struct	rusage		ru;
static	struct	timeval		tv;
static	struct	timezone	tz;

void    timer_SET(t_timer *t,int mode)
{
t->mode=mode;
switch(t->mode)
        {
case(TIMER_OFF):
        return;
case(TIMER_REAL):
	getrusage(RUSAGE_SELF,&ru);
	t->set_sec=ru.ru_utime.tv_sec+ru.ru_stime.tv_sec;
	t->set_usec=ru.ru_utime.tv_usec+ru.ru_stime.tv_usec;
        break;
case(TIMER_CPU):
	gettimeofday(&tv,&tz);
	t->get_sec=tv.tv_sec;
	t->get_usec=tv.tv_usec;
       	break;
        };
}

float   timer_GET(t_timer *t)
{
switch(t->mode)
        {
case(TIMER_OFF):
        return 0.0;
case(TIMER_REAL):
	getrusage(RUSAGE_SELF,&ru);
	t->get_sec=ru.ru_utime.tv_sec+ru.ru_stime.tv_sec;
	t->get_usec=ru.ru_utime.tv_usec+ru.ru_stime.tv_usec;
        break;
case(TIMER_CPU):
	gettimeofday(&tv,&tz);
	t->get_sec=tv.tv_sec;
	t->get_usec=tv.tv_usec;
       	break;
	};
return (1.0*t->get_sec-1.0*t->set_sec)+
        (1.0*t->get_usec-1.0*t->set_usec)/1000000.0;
}

int     timer_MODE(t_timer *t)
{
return t->mode;
}

void	timer_DUMP(float t)
{
	int	th,tm,ts,tp;
ts=t;
tm=ts/60;ts=ts%60;
th=tm/60;tm=tm%60;
tp=t*100;tp=tp%100;
printf("%02i:%02i:%02i.%02i",th,tm,ts,tp);
}

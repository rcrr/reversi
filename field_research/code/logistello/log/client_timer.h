// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#define TIMER_OFF       0
#define TIMER_REAL      1
#define TIMER_CPU       2

typedef struct {
        int     mode;
        long    set_sec,set_usec;
        long    get_sec,get_usec;
} t_timer;

void    timer_SET(t_timer *t,int mode);
float   timer_GET(t_timer *t);
int     timer_MODE(t_timer *t);
void	timer_DUMP(float t);



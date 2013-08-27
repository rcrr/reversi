// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


typedef struct {
        client_type     state;
	t_timer	timer;
	float	timeout;
} t_event;

extern t_event v_event;

void    event_INIT(void);
void    event_JUMP(client_type state);
void    event_NORMAL(void);
void    event_HANDLER(void);
void	event_TEST(void);
void    event_LOOP(void);
void    event_DUMP(void);

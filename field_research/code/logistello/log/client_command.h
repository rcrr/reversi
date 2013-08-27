// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#define COMMAND_SIZE	128
#define COMMAND_NAME	32
#define COMMAND_TEXT	128

#define COMMAND_NONE	0
#define COMMAND_PUB	1
#define COMMAND_PRI    	2
#define COMMAND_ALL	3

typedef struct {
	char	buff[COMMAND_TEXT];
        int     commands;
        int     mode[COMMAND_SIZE];
        char    text[COMMAND_SIZE][COMMAND_TEXT];
	void	(*task[COMMAND_SIZE])(char *);
} t_command;

extern t_command  v_command;

void    command_INIT(void);

void    command_PUB(char *text,void (*task)(char *));
void    command_PRI(char *text,void (*task)(char *));
int	command_MODE(char *name);

int	command_ARGS(char *name);
void	command_ARG(char* line,int n);
char*	command_GET(char *name,int n);
int     command_INTEGER(char *name,int n);
float   command_FLOAT(char *name,int n);

void	command_EXECUTE(int mode,char *line);
int	command_FILE(char *name);

void    command_DUMP(void);


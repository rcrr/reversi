// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include "client_header.h"

t_command v_command;

void    command_INIT(void)
{
v_command.commands=0;
}

void    command_PUB(char *text,void (*task)(char *))
{
if(v_command.commands>=(COMMAND_SIZE-1))
        {
	printf("command Out of Memory!\n");
	exit(0);
 	};
v_command.mode[v_command.commands]=COMMAND_PUB;       strcpy(v_command.text[v_command.commands],text);
v_command.task[v_command.commands]=task;
v_command.commands++;
}

void    command_PRI(char *text,void (*task)(char *))
{
if(v_command.commands>=(COMMAND_SIZE-1))
        {
	printf("command Out of Memory!\n");
	exit(0);
	};
v_command.mode[v_command.commands]=COMMAND_PRI;
strcpy(v_command.text[v_command.commands],text);
v_command.task[v_command.commands]=task;
v_command.commands++;
}

int	command_MODE(char *name)
{
        int     i;
for(i=0;i<v_command.commands;i++)
        {
	command_ARG(v_command.text[i],0);
        if(strcmp(v_command.buff,name)==0)
                {
                return v_command.mode[i];
                };
        };
return COMMAND_NONE;
}
 
int	command_ARGS(char *line)
{
	int	i,j;
i=0;
j=0;
while(1)
	{
	while(line[i]==' ')
		i++;
	if(line[i]=='\0')
		return j;
	while((line[i]!=' ')&&(line[i]!='\0'))
		i++;
	j++;
	if(line[i]=='\0')
		return j;	
	};
}

void	command_ARG(char *line,int n)
{
	int	i,j,k;
i=0;
j=0;
while(1)
	{
	while(line[i]==' ')
		i++;
	if(line[i]=='\0')
		{
		strcpy(v_command.buff,"");
		return;
		};
	k=0;
	while((line[i]!=' ')&&(line[i]!='\0'))
		{
		v_command.buff[k]=line[i];
		k++;i++;
		};
	v_command.buff[k]='\0';
	if(j==n)
		return;
	else
		{
		if(line[i]=='\0')
			{
			strcpy(v_command.buff,"");
			return;
			};
		};
CONT:;
	
	j++;		
	};
}

char*	command_GET(char *name,int n)
{
        int     i,j,k;
for(i=0;i<v_command.commands;i++)
        {
	command_ARG(&(v_command.text[i][0]),0);
	if(strcmp(&(v_command.buff[0]),name)==0)
		{
		command_ARG(&(v_command.text[i][0]),n);
		return(v_command.buff);
		};
        };
return (char *)0;
}

int     command_INTEGER(char *name,int n)
{
return atoi(command_GET(name,n));
}

float   command_FLOAT(char *name,int n)
{
return atof(command_GET(name,n));
}

void	command_EXECUTE(int mode,char *line)
{
        int     i,j,n;
	char	name[COMMAND_TEXT];
	char	file[COMMAND_TEXT];
if(line[0]=='#')
	return;
command_ARG(line,0);
if(mode&COMMAND_PRI)
	{
	sprintf(file,"%s.pri",v_command.buff);
	if(command_FILE(file))
		return;
	sprintf(file,"%s.pub",v_command.buff);
	if(command_FILE(file))
		return;
	};
if(mode&COMMAND_PUB)
	{
	sprintf(file,"%s.pub",v_command.buff);
	if(command_FILE(file))
		return;
	};
strcpy(name,v_command.buff);
for(i=0;i<v_command.commands;i++)
        {
	command_ARG(&(v_command.text[i][0]),0);
        if(strcmp(v_command.buff,name)==0)
                {
			if(v_command.task[i]==NULL)
				{
				strcpy(&(v_command.text[i][0]),line);
				return;
				}
			else
				{
		                (*v_command.task[i])(line);
				return;
				};
		
                };
        };
client_PUT_STRING(line);
client_PUT_STRING("\n");
}

int	command_FILE(char *name)
{
	FILE	*file;
	char	line[COMMAND_TEXT];
	int	l;
file=fopen(name,"r");
if(file==NULL)
	return 0;
while(!feof(file))
	{
	fgets(line,COMMAND_TEXT,file);
	l=strlen(line);
	line[l-1]='\0';
	printf("command >> %s\n",line);
	command_EXECUTE(COMMAND_ALL,line);
	};
fclose(file);
return 1;
}

void    command_DUMP(void)
{
        int     i,j,a;
printf("command %i available.\n",v_command.commands);
for(i=0;i<v_command.commands;i++)
        {
	printf("command ");
        if(v_command.mode[i]==COMMAND_PUB)
        	printf("PUB ");
        if(v_command.mode[i]==COMMAND_PRI)
        	printf("PRI ");
	if(v_command.task[i]==NULL)
		printf("VAL ");
	else
		printf("CMD ");
	a=command_ARGS(v_command.text[i]);
	printf("%2i ",a);
	for(j=0;j<a;j++)
		{
		command_ARG(v_command.text[i],j);
		printf("%s ",v_command.buff);
		};
	printf("\n");
        };
}


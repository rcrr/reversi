// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2


#include <sys/param.h> 
#include <sys/types.h> 
#include <sys/time.h>

#include <sys/socket.h>
#include <sys/errno.h>
#include <sys/time.h>
/*#include <sys/rusage.h>	Where the hell is this file	*/

#include <netinet/in.h>

#include <arpa/inet.h>

#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <malloc.h>
#include <time.h>
#include <setjmp.h>
#include <math.h>
#include <unistd.h>


#include "client_othello.h"
#include "client_constant.h"
#include "client_timer.h"
#include "client_client.h"
#include "client_event.h"
#include "client_command.h"


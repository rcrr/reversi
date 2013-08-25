// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <stdio.h>
#include <time.h>
#include <sys/timeb.h>
#include <sys/times.h>
#include <sys/time.h>


int main(int argc, char **argv)
{
  int last;


  if (argc != 2) { 

error: 
    fprintf(stderr, "*** rand last_number\n"); exit(20);
  }

  if (sscanf(argv[1], "%d", &last)  != 1) goto error;

  if (last) srandom(last); else {

    struct timeval t;
    struct timezone tz;

    gettimeofday(&t, &tz);

    srandom(t.tv_sec);
   
  }


  printf("%d\n", random());

  return 0;
}



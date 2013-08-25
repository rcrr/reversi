// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#include <pthread.h>

pthread_mutex_t mutex;

char *mem;
const int N = 2;
const int LOOPS = 40000000;
const int TN = 2;

#define MOD (1UL<<30)
#define A   8893UL      /* = 5 (8)! */

typedef unsigned long uint4;

void *process(void *s)
{
  uint4 seed = ((((*(uint4*)(s))+3)*A) & ~7UL) + 5;
  int sum = 0;
  int i;

  for (i=0; i < LOOPS; i++) {
    seed = (seed * A) & (MOD-1);
    sum += mem[seed % N];
  }

  return (void*) sum;
}


int main()
{
  int i;

  mem = new char[N];
  pthread_t threads[TN];

  for (i=0; i < TN; i++) 
    if (pthread_create(&threads[i], NULL, process, &i)) {
      perror("cannot make thread\n");
      exit(20);
    }

  for (i=0; i < TN; i++) 
    if (pthread_join(threads[i], NULL)) {
      perror("cannot join thread\n");
      exit(20);
    }

  printf("%d\n", TN*LOOPS);

  return 0;
}

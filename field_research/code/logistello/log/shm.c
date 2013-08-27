// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// shared memory routines

#include "main.h"
#include "crt.h"
#include <sys/shm.h>

const char PROC_ID = 'B';
const int  MAGIC_READY = 0xaf123456;

static char *shm_addr =  0;
static int  shm_size  = -1;
static int  shm_id    = -1;

static char *shm_alloc_addr = 0;
static int shm_i = 0;  // current allocation (for access)

class SH_TAB_INFO {

public:

  enum { N_MAX = 300 };

  int magic;
  int n;      // number of allocs

  struct TAB_INFO {

    int n_bytes;  // alloc data
    int offset;   // address offset

  } info[N_MAX];

};


static SH_TAB_INFO *shm_tab_info = 0;

static key_t shm_key()
{
  return ftok ("/tmp/logipckey", PROC_ID);
}


int shm_create(int size)
{
  shm_id = shmget(shm_key(), size, 0600 | IPC_CREAT);
  //printf("id=%d\n", shm_id);

  if (shm_id < 0) return shm_id;

  shm_addr = (char*)shmat(shm_id, 0, 0); // attach segment

  if (intptr_t(shm_addr+1) == 0) {
    perror("shmat");
    exit(20);
  }

  shm_tab_info = (SH_TAB_INFO*)shm_addr;
  shm_tab_info->magic = 0;

  shm_size = size;

  shm_alloc_addr = shm_addr + ((sizeof(SH_TAB_INFO) + 15) & 0xfffffff0);
  
  shm_tab_info->n     = 0;
  shm_i = 0;

  return shm_id;
}


void shm_mark_ready()
{
  shm_tab_info->magic = MAGIC_READY;
}


int shm_access(int size)
{
  shm_id = shmget(shm_key(), size, 0440);  // read only access
  //printf("id=%d\n", shm_id);

  if (shm_id < 0) return shm_id;

  shm_addr = (char*)shmat(shm_id, 0, 0); // attach segment

  if (intptr_t(shm_addr+1) == 0) {
    perror("shmat");
    exit(20);
  }

  shm_size = size;
  
  shm_tab_info = (SH_TAB_INFO*)shm_addr;

  shm_alloc_addr = 0;
  shm_i = 0;  // for next_alloc

  // wait until creator has setup everything

  FOREVER {

    if (shm_tab_info->magic == MAGIC_READY) break;

    printf("."); fflush(stdout);
    SLEEP(1);
  }

  return shm_id;
}

void shm_release()
{
  if (shm_id >= 0) {
    printf("return shm\n");
    if (shmctl(shm_id, IPC_RMID, NULL) < 0) { // destroy when no longer needed
      perror("remove problems");
      exit(20);
    }
  }
}

char *shm_alloc(int n_bytes)
{
  char *ret;  

  if (n_bytes < 1) Error("shm_alloc: n<1");

  shm_alloc_addr = (char*)(intptr_t(shm_alloc_addr + 15) & ~intptr_t(0xf));
  ret = shm_alloc_addr;
  shm_alloc_addr += n_bytes;

  if (shm_alloc_addr >= shm_addr + shm_size) Error("shm: out of memory");
  if (shm_tab_info->n >= SH_TAB_INFO::N_MAX) Error("too many shm_allocs");

  shm_tab_info->info[shm_tab_info->n].n_bytes = n_bytes;
  shm_tab_info->info[shm_tab_info->n].offset  = ret-shm_addr;
  shm_tab_info->n++;

  return ret;
}


char *shm_next_alloc(int n_bytes)
{
  if (shm_i >= shm_tab_info->n) Error("no next alloc");
  if (shm_tab_info->info[shm_i].n_bytes != n_bytes) Error("# bytes different");

  return shm_addr+shm_tab_info->info[shm_i++].offset;
}


void shm_kill()
{
  int id = shmget(shm_key(), 1000, 0600);
  if (id >= 0) {

    printf("kill shm\n");
    if (shmctl(id, IPC_RMID, NULL) < 0) { // destroy when no longer needed
      Error("shm kill problems");
      exit(20);
    }

  } else Error("already killed");
}

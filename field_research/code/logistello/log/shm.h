// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef SHM_H
#define SHM_H

int  shm_create(int size);
void shm_mark_ready();
int  shm_access(int size);
void shm_release();
char *shm_alloc(int n_bytes);
char *shm_next_alloc(int n_bytes);
void shm_kill();

#endif

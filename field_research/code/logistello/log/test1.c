// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

typedef int TTYPE;

int p[10];
TTYPE t[100];

int val;
int *patterns = p;
TTYPE *tab = t;

#define P(i) *(int*)((char*)pt+i)

int main() 
{
  register int v = val;
  register TTYPE *pt = tab;
  register int *pat = patterns;

  for (register int i=50000000; i > 0; i--) {

#if 0

    __asm__ __volatile__ (

"movl (%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 4(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 8(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 12(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 16(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 20(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 24(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 28(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl (%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 4(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 8(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 12(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 16(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 20(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 24(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 28(%0),%%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n"
	: "=r" (pat), "=r" (pt), "=r" (v)
        : "r" (pat), "r" (pt), "r" (v)
        : "cc", "eax"
    );

#else

    __asm__ __volatile__ (

"movl (%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 4(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 8(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 12(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 16(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 20(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 24(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 28(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl (%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 4(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 8(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 12(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 16(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 20(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 24(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n\t\
movl 28(%0),%%eax\n\t\
negl %%eax\n\t\
movswl (%1,%%eax),%%eax\n\t\
addl %%eax,%2\n"
	: "=r" (pat), "=r" (pt), "=r" (v)
        : "r" (pat), "r" (pt), "r" (v)
        : "cc", "eax", "ebx"
    );



#endif


  }
  return v;
}


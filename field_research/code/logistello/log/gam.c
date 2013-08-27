// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/* genetic algorithm for board classification */

#include "main.h"
#include "sboard.h"
#include "crt.h"
#include "move.h"
#include "eval.h"
#include "killer.h"
#include "featurem.h"
#include "trans.h"

#define POOL_NUM   600  /* %4=0 */
#define START_SIZE 60
#define MAX_SIZE   60
#define TEST_NUM   300

#define SUCC_MAX  3
#define NODE_MAX  (2*MAX_SIZE)

#define INPUT_NUM (DATA_NUM+CONST_NUM+HINT_NUM)
#define DATA_NUM  64
#define CONST_NUM 9
#define HINT_NUM  1

#define F_NUM     9


/* 54 discs, root
     

<= 10 (hints):

            50      100      200     500

    200:   33%(10)


<= 20 (hints):

            50      100      200

    200:   33%(13) 32%(16)  31%(19)


54 discs, random


<= 20 (hints)

            50      100      170  1175

    200:                          33%(19)


<= 50 (hints)

            50      100      170

    200:           28%(47)  26%(46)

*/



#define FIT(s,size) (-(s)-((size)/10000.0))

typedef enum { 

  INPUT=0, IFZ, AD2, AD3, AN2, AN3, PP, PN, NN, PPP, PPN, PNN, NNN 

} FNAME;




#define KMAX      SUCC_MAX

typedef int (*FUNC)(int*);


struct _NODE_INFO;

typedef struct _NODE {

  FNAME        fn;
  FUNC         f;
  int          num_args;
  int          *pinput;  /* if fn==INPUT ... */
  
  struct _NODE *pred, *succ[SUCC_MAX];

  struct _NODE_INFO *pi;

  bool         constant;
  int          c;

} NODE;


typedef struct _NODE_INFO {

  NODE *pn;
  int depth;
  int size;
  double fit;
  int  vals[TEST_NUM][8]; 
  int  *pv;

} NODE_INFO;


typedef struct {

  NODE   *root;
  int    size;
  bool   fitted;
  double fit;

} EVAL_TREE;


typedef struct {

  int boards[8][INPUT_NUM];
  int result;
  int index;

} EXAMPLE;




EVAL_TREE Pool[3*POOL_NUM];

int finput[INPUT_NUM];

EXAMPLE Examples[TEST_NUM];

int num0, num1;



int hint_mob(SPFELD *psf)
{
  SFPOS moves[65];

  return SfMoeglZuege(psf, BLACK, moves) - SfMoeglZuege(psf, BLACK, moves);
}



#define SUM(co,c1,c2,x) 			\
						\
  if (p[co]) {					\
						\
    sum += 4*p[co] + p[c1] + p[c2] + p[x];	\
						\
  } else {					\
						\
    sum += -4*p[x] - 2*p[c1] - 2*p[c2];		\
						\
  }


int hint_corner(SPFELD *psf)
{
  int sum=0;
  SFPOS *p=psf->p;

  SUM(A1,B1,A2,B2);
  SUM(H1,G1,H2,G2);
  SUM(A8,A7,B8,B7);
  SUM(H8,H7,G8,G7);

  return sum;
}


int hint_diff(SPFELD *psf) 
{
  return SfAnzBLACK(psf) - SfAnzWHITE(psf);
}



char *InputNames[INPUT_NUM] = {

/*DATA*/

  "A1",   "B1",   "C1",   "D1",   "E1",   "F1",   "G1",   "H1",
  "A2",   "B2",   "C2",   "D2",   "E2",   "F2",   "G2",   "H2",
  "A3",   "B3",   "C3",   "D3",   "E3",   "F3",   "G3",   "H3",
  "A4",   "B4",   "C4",   "D4",   "E4",   "F4",   "G4",   "H4",
  "A5",   "B5",   "C5",   "D5",   "E5",   "F5",   "G5",   "H5",
  "A6",   "B6",   "C6",   "D6",   "E6",   "F6",   "G6",   "H6",
  "A7",   "B7",   "C7",   "D7",   "E7",   "F7",   "G7",   "H7",
  "A8",   "B8",   "C8",   "D8",   "E8",   "F8",   "G8",   "H8",

/*CONST*/

  "+0",   "+1",   "-1",   "+2",   "-2",   "+3",   "-3",   "+4",
  "-4",

/*HINT*/

  "fd" /*"fm", "fc" */

};


int (*f_hint[HINT_NUM])(SPFELD*) = {

  hint_diff
/*  hint_mob,
  hint_corner
*/
};




int fid(int *r)
{
  return r[0];
}

int fad2(int *r)
{
  return r[0]+r[1];
}

int fad3(int *r)
{
  return r[0]+r[1]+r[2];
}

int fan2(int *r)
{
  return -(r[0]+r[1]);
}

int fan3(int *r)
{
  return -(r[0]+r[1]+r[2]);
}

int fifz(int *r)
{
  return r[0] == 0 ? r[1] : r[2];
}

int fpp(int *r)
{
  if (r[0] > 0 && r[1] > 0) return r[0]+r[1];
  if (r[0] < 0 && r[1] < 0) return r[0]+r[1];
  return 0;
}

int fpn(int *r)
{
  if (r[0] > 0 && r[1] < 0) return r[0]-r[1];
  if (r[0] < 0 && r[1] > 0) return r[0]-r[1];
  return 0;
}

int fnn(int *r)
{
  if (r[0] < 0 && r[1] < 0) return -(r[0]+r[1]);
  if (r[0] > 0 && r[1] > 0) return -(r[0]+r[1]);
  return 0;
}


int fppp(int *r)
{
  if (r[0] > 0 && r[1] > 0 && r[2] > 0) return r[0]+r[1]+r[2];
  if (r[0] < 0 && r[1] < 0 && r[2] < 0) return r[0]+r[1]+r[2];
  return 0;
}

int fppn(int *r)
{
  if (r[0] > 0 && r[1] > 0 && r[2] < 0) return r[0]+r[1]-r[2];
  if (r[0] < 0 && r[1] < 0 && r[2] > 0) return r[0]+r[1]-r[2];
  return 0;
}

int fpnn(int *r)
{
  if (r[0] > 0 && r[1] < 0 && r[2] < 0) return r[0]-r[1]-r[2];
  if (r[0] < 0 && r[1] > 0 && r[2] > 0) return r[0]-r[1]-r[2];
  return 0;
}

int fnnn(int *r)
{
  if (r[0] < 0 && r[1] < 0 && r[2] < 0) return -r[0]-r[1]-r[2];
  if (r[0] > 0 && r[1] > 0 && r[2] > 0) return -r[0]-r[1]-r[2];
  return 0;
}




  
struct FDEF {

  FNAME    fn;
  char     *fstring;
  int      num_args;
  FUNC     f;

} fdefs[] = {

  { INPUT, "INP", 0, NULL   },
  { IFZ,   "IFZ", 3, fifz   }, 
  { AD2,   "AD2", 2, fad2   },
  { AD3,   "AD3", 3, fad3   },
  { AN2,   "AN2", 2, fan2   },
  { AN3,   "AN3", 3, fan3   },
  { PP,    "PP ", 2, fpp    },
  { PN,    "PN ", 2, fpn    },
  { NN,    "NN ", 2, fnn    }

/*
  { PPP,   "PPP", 3, fppp   },
  { PPN,   "PPN", 3, fppn   },
  { PNN,   "PNN", 3, fpnn   },
  { NNN,   "NNN", 3, fnnn   }*/

}; 


void _abort(void) { exit(10); }



void fill_constants(int *p)
{
  int k, l=0;

  FOR (k, CONST_NUM) {
    p[k+DATA_NUM] = l; l = -l;
    if (l >= 0) l++;
  }    
}

void fill_hints(int *p, SPFELD *psf)
{
  int k;

  FOR (k, HINT_NUM) p[k+DATA_NUM+CONST_NUM] = (*f_hint[k])(psf);
}


int randmod(int n)
{
  static double RandMaxInv = 1.0/(RAND_MAX-1);

  return (int) ((random() * RandMaxInv) * n);
}


int comp_EXAMPLE(const void *a, const void *b)
{
  const EXAMPLE *ea=a, *eb=b;

  return (ea->result) - (eb->result);

}


int comp_EVAL_TREE(const void *a, const void *b)
{
  float r;
  const EVAL_TREE *ea=a, *eb=b;

  r = (eb->fit) - (ea->fit);

  if (r > 0.0) return +1;
  if (r < 0.0) return -1;
  return 0;
}


int comp_NODE_INFO(const void *a, const void *b)
{
  float r;
  const NODE_INFO *ea=a, *eb=b;

  r = eb->fit - ea->fit;

  if (r > 0.0) return +1;
  if (r < 0.0) return -1;

  return 0;
}





/* generates "almost" uniformly distributed partition */

static int comp_int(const void *a, const void *b)
{
  return *((int*)a)-*((int*)b);
}


void interval_divide(int n, int k, int *a)
{
  int b0, i, j, st, b[KMAX];
 
  if (k > KMAX) Error("too many intervals");

  if (n == 0) {

    FOR (i, k) a[i] = 0;
    return;

  }

  if (k == 1) {

    a[0] = n;
    return;

  }


  FOR (i, k-1) {

    b[i] = randmod(n);
    if (b[i] >= n) Error(">= n");

  }

  qsort(b, k-1, sizeof(int), comp_int);

  st = randmod(n);

  if (st >= n) Error(">= n");

#if 0
printf("st=%d  ", st);
  FOR (i, k-1) printf("%d ", b[i]);
printf("\n");
#endif


  FOR (i, k-1) if (b[i] >= st) break;


  if (i >= k-1) i = 0;

  b0 = st;

  a[0] = b[i] - st;
  if (a[0] < 0) a[0] += n;

  b0 = b[i];

  for (j=1; j <= k-2; j++) {

    if (j+i >= k-1) { i = -j; }

    a[j] = b[j+i] - b0;

    if (a[j] < 0) a[j] += n;

    b0 = b[j+i];

  }

  a[k-1] = st - b0;
  if (a[k-1] <= 0) a[k-1] += n;  /* <=!!! consider all points equal */

  if (a[0] == 0 || a[k-1] == 0) { /* otherwise insymmetry! */

    if (random() & 1024) { i = a[0]; a[0] = a[k-1]; a[k-1] = i; }
 
  } 


{ int s=0;
  FOR (i, k) s += a[i];
  if (s != n) { 

    printf("\nst=%d n=%d k=%d s=%d\n", st, n, k, s);
    FOR (i, k-1) printf("%d ", b[i]);
    printf(" ->\n");

    FOR (i, k) printf("%d ", a[i]);
    printf("\n");

    Error("s!=n");
  }
}

}



NODE *gen_tree(int n)
{
  int  num_args, i, a[SUCC_MAX];
  NODE *pn = (NODE*) malloc(sizeof(NODE));

  if (!pn) Error("no mem");

  pn->pred = NULL;

  if (n == 2) n = 1;

  if (n-1 > 0) {

/* function */


/* choose function != INPUT with at most n-1 inputs */

    do { 

      i = randmod(F_NUM-1)+1; 

    } while (fdefs[i].num_args > n-1 /*|| (n-1 > 1 && i == ID)*/);

    pn->fn = i;
    pn->f  = fdefs[i].f;
    pn->pinput = NULL;

    num_args = pn->num_args = fdefs[i].num_args;

    interval_divide(n-1-num_args, num_args, a);

    FOR (i, num_args) {

      pn->succ[i] = gen_tree(1+a[i]);
      pn->succ[i]->pred = pn;

    }

    for (; i < SUCC_MAX; i++) pn->succ[i] = NULL;


  } else {


/* input */

    pn->fn     = INPUT;
    pn->pinput = &finput[randmod(INPUT_NUM)];
    pn->num_args = 0;

    FOR (i, SUCC_MAX) pn->succ[i] = NULL;

  }

  return pn;
}




static char out[1000];

static void show_node(NODE *pn)
{
  int i, len, num_succs;
  static int inputs[INPUT_NUM];
  static int init=false;

  if (!init) { fill_constants(inputs); init = true; }

  len = strlen(out);
if (len > 500) Error("outlen");

  num_succs = fdefs[pn->fn].num_args;

  if (num_succs == 0) {

    printf("+:%s\n", InputNames[pn->pinput-finput]);
    return;

  } 

  printf("+%s", fdefs[pn->fn].fstring);


  if (0 < num_succs-1) strcat(out, "   |"); else strcat(out, "    ");

  show_node(pn->succ[0]);

  out[len] = 0;

  for (i=1; i < num_succs; i++) {

    printf("%s   ", out);

    if (i < num_succs-1) strcat(out, "   |"); else strcat(out, "    ");

    show_node(pn->succ[i]);

    out[len] = 0;
  }

}


void show_tree(NODE *pn)
{
  strcpy(out, " ");

  show_node(pn);
}


static void write_tree_sub(NODE *pn)
{
  static int inputs[INPUT_NUM];
  static int init=false;
  int i, num_succs = fdefs[pn->fn].num_args;;


  if (!init) { fill_constants(inputs); init = true; }

  if (num_succs == 0) {

    printf("%s", InputNames[pn->pinput-finput]);

  } else {

    printf("(%s ", fdefs[pn->fn].fstring);
    
    FOR (i, num_succs) { 

      write_tree_sub(pn->succ[i]);

      if (i < num_succs-1) printf(" ");
    }

    printf(")");

  }
}

void write_tree(NODE *pn)
{
  write_tree_sub(pn);
  printf("\n");
}



int eval_tree(NODE *pn)
{
  int a, r[SUCC_MAX];

  if (pn->fn == INPUT) a = *(pn->pinput);

  else {

    switch (pn->num_args) {

      case 3: r[2] = eval_tree(pn->succ[2]);
      case 2: r[1] = eval_tree(pn->succ[1]);
      case 1: r[0] = eval_tree(pn->succ[0]);
              break;

      default: printf("%d\n", pn->num_args); Error("eval_tree");
    }

    a = (*(pn->f))(r);

  }

  *(pn->pi->pv)++ = a;
  return a;

}


#if 0

/* lame */

int eval_tree_iter(NODE *pn)
{
  int depth;
  struct { NODE *pn; int r[SUCC_MAX]; int i; } st[NODE_MAX], *pst;

  st[0].i   = 0;  
 
  st[1].pn  = pn;
  st[1].i   = 0;

  pst = &st[1];

  while (pst > &st[0]) {

/*
printf("%d %d %d\n", pst-st, pst->i, pst->pn->num_args);
*/
    register NODE *pn = pst->pn;

    if (pn->fn == INPUT) {

      pst--;
      pst->r[pst->i] = *(pn->pinput);
      pst->i++;
      
    } else if (pst->i >= pn->num_args) {

      pst--;
      pst->r[pst->i] = (*(pn->f))((pst+1)->r);
      pst->i++;

    } else {

      pst++;
      pst->pn = pn->succ[(pst-1)->i];
      pst->i  = 0;

    }

  }

#if 0
  if (st[0].r[0] != eval_tree(pn)) Error("evals not equal");
#endif

  return st[0].r[0];
}
#endif





int collect_nodes(NODE *pn, NODE_INFO **ppi, int depth)
{
  int i, size=0;
  NODE_INFO *piold=*ppi;

  (*ppi)->pn = pn;
  pn->pi = (*ppi);

  (*ppi)->depth = depth;
  (*ppi)++;

  FOR (i, fdefs[pn->fn].num_args) {

    if (!pn->succ[i]) Error("collect_moves: !succ");

    size += collect_nodes(pn->succ[i], ppi, depth+1);
  }

  piold->size = size+1;

  return size+1;
}


/* first choose depth, then node at this depth */

NODE *choose_node(NODE *pn, int *pnum_nodes)
{
  int       i, k, d, max_depth, num;
  NODE_INFO a[NODE_MAX], *ppi=a;

  *pnum_nodes = num = collect_nodes(pn, &ppi, 0);

  max_depth = -1;
  FOR (i, num) if (a[i].depth > max_depth) max_depth = a[i].depth;

  d = randmod(max_depth+1);

  k = 0;

  FOR (i, num) if (a[i].depth == d) { a[k] = a[i]; k++; }

  return a[randmod(k)].pn;
}





NODE *clone_tree(NODE *pn)
{
  int i;
  NODE *pnew = (NODE*)malloc(sizeof(NODE));

  if (!pnew) Error("no mem");

  *pnew = *pn;

  FOR (i, fdefs[pn->fn].num_args) {

    pnew->succ[i] = clone_tree(pn->succ[i]);
    pnew->succ[i]->pred = pnew;
    
  }

  return pnew;
}



void free_tree(NODE *pn)
{
  int i;

  FOR (i, fdefs[pn->fn].num_args) free_tree(pn->succ[i]);

  free(pn);
}


int trees_equal(NODE *p1, NODE *p2)
{
  int i;

  if (p1->fn == p2->fn && p1->pinput == p2->pinput && 
      !((p1->pred == NULL) ^ (p2->pred == NULL))) {

    FOR (i, fdefs[p1->fn].num_args) {

      if (!trees_equal(p1->succ[i], p2->succ[i])) return 0;

      if (p1->succ[i]->pred != p1 || p2->succ[i]->pred != p2) return 0;
    }

    return 1;
  }

  return 0;
}


void mutate_node(NODE *pn)
{
  int i;

  if (pn->fn == INPUT) {

    pn->pinput = &finput[randmod(INPUT_NUM)];

  } else {

    do { 

      i = randmod(F_NUM-1)+1; 

    } while (fdefs[i].num_args != fdefs[pn->fn].num_args);


    pn->fn = i;
    pn->f  = fdefs[i].f;
  }
}


void mutate_random_node(NODE *root)
{
  int num;
  NODE *pn = choose_node(root, &num);

  mutate_node(pn);
}


void swap_random_trees(NODE **pp1, NODE **pp2)
{
  int  i, num1, num2;
  NODE root1, root2, *pn1, *pn2, *pred1, *pred2, *pn;
  
  root1.fn = INPUT;
  root1.succ[0] = *pp1;
  root1.pred = NULL;
  (*pp1)->pred = &root1;

  root2.fn = INPUT;
  root2.succ[0] = *pp2;
  root2.pred = NULL;
  (*pp2)->pred = &root2;

  pn1 = choose_node(*pp1, &num1);
  pn2 = choose_node(*pp2, &num2);


#if 0
printf("pn1:\n");
show_tree(pn1);
printf("pn2:\n");
show_tree(pn2);
#endif

  pred1 = pn = pn1->pred;
  FOR (i, fdefs[pn->fn].num_args) if (pn->succ[i] == pn1) break;
  pn->succ[i] = pn2;

  pred2 = pn = pn2->pred;
  FOR (i, fdefs[pn->fn].num_args) if (pn->succ[i] == pn2) break;
  pn->succ[i] = pn1;

  pn1->pred = pred2;
  pn2->pred = pred1;

  *pp1 = root1.succ[0];
  *pp2 = root2.succ[0];

  (*pp1)->pred = NULL;
  (*pp2)->pred = NULL;
  
}



#if 0

static void (*f_node)(NODE *pn);

static void f_nodes(NODE *pn)
{
  int i;

  FOR (i, fdefs[pn->fn].num_args) f_nodes(pn->succ[i]);

  (*f_node)(pn);

}


void f_tree(NODE *root, void (*f)(NODE*))
{
  f_node = f;
  f_nodes(root);
}


static int  f_result;

void f_update_s(NODE *pn) 
{ 
  if      (pn->v > 0) pn->v = +1;
  else if (pn->v < 0) pn->v = -1;

  pn->s += fabs(pn->v - f_result);
}

#endif


/* fitting */

void fit(EVAL_TREE *pt, EVAL_TREE *pt2)
{
  int i, j, k, v, num, vals[TEST_NUM], su;
  NODE_INFO a[MAX_SIZE], *ppi=a;

  num = collect_nodes(pt->root, &ppi, 0);

  FOR (i, num) a[i].pv = &a[i].vals[0][0];

  FOR (i, TEST_NUM) {

    FOR (j, 8) {

      memcpy(finput, Examples[i].boards[j], sizeof(finput));

      eval_tree(pt->root);

    }
  }

  FOR (k, num) {

    FOR (i, TEST_NUM) {

      v = 0;
      FOR (j, 8) { 
        v += a[k].vals[i][j]; 
      }

      vals[i] = v;

    }

#if 0
    FOR (i, TEST_NUM-1) if (vals[i] != vals[i+1]) break;

    a[k].constant = (i >= TEST_NUM);
    a[k].c        = vals[0];
#endif

/* compute discordance */

    qsort(vals,      num0, sizeof(vals[0]), comp_int);
    qsort(vals+num0, num1, sizeof(vals[0]), comp_int);

/* count pairs with val1 > val2 but cl1=loss and cl2=win */

    su = 0; i = 0;

    FOR (j, num0) {

      while (i < num1 && vals[j] >= vals[num0+i]) i++;

      su += i;
    }

    a[k].fit = FIT(su, a[k].size); 

  }




  {
    NODE *oldroot;


    qsort(a, pt->size, sizeof(a[0]), comp_NODE_INFO);

#if 0
    FOR (i, pt->size) {

      show_tree(a[i].pn);
      printf("%f\n", FIT(a[i].pn->s,a[i].size));
    }

    printf("\n\n\n\n");
#endif

/* return 2 best subtrees */

    oldroot = pt->root;

    pt->root = clone_tree(a[0].pn);
    pt->size = a[0].size;
    pt->root->pred = NULL;
    pt->fit  = a[0].fit;
    pt->fitted = true;

    if (pt->size > 1) {

      pt2->root = clone_tree(a[1].pn);
      pt2->size = a[1].size;
      pt2->root->pred = NULL;
      pt2->fit  = a[1].fit;
      pt2->fitted = true;
      
    } else pt2->root = NULL;

#if 0
show_tree(pt->root);
puts("");
show_tree(pt->root);
puts("");
show_tree(pt->root);
puts("");
#endif

    free_tree(oldroot);



  }
}






#define K  3
#define N  10


int main(int argc, char **argv)
{
  int i, j, k, l, v, num=0, sum_fits=0, a[KMAX], gen, ok=0;
  double s=0;
  long long sum[KMAX];
  NODE *pn, *pn1, *pn2, *pa, *pb;
  float sf;
  FILE  *fp;
  SPFELD board, boards[8];

  InitCrt();

  if (argc != 2) Error("call: oga sfk-file");

  FOR (i, KMAX) sum[i] = 0;

#if 0
  pa = gen_tree(10);

  show_tree(pa);

exit(0);
#endif

#if 0

  FOREVER {

  interval_divide(N, K, a);

#if 0
  FOR (i, K) printf("%d ", a[i]);
  printf("\n");
#endif

  num++;
  FOR (i, K) sum[i] += a[i];

  if ((num & 65535) == 0) {

    FOR (i, K) printf("%.3f ", (double)sum[i]/num);
    printf("\n");
  }

  }
#endif

#if 0

FOREVER {
  pn1 = gen_tree(20);
  pn2 = gen_tree(20);

  { NODE *pa = clone_tree(pn1);

printf(">> %d\n", comp_trees(pa, pn1));

    free_tree(pa);

  }
#if 0
printf("\nt1:\n");
  show_tree(pn1);

printf("t2:\n");
  show_tree(pn2);

  swap_random_trees(&pn1, &pn2);


printf("t1':\n");
  show_tree(pn1);

printf("t2':\n");
  show_tree(pn2);
#endif
}
#endif


  FOR (i, POOL_NUM+POOL_NUM/2) {

    Pool[i].root = gen_tree(randmod(MAX_SIZE)+1);
    choose_node(Pool[i].root, &Pool[i].size);
    Pool[i].fitted = false;

  }


#if 0

  FOR (i, TEST_NUM) {
    int j, k;

    FOR (j, DATA_NUM) test_inputs[i][j] = random() % 100 - 50; 

    k = 0;
    for (; j < INPUT_NUM; j++) { 
      test_inputs[i][j] = k; k = -k;
      if (k >= 0) k++;
    }
  }  

#else

  fp = fopen(argv[1], "r");

  FOR (i, TEST_NUM) {
  
    int j, k;

    do {

      if (fSfRead(fp, &board, 1) != 1) Error("too few boards");

      if (board.Marke < MA_DIFF || board.Marke > MA_DIFF+128) 
	Error("unknown label");

    } while(board.Marke == MA_DIFF + 64);

#if 1

    if (board.Marke > MA_DIFF + 64) Examples[i].result = +1; 
    else                            Examples[i].result = -1;

#else

    test_results[i] = board.p[A1]+board.p[A8]+board.p[H1]+board.p[H8];

#endif

    Examples[i].index = i;

    Transform(&board, boards);

    v = 0;

    FOR (j, 8) {

      FOR (k, 64) {

        Examples[i].boards[j][k] = boards[j].p[11+k/8+10*(k & 7)]; 

      }

      fill_constants(Examples[i].boards[j]);
      fill_hints(Examples[i].boards[j], &boards[j]);

      v += boards[j].p[A1];

    }

    /*
      if (v > 0) v = +1;
      if (v < 0) v = -1;
      
      s += fabs(v - test_results[i]);
    */

  }

#endif


  /* sort examples loss<win */

  qsort(Examples, TEST_NUM, sizeof(Examples[0]), comp_EXAMPLE);

  FOR (i, TEST_NUM) if (Examples[i].result > 0) break;

  num0 = i;
  num1 = TEST_NUM - num0;  

  printf("num0=%d num1=%d\n", num0, num1);


  for (gen=1;; gen++) {

    int new_, num_fits=0;

    printf("\ngen=%5.0d ", gen);

pool_fit:

    k = POOL_NUM+POOL_NUM/2;

    FOR (i, POOL_NUM+POOL_NUM/2) 

      if (!Pool[i].fitted) {

	num_fits++;
	sum_fits++;

        fit(&Pool[i], &Pool[k]);

        if (Pool[k].root) k++;
      }

    qsort(Pool, k, sizeof(EVAL_TREE), comp_EVAL_TREE);

    for (i=POOL_NUM+POOL_NUM/2; i < k; i++) free_tree(Pool[i].root);


#if 1

    /* detect double occurrence, replace copies by new generated trees */

    new_ = 0;

    FOR (i, POOL_NUM+POOL_NUM/2-1) {

      if (Pool[i].size == Pool[i+1].size &&
          (fabs(Pool[i].fit - Pool[i+1].fit)) < 1) {

        if (trees_equal(Pool[i].root, Pool[i+1].root)) {

          free_tree(Pool[i].root);

          Pool[i].root = gen_tree(randmod(MAX_SIZE)+1);
          choose_node(Pool[i].root, &Pool[i].size);
          Pool[i].fitted = false;
          new_++;
	}

      }

    }


    if (new_) { /*printf("%d equal\n", new_);*/ goto pool_fit; }

#endif

printf("%7d %5d ", sum_fits, num_fits);

#if 1

printf("\n");
show_tree(Pool[1].root);
printf("second=%f %d\n\n", Pool[1].fit, Pool[1].size); 
show_tree(Pool[0].root);
printf("best=%f %d\n\n", Pool[0].fit, Pool[0].size); 


#if 0
show_tree(Pool[1].root);
printf("second=%f\n", Pool[1].fit); 
#endif
#endif

FOR (i, 6) printf("[%d:%.1f %d] ", i+1, (100.0*Pool[i].fit)/num0/num1, Pool[i].size);
printf("\n");

FOR (i, POOL_NUM-1) if (Pool[i].fit < Pool[i+1].fit) Error("qsort?");


/* free worst */

    FOR (i, POOL_NUM/2) free_tree(Pool[i+POOL_NUM].root);


/* new */

    for (i=POOL_NUM+POOL_NUM/4; i <= POOL_NUM+POOL_NUM/2; i++) {

      Pool[i].root = gen_tree(randmod(MAX_SIZE)+1);
      choose_node(Pool[i].root, &Pool[i].size);
      Pool[i].fitted = false;

    }


    /* add / crossover */

    FOR (i, POOL_NUM/8) {

      int k1, k2, numa, numb;


      k1 = randmod((POOL_NUM-1) * (1.0 - sqrt(1.0-((float)random()/RAND_MAX))));
      k2 = randmod((POOL_NUM-1) * (1.0 - sqrt(1.0-((float)random()/RAND_MAX))));

      pa = clone_tree(Pool[k1].root);
      numa = Pool[k1].size;

      pb = clone_tree(Pool[k2].root);
      numb = Pool[k2].size;

      
      if (numa + numb < MAX_SIZE && randmod(100) <= 75) {

	/* add trees */

        NODE *root = (NODE*) malloc(sizeof(NODE));

        if (!root) Error("mem");

        root->pred     = NULL;
        root->fn       = AD2;
        root->f        = fad2;
        root->num_args = 2;
        root->pinput   = NULL;
        
        root->succ[0] = pa;
        root->succ[1] = pb;
        pa->pred = root;
        pb->pred = root;

        pa = root;
        pb = clone_tree(root);

        mutate_random_node(pb);
   
        numa = numb = numa + numb + 1;


      } else {

	/* crossover */

        do {
          
          swap_random_trees(&pa, &pb);
          choose_node(pa, &numa);
          choose_node(pb, &numb);
 
        } while (numa > MAX_SIZE || numb > MAX_SIZE);

	/* mutate new trees */

        mutate_random_node(pa);
        mutate_random_node(pb);


      }


      j = POOL_NUM + 2*i;

      Pool[j].root   = pa;
      Pool[j].size   = numa;
      Pool[j].fitted = false;
      
      Pool[j+1].root = pb;
      Pool[j+1].size = numb;
      Pool[j+1].fitted = false;

    }

  }

  return 0;
}

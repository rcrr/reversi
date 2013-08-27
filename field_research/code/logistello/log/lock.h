// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef LOCK_H
#define LOCK_H

// simple locking


// enter critical region

#define CR_ENTER(lock) while (lock.test_and_set());
#define CR_ENTER_VERBOSE(lock) \
  { static int num = 0, sum = 0;\
    num++;\
    while (lock.test_and_set()) sum++; \
    if ((num & (1048576*2-1)) == 0) cout << endl << num << ": " << double(sum)/num << endl;  \
  }

// leave critical region

#define CR_LEAVE(lock) lock.reset();



class Lock {

public:

  volatile uint1 val;

  inline Lock() { reset(); }

  inline void reset() { val = 0; }

  inline bool test_and_set()
  {
    sint4 old_bit;
    
    __asm__ __volatile__("lock; btsb $0,%1\n\tsbbl %0,%0"
			 :"=r" (old_bit),"=m" (val));
    return old_bit != 0;
  }


  inline void enter()
  {
    __asm__ __volatile__(
      "1: lock; btsb $0,%0\n\t"
      "   jnc 2f\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   testl %%eax,%%eax\n\t"
      "   jmp 1b\n\t"
      "2:\n\t"
      : "=m" (val) /* output */
      :            /* input */
      : "cc"       /* condition codes clobbered */
    );			 
  }

  inline void leave()
  {
    reset();
  }


  /*
  inline bool test_and_clear()
  {
    sint4 old_bit;
    
    __asm__ __volatile__("lock; btrl $0,%1\n\tsbbl %0,%0"
			 :"=r" (old_bit),"=m" (val));
    return old_bit != 0;
  }
  */

};


#endif

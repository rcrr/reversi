#include <signal.h>

#if !defined (NSIG)
#  if defined (_NSIG)
#    define NSIG _NSIG
#  else
#    define NSIG 64
#  endif /* !_NSIG */
#endif /* !NSIG */

static char *sig_list[NSIG];

static void sig_init_list( void )
{
register int i;

  for (i = 0; i < NSIG; i++)
    sig_list[i] = (char *)0x0;

  sig_list[0] = "Bogus signal";

#if defined (SIGHUP)
  sig_list[SIGHUP] = "Hangup signal";
#endif

#if defined (SIGINT)
  sig_list[SIGINT] = "Interrupt";
#endif

#if defined (SIGQUIT)
  sig_list[SIGQUIT] = "Quit signal";
#endif

#if defined (SIGILL)
  sig_list[SIGILL] = "Illegal instruction";
#endif

#if defined (SIGTRAP)
  sig_list[SIGTRAP] = "BPT trace/trap";
#endif

#if defined (SIGIOT) && !defined (SIGABRT)
#define SIGABRT SIGIOT
#endif

#if defined (SIGABRT)
  sig_list[SIGABRT] = "ABORT instruction";
#endif

#if defined (SIGEMT)
  sig_list[SIGEMT] = "EMT instruction";
#endif

#if defined (SIGFPE)
  sig_list[SIGFPE] = "Floating point exception";
#endif

#if defined (SIGKILL)
  sig_list[SIGKILL] = "Kill signal";
#endif

#if defined (SIGBUS)
  sig_list[SIGBUS] = "Bus error";
#endif

#if defined (SIGSEGV)
  sig_list[SIGSEGV] = "Segmentation fault";
#endif

#if defined (SIGSYS)
  sig_list[SIGSYS] = "Bad system call";
#endif

#if defined (SIGPIPE)
  sig_list[SIGPIPE] = "Broken pipe condition";
#endif

#if defined (SIGALRM)
  sig_list[SIGALRM] = "Alarm clock signal";
#endif

#if defined (SIGTERM)
  sig_list[SIGTERM] = "Termination signal";
#endif

#if defined (SIGURG)
  sig_list[SIGURG] = "Urgent IO condition";
#endif

#if defined (SIGSTOP)
  sig_list[SIGSTOP] = "Stop signal";
#endif

#if defined (SIGTSTP)
  sig_list[SIGTSTP] = "Stopped";
#endif

#if defined (SIGCONT)
  sig_list[SIGCONT] = "Continue signal";
#endif

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif

#if defined (SIGCHLD)
  sig_list[SIGCHLD] = "Child signal";
#endif

#if defined (SIGTTIN)
  sig_list[SIGTTIN] = "Stop (tty input) signal";
#endif

#if defined (SIGTTOU)
  sig_list[SIGTTOU] = "Stop (tty output) signal";
#endif

#if defined (SIGIO)
  sig_list[SIGIO] = "I/O ready signal";
#endif

#if defined (SIGXCPU)
  sig_list[SIGXCPU] = "CPU limit exceeded";
#endif

#if defined (SIGXFSZ)
  sig_list[SIGXFSZ] = "File limit exceeded";
#endif

#if defined (SIGVTALRM)
  sig_list[SIGVTALRM] = "Alarm (virtual)";
#endif

#if defined (SIGPROF)
  sig_list[SIGPROF] = "Alarm (profile)";
#endif

#if defined (SIGWINCH)
  sig_list[SIGWINCH] = "Window change";
#endif

#if defined (SIGLOST)
  sig_list[SIGLOST] = "Record lock signal";
#endif

#if defined (SIGUSR1)
  sig_list[SIGUSR1] = "User signal 1";
#endif

#if defined (SIGUSR2)
  sig_list[SIGUSR2] = "User signal 2";
#endif

#if defined (SIGMSG)
  sig_list[SIGMSG] = "HFT input data pending signal";
#endif 

#if defined (SIGPWR)
  sig_list[SIGPWR] = "power failure imminent signal";
#endif 

#if defined (SIGDANGER)
  sig_list[SIGDANGER] = "system crash imminent signal";
#endif 

#if defined (SIGMIGRATE)
  sig_list[SIGMIGRATE] = "Process migration";
#endif 

#if defined (SIGPRE)
  sig_list[SIGPRE] = "Programming error signal";
#endif 

#if defined (SIGGRANT)
  sig_list[SIGGRANT] = "HFT monitor mode granted signal";
#endif 

#if defined (SIGRETRACT)
  sig_list[SIGRETRACT] = "HFT monitor mode retracted signal";
#endif 

#if defined (SIGSOUND)
  sig_list[SIGSOUND] = "HFT sound sequence has completed signal";
#endif 

  for (i = 0; i < NSIG; i++)
     if (!sig_list[i]) {
	 sig_list[i] = (char*) malloc (10 + strlen ("Unknown Signal #"));
	 sprintf (sig_list[i], "Unknown Signal #%d", i);
     }
}

static void sig_handler(int i)
{
   fprintf(stderr,"[SIGNAL]: %d / %s - IGNORE !\n", i, sig_list[i]) ;
   return;
}

static void sig_init( int n )
{
int i ;

   sig_init_list();	
   if ( n >= 0 ) 
#ifdef __sparc__
signal(n, sig_handler, -1);
#else
signal(n, sig_handler);
#endif
   else
      for (i = 0; i < NSIG; i++) 
#ifdef __sparc__
signal(i, sig_handler, -1);
#else
signal(i, sig_handler);
#endif
   fprintf(stderr,"[SIGNAL %d] handler ready\n",n);
}


// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// simple timer (CPU + REAL)

#ifndef Timer_H
#define Timer_H

#include <sys/time.h>
#include <sys/resource.h>


class Timer {

public:

  enum Mode { CPU_TIME, REAL_TIME };

private:

  int sec, usec;
  Mode mode;

public: 

  Timer(Mode m = CPU_TIME) { in_mode(m); stamp(); }
  
  void in_mode(Mode m) { mode = m; }
  Mode in_mode() const { return mode; }

  void stamp() { 

    if (mode == CPU_TIME) {

      struct rusage ru; 

      if (getrusage(RUSAGE_SELF, &ru) == -1 ) Error("getrusage");

      sec  = ru.ru_utime.tv_sec;
      usec = ru.ru_utime.tv_usec;

      // add system time (SMP bug workaround)

      sec  += ru.ru_stime.tv_sec;
      usec += ru.ru_stime.tv_usec;
  
      if (usec >= 1000000) {
	usec -= 1000000;
	sec++;
      }

    } else {

      struct timeval tv;
      struct timezone tz;

      gettimeofday(&tv, &tz);

      sec  = tv.tv_sec;
      usec = tv.tv_usec;
    }
  }

 
  real diff(Timer &t) {
    return real(sec - t.sec) + real(usec - t.usec) / 1000000;
  }
};


#endif

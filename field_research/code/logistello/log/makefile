# LINUX makefile

CC	= g++
CC_OPTS	= -DGCC -pipe 

#CC     = icc -O3 -Kc++ -xK -w -tpp6 -ipo -i_dynamic
#CC_OPTS        = 
# change icc option below!

# normal, debug, prof

MODE = normal


# gcc/g++ settings

WARN	= -Wall 

OPT0	= -O16 \
	  -fstrength-reduce \
	  -fthread-jumps \
          -frerun-cse-after-loop \
	  -fexpensive-optimizations \
	  -ffast-math \
	  -finline-functions

OBJ       = ../obj
OBJ_DBG   = ../obj.debug
OBJ_PROF  = ../obj.prof
OBJ_BP    = ../obj.bp
OBJ_ARCS  = ../obj.arcs

LIBS    = -lm
BP_LIBS = /usr/lib/bmon.o -lm
PROF_LIBS = -lm

EVAL_EXCEPTION = -fno-force-addr

MACHINE_OPT = $(OPT0)
OPT         = -fomit-frame-pointer

SELF_PROTOS = 0
USLEEP      = 1
SMALL_HASH  = 0

include make.i



OFLAGS	= $(WARN) $(OPT) $(MACHINE_OPT)
DFLAGS	= $(WARN) -g
PFLAGS	= $(WARN) $(MACHINE_OPT) -pg -lc
BPFLAGS = $(WARN) -O2 -g

ifeq ("$(MODE)", "normal")
  FLAGS   = $(OFLAGS) -DNDEBUG
  STRIP   = strip
else

ifeq ("$(MODE)", "prof")
  FLAGS   = $(PFLAGS) -DNDEBUG
  STRIP   = echo
  OBJ = $(OBJ_PROF)
  LIBS = $(PROF_LIBS)
else

ifeq ("$(MODE)", "bp")
  FLAGS   = $(BPFLAGS) -DNDEBUG
  STRIP   = echo
  OBJ = $(OBJ_BP)
  LIBS = $(BP_LIBS)
else

ifeq ("$(MODE)", "dbg")
  FLAGS   = $(DFLAGS)
  STRIP   = echo
  OBJ = $(OBJ_DBG)
else

ifeq ("$(MODE)", "arcsp")
  FLAGS   = $(OFLAGS) -DNDEBUG -fprofile-arcs 
  STRIP   = strip
  OBJ = $(OBJ_ARCS)
else

ifeq ("$(MODE)", "arcsc")
  FLAGS   = $(OFLAGS) -DNDEBUG -fbranch-probabilities 
  STRIP   = strip
  OBJ = $(OBJ_ARCS)
else
  error
endif
endif
endif
endif
endif
endif

SRC	= 


CCOPTS	= -c -DUNIX $(CC_OPTS) -DSELF_PROTOS=$(SELF_PROTOS) \
             -DUSLEEP=$(USLEEP) -DSMALL_HASH=$(SMALL_HASH)  \
             $(FLAGS) 

LNOPTS  = $(FLAGS) 

LN	= $(CC)

GOOD_OBJ    =  	$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
	    	$(OBJ)/goodies.o

ZUGNULL_OBJ =  	$(OBJ)/sboard.o	\
		$(OBJ)/move.o	\
		$(OBJ)/eval.o	\
	   	$(OBJ)/board.o	\
		$(OBJ)/mid.o	\
		$(OBJ)/killer.o \
		$(OBJ)/hash.o \
		$(OBJ)/end.o	\
	   	$(OBJ)/patt.o 	\
	   	$(OBJ)/fpatt.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/trans.o	\
		$(OBJ)/game.o	\
		$(OBJ)/newgame.o\
	   	$(OBJ)/order.o	\
		$(OBJ)/newendeval.o\
		$(OBJ)/pcstat.o \
		$(OBJ)/util.o \
		$(OBJ)/normal.o \
		$(OBJ)/shm.o \
		$(OBJ)/pmove.o
 
PLAY_OBJ =	$(ZUGNULL_OBJ)			\
		$(GOOD_OBJ)			\
		$(OBJ)/filecom.o		\
		$(OBJ)/socketcom.o		\
		$(OBJ)/lib.o			\
		$(OBJ)/book.o			\
		$(OBJ)/Client.o			\
		$(OBJ)/ClientBoard.o		\
		$(OBJ)/ClientBoardType.o	\
		$(OBJ)/ClientClock.o		\
		$(OBJ)/ClientGame.o		\
		$(OBJ)/ClientGameType.o		\
		$(OBJ)/ClientMove.o		\
		$(OBJ)/ClientString.o		\
		$(OBJ)/ClientTypes.o		\
		$(OBJ)/OthClientBoard.o		\
		$(OBJ)/OthClientMove.o

oplaya_OBJ = 	$(OBJ)/playma.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/sparse.o \
		$(OBJ)/psconf.o \
		$(OBJ)/nnentry.o \
		$(OBJ)/nnbinfo.o \
		$(OBJ)/nnpatt.o \
		$(OBJ)/nnfeat.o \
		$(OBJ)/nnspatt.o \
		$(OBJ)/nndpatt.o \
		$(PLAY_OBJ)

oplay_OBJ = 	$(OBJ)/playma.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/f.o      \
		$(PLAY_OBJ)

oplayl_OBJ = 	$(OBJ)/playml.o	\
		$(OBJ)/evall.o	\
		$(PLAY_OBJ)

oplayb_OBJ = 	$(OBJ)/playmb.o	\
		$(OBJ)/evalb.o	\
		$(PLAY_OBJ)

oplayk_OBJ = 	$(OBJ)/playmk.o	\
		$(OBJ)/evalk.o	\
		$(OBJ)/sparse.o \
		$(OBJ)/psconf.o \
		$(PLAY_OBJ)

oplayd_OBJ = 	$(OBJ)/playmd.o	\
		$(OBJ)/evald.o	\
		$(PLAY_OBJ)

ohist_OBJ = 	$(OBJ)/histm.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/logwls.o	\
		$(OBJ)/logmat.o \
		$(OBJ)/logall.o

otest_OBJ = 	$(OBJ)/testm.o 	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(OBJ)/sparse.o	\
		$(OBJ)/psconf.o	\
		$(OBJ)/evall.o	\
		$(OBJ)/evalk.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o \
		$(OBJ)/sparse.o \
		$(OBJ)/psconf.o \
		$(OBJ)/nnentry.o \
		$(OBJ)/nnbinfo.o \
		$(OBJ)/nnpatt.o \
		$(OBJ)/nnfeat.o \
		$(OBJ)/nnspatt.o \
		$(OBJ)/nndpatt.o

odisp_OBJ = 	$(OBJ)/dispm.o \
	        $(OBJ)/crt.o \
		$(OBJ)/filecom.o \
	        $(OBJ)/game.o \
		$(OBJ)/goodies.o	\
		$(OBJ)/trans.o	\
	        $(OBJ)/sboard.o
 
okshm_OBJ = 	$(OBJ)/kshmm.o \
	        $(OBJ)/crt.o \
		$(OBJ)/shm.o

otextio_OBJ = 	$(OBJ)/textiom.o\
                $(OBJ)/sboard.o \
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/trans.o\
		$(OBJ)/game.o\
		$(OBJ)/filecom.o\
		$(OBJ)/goodies.o 

otcmp_OBJ = 	$(OBJ)/tcmpm.o\
                $(OBJ)/sboard.o \
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/trans.o\
		$(OBJ)/game.o\
		$(OBJ)/filecom.o\
		$(OBJ)/goodies.o 

ofil_OBJ = 	$(OBJ)/film.o\
                $(OBJ)/sboard.o \
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/trans.o\
		$(OBJ)/game.o\
		$(OBJ)/filecom.o\
		$(OBJ)/goodies.o 

omana_OBJ = 	$(OBJ)/manam.o\
                $(OBJ)/sboard.o \
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/trans.o\
		$(OBJ)/game.o\
		$(OBJ)/filecom.o\
		$(OBJ)/goodies.o 

odis_OBJ = 	$(OBJ)/dism.o \
		$(ZUGNULL_OBJ)	\
		$(OBJ)/crt.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/distr.o   \
		$(OBJ)/crtt.o   \
		$(OBJ)/goodies.o 

onodes_OBJ = 	$(OBJ)/nodesm.o \
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o 

oendstat_OBJ = 	$(OBJ)/endstatm.o 	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o 

oecmp_OBJ = 	$(OBJ)/ecmpm.o 	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o 

oendtest_OBJ = 	$(OBJ)/endtestm.o\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evall.o 	\
		$(OBJ)/evala.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o\
                $(OBJ)/newend.o
 
ohole_OBJ =	$(OBJ)/holem.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evall.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o 

otext_OBJ = 	$(OBJ)/textm.o	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o\
		$(OBJ)/filecom.o\
		$(OBJ)/sboard.o \
            	$(OBJ)/trans.o	\
		$(OBJ)/game.o

otex_OBJ = 	$(OBJ)/texm.o 	\
		$(OBJ)/crt.o	\
		$(OBJ)/crtt.o	\
		$(OBJ)/goodies.o\
		$(OBJ)/filecom.o\
		$(OBJ)/sboard.o \
            	$(OBJ)/trans.o	\
		$(OBJ)/game.o

otexsf_OBJ = 	$(OBJ)/texsfm.o \
		$(GOOD_OBJ)	\
		$(OBJ)/filecom.o\
		$(OBJ)/sboard.o \
            	$(OBJ)/trans.o	\
		$(OBJ)/game.o

omatch_OBJ = 	$(OBJ)/matchm.o \
		$(GOOD_OBJ)	\
		$(OBJ)/filecom.o\
		$(OBJ)/sboard.o \
            	$(OBJ)/trans.o	\
		$(OBJ)/game.o

otime_OBJ = 	$(OBJ)/timem.o	\
		$(ZUGNULL_OBJ)	\
		$(GOOD_OBJ)	\
		$(OBJ)/jcwend.o \
		$(OBJ)/evala.o

opartab_OBJ  = 	$(OBJ)/partabm.o	\
		$(ZUGNULL_OBJ)	\
		$(GOOD_OBJ)

ogen_OBJ  = 	$(OBJ)/genm.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evall.o	\
		$(GOOD_OBJ)

oga_OBJ   = 	$(OBJ)/gam.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(GOOD_OBJ)     \
		$(OBJ)/trans.o	\
		$(OBJ)/hash.o

ofgen_OBJ  = 	$(OBJ)/fgenm.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(OBJ)/f.o	\
		$(GOOD_OBJ)

ogenf_OBJ  = 	$(OBJ)/genfm.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o  \
	        $(OBJ)/f.o	\
		$(GOOD_OBJ)

obf_OBJ  = 	$(OBJ)/bfm.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o  \
		$(OBJ)/filecom.o\
		$(OBJ)/distr.o   \
		$(GOOD_OBJ)

ointer_OBJ  = 	$(OBJ)/interm.o	\
		$(OBJ)/sboard.o \
		$(OBJ)/nnentry.o \
		$(OBJ)/nnbinfo.o \
		$(OBJ)/nnpatt.o \
		$(OBJ)/nnfeat.o \
		$(OBJ)/nnspatt.o \
		$(OBJ)/nndpatt.o \
		$(OBJ)/sparse.o \
		$(OBJ)/psconf.o \
		$(ZUGNULL_OBJ)	\
		$(GOOD_OBJ)

osel_OBJ  = 	$(OBJ)/selm.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(GOOD_OBJ)

bpipint_OBJ =   $(OBJ)/bpipint.o \
		$(OBJ)/evala.o	\
		$(PLAY_OBJ)

        
ost_OBJ  = 	$(OBJ)/stm.o	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o	\
		$(GOOD_OBJ)
  
osign_OBJ = 	$(OBJ)/signm.o	\
		$(OBJ)/crt.o
  
oauto_OBJ  = 	$(OBJ)/autom.o	\
		$(OBJ)/filecom.o\
		$(GOOD_OBJ)	\
		$(OBJ)/sboard.o \
                $(OBJ)/trans.o  \
                $(OBJ)/game.o

obpat_OBJ  = 	$(OBJ)/bpatm.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/sboard.o \
                $(OBJ)/trans.o  \
                $(OBJ)/game.o

oextr_OBJ =	$(OBJ)/extrm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/trans.o	\
		$(OBJ)/game.o

opcextr_OBJ =	$(OBJ)/pcextrm.o\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/trans.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/game.o

oevol_OBJ =	$(OBJ)/evolm.o\
		$(OBJ)/evolopt.o\
		$(OBJ)/Rnd.o\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/trans.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/game.o

ofeature_OBJ = 	$(OBJ)/featurem.o\
		$(OBJ)/fpatt.o	\
		$(OBJ)/newendeval.o \
		$(OBJ)/patt.o	\
		$(OBJ)/board.o	\
		$(OBJ)/hash.o	\
                $(GOOD_OBJ)	\
		$(OBJ)/sboard.o	\
		$(OBJ)/feature.o\
		$(OBJ)/trans.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/eval.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/game.o

olog_OBJ  = 	$(OBJ)/logall.o	\
		$(OBJ)/logio.o	\
		$(OBJ)/logm.o	\
		$(OBJ)/logmat.o	\
		$(OBJ)/logwls.o	\
		$(OBJ)/logeig.o	\
	        $(GOOD_OBJ)	\
		$(OBJ)/sboard.o	\
		$(OBJ)/trans.o	\
		$(OBJ)/game.o

obay_OBJ =  	$(OBJ)/baym.	\
		$(OBJ)/bayan.o	\
		$(OBJ)/bayin.o 	\
	    	$(OBJ)/merkmo.o	\
		$(OBJ)/merkpo.o	\
		$(OBJ)/merkst.o	\
	    	$(OBJ)/sboard.o	\
		$(OBJ)/game.o	\
		$(OBJ)/crt.o

oedge_OBJ = 	$(OBJ)/edgem.o	\
		$(OBJ)/sboard.o	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o	\
		$(GOOD_OBJ)

omobtab_OBJ = 	$(OBJ)/mobtabm.o\
		$(GOOD_OBJ)	\
		$(OBJ)/trans.o	\
		$(OBJ)/sboard.o	\
		$(OBJ)/game.o

oqui_OBJ = 	$(OBJ)/quim.o	\
		$(OBJ)/sboard.o	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o	\
		$(GOOD_OBJ)

osta_OBJ = 	$(OBJ)/stam.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/trans.o	\
		$(OBJ)/sboard.o	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o

ostanew_OBJ = 	$(OBJ)/stanewm.o\
		$(GOOD_OBJ)	\
		$(OBJ)/trans.o	\
		$(OBJ)/sboard.o	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o

oturn_OBJ = 	$(OBJ)/turnm.o	\
		$(ZUGNULL_OBJ)	\
		$(GOOD_OBJ)	\
		$(OBJ)/evala.o	\
		$(OBJ)/evall.o

olook_OBJ = 	$(OBJ)/lookm.o	\
		$(OBJ)/sboard.o	\
		$(OBJ)/game.o	\
		$(GOOD_OBJ)	\
	   	$(OBJ)/hash.o	\
		$(OBJ)/trans.o

oseqtab_OBJ = 	$(OBJ)/seqtabm.o

oseqtab2_OBJ = 	$(OBJ)/seqtab2m.o

orobin_OBJ = 	$(OBJ)/robinm.o\


olib_OBJ = 	$(OBJ)/libm.o	\
	        $(OBJ)/weight.o \
		$(PLAY_OBJ)

obook_OBJ = 	$(OBJ)/bookm.o	\
	        $(OBJ)/weight.o \
		$(PLAY_OBJ)

opick_OBJ = 	$(OBJ)/pickm.o	\
	        $(OBJ)/weight.o \
		$(PLAY_OBJ)

ompatt_OBJ = 	$(OBJ)/mpattm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o 



orayest_OBJ = 	$(OBJ)/rayestm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/fpatt.o	\
		$(OBJ)/patt.o	\
		$(OBJ)/board.o 	\
           	$(OBJ)/hash.o	\
		$(OBJ)/eval.o	\
		$(OBJ)/trans.o

otab_OBJ = 	$(OBJ)/tabm.o	\
	        $(OBJ)/weight.o \
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/fpatt.o	\
		$(OBJ)/patt.o	\
		$(OBJ)/board.o	\
           	$(OBJ)/hash.o	\
		$(OBJ)/eval.o	\
		$(OBJ)/trans.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/game.o

opatt_OBJ = 	$(OBJ)/pattm.o	\
		$(GOOD_OBJ)	\
		$(ZUGNULL_OBJ)


olegal_OBJ =	$(OBJ)/legalm.o	\
		$(OBJ)/trans.o	\
 		$(PLAY_OBJ)


olc_OBJ = 	$(OBJ)/lcm.o	\
		$(GOOD_OBJ)     \
		$(ZUGNULL_OBJ)


ohalf_OBJ =	$(OBJ)/halfm.o	\
		$(GOOD_OBJ)	\
		$(ZUGNULL_OBJ)	\
		$(OBJ)/evala.o

ocor_OBJ =	$(OBJ)/corm.o	\
		$(PLAY_OBJ)	\
		$(OBJ)/evall.o

ochk_OBJ = 	$(OBJ)/chkm.o	\
		$(PLAY_OBJ)	\
		$(OBJ)/evala.o

oeval_OBJ = 	$(OBJ)/evalm.o	\
		$(PLAY_OBJ)	\
		$(OBJ)/speed.o  \
		$(OBJ)/evall.o


oexplore_OBJ = 	$(OBJ)/explorem.o	\
		$(PLAY_OBJ)	\
		$(OBJ)/evala.o

oval_OBJ = 	$(OBJ)/valm.o	\
		$(PLAY_OBJ)	\
                $(OBJ)/evalb.o  \
		$(OBJ)/sparse.o \
		$(OBJ)/psconf.o \
                $(OBJ)/evall.o

orating_OBJ = 	$(OBJ)/ratingm.o 


otrans_OBJ = 	$(OBJ)/transm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/newgame.o \
		$(OBJ)/trans.o 

odelta_OBJ = 	$(OBJ)/deltam.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/trans.o 

onn_OBJ = 	$(OBJ)/nnm.o	\
		$(OBJ)/nnentry.o \
		$(OBJ)/nnbinfo.o \
		$(OBJ)/nnpatt.o \
		$(OBJ)/nnfeat.o \
		$(OBJ)/nnspatt.o \
		$(OBJ)/nndpatt.o \
		$(ZUGNULL_OBJ) \
		$(GOOD_OBJ)    \
		$(OBJ)/psconf.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/evalk.o	\
		$(OBJ)/sparse.o	\
	 	$(OBJ)/disco.o

onn2_OBJ = 	$(OBJ)/nn2m.o	\
		$(ZUGNULL_OBJ) \
		$(GOOD_OBJ)    \
	 	$(OBJ)/disco.o


oreg_OBJ = 	$(OBJ)/regm.o	\
		$(OBJ)/regentry.o \
		$(OBJ)/regbinfo.o \
		$(OBJ)/regpatt.o \
		$(OBJ)/regspatt.o \
		$(OBJ)/regdpatt.o \
		$(OBJ)/regfeat.o \
		$(ZUGNULL_OBJ) \
		$(GOOD_OBJ)    \
		$(OBJ)/psconf.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/evalk.o	\
		$(OBJ)/sparse.o	\
	 	$(OBJ)/disco.o


orat_OBJ = 	$(OBJ)/ratm.o	\
		$(OBJ)/ratentry.o \
		$(OBJ)/ratbinfo.o \
		$(OBJ)/ratpatt.o \
		$(OBJ)/ratfeat.o \
		$(OBJ)/nnspatt.o \
		$(OBJ)/nndpatt.o \
		$(ZUGNULL_OBJ) \
		$(GOOD_OBJ)    \
		$(OBJ)/psconf.o	\
		$(OBJ)/evala.o	\
		$(OBJ)/evalk.o	\
		$(OBJ)/sparse.o	\
	 	$(OBJ)/disco.o

oidiots_OBJ = 	$(OBJ)/idiotsm.o	\
		$(ZUGNULL_OBJ) \
		$(GOOD_OBJ)    \

ortest_OBJ = 	$(OBJ)/rtestm.o


opack_OBJ    =  $(OBJ)/packm.o \
	        $(OBJ)/sparse.o \
	        $(OBJ)/psconf.o \
	        $(ZUGNULL_OBJ)\
		$(GOOD_OBJ)

opsearch_OBJ = 	$(OBJ)/psearchm.o \
		$(OBJ)/pssub.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/trans.o 

opattgen_OBJ = 	$(OBJ)/pattgenm.o \
		$(OBJ)/psfast.o	\
		$(OBJ)/psconf.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/trans.o 

ops_OBJ = 	$(OBJ)/psm.o \
		$(OBJ)/psfast.o	\
		$(OBJ)/psconf.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o	\
		$(OBJ)/trans.o 

obrute4_OBJ = 	$(OBJ)/brute4m.o \
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o	\
	        $(OBJ)/goodies.o \
		$(OBJ)/trans.o 

opf_OBJ = 	$(OBJ)/pfm.o \
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/tab.o	\
	        $(OBJ)/goodies.o \
		$(OBJ)/Rnd.o \
		$(OBJ)/trans.o 

oseq_OBJ = 	$(OBJ)/seqm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o 

otranspo_OBJ = 	$(OBJ)/transpom.o\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/lib.o	\
		$(OBJ)/game.o	\
		$(OBJ)/newgame.o	\
		$(OBJ)/trans.o	\
		$(OBJ)/hash.o

obc_OBJ = 	$(OBJ)/bcm.o\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/lib.o	\
		$(OBJ)/game.o	\
		$(OBJ)/newgame.o	\
		$(OBJ)/trans.o	\
		$(OBJ)/hash.o

otexsf_OBJ =	$(OBJ)/texsfm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o 

osp2html_OBJ =	$(OBJ)/sp2htmlm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o 

ogrep_OBJ = 	$(OBJ)/grepm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o 

osep_OBJ = 	$(OBJ)/sepm.o	\
		$(OBJ)/sboard.o	\
		$(GOOD_OBJ)	\
		$(OBJ)/game.o	\
		$(OBJ)/trans.o 

ktour_OBJ = 	$(OBJ)/ktourm.o


#
# rule for c-files
#
$(OBJ)/%.o: %.c 
	$(CC) $(CCOPTS) -o $@ $<

$(OBJ)/%.o: %.C 
	$(CC) $(CCOPTS) -o $@ $<


dep:
	cd src;\
	dep -s '$$(SRC)/' -os o -op '$$(OBJ)/' *.c > ../dependencies


bpip.o:	$(bpipint_OBJ)
	ld -r -o bpip.o $(bpipint_OBJ)


$(OBJ)/playml.o: playm.c
	$(CC) $(CCOPTS) -o $(OBJ)/playml.o -DEVAL_L playm.c

$(OBJ)/playmb.o: playm.c
	$(CC) $(CCOPTS) -o $(OBJ)/playmb.o -DEVAL_B playm.c



../ogrep:	$(ogrep_OBJ)
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../osep:	$(osep_OBJ)
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oplaya:	$(oplaya_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oplay:	$(oplay_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oplayb:	$(oplayb_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oplayl:	$(oplayl_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oplayk:	$(oplayk_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oplayd:	$(oplayd_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ohist:	$(ohist_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oauto:	$(oauto_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../obpat:	$(obpat_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ohole:	$(ohole_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otest:	$(otest_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otcmp:	$(otcmp_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ofil:	$(ofil_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otextio:	$(otextio_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../odisp:	$(odisp_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../okshm:	$(okshm_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../omana:	$(omana_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../odis:	$(odis_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../onodes:	$(onodes_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oendstat:	$(oendstat_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oecmp:	$(oecmp_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oendtest:$(oendtest_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otext:	$(otext_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otex:	$(otex_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../osp2html:	$(osp2html_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otexsf:	$(otexsf_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../omatch:	$(omatch_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otime:	$(otime_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oextr:	$(oextr_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opcextr:	$(opcextr_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oevol:	$(oevol_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ofeature:	$(ofeature_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../olog:	$(olog_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../obay:	$(obay_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oedge:	$(oedge_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../omobtab:	$(omobtab_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oqui:	$(oqui_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../obc:	$(obc_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../osta:	$(osta_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ostanew:$(ostanew_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../orating:	$(orating_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oturn:	$(oturn_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../olook:	$(olook_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oseqtab:$(oseqtab_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oseqtab2:$(oseqtab2_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../orobin:	$(orobin_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../olib:	$(olib_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../obook:	$(obook_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opick:	$(opick_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ompatt:	$(ompatt_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../orayest:	$(orayest_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otab:	$(otab_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opatt:	$(opatt_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@
 
../olc:	$(olc_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../olegal:	$(olegal_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@
 
../ohalf:	$(ohalf_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otrans:	$(otrans_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../odelta:	$(odelta_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../onn:	$(onn_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS) -lpthread
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oreg:	$(oreg_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS) -lpthread
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../orat:$(orat_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS) -lpthread
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ortest:$(ortest_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS) -lpthread
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../onn2:$(onn2_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oidiots:	$(oidiots_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opack:	$(opack_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opsearch:	$(opsearch_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opattgen:	$(opattgen_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ops:	$(ops_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../obrute4:	$(obrute4_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opf:	$(opf_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oseq:	$(oseq_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../otranspo:	$(otranspo_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ocor:	$(ocor_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ochk:	$(ochk_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oeval:	$(oeval_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oexplore:	$(oexplore_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oval:	$(oval_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../olab:	$(olab_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../osel:	$(osel_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ogen:	$(ogen_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ointer:	$(ointer_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oga:	$(oga_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ofgen:	$(ofgen_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@
 
../ogenf:	$(ogenf_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../obf:	$(obf_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@
        
../ost:	$(ost_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../osign:	$(osign_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../oslib:	$(oslib_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../opartab:	$(opartab_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@

../ktour: $(ktour_OBJ) 
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@


#### automatically generated, do not change

$(OBJ)/amiga.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				amiga.h attr.h board.h crt.h filecom.h game.h 	\
				goodies.h hash.h int.h lib.h main.h move.h 	\
				newgame.h playgm.h sboard.h sys.h wert.h 
$(OBJ)/autom.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h filecom.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				order.h playgm.h sboard.h trans.h wert.h 
$(OBJ)/bcm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/bfm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h distr.h eval.h featurem.h 	\
				filecom.h fpatt.h game.h goodies.h hash.h 	\
				killer.h lib.h main.h mid.h move.h newgame.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/board.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h f.h featurem.h game.h 	\
				goodies.h hash.h main.h move.h patt.h sboard.h 	\
				setarrays.h tab.h wert.h 
$(OBJ)/book.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h book.h crt.h game.h goodies.h 	\
				hash.h killer.h main.h move.h newgame.h playm.h 	\
				pmove.h sboard.h trans.h wert.h 
$(OBJ)/bookm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h book.h crt.h featurem.h game.h 	\
				goodies.h hash.h main.h move.h newgame.h patt.h 	\
				sboard.h tab.h weight.h wert.h 
$(OBJ)/bpatm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h trans.h wert.h 
$(OBJ)/bpipint.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h mid.h 	\
				move.h patt.h sboard.h tab.h wert.h 
$(OBJ)/brute4m.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h goodies.h main.h psearchm.h 	\
				pssub.h sboard.h trans.h 
$(OBJ)/chkm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h lib.h main.h 	\
				move.h newgame.h patt.h sboard.h tab.h wert.h 
$(OBJ)/client.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				client.h client_util.i crt.h main.h sboard.h 	\
				sig.i 
$(OBJ)/client_client.o	:	client_client.h client_command.h 	\
				client_constant.h client_event.h 	\
				client_header.h client_othello.h 	\
				client_sig.i client_timer.h 
$(OBJ)/client_command.o	:	client_client.h client_command.h 	\
				client_constant.h client_event.h 	\
				client_header.h client_othello.h 	\
				client_timer.h 
$(OBJ)/client_event.o	:	client_client.h client_command.h 	\
				client_constant.h client_event.h 	\
				client_header.h client_othello.h 	\
				client_timer.h 
$(OBJ)/client_othello.o	:	client_client.h client_command.h 	\
				client_constant.h client_event.h 	\
				client_header.h client_othello.h 	\
				client_timer.h 
$(OBJ)/client_timer.o	:	client_client.h client_command.h 	\
				client_constant.h client_event.h 	\
				client_header.h client_othello.h 	\
				client_timer.h 
$(OBJ)/convm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/corm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h move.h 	\
				patt.h pcstat.h sboard.h tab.h wert.h 
$(OBJ)/crt.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h main.h sboard.h 
$(OBJ)/crtg.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h main.h sboard.h 
$(OBJ)/crtt.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h main.h sboard.h 
$(OBJ)/deltam.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h tab.h 
$(OBJ)/disco.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h disco.h main.h sboard.h 
$(OBJ)/dism.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h distr.h eval.h game.h 	\
				goodies.h hash.h main.h move.h sboard.h wert.h 
$(OBJ)/dispm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h filecom.h game.h goodies.h 	\
				hash.h lib.h main.h move.h newgame.h sboard.h 	\
				wert.h 
$(OBJ)/distr.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h distr.h main.h sboard.h 
$(OBJ)/ecmpm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h diagvar.inc edgevar.inc 	\
				eval.h featurem.h fpatt.h game.h goodies.h 	\
				hash.h killer.h main.h move.h patt.h sboard.h 	\
				tab.h wert.h 
$(OBJ)/edgem.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h move.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/end.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h end.h eval.h 	\
				fastend.h featurem.h ffend.h game.h goodies.h 	\
				hash.h killer.h main.h mid.h move.h order.h 	\
				pcstat.h sboard.h wert.h 
$(OBJ)/endstatm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h move.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/endtestm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h move.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/eval.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h sboard.h wert.h 
$(OBJ)/eval8.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/evala.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nndpatt.h nnentry.h nnm.h patt.h psconf.h 	\
				sboard.h shm.h sparse.h tab.h tabtype.h trans.h 	\
				wert.h 
$(OBJ)/evala.old.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/evalb.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h shm.h tab.h tabtype.h wert.h 
$(OBJ)/evald.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				quad.par sboard.h tab.h wert.h 
$(OBJ)/evalk.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				psconf.h sboard.h shm.h sparse.h tab.h 	\
				tabtype.h wert.h 
$(OBJ)/evall.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h shm.h tab.h tabtype.h wert.h 
$(OBJ)/evalm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h lib.h main.h 	\
				move.h newgame.h patt.h pcstat.h sboard.h 	\
				speed.h tab.h wert.h 
$(OBJ)/evolm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				MLCG.h Rnd.H RndInt.h crt.h evolopt.h main.h 	\
				sboard.h trans.h 
$(OBJ)/evolopt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h evolopt.h main.h sboard.h 
$(OBJ)/explorem.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h lib.h main.h 	\
				move.h newgame.h patt.h sboard.h tab.h wert.h 
$(OBJ)/extrm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/fastend.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h end.h game.h 	\
				goodies.h hash.h main.h move.h sboard.h wert.h 
$(OBJ)/feature.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h 	\
				newendeval.h patt.h sboard.h tab.h wert.h 
$(OBJ)/featurem.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h feature.h featurem.h 	\
				goodies.h main.h sboard.h 
$(OBJ)/ffend.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h end.h ffend.h game.h 	\
				goodies.h hash.h main.h move.h sboard.h wert.h 
$(OBJ)/fgenm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h killer.h main.h move.h sboard.h wert.h 
$(OBJ)/filecom.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h filecom.h game.h goodies.h 	\
				hash.h lib.h main.h move.h newgame.h sboard.h 	\
				wert.h 
$(OBJ)/film.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h sboard.h wert.h 
$(OBJ)/fpatt.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/gam.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h game.h 	\
				goodies.h hash.h killer.h main.h move.h 	\
				sboard.h trans.h wert.h 
$(OBJ)/game.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				killer.h lib.h main.h move.h newgame.h playm.h 	\
				pmove.h sboard.h trans.h wert.h 
$(OBJ)/genfm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h trans.h wert.h 
$(OBJ)/genm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h game.h 	\
				goodies.h hash.h killer.h main.h move.h 	\
				sboard.h wert.h 
$(OBJ)/goodies.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h goodies.h main.h sboard.h 
$(OBJ)/grepm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h game.h main.h sboard.h 
$(OBJ)/halfm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h playgm.h sboard.h wert.h 
$(OBJ)/hash.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h goodies.h hash.h main.h 	\
				sboard.h wert.h 
$(OBJ)/histm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logm.h logmat.h logwls.h main.h sboard.h 
$(OBJ)/holem.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h killer.h main.h 	\
				mid.h move.h order.h patt.h sboard.h tab.h 	\
				wert.h 
$(OBJ)/int.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h filecom.h game.h 	\
				goodies.h hash.h int.h lib.h main.h move.h 	\
				newgame.h playgm.h sboard.h sys.h wert.h 
$(OBJ)/interm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnentry.h nnm.h nnpatt.h patt.h psconf.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/killer.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				killer.h main.h move.h sboard.h wert.h 
$(OBJ)/kshmm.o		:	shm.h 
$(OBJ)/lcm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				board.h crt.h main.h sboard.h tab.h trans.h 
$(OBJ)/legalm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h trans.h 
$(OBJ)/lib.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				killer.h lib.h main.h move.h newgame.h playm.h 	\
				pmove.h sboard.h trans.h wert.h 
$(OBJ)/libm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h book.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h weight.h wert.h 
$(OBJ)/linm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logall.h logio.h logm.h logmat.h main.h 	\
				sboard.h 
$(OBJ)/logall.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logall.h logm.h logmat.h main.h sboard.h 
$(OBJ)/logeig.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logeig.h logm.h logmat.h main.h sboard.h 
$(OBJ)/logio.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logio.h logm.h logmat.h main.h sboard.h 
$(OBJ)/logm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h goodies.h logall.h logeig.h 	\
				logio.h logm.h logmat.h logwls.h main.h 	\
				sboard.h 
$(OBJ)/logmat.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logm.h logmat.h main.h sboard.h 
$(OBJ)/lognew.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logall.h logio.h logm.h logmat.h lognew.h 	\
				main.h sboard.h 
$(OBJ)/logwls.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h logall.h logio.h logm.h logmat.h logwls.h 	\
				main.h sboard.h 
$(OBJ)/lookm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				main.h move.h sboard.h wert.h 
$(OBJ)/lpattm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h trans.h wert.h 
$(OBJ)/manam.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h 	\
				filecom.h fpatt.h game.h goodies.h hash.h lib.h 	\
				main.h move.h newgame.h patt.h playgm.h playm.h 	\
				pmove.h sboard.h tab.h wert.h 
$(OBJ)/matchm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h killer.h main.h move.h sboard.h trans.h 	\
				wert.h 
$(OBJ)/mid.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h end.h eval.h 	\
				featurem.h fpatt.h game.h goodies.h hash.h 	\
				killer.h main.h mid.h move.h order.h patt.h 	\
				pcstat.h sboard.h tab.h util.h wert.h 
$(OBJ)/mobtabm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				board.h crt.h main.h sboard.h 
$(OBJ)/move.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h end.h eval.h game.h 	\
				goodies.h hash.h killer.h main.h mid.h move.h 	\
				newend.h sboard.h sel.h wert.h 
$(OBJ)/mpattm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h main.h patt.h sboard.h tab.h trans.h 
$(OBJ)/newend.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h end.h eval.h 	\
				featurem.h fpatt.h game.h goodies.h hash.h 	\
				killer.h main.h mid.h move.h newend.h order.h 	\
				patt.h pmove.h sboard.h tab.h wert.h 
$(OBJ)/newendeval.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/newgame.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				lib.h main.h move.h newgame.h sboard.h trans.h 	\
				wert.h 
$(OBJ)/newhash.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				main.h move.h newhash.h sboard.h wert.h 
$(OBJ)/nn2m.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h sboard.h tab.h trans.h wert.h 
$(OBJ)/nnbinfo.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnbinfo.h nndpatt.h nnentry.h nnfeat.h nnm.h 	\
				nnpatt.h nnspatt.h patt.h psconf.h sboard.h 	\
				sparse.h tab.h trans.h wert.h 
$(OBJ)/nndpatt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nndpatt.h nnentry.h nnm.h patt.h psconf.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/nnentry.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnentry.h nnm.h patt.h psconf.h sboard.h 	\
				sparse.h tab.h trans.h wert.h 
$(OBJ)/nnfeat.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnentry.h nnfeat.h nnm.h patt.h psconf.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/nnm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnbinfo.h nndpatt.h nnentry.h nnfeat.h nnm.h 	\
				nnpatt.h nnspatt.h patt.h psconf.h sboard.h 	\
				sparse.h tab.h timer.h trans.h wert.h 
$(OBJ)/nnpatt.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnentry.h nnm.h nnpatt.h patt.h psconf.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/nnregm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnbinfo.h nndpatt.h nnentry.h nnfeat.h nnm.h 	\
				nnpatt.h nnregm.h nnspatt.h patt.h psconf.h 	\
				sboard.h sparse.h tab.h timer.h trans.h wert.h 
$(OBJ)/nnspatt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				nnentry.h nnm.h nnspatt.h patt.h psconf.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/nodesm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h move.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/normal.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h normal.h sboard.h 
$(OBJ)/o_stubs.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h icon.h int.h main.h o_ui.h playgm.h 	\
				sboard.h sys.h 
$(OBJ)/o_ui.o		:	o_ui.h 
$(OBJ)/order.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h eval.h game.h 	\
				goodies.h hash.h killer.h main.h mid.h move.h 	\
				sboard.h wert.h 
$(OBJ)/ox_client.o	:	ox_client.h sig.i 
$(OBJ)/packm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psconf.h sboard.h sparse.h tab.h 
$(OBJ)/parm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/partabm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/passm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/patt.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h game.h 	\
				goodies.h hash.h killer.h main.h move.h patt.h 	\
				sboard.h tab.h trans.h wert.h 
$(OBJ)/pattgenm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				pattgenm.h sboard.h tab.h trans.h wert.h 
$(OBJ)/pattm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h trans.h wert.h 
$(OBJ)/pcextrm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h tab.h 
$(OBJ)/pcstat.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h pcstat.h sboard.h wert.h 
$(OBJ)/pfm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				MLCG.h Rnd.H RndInt.h attr.h crt.h goodies.h 	\
				main.h psearchm.h pssub.h sboard.h trans.h 
$(OBJ)/pickm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h book.h crt.h eval.h featurem.h 	\
				filecom.h fpatt.h game.h goodies.h hash.h 	\
				killer.h lib.h main.h move.h newgame.h patt.h 	\
				pcstat.h playgm.h playm.h pmove.h sboard.h 	\
				tab.h wert.h 
$(OBJ)/pipecom.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h pipecom.h 	\
				sboard.h 
$(OBJ)/playgm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h filecom.h game.h goodies.h 	\
				hash.h int.h lib.h main.h move.h newgame.h 	\
				playgm.h sboard.h sys.h wert.h 
$(OBJ)/playm.o		:	Client.H ClientBoard.H ClientBoardType.H 	\
				ClientClock.H ClientGame.H ClientGameType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H OthClient.H 	\
				OthClientBoard.H OthClientBoardType.H 	\
				OthClientGame.H OthClientGameType.H 	\
				OthClientMove.H attr.h board.h book.h crt.h 	\
				eval.h featurem.h filecom.h fpatt.h game.h 	\
				goodies.h hash.h killer.h lib.h main.h move.h 	\
				newgame.h patt.h pcstat.h playgm.h playm.h 	\
				pmove.h rndbook.h sboard.h socketcom.h tab.h 	\
				timer.h wert.h 
$(OBJ)/pmove.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h eval.h game.h 	\
				goodies.h hash.h int.h killer.h main.h move.h 	\
				newend.h playgm.h playm.h pmove.h sboard.h 	\
				wert.h 
$(OBJ)/psbinfo.o	:	psbinfo.h 
$(OBJ)/psconf.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psconf.h sboard.h trans.h 
$(OBJ)/psearchm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psearchm.h pssub.h sboard.h 	\
				trans.h 
$(OBJ)/psfast.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psearchm.h sboard.h 
$(OBJ)/psm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psbinfo.h psconf.h psfast.h 	\
				sboard.h trans.h 
$(OBJ)/pssub.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psearchm.h sboard.h 
$(OBJ)/quim.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/ratbinfo.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h ratbinfo.h ratentry.h 	\
				ratfeat.h ratm.h ratpatt.h sboard.h sparse.h 	\
				tab.h trans.h wert.h 
$(OBJ)/ratentry.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h ratentry.h ratm.h sboard.h 	\
				sparse.h tab.h trans.h wert.h 
$(OBJ)/ratfeat.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h ratentry.h ratfeat.h ratm.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/ratm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h ratbinfo.h ratentry.h 	\
				ratfeat.h ratm.h ratpatt.h sboard.h sparse.h 	\
				tab.h timer.h trans.h wert.h 
$(OBJ)/ratpatt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h ratentry.h ratm.h ratpatt.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/rayestm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/regbinfo.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regbinfo.h regdpatt.h 	\
				regentry.h regfeat.h regm.h regpatt.h 	\
				regspatt.h sboard.h sparse.h tab.h trans.h 	\
				wert.h 
$(OBJ)/regdpatt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regdpatt.h regentry.h regm.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/regentry.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regentry.h regm.h sboard.h 	\
				sparse.h tab.h trans.h wert.h 
$(OBJ)/regfeat.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regentry.h regfeat.h regm.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/regm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regbinfo.h regdpatt.h 	\
				regentry.h regfeat.h regm.h regpatt.h 	\
				regspatt.h sboard.h sparse.h tab.h timer.h 	\
				trans.h wert.h 
$(OBJ)/regpatt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regentry.h regm.h regpatt.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/regspatt.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h disco.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h main.h move.h 	\
				patt.h psconf.h regentry.h regm.h regspatt.h 	\
				sboard.h sparse.h tab.h trans.h wert.h 
$(OBJ)/rndbook.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h game.h main.h newgame.h rndbook.h 	\
				sboard.h 
$(OBJ)/rtestm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/sboard.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h main.h sboard.h trans.h 
$(OBJ)/sel.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h eval.h game.h 	\
				goodies.h hash.h killer.h main.h mid.h move.h 	\
				order.h sboard.h sel.h wert.h 
$(OBJ)/selm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h order.h playgm.h sboard.h 	\
				trans.h wert.h 
$(OBJ)/sepm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h sboard.h 
$(OBJ)/seqm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/seqtab2m.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				main.h move.h sboard.h wert.h 
$(OBJ)/seqtabm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				main.h move.h sboard.h wert.h 
$(OBJ)/setarrays.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h set3b.inc set3w.inc 	\
				set4b.inc set4w.inc set5b.inc set5w.inc 	\
				set6b.inc set6w.inc set7b.inc set7w.inc 	\
				set8b.inc set8w.inc 
$(OBJ)/shm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/signm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/simlib.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h sboard.h simlib.h trans.h 	\
				wert.h 
$(OBJ)/slibm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h killer.h main.h move.h sboard.h simlib.h 	\
				trans.h wert.h 
$(OBJ)/socketcom.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h sboard.h 	\
				socketcom.h 
$(OBJ)/sp2htmlm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h game.h main.h sboard.h 
$(OBJ)/sparse.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h psconf.h sboard.h sparse.h 
$(OBJ)/speed.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h goodies.h main.h sboard.h 
$(OBJ)/spiel.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h game.h goodies.h hash.h 	\
				main.h move.h sboard.h wert.h 
$(OBJ)/stable.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h goodies.h 	\
				main.h o_.sem sboard.h stable.h stable.inc 
$(OBJ)/stam.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h tab.h 
$(OBJ)/stanewm.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h tab.h 
$(OBJ)/stm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h crtg.h eval.h featurem.h 	\
				fpatt.h game.h goodies.h hash.h killer.h main.h 	\
				mid.h move.h order.h patt.h sboard.h tab.h 	\
				wert.h 
$(OBJ)/tab.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/tabm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h main.h move.h patt.h 	\
				sboard.h tab.h wert.h 
$(OBJ)/tcmpm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h 	\
				filecom.h fpatt.h game.h goodies.h hash.h lib.h 	\
				main.h move.h newgame.h patt.h playgm.h playm.h 	\
				pmove.h sboard.h tab.h wert.h 
$(OBJ)/testm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h move.h 	\
				patt.h sboard.h tab.h wert.h 
$(OBJ)/tex2m.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h sboard.h 
$(OBJ)/texm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h sboard.h 
$(OBJ)/texsfm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h sboard.h 
$(OBJ)/textiom.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h 	\
				filecom.h fpatt.h game.h goodies.h hash.h lib.h 	\
				main.h move.h newgame.h patt.h playgm.h playm.h 	\
				pmove.h sboard.h tab.h wert.h 
$(OBJ)/textm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h filecom.h game.h goodies.h 	\
				hash.h int.h lib.h main.h move.h newgame.h 	\
				playgm.h sboard.h sys.h wert.h 
$(OBJ)/timem.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h main.h mid.h 	\
				move.h patt.h sboard.h tab.h wert.h 
$(OBJ)/trans.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h trans.h 
$(OBJ)/transm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h crt.h game.h goodies.h main.h newgame.h 	\
				sboard.h trans.h 
$(OBJ)/transpom.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h trans.h wert.h 
$(OBJ)/trapom.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h featurem.h game.h 	\
				goodies.h hash.h lib.h main.h move.h newgame.h 	\
				patt.h sboard.h tab.h trans.h wert.h 
$(OBJ)/turnm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h game.h goodies.h 	\
				hash.h main.h move.h playgm.h sboard.h wert.h 
$(OBJ)/util.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h normal.h sboard.h 
$(OBJ)/valm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h eval.h featurem.h fpatt.h 	\
				game.h goodies.h hash.h killer.h lib.h main.h 	\
				move.h newgame.h patt.h pcstat.h sboard.h tab.h 	\
				wert.h 
$(OBJ)/weight.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				crt.h main.h sboard.h 
$(OBJ)/xm.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				attr.h board.h crt.h filecom.h game.h goodies.h 	\
				hash.h int.h lib.h main.h move.h newgame.h 	\
				playgm.h sboard.h sys.h wert.h 
$(OBJ)/Client.o		:	Client.H ClientBoard.H ClientBoardType.H 	\
				ClientClock.H ClientGame.H ClientGameType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H 
$(OBJ)/ClientBoard.o	:	ClientBoard.H ClientBoardType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H 
$(OBJ)/ClientBoardType.o:	ClientBoardType.H ClientIncludes.H 	\
				ClientPTR.H ClientString.H 
$(OBJ)/ClientClock.o	:	Client.H ClientBoard.H ClientBoardType.H 	\
				ClientClock.H ClientGame.H ClientGameType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H 
$(OBJ)/ClientGame.o	:	ClientBoard.H ClientBoardType.H 	\
				ClientClock.H ClientGame.H ClientGameType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H 
$(OBJ)/ClientGameType.o	:	ClientBoardType.H ClientGameType.H 	\
				ClientIncludes.H ClientPTR.H ClientString.H 	\
				ClientTypes.H 
$(OBJ)/ClientMove.o	:	ClientBoard.H ClientBoardType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H 
$(OBJ)/ClientString.o	:	ClientIncludes.H ClientPTR.H ClientString.H 
$(OBJ)/ClientTypes.o	:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				ClientTypes.H 
$(OBJ)/OthClientBoard.o	:	ClientBoard.H ClientBoardType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H 	\
				OthClientBoard.H OthClientBoardType.H 	\
				OthClientMove.H 
$(OBJ)/OthClientMove.o	:	ClientBoard.H ClientBoardType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H OthClientMove.H 
$(OBJ)/Rnd.o		:	ClientIncludes.H ClientPTR.H ClientString.H 	\
				MLCG.h Rnd.H RndInt.h crt.h main.h sboard.h 
$(OBJ)/main.o		:	Client.H ClientBoard.H ClientBoardType.H 	\
				ClientClock.H ClientGame.H ClientGameType.H 	\
				ClientIncludes.H ClientMove.H ClientPTR.H 	\
				ClientString.H ClientTypes.H OthClient.H 	\
				OthClientBoard.H OthClientBoardType.H 	\
				OthClientGame.H OthClientGameType.H 	\
				OthClientMove.H 

######## ox story ... #########

# Compiler flags.

CPPFLAGS += -traditional -I~/c/o/src -I$(SRC) -I/usr/local/X11/include
#-I$(GUIDEHOME)/include -I$(OPENWINHOME)/include
LDFLAGS  += -L/usr/X11/lib -L/usr/openwin/lib
#-L$(GUIDEHOME)/lib -L$(OPENWINHOME)/lib
LDLIBS   +=  -lX11 -lxview -lolgx 
#-lguidexv 
#-lguide  

ox_OBJ	 = 	$(OBJ)/xm.o	\
	  	$(OBJ)/playgm.o	\
		$(OBJ)/int.o	\
		$(OBJ)/o_stubs.o\
		$(OBJ)/o_ui.o	\
		$(PLAY_OBJ)

o_stubs.c o_ui.c: o.G
	$(GUIDEHOME)/bin/gxv o.G

../ox:	$(ox_OBJ)
	$(LN) -o $(OBJ)/temp $(LNOPTS) $^ $(LIBS)
	$(STRIP) $(OBJ)/temp
	mv $(OBJ)/temp $@
	
$(OBJ)/o_stubs.o:	o_stubs.c o.G
	g++ -c $(CCOPTS) -DUNIX -pipe -I/usr/local/X11/include -o $(OBJ)/o_stubs.o o_stubs.c 

$(OBJ)/o_ui.o:	o_ui.c o.G
	g++ -c $(CCOPTS) -DUNIX -pipe -I/usr/local/X11/include -o $(OBJ)/o_ui.o o_ui.c 


############################

TAR = log.tar

pack:
	touch $(TAR).bz2
	rm $(TAR).bz2
	tar -cpof $(TAR) *
	gzip -9 $(TAR)

unpack:
	gunzip $(TAR).gz
	tar -xpof $(TAR)
	rm $(TAR)

clean:
	rm -f $(OBJ)/* $(OBJ_DBG)/* $(OBJ_BP)/* $(OBJ_PROF)/*

init:
	mkdir $(OBJ_DBG) $(OBJ_BP) $(OBJ_PROF) $(OBJ)

cleanobjarcs:
	rm -f $(OBJ_ARCS)/*.o

cleanprofarcs:
	rm -f $(OBJ_ARCS)/*.da

src_from_hal:
	rsync -aczvr --delete -e ssh mic@buro.dnsalias.net:c/o/src/. .

src_to_hal:
	rsync -aczvr --delete -e ssh . mic@buro.dnsalias.net:c/o/src/.


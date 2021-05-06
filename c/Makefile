#
#  Makefile
#
#  This file is part of the reversi program
#  http://github.com/rcrr/reversi
#
#  Copyright (c) 2013, 2014, 2016, 2017, 2018, 2019, 2020, 2021 Roberto Corradini. All rights reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3, or (at your option) any
#  later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
#  or visit the site <http://www.gnu.org/licenses/>.
#

#
# Needed for PostgreSQL libpq API
#
PG_INCLUDE_DIR := `pg_config --includedir`
PG_LIB_DIR := `pg_config --pkglibdir`
PG_LIBS := -lpq

#
# BLIS and FLAME libraries.
#
# Read the file ./ext/README-BLIS-FLAME for info on how to build them.
#
BLIS_LIB  := ./ext/lib/libblis.a
FLAME_LIB := ./ext/lib/libflame.a

#
# Add all the external header file in the ext/include directory.
# Currently they are:
#
# - blis.h
# - FLAME.h
#
EXT_INC := ./ext/include

EXT_HDR_BLIS := $(EXT_INC)/blis.h
EXT_HDR_FLAME := $(EXT_INC)/FLAME.h

#
# All test programs include the src directory to find the headers.
#
TEST_INCLUDE = -I./src

CC = gcc

# SRCDIR : source and header files for modules and main programs.
SRCDIR := src

# TESTDIR : source and header files for unit tests.
TESTDIR := test

# transient locations
# .
BUILDDIR        := build
# build
ASMDIR          := $(BUILDDIR)/asm
BINDIR          := $(BUILDDIR)/bin
DOCDIR          := $(BUILDDIR)/doc
OBJDIR          := $(BUILDDIR)/obj
ENDGAME_LOG_DIR := $(BUILDDIR)/out
BTESTDIR        := $(BUILDDIR)/test
TMPDIR          := $(BUILDDIR)/tmp
# build/test
TEST_OBJDIR     := $(BTESTDIR)/obj
TEST_BINDIR     := $(BTESTDIR)/bin

#
# Add all the programs that has a main and that will be compiled and linked as a bin executable.
#
MAINS := endgame_solver gpdb_verify intel_intrinsics_trial \
         read_game_tree_log read_pve_dump regab rglm rglmw \
         utest

#
# Add all the modules that will be compiled to object code.
#
MODULES := arch binary_heap bit_works board board_pattern board_trans cfg distributions endgame_utils exact_solver file_utils \
           game_position_db game_tree_logger game_tree_utils game_value_estimator improved_fast_endgame_solver \
           isqrt linear_algebra linked_list main_option_parse memory_manager memory_object_pool \
           minimax_solver principal_variation prng red_black_tree \
           rglm_data_files rglm_solver rglm_utils sha3 sort_utils time_utils \
           transposition_table unit_test

#
# Add all the unit tests that will be compiled and linked.
#
UTEST_MAINS := ut_binary_heap ut_bit_works ut_board ut_board_pattern ut_board_trans ut_cfg ut_distributions \
               ut_endgame_utils ut_es_pv ut_file_utils ut_flame ut_game_position ut_game_position_db \
               ut_game_tree_utils ut_isqrt ut_linear_algebra ut_linked_list ut_main_option_parse \
               ut_memory_object_pool ut_prng ut_red_black_tree ut_rglm_data_files ut_rglm_utils \
               ut_sha3 ut_sort_utils ut_transposition_table ut_trsm ut_utest

MAINS_AND_MODULES := $(sort $(MAINS) $(MODULES))

SRCS = $(wildcard $(SRCDIR)/*.c)
HDRS = $(wildcard $(SRCDIR)/*.h)
OBJS = $(addsuffix .o,$(basename $(subst $(SRCDIR),$(OBJDIR),$(SRCS))))
ASMS = $(addsuffix .s,$(basename $(subst $(SRCDIR),$(ASMDIR),$(SRCS))))
BINS = $(addprefix $(BINDIR)/,$(MAINS))

#
# Executable files (main programs), it takes the list in the MAINS variable,
# and for each entry it generates a new variable and assign the fully qualified
# executable file name.
#
# BIN_ENDGAME_SOLVER := build/bin/endgame_solver
# ...
# BIN_UTEST := build/bin/utest
#
$(foreach main,$(MAINS),$(eval BIN_$(shell echo $(main) | tr a-z A-Z) := $(BINDIR)/$(main)))

#
# List of source files having a main function.
#
# The code checks that these files are found in the SRCDIR location.
# If not it raises an error and stops.
#
SRC_MAINS := $(addsuffix .c,$(addprefix $(SRCDIR)/,$(MAINS)))
SRC_MAINS_ERR := $(filter-out $(SRCS),$(SRC_MAINS))
ifdef SRC_MAINS_ERR
$(error Main files listed in the MAINS variable are missing [$(SRC_MAINS_ERR)])
endif

#
# List of source files providing a module definition.
#
# The code checks that these files are found in the SRCDIR location.
# If not it raises an error and stops.
#
SRC_MODULES := $(addsuffix .c,$(addprefix $(SRCDIR)/,$(MODULES)))
SRC_MODULES_ERR := $(filter-out $(SRCS),$(SRC_MODULES))
ifdef SRC_MODULES_ERR
$(error Module files listed in the MODULES variable are missing [$(SRC_MODULES_ERR)])
endif

#
# Source files, it takes the list from the MAINS variable and join it with MODULES,
# and for each entry it generates a new variable and assign the fully qualified
# source file name.
#
# SRC_ARCH := src/arch.c
# SRC_BIT_WORKS := src/bit_works.c
# ...
# SRC_UTEST := src/utest.c
#
$(foreach mm,$(MAINS_AND_MODULES),$(eval SRC_$(shell echo $(mm) | tr a-z A-Z) := $(SRCDIR)/$(mm).c))

#
# Header files, it takes the list from the MODULES variable
# and for each entry it generates a new variable and assign the fully qualified
# header file name.
#
# HDR_ARCH := src/arch.h
# ...
# HDR_UNIT_TEST := src/unit_test.h
#
$(foreach mod,$(MODULES),$(eval HDR_$(shell echo $(mod) | tr a-z A-Z) := $(SRCDIR)/$(mod).h))

#
# List of header files providing a module definition.
#
# The code checks that these files are found in the SRCDIR location.
# If not it raises an error and stops.
#
HDR_MODULES := $(addsuffix .h,$(addprefix $(SRCDIR)/,$(MODULES)))
HDR_MODULES_ERR := $(filter-out $(HDRS),$(HDR_MODULES))
ifdef HDR_MODULES_ERR
$(error Module files listed in the MODULES variable are missing [$(HDR_MODULES_ERR)])
endif

#
# Assembly files, it takes the list of MAINS & MODULES variables and
# for each intry it generates a new variable and assign to it the fully qualified
# assembly file name.
#
# ASM_ARCH := build/asm/arch.s
# ...
# ASM_UTEST := build/asm/utest.s
#
$(foreach mm,$(MAINS_AND_MODULES),$(eval ASM_$(shell echo $(mm) | tr a-z A-Z) := $(ASMDIR)/$(mm).s))

#
# Object files, it takes the list of MAINS & MODULES variables and
# for each entry it generates a new variable and assign to it the fully qualified
# object file name.
#
# OBJ_ARCH := build/obj/arch.o
# ...
# OBJ_UTEST := build/obj/utest.o
#
$(foreach mm,$(MAINS_AND_MODULES),$(eval OBJ_$(shell echo $(mm) | tr a-z A-Z) := $(OBJDIR)/$(mm).o))

#
# Unit test source files, it takes the list of UTEST_MAINS variable and
# for each entry it generates a new variable and assign to it the fully qualified
# source file name.
#
# It checks that all the UTEST_MAINS entries begin with the "ut_" prefix.
#
# UTS_BIT_WORKS := test/ut_bit_works.c
# UTS_BOARD := test/ut_board.c
# ...
# UTS_TRSM := test/ut_trsm.c
# UTS_UTEST := test/ut_utest.c
#
#
# Then it process the unit test object files ...
#
# UTO_BIT_WORKS = build/test/obj/ut_bit_works.o
# UTO_BOARD = build/test/obj/ut_board.o
# ...
# UTO_TRSM = build/test/obj/ut_trsm.o
# UTO_UTEST = build/test/obj/ut_utest.o
#
#
# And finally it process the unit test executable files ...
#
# UTE_BIT_WORKS = build/test/bin/ut_bit_works
# UTE_BOARD = build/test/bin/ut_board
# ...
# UTE_TRSM = build/test/bin/ut_trsm
# UTE_UTEST = build/test/bin/ut_utest
#
UTEST_MAIN_PREFIXES := $(foreach x,$(UTEST_MAINS),$(shell echo $(x) | cut -c 1-3))
UTEST_MAIN_PREFIXES_ERR := $(filter-out ut_,$(UTEST_MAIN_PREFIXES))
ifdef UTEST_MAIN_PREFIXES_ERR
$(error Unit test modules (main programs) must be named with a starting ut_ prefix, found instead [$(UTEST_MAIN_PREFIXES_ERR)])
endif
UTEST_MAINS_TRANSF := $(patsubst ut_%,%,$(UTEST_MAINS))
$(foreach uts,$(UTEST_MAINS_TRANSF),$(eval UTS_$(shell echo $(uts) | tr a-z A-Z) := $(TESTDIR)/ut_$(uts).c))
$(foreach uto,$(UTEST_MAINS_TRANSF),$(eval UTO_$(shell echo $(uto) | tr a-z A-Z) := $(TEST_OBJDIR)/ut_$(uto).o))
$(foreach ute,$(UTEST_MAINS_TRANSF),$(eval UTE_$(shell echo $(ute) | tr a-z A-Z) := $(TEST_BINDIR)/ut_$(ute)))

#
# List of unit test executable files.
#
UTEST_BINS := $(addprefix $(TEST_BINDIR)/,$(UTEST_MAINS))


#
# Flags
#

#
# Add -pg to CFLAGS and DFLAGS to profile.
#
# add  -fopt-info-vec-all for very verbose log on code vectorization. Compile with: "make 2> make.log"
# add -pg for gprof execution.
# add -DNDEBUG to disable assertions
# add -fno-inline to prevent inlining
# add -ftree-vectorize -ftree-vectorizer-verbose=2 to get from gcc the explanation for code vectorization
# add -flto to the CFLAGS list to improve execution speed ( it takes much more to link executables )
ARCH_FLAGS := -mpopcnt -mavx2 -mfma
CFLAGS := -std=c18 -D_POSIX_C_SOURCE=200112L -pedantic-errors -Wall -g -O3 $(ARCH_FLAGS) -I$(PG_INCLUDE_DIR) -I$(EXT_INC) -fopenmp -fno-inline
CFLAGS_TEST := -std=c18 -D_POSIX_C_SOURCE=200112L -pedantic-errors -Wall -g -O3 $(ARCH_FLAGS) -I$(EXT_INC)
LDFLAGS := 
LDFLAGS_TEST :=
#
# add -masm=intel to have the intel assembly format.
# BE CAREFULL because assembly code inlined as __asm__ compiles but will NOT work as expected.
#
ASMFLAGS := -std=c18 -D_POSIX_C_SOURCE=200112L -pedantic-errors -Wall -O3 $(ARCH_FLAGS) -I$(PG_INCLUDE_DIR) -I$(EXT_INC) -fverbose-asm -fopenmp -fno-inline

##
## Source and header file dependencies
##

HDR_ARCH_DEP := $(sort $(HDR_ARCH))
SRC_ARCH_DEP := $(SRC_ARCH) $(sort $(HDR_ARCH_DEP))

HDR_TIME_UTILS_DEP := $(sort $(HDR_TIME_UTILS))
SRC_TIME_UTILS_DEP := $(SRC_TIME_UTILS) $(sort $(HDR_TIME_UTILS_DEP))

HDR_BINARY_HEAP_DEP := $(sort $(HDR_BINARY_HEAP))
SRC_BINARY_HEAP_DEP := $(SRC_BINARY_HEAP) $(sort $(HDR_BINARY_HEAP_DEP))

HDR_BIT_WORKS_DEP := $(sort $(HDR_BIT_WORKS))
SRC_BIT_WORKS_DEP := $(SRC_BIT_WORKS) $(sort $(HDR_BIT_WORKS_DEP))

HDR_PRNG_DEP := $(sort $(HDR_PRNG))
SRC_PRNG_DEP := $(SRC_PRNG) $(sort $(HDR_PRNG_DEP) $(HDR_TIME_UTILS_DEP) $(HDR_BIT_WORKS_DEP))

HDR_BOARD_DEP := $(sort $(HDR_BOARD) $(HDR_BIT_WORKS_DEP) $(HDR_PRNG_DEP))
SRC_BOARD_DEP := $(SRC_BOARD) $(sort $(HDR_BOARD_DEP) $(HDR_ARCH_DEP))

HDR_BOARD_TRANS_DEP := $(sort $(HDR_BOARD_TRANS) $(HDR_BOARD_DEP))
SRC_BOARD_TRANS_DEP := $(SRC_BOARD_TRANS) $(sort $(HDR_BOARD_TRANS_DEP))

HDR_BOARD_PATTERN_DEP := $(sort $(HDR_BOARD_PATTERN) $(HDR_BOARD_TRANS_DEP))
SRC_BOARD_PATTERN_DEP := $(SRC_BOARD_PATTERN) $(sort $(HDR_BOARD_PATTERN_DEP))

HDR_CFG_DEP := $(sort $(HDR_CFG))
SRC_CFG_DEP := $(SRC_CFG) $(sort $(HDR_CFG_DEP))

HDR_DISTRIBUTIONS_DEP := $(sort $(HDR_DISTRIBUTIONS))
SRC_DISTRIBUTIONS_DEP := $(SRC_DISTRIBUTIONS) $(sort $(HDR_DISTRIBUTIONS_DEP))

HDR_FILE_UTILS_DEP := $(sort $(HDR_FILE_UTILS))
SRC_FILE_UTILS_DEP := $(SRC_FILE_UTILS) $(sort $(HDR_FILE_UTILS_DEP))

HDR_ISQRT_DEP := $(sort $(HDR_ISQRT))
SRC_ISQRT_DEP := $(SRC_ISQRT) $(sort $(HDR_ISQRT_DEP))

HDR_LINKED_LIST_DEP := $(sort $(HDR_LINKED_LIST))
SRC_LINKED_LIST_DEP := $(SRC_LINKED_LIST) $(sort $(HDR_LINKED_LIST_DEP))

HDR_MAIN_OPTION_PARSE_DEP := $(sort $(HDR_MAIN_OPTION_PARSE))
SRC_MAIN_OPTION_PARSE_DEP := $(SRC_MAIN_OPTION_PARSE) $(sort $(HDR_MAIN_OPTION_PARSE_DEP))

HDR_SORT_UTILS_DEP := $(sort $(HDR_SORT_UTILS))
SRC_SORT_UTILS_DEP := $(SRC_SORT_UTILS) $(sort $(HDR_SORT_UTILS_DEP))

HDR_MEMORY_MANAGER_DEP := $(sort $(HDR_MEMORY_MANAGER))
SRC_MEMORY_MANAGER_DEP := $(SRC_MEMORY_MANAGER) $(sort $(HDR_MEMORY_MANAGER_DEP))

HDR_MEMORY_OBJECT_POOL_DEP := $(sort $(HDR_MEMORY_OBJECT_POOL))
SRC_MEMORY_OBJECT_POOL_DEP := $(SRC_MEMORY_OBJECT_POOL) $(sort $(HDR_MEMORY_OBJECT_POOL_DEP) $(HDR_SORT_UTILS_DEP))

HDR_RED_BLACK_TREE_DEP := $(sort $(HDR_RED_BLACK_TREE) $(HDR_MEMORY_MANAGER))
SRC_RED_BLACK_TREE_DEP := $(SRC_RED_BLACK_TREE) $(sort $(HDR_RED_BLACK_TREE_DEP))

HDR_SHA3_DEP := $(sort $(HDR_SHA3))
SRC_SHA3_DEP := $(SRC_SHA3) $(sort $(HDR_SHA3_DEP) $(HDR_BIT_WORKS))

HDR_UNIT_TEST_DEP := $(sort $(HDR_UNIT_TEST) $(HDR_LINKED_LIST_DEP) $(HDR_TIME_UTILS_DEP))
SRC_UNIT_TEST_DEP := $(SRC_UNIT_TEST) $(sort $(HDR_UNIT_TEST_DEP) $(HDR_MAIN_OPTION_PARSE_DEP))

HDR_PRINCIPAL_VARIATION_DEP := $(sort $(HDR_PRINCIPAL_VARIATION))
SRC_PRINCIPAL_VARIATION_DEP := $(SRC_PRINCIPAL_VARIATION) $(sort $(HDR_PRINCIPAL_VARIATION_DEP))

HDR_TRANSPOSITION_TABLE_DEP := $(sort $(HDR_TRANSPOSITION_TABLE) $(HDR_BOARD_DEP))
SRC_TRANSPOSITION_TABLE_DEP := $(SRC_TRANSPOSITION_TABLE) $(sort $(HDR_TRANSPOSITION_TABLE_DEP))

HDR_GAME_POSITION_DB_DEP := $(sort $(HDR_GAME_POSITION_DB) $(HDR_RED_BLACK_TREE_DEP) $(HDR_LINKED_LIST_DEP) $(HDR_BOARD_DEP))
SRC_GAME_POSITION_DB_DEP := $(SRC_GAME_POSITION_DB) $(sort $(HDR_GAME_POSITION_DB_DEP) $(HDR_FILE_UTILS_DEP))

HDR_GAME_TREE_UTILS_DEP := $(sort $(HDR_GAME_TREE_UTILS) $(HDR_BOARD_DEP))
SRC_GAME_TREE_UTILS_DEP := $(SRC_GAME_TREE_UTILS) $(sort $(HDR_GAME_TREE_UTILS_DEP) $(HDR_SORT_UTILS_DEP) $(HDR_TIME_UTILS_DEP))

HDR_ENDGAME_UTILS_DEP := $(sort $(HDR_ENDGAME_UTILS))
SRC_ENDGAME_UTILS_DEP := $(SRC_ENDGAME_UTILS) $(sort $(HDR_ENDGAME_UTILS_DEP))

HDR_GAME_TREE_LOGGER_DEP := $(sort $(HDR_GAME_TREE_LOGGER) $(HDR_ENDGAME_UTILS_DEP))
SRC_GAME_TREE_LOGGER_DEP := $(SRC_GAME_TREE_LOGGER) $(sort $(HDR_GAME_TREE_LOGGER_DEP) $(HDR_FILE_UTILS_DEP))

HDR_IMPROVED_FAST_ENDGAME_SOLVER_DEP := $(sort $(HDR_IMPROVED_FAST_ENDGAME_SOLVER) $(HDR_ENDGAME_UTILS_DEP))
SRC_IMPROVED_FAST_ENDGAME_SOLVER_DEP := $(SRC_IMPROVED_FAST_ENDGAME_SOLVER) $(sort $(HDR_IMPROVED_FAST_ENDGAME_SOLVER_DEP) $(HDR_GAME_TREE_LOGGER_DEP))

HDR_MINIMAX_SOLVER_DEP := $(sort $(HDR_MINIMAX_SOLVER) $(HDR_ENDGAME_UTILS_DEP))
SRC_MINIMAX_SOLVER_DEP := $(SRC_MINIMAX_SOLVER) $(sort $(HDR_MINIMAX_SOLVER_DEP) $(HDR_GAME_TREE_LOGGER_DEP) $(HDR_BOARD_PATTERN_DEP))

HDR_GAME_VALUE_ESTIMATOR_DEP := $(sort $(HDR_GAME_VALUE_ESTIMATOR) $(HDR_ENDGAME_UTILS_DEP))
SRC_GAME_VALUE_ESTIMATOR_DEP := $(SRC_GAME_VALUE_ESTIMATOR) $(sort $(HDR_GAME_VALUE_ESTIMATOR_DEP) $(HDR_CFG_DEP) \
                                $(HDR_FILE_UTILS_DEP) $(HDR_RGLM_DATA_FILES_DEP) $(HDR_RGLM_UTILS_DEP) $(HDR_BOARD_DEP) \
                                $(HDR_TRANSPOSITION_TABLE_DEP) $(HDR_TIME_UTILS_DEP))

HDR_EXACT_SOLVER_DEP := $(sort $(HDR_EXACT_SOLVER) $(HDR_ENDGAME_UTILS_DEP))
SRC_EXACT_SOLVER_DEP := $(SRC_EXACT_SOLVER) $(sort $(HDR_EXACT_SOLVER_DEP) $(HDR_GAME_TREE_LOGGER_DEP))

HDR_LINEAR_ALGEBRA_DEP := $(sort $(HDR_LINEAR_ALGEBRA))
SRC_LINEAR_ALGEBRA_DEP := $(SRC_LINEAR_ALGEBRA) $(sort $(HDR_LINEAR_ALGEBRA_DEP) $(HDR_FILE_UTILS_DEP) $(HDR_TIME_UTILS_DEP))

HDR_RGLM_DATA_FILES_DEP := $(sort $(HDR_RGLM_DATA_FILES) $(HDR_BOARD_PATTERN_DEP))
SRC_RGLM_DATA_FILES_DEP := $(SRC_RGLM_DATA_FILES) $(sort $(HDR_RGLM_DATA_FILES_DEP) $(HDR_SHA3_DEP) $(HDR_FILE_UTILS_DEP))

HDR_RGLM_UTILS_DEP := $(sort $(HDR_RGLM_UTILS) $(HDR_RGLM_DATA_FILES_DEP))
SRC_RGLM_UTILS_DEP := $(SRC_RGLM_UTILS) $(sort $(HDR_RGLM_UTILS_DEP))

HDR_RGLM_SOLVER_DEP := $(sort $(HDR_RGLM_SOLVER) $(HDR_ENDGAME_UTILS_DEP))
SRC_RGLM_SOLVER_DEP := $(SRC_RGLM_SOLVER) $(sort $(HDR_RGLM_SOLVER_DEP) $(HDR_FILE_UTILS_DEP) $(HDR_CFG_DEP) $(HDR_GAME_TREE_LOGGER_DEP) \
                       $(HDR_RGLM_DATA_FILES_DEP) $(HDR_RGLM_UTILS_DEP))

SRC_ENDGAME_SOLVER_DEP := $(SRC_ENDGAME_SOLVER) $(sort $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_FILE_UTILS_DEP) $(HDR_ENDGAME_UTILS_DEP) \
                          $(HDR_GAME_POSITION_DB_DEP) $(HDR_GAME_TREE_LOGGER_DEP) $(HDR_IMPROVED_FAST_ENDGAME_SOLVER_DEP) \
                          $(HDR_MINIMAX_SOLVER_DEP) $(HDR_EXACT_SOLVER_DEP) $(HDR_BOARD_PATTERN_DEP) $(HDR_RGLM_SOLVER_DEP) \
                          $(HDR_GAME_VALUE_ESTIMATOR_DEP) $(HDR_TRANSPOSITION_TABLE_DEP))

SRC_UTEST_DEP := $(SRC_UTEST) $(sort $(HDR_UNIT_TEST_DEP))

SRC_INTEL_INTRINSICS_TRIAL_DEP := $(SRC_INTEL_INTRINSICS_TRIAL)

SRC_GPDB_VERIFY_DEP := $(SRC_GPDB_VERIFY) $(sort $(HDR_FILE_UTILS_DEP) $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_GAME_POSITION_DB_DEP))

SRC_READ_GAME_TREE_LOG_DEP := $(SRC_READ_GAME_TREE_LOG) $(sort $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_GAME_TREE_LOGGER_DEP))

SRC_READ_PVE_DUMP_DEP := $(SRC_READ_PVE_DUMP) $(sort $(HDR_FILE_UTILS_DEP) $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_GAME_TREE_UTILS_DEP))

SRC_REGAB_DEP := $(SRC_REGAB) $(sort $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_FILE_UTILS_DEP) $(HDR_CFG_DEP) $(HDR_PRNG_DEP) \
                                     $(HDR_EXACT_SOLVER_DEP) $(HDR_RGLM_SOLVER_DEP) $(HDR_SORT_UTILS_DEP) \
                                     $(HDR_RGLM_DATA_FILES_DEP) $(HDR_RGLM_UTILS_DEP))

SRC_RGLM_DEP := $(SRC_RGLM) $(sort $(HDR_TIME_UTILS_DEP) $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_RGLM_UTILS_DEP) $(HDR_LINEAR_ALGEBRA_DEP))

SRC_RGLMW_DEP := $(SRC_RGLMW) $(sort $(HDR_TIME_UTILS_DEP) $(HDR_MAIN_OPTION_PARSE_DEP) $(HDR_RGLM_UTILS_DEP) $(HDR_LINEAR_ALGEBRA_DEP))

#
# For further reduction of redundant code recipes could be generated with eval.
# Look at this page for info.
#
# https://make.mad-scientist.net/the-eval-function/
#
# The tradeoff between readable code and avoiding duplication is not clear.
# let's keep the makefile as it is for a while.
#

##
## Unit test source file dependencies
##

UTS_BIT_WORKS_DEP := $(UTS_BIT_WORKS) $(HDR_BIT_WORKS_DEP) $(HDR_UNIT_TEST_DEP)

#
# Targets
#

all: build_dirs $(MAINS) $(MODULES) $(UTEST_MAINS)

test: $(UTEST_MAINS) $(BIN_UTEST)
	./build/bin/utest -s=/utest/fail -s=/utest/abort -s=/utest/exit -s=/utest/assert_false $(UTEST_BINS)

asm: $(ASMS)

build_dirs: $(BUILDDIR) $(OBJDIR) $(BINDIR) $(DOCDIR) $(BTESTDIR) $(ASMDIR) $(TMPDIR) \
            $(ENDGAME_LOG_DIR) $(TEST_OBJDIR) $(TEST_BINDIR)

$(BUILDDIR): $(BUILDDIR)/.dir_sentinel

$(BUILDDIR)/.dir_sentinel:
	mkdir -p $(BUILDDIR)
	touch $(BUILDDIR)/.dir_sentinel

$(OBJDIR): $(OBJDIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(OBJDIR)/.dir_sentinel:
	mkdir -p $(OBJDIR)
	touch $(OBJDIR)/.dir_sentinel

$(BINDIR): $(BINDIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(BINDIR)/.dir_sentinel:
	mkdir -p $(BINDIR)
	touch $(BINDIR)/.dir_sentinel

$(TEST_OBJDIR): $(TEST_OBJDIR)/.dir_sentinel $(BTESTDIR)/.dir_sentinel

$(TEST_OBJDIR)/.dir_sentinel:
	mkdir -p $(TEST_OBJDIR)
	touch $(TEST_OBJDIR)/.dir_sentinel

$(TEST_BINDIR): $(TEST_BINDIR)/.dir_sentinel $(BTESTDIR)/.dir_sentinel

$(TEST_BINDIR)/.dir_sentinel:
	mkdir -p $(TEST_BINDIR)
	touch $(TEST_BINDIR)/.dir_sentinel

$(DOCDIR): $(DOCDIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(DOCDIR)/.dir_sentinel:
	mkdir -p $(DOCDIR)
	touch $(DOCDIR)/.dir_sentinel

$(ASMDIR): $(ASMDIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(ASMDIR)/.dir_sentinel:
	mkdir -p $(ASMDIR)
	touch $(ASMDIR)/.dir_sentinel

$(TMPDIR): $(TMPDIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(TMPDIR)/.dir_sentinel:
	mkdir -p $(TMPDIR)
	touch $(TMPDIR)/.dir_sentinel

$(BTESTDIR): $(BTESTDIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(BTESTDIR)/.dir_sentinel:
	mkdir -p $(BTESTDIR)
	touch $(BTESTDIR)/.dir_sentinel

##
## arch
##
## arch.h : (null)
## arch.c : arch.h
##
arch: $(OBJ_ARCH)
asm_arch: $(ASM_ARCH)

$(ASM_ARCH): $(SRC_ARCH_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@ 

$(OBJ_ARCH): $(SRC_ARCH_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## binary_heap
##
## binary_heap.h    : (null)
## binary_heap.c    : binary_heap.h
## ut_binary_heap.c : unit_test.h binary_heap.h
##
binary_heap: $(OBJ_BINARY_HEAP)
asm_binary_heap: $(ASM_BINARY_HEAP)

$(ASM_BINARY_HEAP): $(SRC_BINARY_HEAP_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_BINARY_HEAP): $(SRC_BINARY_HEAP_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_binary_heap: $(UTE_BINARY_HEAP)

$(UTO_BINARY_HEAP): $(UTS_BINARY_HEAP) $(OBJ_UNIT_TEST) $(OBJ_BINARY_HEAP) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_BINARY_HEAP_OBJ_DEPENDENCIES := \
  $(OBJ_LINKED_LIST) $(OBJ_UNIT_TEST) $(OBJ_TIME_UTILS) $(OBJ_MAIN_OPTION_PARSE) \
  $(OBJ_BINARY_HEAP)

$(UTE_BINARY_HEAP): $(UTO_BINARY_HEAP) $(UTE_BINARY_HEAP_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_BINARY_HEAP_OBJ_DEPENDENCIES) -o $@

##
## bit_works
##
## bit_works.h    : (null)
## bit_works.c    : bit_works.h
## ut_bit_works.c : unit_test.h bit_works.h
##
bit_works: $(OBJ_BIT_WORKS)
asm_bit_works: $(ASM_BIT_WORKS)

$(ASM_BIT_WORKS): $(SRC_BIT_WORKS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_BIT_WORKS): $(SRC_BIT_WORKS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_bit_works: $(UTE_BIT_WORKS)

$(UTO_BIT_WORKS): $(UTS_BIT_WORKS) $(OBJ_UNIT_TEST) $(OBJ_BIT_WORKS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_BIT_WORKS_OBJ_DEPENDENCIES := \
  $(OBJ_LINKED_LIST) $(OBJ_UNIT_TEST) $(OBJ_TIME_UTILS) $(OBJ_MAIN_OPTION_PARSE) \
  $(OBJ_BIT_WORKS)

$(UTE_BIT_WORKS): $(UTO_BIT_WORKS) $(UTE_BIT_WORKS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_BIT_WORKS_OBJ_DEPENDENCIES) -o $@

##
## board
##
## board.h : bit_works.h prng.h
## board.c : arch.h board.h
##
board: $(OBJ_BOARD)
asm_board: $(ASM_BOARD)

$(ASM_BOARD): $(SRC_BOARD_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_BOARD): $(SRC_BOARD_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_board: $(UTE_BOARD)

$(UTO_BOARD): $(UTS_BOARD) $(OBJ_UNIT_TEST) $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_BOARD_OBJ_DEPENDENCIES := \
  $(OBJ_LINKED_LIST) $(OBJ_UNIT_TEST) $(OBJ_TIME_UTILS) $(OBJ_MAIN_OPTION_PARSE) \
  $(OBJ_PRNG) $(OBJ_ARCH) $(OBJ_BIT_WORKS) \
  $(OBJ_BOARD)

$(UTE_BOARD): $(UTO_BOARD) $(UTE_BOARD_OBJ_DEPENDENCIES) $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_BOARD_OBJ_DEPENDENCIES) -o $@

##
## board_pattern
##
## board_pattern.h : board_trans.h
## board_pattern.c : board_pattern.h
##
board_pattern: $(OBJ_BOARD_PATTERN)
asm_board_pattern: $(ASM_BOARD_PATTERN)

$(ASM_BOARD_PATTERN): $(SRC_BOARD_PATTERN_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_BOARD_PATTERN): $(SRC_BOARD_PATTERN_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_board_pattern: $(UTE_BOARD_PATTERN)

$(UTO_BOARD_PATTERN): $(UTS_BOARD_PATTERN) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_BOARD_PATTERN_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_BOARD) $(OBJ_ARCH) $(OBJ_BIT_WORKS) $(OBJ_PRNG) $(OBJ_BOARD_TRANS) \
  $(OBJ_BOARD_PATTERN)

$(UTE_BOARD_PATTERN): $(UTO_BOARD_PATTERN) $(UTE_BOARD_PATTERN_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_BOARD_PATTERN_OBJ_DEPENDENCIES) -o $@

##
## board_trans
##
## board_trans.h : board.h
## board_trans.c : board_trans.h
##
board_trans: $(OBJ_BOARD_TRANS)
asm_board_trans: $(ASM_BOARD_TRANS)

$(ASM_BOARD_TRANS): $(SRC_BOARD_TRANS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_BOARD_TRANS): $(SRC_BOARD_TRANS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

$(UTO_BOARD_TRANS): $(UTS_BOARD_TRANS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

ut_board_trans: $(UTE_BOARD_TRANS)

UTE_BOARD_TRANS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_BOARD) $(OBJ_ARCH) $(OBJ_BIT_WORKS) $(OBJ_PRNG) \
  $(OBJ_BOARD_TRANS)

$(UTE_BOARD_TRANS): $(UTO_BOARD_TRANS) $(UTE_BOARD_TRANS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_BOARD_TRANS_OBJ_DEPENDENCIES) -o $@

##
## cfg
##
## cfg.h : (null)
## cfg.c : cfg.h
##
cfg: $(OBJ_CFG)
asm_cfg: $(ASM_CFG)

$(ASM_CFG): $(SRC_CFG_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_CFG): $(SRC_CFG_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_cfg: $(UTE_CFG)

$(UTO_CFG): $(UTS_CFG) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_CFG_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_CFG)

$(UTE_CFG): $(UTO_CFG) $(UTE_CFG_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_CFG_OBJ_DEPENDENCIES) -o $@

##
## distributions
##
## distributions.h : (null)
## distributions.c : distributions.h
##
distributions: $(OBJ_DISTRIBUTIONS)
asm_distributions: $(ASM_DISTRIBUTIONS)

$(ASM_DISTRIBUTIONS): $(SRC_DISTRIBUTIONS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_DISTRIBUTIONS): $(SRC_DISTRIBUTIONS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_distributions: $(UTE_DISTRIBUTIONS)

$(UTO_DISTRIBUTIONS): $(UTS_DISTRIBUTIONS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_DISTRIBUTIONS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_DISTRIBUTIONS)

LIB_DISTRIBUTIONS := -lm

$(UTE_DISTRIBUTIONS): $(UTO_DISTRIBUTIONS) $(UTE_DISTRIBUTIONS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_DISTRIBUTIONS_OBJ_DEPENDENCIES) $(LIB_DISTRIBUTIONS) -o $@

##
## endgame_solver
##
## endgame_solver.c : main_option_parse.h file_utils.h game_position_db.h game_tree_logger.h
##                    improved_fast_endgame_solver.h minimax_solver.h exact_solver.h board_pattern.h
##                    rglm_solver.h
##
endgame_solver: $(BIN_ENDGAME_SOLVER)
asm_endgame_solver: $(ASM_ENDGAME_SOLVER)

$(ASM_ENDGAME_SOLVER): $(SRC_ENDGAME_SOLVER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_ENDGAME_SOLVER): $(SRC_ENDGAME_SOLVER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_ENDGAME_SOLVER_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_GAME_POSITION_DB) $(OBJ_BOARD) $(OBJ_GAME_TREE_UTILS) $(OBJ_FILE_UTILS) \
  $(OBJ_GAME_TREE_LOGGER) $(OBJ_EXACT_SOLVER) $(OBJ_IMPROVED_FAST_ENDGAME_SOLVER) $(OBJ_MINIMAX_SOLVER) \
  $(OBJ_RGLM_SOLVER) $(OBJ_BOARD_PATTERN) $(OBJ_BOARD_TRANS) $(OBJ_RGLM_DATA_FILES) $(OBJ_CFG) \
  $(OBJ_SORT_UTILS) $(OBJ_LINKED_LIST) $(OBJ_SHA3) $(OBJ_PRNG) $(OBJ_TIME_UTILS) $(OBJ_BIT_WORKS) \
  $(OBJ_RGLM_UTILS) $(OBJ_RED_BLACK_TREE) $(OBJ_ARCH) $(OBJ_MEMORY_MANAGER) $(OBJ_GAME_VALUE_ESTIMATOR) \
  $(OBJ_TRANSPOSITION_TABLE)

LIB_ENDGAME_SOLVER := -lm

$(BIN_ENDGAME_SOLVER): $(OBJ_ENDGAME_SOLVER) $(BIN_ENDGAME_SOLVER_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_ENDGAME_SOLVER_OBJ_DEPENDENCIES) $(LIB_ENDGAME_SOLVER) -o $@

##
## endgame_utils
##
## endgame_utils.h : game_tree_utils.h
## endgame_utils.c : endgame_utils.h
##
endgame_utils: $(OBJ_ENDGAME_UTILS)
asm_endgame_utils: $(ASM_ENDGAME_UTILS)

$(ASM_ENDGAME_UTILS): $(SRC_ENDGAME_UTILS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_ENDGAME_UTILS): $(SRC_ENDGAME_UTILS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_endgame_utils: $(UTE_ENDGAME_UTILS)

$(UTO_ENDGAME_UTILS): $(UTS_ENDGAME_UTILS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_ENDGAME_UTILS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) $(OBJ_BIT_WORKS) $(OBJ_RED_BLACK_TREE) $(OBJ_FILE_UTILS) $(OBJ_SORT_UTILS) $(OBJ_ARCH) $(OBJ_MEMORY_MANAGER) \
  $(OBJ_BOARD) $(OBJ_BOARD_TRANS) $(OBJ_BOARD_PATTERN) $(OBJ_GAME_POSITION_DB) \
  $(OBJ_GAME_TREE_UTILS) $(OBJ_GAME_TREE_LOGGER) $(OBJ_CFG) $(OBJ_SHA3) $(OBJ_RGLM_UTILS) $(OBJ_RGLM_DATA_FILES) \
  $(OBJ_EXACT_SOLVER) $(OBJ_MINIMAX_SOLVER) $(OBJ_IMPROVED_FAST_ENDGAME_SOLVER) $(OBJ_RGLM_SOLVER)

LIB_ENDGAME_UTILS := -lm

$(UTE_ENDGAME_UTILS): $(UTO_ENDGAME_UTILS) $(UTE_ENDGAME_UTILS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_ENDGAME_UTILS_OBJ_DEPENDENCIES) $(LIB_ENDGAME_UTILS) -o $@

##
## es_pv (exact solver principal variation)
##
ut_es_pv: $(UTE_ES_PV)

$(UTO_ES_PV): $(UTS_ES_PV) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_ES_PV_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_SHA3) $(OBJ_GAME_POSITION_DB) $(OBJ_RED_BLACK_TREE) $(OBJ_FILE_UTILS) $(OBJ_MEMORY_MANAGER) \
  $(OBJ_BOARD) $(OBJ_BIT_WORKS) $(OBJ_PRNG) $(OBJ_ARCH) $(OBJ_GAME_TREE_UTILS) $(OBJ_SORT_UTILS) \
  $(OBJ_EXACT_SOLVER) $(OBJ_GAME_TREE_LOGGER)

LIB_ES_PV := 

$(UTE_ES_PV): $(UTO_ES_PV) $(UTE_ES_PV_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_ES_PV_OBJ_DEPENDENCIES) $(LIB_ES_PV) -o $@

##
## exact_solver
##
## exact_solver.h : endgame_utils.h
## exact_solver.c : exact_solver.h game_tree_logger.h
##
exact_solver: $(OBJ_EXACT_SOLVER)
asm_exact_solver: $(ASM_EXACT_SOLVER)

$(ASM_EXACT_SOLVER): $(SRC_EXACT_SOLVER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_EXACT_SOLVER): $(SRC_EXACT_SOLVER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## file_utils
##
## file_utils.h : (null)
## file_utils.c : file_utils.h
##
file_utils: $(OBJ_FILE_UTILS)
asm_file_utils: $(ASM_FILE_UTILS)

$(ASM_FILE_UTILS): $(SRC_FILE_UTILS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_FILE_UTILS): $(SRC_FILE_UTILS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_file_utils: $(UTE_FILE_UTILS)

$(UTO_FILE_UTILS): $(UTS_FILE_UTILS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_FILE_UTILS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) \
  $(OBJ_FILE_UTILS)

$(UTE_FILE_UTILS): $(UTO_FILE_UTILS) $(UTE_FILE_UTILS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_FILE_UTILS_OBJ_DEPENDENCIES) -o $@

##
## flame
##
ut_flame: $(UTE_FLAME)

$(UTO_FLAME): $(UTS_FLAME) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_FLAME_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_LINEAR_ALGEBRA)

LIB_FLAME := -lm $(FLAME_LIB) $(BLIS_LIB) -lgomp -lpthread 

$(UTE_FLAME): $(UTO_FLAME) $(UTE_FLAME_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_FLAME_OBJ_DEPENDENCIES) $(LIB_FLAME) -o $@

##
## game_position
##
ut_game_position: $(UTE_GAME_POSITION)

$(UTO_GAME_POSITION): $(UTS_GAME_POSITION) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_GAME_POSITION_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_GAME_POSITION_DB) $(OBJ_BOARD) $(OBJ_RED_BLACK_TREE) $(OBJ_FILE_UTILS) \
  $(OBJ_ARCH) $(OBJ_PRNG) $(OBJ_BIT_WORKS) $(OBJ_MEMORY_MANAGER)

LIB_GAME_POSITION :=

$(UTE_GAME_POSITION): $(UTO_GAME_POSITION) $(UTE_GAME_POSITION_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_GAME_POSITION_OBJ_DEPENDENCIES) $(LIB_GAME_POSITION) -o $@

##
## game_position_db
##
## game_position_db.h : red_black_tree.h linked_list.h board.h
## game_position_db.c : game_position_db.h file_utils.h
##
game_position_db: $(OBJ_GAME_POSITION_DB)
asm_game_position_db: $(ASM_GAME_POSITION_DB)

$(ASM_GAME_POSITION_DB): $(SRC_GAME_POSITION_DB_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_GAME_POSITION_DB): $(SRC_GAME_POSITION_DB_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_game_position_db: $(UTE_GAME_POSITION_DB)

$(UTO_GAME_POSITION_DB): $(UTS_GAME_POSITION_DB) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_GAME_POSITION_DB_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_BOARD) $(OBJ_PRNG) $(OBJ_FILE_UTILS) $(OBJ_BIT_WORKS) $(OBJ_RED_BLACK_TREE) $(OBJ_ARCH) $(OBJ_MEMORY_MANAGER) \
  $(OBJ_GAME_POSITION_DB)

$(UTE_GAME_POSITION_DB): $(UTO_GAME_POSITION_DB) $(UTE_GAME_POSITION_DB_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_GAME_POSITION_DB_OBJ_DEPENDENCIES) -o $@

##
## game_tree_logger
##
## game_tree_logger.h : endgame_utils.h
## game_tree_logger.c : game_tree_logger.h file_utils.h
##
game_tree_logger: $(OBJ_GAME_TREE_LOGGER)
asm_game_tree_logger: $(ASM_GAME_TREE_LOGGER)

$(ASM_GAME_TREE_LOGGER): $(SRC_GAME_TREE_LOGGER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_GAME_TREE_LOGGER): $(SRC_GAME_TREE_LOGGER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## game_value_estimator
##
## game_value_estimator.h : endgame_utils.h
## game_value_estimator.c : game_value_estimator.h cfg.h file_utils.h rglm_data_files.h rglm_utils.h board.h
##                          transposition_table.h time_utils.h
##
game_value_estimator: $(OBJ_GAME_VALUE_ESTIMATOR)
asm_game_value_estimator: $(ASM_GAME_VALUE_ESTIMATOR)

$(ASM_GAME_VALUE_ESTIMATOR): $(SRC_GAME_VALUE_ESTIMATOR_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_GAME_VALUE_ESTIMATOR): $(SRC_GAME_VALUE_ESTIMATOR_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@


##
## game_tree_utils
##
## game_tree_utils.h : board.h
## game_tree_utils.c : game_tree_utils.h sort_utils.h time_utils.h
##
game_tree_utils: $(OBJ_GAME_TREE_UTILS)
asm_game_tree_utils: $(ASM_GAME_TREE_UTILS)

$(ASM_GAME_TREE_UTILS): $(SRC_GAME_TREE_UTILS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_GAME_TREE_UTILS): $(SRC_GAME_TREE_UTILS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_game_tree_utils: $(UTE_GAME_TREE_UTILS)

$(UTO_GAME_TREE_UTILS): $(UTS_GAME_TREE_UTILS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_GAME_TREE_UTILS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) $(OBJ_ARCH) $(OBJ_BIT_WORKS) $(OBJ_SORT_UTILS) $(OBJ_BOARD) \
  $(OBJ_GAME_TREE_UTILS)

$(UTE_GAME_TREE_UTILS): $(UTO_GAME_TREE_UTILS) $(UTE_GAME_TREE_UTILS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel $(TMPDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_GAME_TREE_UTILS_OBJ_DEPENDENCIES) -o $@

##
## gpdb_verify
##
## gpdb_verify.c : file_utils.h game_position_db.h main_option_parse.h
##
gpdb_verify: $(BIN_GPDB_VERIFY)
asm_gpdb_verify: $(ASM_GPDB_VERIFY)

$(ASM_GPDB_VERIFY): $(SRC_GPDB_VERIFY_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_GPDB_VERIFY): $(SRC_GPDB_VERIFY_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_GPDB_VERIFY_OBJ_DEPENDENCIES := $(OBJ_FILE_UTILS) $(OBJ_GAME_POSITION_DB) $(OBJ_MAIN_OPTION_PARSE) \
  $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_BOARD) $(OBJ_PRNG) $(OBJ_BIT_WORKS) $(OBJ_RED_BLACK_TREE) $(OBJ_ARCH) $(OBJ_MEMORY_MANAGER)

$(BIN_GPDB_VERIFY): $(OBJ_GPDB_VERIFY) $(BIN_GPDB_VERIFY_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_GPDB_VERIFY_OBJ_DEPENDENCIES) -o $@

##
## improved_fast_endgame_solver
##
## improved_fast_endgame_solver.h : endgame_utils.h
## improved_fast_endgame_solver.c : improved_fast_endgame_solver.h game_tree_logger.h
##
improved_fast_endgame_solver: $(OBJ_IMPROVED_FAST_ENDGAME_SOLVER)
asm_improved_fast_endgame_solver: $(ASM_IMPROVED_FAST_ENDGAME_SOLVER)

$(ASM_IMPROVED_FAST_ENDGAME_SOLVER): $(SRC_IMPROVED_FAST_ENDGAME_SOLVER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_IMPROVED_FAST_ENDGAME_SOLVER): $(SRC_IMPROVED_FAST_ENDGAME_SOLVER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## intel_intrinsics_trial
##
## intel_intrinsics_trial.c : (null)
##
intel_intrinsics_trial: $(BIN_INTEL_INTRINSICS_TRIAL)
asm_intel_intrinsics_trial: $(ASM_INTEL_INTRINSICS_TRIAL)

$(ASM_INTEL_INTRINSICS_TRIAL): $(SRC_INTEL_INTRINSICS_TRIAL_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_INTEL_INTRINSICS_TRIAL): $(SRC_INTEL_INTRINSICS_TRIAL_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

$(BIN_INTEL_INTRINSICS_TRIAL): $(OBJ_INTEL_INTRINSICS_TRIAL) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< -o $@

##
## isqrt
##
## isqrt.h : (null)
## isqrt.c : isqrt.h
##
isqrt: $(OBJ_ISQRT)
asm_isqrt: $(ASM_ISQRT)

$(ASM_ISQRT): $(SRC_ISQRT_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_ISQRT): $(SRC_ISQRT_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_isqrt: $(UTE_ISQRT)

$(UTO_ISQRT): $(UTS_ISQRT) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_ISQRT_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_ISQRT)

$(UTE_ISQRT): $(UTO_ISQRT) $(UTE_ISQRT_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_ISQRT_OBJ_DEPENDENCIES) -o $@

##
## linear_algebra
##
## linear_algebra.h : blis.h FLAME.h
## linear_algebra.c : linear_algebra.h time_utils.h file_utils.h
##
linear_algebra: $(OBJ_LINEAR_ALGEBRA)
asm_linear_algebra: $(ASM_LINEAR_ALGEBRA)

$(ASM_LINEAR_ALGEBRA): $(SRC_LINEAR_ALGEBRA_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_LINEAR_ALGEBRA): $(SRC_LINEAR_ALGEBRA_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_linear_algebra: $(UTE_LINEAR_ALGEBRA)

$(UTO_LINEAR_ALGEBRA): $(UTS_LINEAR_ALGEBRA) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_LINEAR_ALGEBRA_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) $(OBJ_PRNG) \
  $(OBJ_LINEAR_ALGEBRA)

LIB_LINEAR_ALGEBRA := -lm $(FLAME_LIB) $(BLIS_LIB) -lgomp -lpthread

$(UTE_LINEAR_ALGEBRA): $(UTO_LINEAR_ALGEBRA) $(UTE_LINEAR_ALGEBRA_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_LINEAR_ALGEBRA_OBJ_DEPENDENCIES) $(LIB_LINEAR_ALGEBRA) -o $@

##
## linked_list
##
## linked_list.h : (null)
## linked_list.c : linked_list.h
##
linked_list: $(OBJ_LINKED_LIST)
asm_linked_list: $(ASM_LINKED_LIST)

$(ASM_LINKED_LIST): $(SRC_LINKED_LIST_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_LINKED_LIST): $(SRC_LINKED_LIST_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_linked_list: $(UTE_LINKED_LIST)

$(UTO_LINKED_LIST): $(UTS_LINKED_LIST) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_LINKED_LIST_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS)

$(UTE_LINKED_LIST): $(UTO_LINKED_LIST) $(UTE_LINKED_LIST_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_LINKED_LIST_OBJ_DEPENDENCIES) -o $@

##
## main_option_parse
##
## main_option_parse.h : (null)
## main_option_parse.c : main_option_parse.h
##
main_option_parse: $(OBJ_MAIN_OPTION_PARSE)
asm_main_option_parse: $(ASM_MAIN_OPTION_PARSE)

$(ASM_MAIN_OPTION_PARSE): $(SRC_MAIN_OPTION_PARSE_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_MAIN_OPTION_PARSE): $(SRC_MAIN_OPTION_PARSE_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_main_option_parse: $(UTE_MAIN_OPTION_PARSE)

$(UTO_MAIN_OPTION_PARSE): $(UTS_MAIN_OPTION_PARSE) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_MAIN_OPTION_PARSE_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS)

$(UTE_MAIN_OPTION_PARSE): $(UTO_MAIN_OPTION_PARSE) $(UTE_MAIN_OPTION_PARSE_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_MAIN_OPTION_PARSE_OBJ_DEPENDENCIES) -o $@

##
## memory_manager
##
## memory_manager.h : (null)
## memory_manager.c : memory_manager.h
##
memory_manager: $(OBJ_MEMORY_MANAGER)
asm_memory_manager: $(ASM_MEMORY_MANAGER)

$(ASM_MEMORY_MANAGER): $(SRC_MEMORY_MANAGER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_MEMORY_MANAGER): $(SRC_MEMORY_MANAGER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## memory_object_pool
##
## memory_object_pool.h : (null)
## memory_object_pool.c : object_pool.h sort_utils.h
##
memory_object_pool: $(OBJ_MEMORY_OBJECT_POOL)
asm_memory_object_pool: $(ASM_MEMORY_OBJECT_POOL)

$(ASM_MEMORY_OBJECT_POOL): $(SRC_MEMORY_OBJECT_POOL_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_MEMORY_OBJECT_POOL): $(SRC_MEMORY_OBJECT_POOL_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_memory_object_pool: $(UTE_MEMORY_OBJECT_POOL)

$(UTO_MEMORY_OBJECT_POOL): $(UTS_MEMORY_OBJECT_POOL) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_MEMORY_OBJECT_POOL_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_SORT_UTILS) \
  $(OBJ_MEMORY_OBJECT_POOL)

$(UTE_MEMORY_OBJECT_POOL): $(UTO_MEMORY_OBJECT_POOL) $(UTE_MEMORY_OBJECT_POOL_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_MEMORY_OBJECT_POOL_OBJ_DEPENDENCIES) -o $@

##
## minimax_solver
##
## minimax_solver.h : endgame_utils.h
## minimax_solver.c : minimax_solver.h game_tree_logger.h board_pattern.h
##
minimax_solver: $(OBJ_MINIMAX_SOLVER)
asm_minimax_solver: $(ASM_MINIMAX_SOLVER)

$(ASM_MINIMAX_SOLVER): $(SRC_MINIMAX_SOLVER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_MINIMAX_SOLVER): $(SRC_MINIMAX_SOLVER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## principal_variation
##
## principal_variation.h : (null)
## principal_variation.c : principal_variation.h
##
principal_variation: $(OBJ_PRINCIPAL_VARIATION)
asm_principal_variation: $(ASM_PRINCIPAL_VARIATION)

$(ASM_PRINCIPAL_VARIATION): $(SRC_PRINCIPAL_VARIATION_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_PRINCIPAL_VARIATION): $(SRC_PRINCIPAL_VARIATION_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## prng
##
## prng.h : (null)
## prng.c : prng.h time_utils.h bit_works.h
##
prng: $(OBJ_PRNG)
asm_prng: $(ASM_PRNG)

$(ASM_PRNG): $(SRC_PRNG_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_PRNG): $(SRC_PRNG_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_prng: $(UTE_PRNG)

$(UTO_PRNG): $(UTS_PRNG) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_PRNG_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG)

$(UTE_PRNG): $(UTO_PRNG) $(UTE_PRNG_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_PRNG_OBJ_DEPENDENCIES) -o $@

##
## read_game_tree_log
##
## read_game_tree_log.c : game_tree_logger.h main_option_parse.h
##
read_game_tree_log: $(BIN_READ_GAME_TREE_LOG)
asm_read_game_tree_log: $(ASM_READ_GAME_TREE_LOG)

$(ASM_READ_GAME_TREE_LOG): $(SRC_READ_GAME_TREE_LOG_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_READ_GAME_TREE_LOG): $(SRC_READ_GAME_TREE_LOG_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_READ_GAME_TREE_LOG_OBJ_DEPENDENCIES := $(OBJ_MAIN_OPTION_PARSE) $(OBJ_ARCH) $(OBJ_PRNG) $(OBJ_TIME_UTILS) $(OBJ_BIT_WORKS) \
  $(OBJ_BOARD)

$(BIN_READ_GAME_TREE_LOG): $(OBJ_READ_GAME_TREE_LOG) $(BIN_READ_GAME_TREE_LOG_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_READ_GAME_TREE_LOG_OBJ_DEPENDENCIES) -o $@

##
## read_pve_dump
##
## read_pve_dump.c : file_utils.h game_tree_utils.h main_option_parse.h
##
read_pve_dump: $(BIN_READ_PVE_DUMP)
asm_read_pve_dump: $(ASM_READ_PVE_DUMP)

$(ASM_READ_PVE_DUMP): $(SRC_READ_PVE_DUMP_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_READ_PVE_DUMP): $(SRC_READ_PVE_DUMP_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_READ_PVE_DUMP_OBJ_DEPENDENCIES := $(OBJ_MAIN_OPTION_PARSE) $(OBJ_ARCH) $(OBJ_PRNG) $(OBJ_TIME_UTILS) $(OBJ_BIT_WORKS) \
  $(OBJ_BOARD) $(OBJ_FILE_UTILS) $(OBJ_GAME_TREE_UTILS) $(OBJ_SORT_UTILS)

$(BIN_READ_PVE_DUMP): $(OBJ_READ_PVE_DUMP) $(BIN_READ_PVE_DUMP_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_READ_PVE_DUMP_OBJ_DEPENDENCIES) -o $@

##
## red_black_tree
##
## red_black_tree.h : memory_manager.h
## red_black_tree.c : red_black_tree.h memory_manager.h
##
red_black_tree: $(OBJ_RED_BLACK_TREE)
asm_red_black_tree: $(ASM_RED_BLACK_TREE)

$(ASM_RED_BLACK_TREE): $(SRC_RED_BLACK_TREE) $(HDR_RED_BLACK_TREE) $(HDR_TIME_UTILS) $(HDR_BIT_WORKS) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_RED_BLACK_TREE): $(SRC_RED_BLACK_TREE) $(HDR_RED_BLACK_TREE) $(HDR_TIME_UTILS) $(HDR_BIT_WORKS) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_red_black_tree: $(UTE_RED_BLACK_TREE)

$(UTO_RED_BLACK_TREE): $(UTS_RED_BLACK_TREE) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_RED_BLACK_TREE_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) $(OBJ_MEMORY_MANAGER) $(OBJ_SORT_UTILS) \
  $(OBJ_RED_BLACK_TREE)

$(UTE_RED_BLACK_TREE): $(UTO_RED_BLACK_TREE) $(UTE_RED_BLACK_TREE_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_RED_BLACK_TREE_OBJ_DEPENDENCIES) -o $@

##
## regab
##
## regab.c : main_option_parse.h file_utils.h cfg.h prng.h exact_solver.h rglm_solver.h sort_utils.h rglm_data_files.h rglm_utils.h
##
regab: $(BIN_REGAB)
asm_regab: $(ASM_REGAB)

$(ASM_REGAB): $(SRC_REGAB_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_REGAB): $(SRC_REGAB_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_REGAB_OBJ_DEPENDENCIES := $(OBJ_LINKED_LIST) $(OBJ_MAIN_OPTION_PARSE) $(OBJ_TIME_UTILS) \
  $(OBJ_BIT_WORKS) $(OBJ_PRNG) $(OBJ_ARCH) $(OBJ_CFG) $(OBJ_SORT_UTILS) $(OBJ_RGLM_DATA_FILES) \
  $(OBJ_SHA3) $(OBJ_FILE_UTILS) $(OBJ_RGLM_SOLVER) $(OBJ_GAME_TREE_UTILS) $(OBJ_GAME_TREE_LOGGER) \
  $(OBJ_BOARD) $(OBJ_BOARD_TRANS) $(OBJ_BOARD_PATTERN) $(OBJ_RGLM_UTILS) $(OBJ_EXACT_SOLVER)

$(BIN_REGAB): $(OBJ_REGAB) $(BIN_REGAB_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_REGAB_OBJ_DEPENDENCIES) -lm $(PG_LIBS) -o $@

##
## rglm
##
## rglm.c : time_utils.h main_option_parse.h rglm_utils.h linear_algebra.h
##
rglm: $(BIN_RGLM)
asm_rglm: $(ASM_RGLM)

$(ASM_RGLM): $(SRC_RGLM_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_RGLM): $(SRC_RGLM_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_RGLM_OBJ_DEPENDENCIES := \
  $(OBJ_LINEAR_ALGEBRA) $(OBJ_TIME_UTILS) $(OBJ_RGLM_UTILS) $(OBJ_BOARD_PATTERN) $(OBJ_BOARD_TRANS) \
  $(OBJ_BOARD) $(OBJ_BIT_WORKS) $(OBJ_PRNG) $(OBJ_ARCH) $(OBJ_RGLM_DATA_FILES) $(OBJ_SHA3) $(OBJ_FILE_UTILS) \
  $(OBJ_MAIN_OPTION_PARSE)

$(BIN_RGLM): $(OBJ_RGLM) $(BIN_RGLM_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_RGLM_OBJ_DEPENDENCIES) -lm $(FLAME_LIB) $(BLIS_LIB) -lgomp -lpthread -o $@

##
## rglm_data_files
##
## rglm_data_files.h : board_pattern.h
## rglm_data_files.c : rglm_data_files.h sha.3 file_utils.h
##
rglm_data_files: $(OBJ_RGLM_DATA_FILES)
asm_rglm_data_files: $(ASM_RGLM_DATA_FILES)

$(ASM_RGLM_DATA_FILES): $(SRC_RGLM_DATA_FILES_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_RGLM_DATA_FILES): $(SRC_RGLM_DATA_FILES_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_rglm_data_files: $(UTE_RGLM_DATA_FILES)

$(UTO_RGLM_DATA_FILES): $(UTS_RGLM_DATA_FILES) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_RGLM_DATA_FILES_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) $(OBJ_MEMORY_MANAGER) $(OBJ_SORT_UTILS) \
  $(OBJ_RGLM_DATA_FILES) $(OBJ_SHA3) $(OBJ_FILE_UTILS) $(OBJ_BOARD_PATTERN) $(OBJ_BOARD_TRANS) \
  $(OBJ_BOARD) $(OBJ_ARCH) $(OBJ_BIT_WORKS)

$(UTE_RGLM_DATA_FILES): $(UTO_RGLM_DATA_FILES) $(UTE_RGLM_DATA_FILES_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_RGLM_DATA_FILES_OBJ_DEPENDENCIES) -o $@

##
## rglm_solver
##
## rglm_solver.h : endgame_utils.h
## rglm_solver.c : rglm_solver.h file_utils.h cfg.h game_tree_logger.h rglm_data_files.h rglm_utils.h
##
rglm_solver: $(OBJ_RGLM_SOLVER)
asm_rglm_solver: $(ASM_RGLM_SOLVER)

$(ASM_RGLM_SOLVER): $(SRC_RGLM_SOLVER_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_RGLM_SOLVER): $(SRC_RGLM_SOLVER_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## rglm_utils
##
## rglm_utils.h : rglm_data_files.h
## rglm_utils.c : rglm_utils.h
##
rglm_utils: $(OBJ_RGLM_UTILS)
asm_rglm_utils: $(ASM_RGLM_UTILS)

$(ASM_RGLM_UTILS): $(SRC_RGLM_UTILS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_RGLM_UTILS): $(SRC_RGLM_UTILS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_rglm_utils: $(UTE_RGLM_UTILS)

$(UTO_RGLM_UTILS): $(UTS_RGLM_UTILS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_RGLM_UTILS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) $(OBJ_MEMORY_MANAGER) $(OBJ_SORT_UTILS) \
  $(OBJ_RGLM_UTILS) $(OBJ_SHA3) $(OBJ_FILE_UTILS) $(OBJ_BOARD_PATTERN) $(OBJ_BOARD_TRANS) \
  $(OBJ_BOARD) $(OBJ_ARCH) $(OBJ_BIT_WORKS) \
  $(OBJ_RGLM_DATA_FILES)

LIB_RGLM_UTILS := -lm

$(UTE_RGLM_UTILS): $(UTO_RGLM_UTILS) $(UTE_RGLM_UTILS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_RGLM_UTILS_OBJ_DEPENDENCIES) $(LIB_RGLM_UTILS) -o $@

##
## rglmw
##
## rglmw.c : time_utils.h main_option_parse.h rglm_utils.h linear_algebra.h
##
rglmw: $(BIN_RGLMW)
asm_rglmw: $(ASM_RGLMW)

$(ASM_RGLMW): $(SRC_RGLMW_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_RGLMW): $(SRC_RGLMW_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_RGLMW_OBJ_DEPENDENCIES := \
  $(OBJ_LINEAR_ALGEBRA) $(OBJ_TIME_UTILS) $(OBJ_RGLM_UTILS) $(OBJ_BOARD_PATTERN) $(OBJ_BOARD_TRANS) \
  $(OBJ_BOARD) $(OBJ_BIT_WORKS) $(OBJ_PRNG) $(OBJ_ARCH) $(OBJ_RGLM_DATA_FILES) $(OBJ_SHA3) $(OBJ_FILE_UTILS) \
  $(OBJ_MAIN_OPTION_PARSE)

$(BIN_RGLMW): $(OBJ_RGLMW) $(BIN_RGLMW_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_RGLMW_OBJ_DEPENDENCIES) -lm $(FLAME_LIB) $(BLIS_LIB) -lgomp -lpthread -o $@

##
## sha3
##
## sha3.h : (null)
## sha3.c : sha3.h bit_works.h
##
sha3: $(OBJ_SHA3)
asm_sha3: $(ASM_SHA3)

$(ASM_SHA3): $(SRC_SHA3) $(HDR_SHA3) $(HDR_BIT_WORKS) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_SHA3): $(SRC_SHA3) $(HDR_SHA3) $(HDR_BIT_WORKS) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_sha3: $(UTE_SHA3)

$(UTO_SHA3): $(UTS_SHA3) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_SHA3_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_SHA3)

$(UTE_SHA3): $(UTO_SHA3) $(UTE_SHA3_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_SHA3_OBJ_DEPENDENCIES) -o $@

##
## sort_utils
##
## sort_utils.h : (null)
## sort_utils.c : sort_utils.h
##
sort_utils: $(OBJ_SORT_UTILS)
asm_sort_utils: $(ASM_SORT_UTILS)

$(ASM_SORT_UTILS): $(SRC_SORT_UTILS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_SORT_UTILS): $(SRC_SORT_UTILS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_sort_utils: $(UTE_SORT_UTILS)

$(UTO_SORT_UTILS): $(UTS_SORT_UTILS) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_SORT_UTILS_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) \
  $(OBJ_PRNG) \
  $(OBJ_SORT_UTILS)

$(UTE_SORT_UTILS): $(UTO_SORT_UTILS) $(UTE_SORT_UTILS_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_SORT_UTILS_OBJ_DEPENDENCIES) -o $@

##
## time_utils
##
## time_utils.h : (null)
## time_utils.c : time_utils.h
##
time_utils: $(OBJ_TIME_UTILS)
asm_time_utils: $(ASM_TIME_UTILS)

$(ASM_TIME_UTILS): $(SRC_TIME_UTILS_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_TIME_UTILS): $(SRC_TIME_UTILS_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## transposition_table
##
## transposition_table.h : board.h
## transposition_table.c : transposition_table.h
##
transposition_table: $(OBJ_TRANSPOSITION_TABLE)
asm_transposition_table: $(ASM_TRANSPOSITION_TABLE)

$(ASM_TRANSPOSITION_TABLE): $(SRC_TRANSPOSITION_TABLE_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_TRANSPOSITION_TABLE): $(SRC_TRANSPOSITION_TABLE_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

ut_transposition_table: $(UTE_TRANSPOSITION_TABLE)

$(UTO_TRANSPOSITION_TABLE): $(UTS_TRANSPOSITION_TABLE) $(OBJ_UNIT_TEST) $(OBJ_TRANSPOSITION_TABLE) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_TRANSPOSITION_TABLE_OBJ_DEPENDENCIES := \
  $(OBJ_LINKED_LIST) $(OBJ_UNIT_TEST) $(OBJ_TIME_UTILS) $(OBJ_MAIN_OPTION_PARSE) $(OBJ_BIT_WORKS) \
  $(OBJ_BOARD) $(OBJ_PRNG) $(OBJ_ARCH) \
  $(OBJ_TRANSPOSITION_TABLE)

$(UTE_TRANSPOSITION_TABLE): $(UTO_TRANSPOSITION_TABLE) $(UTE_TRANSPOSITION_TABLE_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_TRANSPOSITION_TABLE_OBJ_DEPENDENCIES) -o $@

##
## trsm
##
ut_trsm: $(UTE_TRSM)

$(UTO_TRSM): $(UTS_TRSM) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_TRSM_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS) $(OBJ_PRNG) \
  $(OBJ_TRSM) $(OBJ_LINEAR_ALGEBRA)

LIB_TRSM := -lm $(FLAME_LIB) $(BLIS_LIB) -lgomp -lpthread

$(UTE_TRSM): $(UTO_TRSM) $(UTE_TRSM_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_TRSM_OBJ_DEPENDENCIES) $(LIB_TRSM) -o $@

##
## unit_test
##
## unit_test.h : linked_list.h time_utils.h
## unit_test.c : unit_test.h main_option_parse.h
##
unit_test: $(OBJ_UNIT_TEST)
asm_unit_test: $(ASM_UNIT_TEST)

$(ASM_UNIT_TEST): $(SRC_UNIT_TEST_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_UNIT_TEST): $(SRC_UNIT_TEST_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

##
## utest
##
## utest.c : unit_test.h
##
utest: $(BIN_UTEST)
asm_utest: $(ASM_UTEST)

$(ASM_UTEST): $(SRC_UTEST_DEP) Makefile $(ASMDIR)/.dir_sentinel
	@printf "Compiling source file $< into assembly $@\n"
	@$(CC) $(ASMFLAGS) -S $< -o $@

$(OBJ_UTEST): $(SRC_UTEST_DEP) Makefile $(OBJDIR)/.dir_sentinel
	@printf "Compiling $< into $@\n"
	@$(CC) $(CFLAGS) -c $< -o $@

BIN_UTEST_OBJ_DEPENDENCIES := $(OBJ_LINKED_LIST) $(OBJ_UNIT_TEST) $(OBJ_TIME_UTILS) $(OBJ_MAIN_OPTION_PARSE)

$(BIN_UTEST): $(OBJ_UTEST) $(BIN_UTEST_OBJ_DEPENDENCIES) Makefile $(BINDIR)/.dir_sentinel
	@printf "Linking executable $@\n"
	@$(CC) $(LDFLAGS) $< $(BIN_UTEST_OBJ_DEPENDENCIES) -o $@

ut_utest: $(UTE_UTEST)

$(UTO_UTEST): $(UTS_UTEST) Makefile $(TEST_OBJDIR)/.dir_sentinel
	@printf "Test suite: compiling $< into $@\n"
	@$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

UTE_UTEST_OBJ_DEPENDENCIES := \
  $(OBJ_MAIN_OPTION_PARSE) $(OBJ_UNIT_TEST) $(OBJ_LINKED_LIST) $(OBJ_TIME_UTILS)

$(UTE_UTEST): $(UTO_UTEST) $(UTE_UTEST_OBJ_DEPENDENCIES) Makefile $(TEST_BINDIR)/.dir_sentinel
	@printf "Test suite: linking executable $@\n"
	@$(CC) $(LDFLAGS_TEST) $< $(UTE_UTEST_OBJ_DEPENDENCIES) -o $@


###
### ### ### ### ### ### ### ### ### ###
###


doxygen: $(DOCDIR)/.dir_sentinel
	doxygen > build/doxygen.log

clean:
	rm -Rf $(BUILDDIR)

info:
	@echo "MAINS = "$(MAINS)
	@echo "MODULES = "$(MODULES)
	@echo "UTEST_MAINS = "$(UTEST_MAINS)
	@echo "HDRS = "$(HDRS)
	@echo "OBJS = "$(OBJS)
	@echo "BINS = "$(BINS)
	@echo "ASMS = "$(ASMS)
	@echo "UTEST_BINS = "$(UTEST_BINS)
	@echo "TEST_OBJDIR = "$(TEST_OBJDIR)
	@echo "TESTDIR = "$(TESTDIR)
	@echo "TEST_INCLUDE = "$(TEST_INCLUDE)
	@echo "CFLAGS = "$(CFLAGS)
	@echo "PG_INCLUDE_DIR = "$(PG_INCLUDE_DIR)


$(ENDGAME_LOG_DIR): $(ENDGAME_LOG_DIR)/.dir_sentinel $(BUILDDIR)/.dir_sentinel

$(ENDGAME_LOG_DIR)/.dir_sentinel:
	mkdir -p $(ENDGAME_LOG_DIR)
	touch $(ENDGAME_LOG_DIR)/.dir_sentinel


endgame_log_files: endgame_solver $(ENDGAME_LOG_DIR)/.dir_sentinel
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt -q ffo-01 -s es -l $(ENDGAME_LOG_DIR)/exact_solver_log-ffo-01
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt -q ffo-01 -s ifes -l $(ENDGAME_LOG_DIR)/ifes_solver_log-ffo-01
	./$(BINDIR)/endgame_solver -f db/gpdb-sample-games.txt -q initial -s rand -n 100 -l $(ENDGAME_LOG_DIR)/random_game_sampler_log-t100
	./$(BINDIR)/endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s minimax -l $(ENDGAME_LOG_DIR)/minimax_log-ffo-01-simplified-4
	./$(BINDIR)/endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s rab -n 3 -l $(ENDGAME_LOG_DIR)/rab_solver_log-ffo-01-simplified-4_n3
	./$(BINDIR)/endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s ab -l $(ENDGAME_LOG_DIR)/ab_solver_log-ffo-01-simplified-4

endgame_pv_files: endgame_solver $(ENDGAME_LOG_DIR)/.dir_sentinel
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-01 -d $(ENDGAME_LOG_DIR)/pve-ffo-01.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-02 -d $(ENDGAME_LOG_DIR)/pve-ffo-02.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-03 -d $(ENDGAME_LOG_DIR)/pve-ffo-03.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-04 -d $(ENDGAME_LOG_DIR)/pve-ffo-04.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-05 -d $(ENDGAME_LOG_DIR)/pve-ffo-05.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-06 -d $(ENDGAME_LOG_DIR)/pve-ffo-06.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-07 -d $(ENDGAME_LOG_DIR)/pve-ffo-07.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-08 -d $(ENDGAME_LOG_DIR)/pve-ffo-08.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-09 -d $(ENDGAME_LOG_DIR)/pve-ffo-09.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-10 -d $(ENDGAME_LOG_DIR)/pve-ffo-10.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-11 -d $(ENDGAME_LOG_DIR)/pve-ffo-11.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-12 -d $(ENDGAME_LOG_DIR)/pve-ffo-12.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-13 -d $(ENDGAME_LOG_DIR)/pve-ffo-13.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-14 -d $(ENDGAME_LOG_DIR)/pve-ffo-14.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-15 -d $(ENDGAME_LOG_DIR)/pve-ffo-15.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-16 -d $(ENDGAME_LOG_DIR)/pve-ffo-16.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-17 -d $(ENDGAME_LOG_DIR)/pve-ffo-17.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-18 -d $(ENDGAME_LOG_DIR)/pve-ffo-18.dat
	./$(BINDIR)/endgame_solver -f db/gpdb-ffo.txt --pv-full-rec --pv-no-print -s es -q ffo-19 -d $(ENDGAME_LOG_DIR)/pve-ffo-19.dat

dat2csv: read_game_tree_log $(ENDGAME_LOG_DIR)/.dir_sentinel
	./$(BINDIR)/read_game_tree_log -f $(ENDGAME_LOG_DIR)/ab_solver_log-ffo-01-simplified-4.dat >     $(ENDGAME_LOG_DIR)/ab_solver_log-ffo-01-simplified-4.csv
	./$(BINDIR)/read_game_tree_log -f $(ENDGAME_LOG_DIR)/exact_solver_log-ffo-01.dat >               $(ENDGAME_LOG_DIR)/exact_solver_log-ffo-01.csv
	./$(BINDIR)/read_game_tree_log -f $(ENDGAME_LOG_DIR)/ifes_solver_log-ffo-01.dat >                $(ENDGAME_LOG_DIR)/ifes_solver_log-ffo-01.csv
	./$(BINDIR)/read_game_tree_log -f $(ENDGAME_LOG_DIR)/minimax_log-ffo-01-simplified-4.dat >       $(ENDGAME_LOG_DIR)/minimax_log-ffo-01-simplified-4.csv
	./$(BINDIR)/read_game_tree_log -f $(ENDGAME_LOG_DIR)/rab_solver_log-ffo-01-simplified-4_n3.dat > $(ENDGAME_LOG_DIR)/rab_solver_log-ffo-01-simplified-4_n3.csv
	./$(BINDIR)/read_game_tree_log -f $(ENDGAME_LOG_DIR)/random_game_sampler_log-t100.dat >          $(ENDGAME_LOG_DIR)/random_game_sampler_log-t100.csv

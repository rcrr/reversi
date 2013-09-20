#
#  Makefile
#
#  This file is part of the reversi program
#  http://github.com/rcrr/reversi
#
#  Copyright (c) 2013 Roberto Corradini. All rights reserved.
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

# Add all the programs that has a main and that will be compiled and linked as a bin executable.
MAINS = endgame_solver gpdb_verify

# Add all the test programs that has a main and that will be compiled and linked as a bin executable.
TEST_PROGS = bit_works_test board_test game_position_db_test game_position_test exact_solver_test \
             improved_fast_endgame_solver_test

#
# No changes from here to the end, unless a structurar change of the build process happens.
#

#
# Add -pg to CFLAGS and DFLAGS to profile.

CC = gcc 
#CFLAGS = -std=c99 -pedantic -Wall -g -O0 `pkg-config --cflags glib-2.0` -D_POSIX_SOURCE
#LDFLAGS =
# add -pg for gprof execution.
CFLAGS = -std=c99 -pedantic -Wall -g -O3 -DG_DISABLE_ASSERT `pkg-config --cflags glib-2.0` -D_POSIX_SOURCE
LDFLAGS = 
CFLAGS_TEST = -std=c99 -pedantic -Wall -g -O2 `pkg-config --cflags glib-2.0` -D_POSIX_SOURCE
LDFLAGS_TEST =
ASMFLAGS = -std=c99 -pedantic -Wall -O3 -DG_DISABLE_ASSERT -masm=intel `pkg-config --cflags glib-2.0` -D_POSIX_SOURCE
LIBS = `pkg-config --libs glib-2.0`
TEST_LIBS =
SRCDIR = src
TESTDIR = test

BUILDDIR = build
OBJDIR   = $(BUILDDIR)/obj
BINDIR   = $(BUILDDIR)/bin
DOCDIR   = $(BUILDDIR)/doc
BTESTDIR = $(BUILDDIR)/test
ASMDIR   = $(BUILDDIR)/asm

SRCS = $(wildcard $(SRCDIR)/*.c)
HDRS = $(wildcard $(SRCDIR)/*.h)
OBJS = $(addsuffix .o,$(basename $(subst $(SRCDIR),$(OBJDIR),$(SRCS))))
ASMS = $(addsuffix .asm,$(basename $(subst $(SRCDIR),$(ASMDIR),$(SRCS))))
BINS = $(addprefix $(BINDIR)/,$(MAINS))

OBJS_WITH_MAIN = $(addsuffix .o,$(addprefix $(OBJDIR)/,$(MAINS)))
OBJS_NO_MAIN = $(filter-out $(OBJS_WITH_MAIN),$(OBJS))

TEST_OBJDIR  = $(BTESTDIR)/obj
TEST_BINDIR  = $(BTESTDIR)/bin
TEST_SRCS    = $(wildcard $(TESTDIR)/*.c)
TEST_OBJS    = $(addsuffix .o,$(basename $(subst $(TESTDIR)/,$(TEST_OBJDIR)/,$(TEST_SRCS))))
TEST_BINS    = $(addprefix $(TEST_BINDIR)/,$(TEST_PROGS))
TEST_INCLUDE = -I./src

TEST_OBJS_WITH_MAIN = $(addsuffix .o,$(addprefix $(TEST_OBJDIR)/,$(TEST_PROGS)))
TEST_OBJS_NO_MAIN = $(filter-out $(TEST_OBJS_WITH_MAIN),$(TEST_OBJS))

all: $(BINS) $(TEST_BINS)

$(BINS): $(OBJS) | $(BINDIR)
	$(eval OBJECT = $(addsuffix .o,$(basename $(subst $(BINDIR),$(OBJDIR),$@))))
	$(CC) $(LDFLAGS) $(OBJECT) $(OBJS_NO_MAIN) $(LIBS) -o $@

$(OBJS): $(OBJDIR)/%.o: $(SRCDIR)/%.c $(HDRS) | $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: asm
asm: $(ASMS)

$(ASMS): $(ASMDIR)/%.asm: $(SRCDIR)/%.c $(HDRS) | $(ASMDIR)
	$(CC) $(ASMFLAGS) -S $< -o $@

.PHONY: test_obj
test_obj: $(TEST_OBJS)

$(TEST_OBJS): $(TEST_OBJDIR)/%.o: $(TESTDIR)/%.c | $(TEST_OBJDIR)
	$(CC) $(CFLAGS_TEST) -c $< -o $@ $(TEST_INCLUDE)

.PHONY: test_bin
test_bin: $(OBJS) $(TEST_OBJS) $(TEST_BINS)

$(TEST_BINS): $(TEST_OBJS) $(OBJS) | $(TEST_BINDIR)
	$(eval OBJECT = $(addsuffix .o,$(basename $(subst $(TEST_BINDIR),$(TEST_OBJDIR),$@))))
	$(CC) $(LDFLAGS_TEST) $(OBJECT) $(TEST_OBJS_NO_MAIN) $(OBJS_NO_MAIN) $(LIBS) $(TEST_LIBS) -o $@

.PHONY: test
test: $(TEST_BINS)
	gtester --verbose -k -o $(BTESTDIR)/gtester-log.xml $(TEST_BINS)

.PHONY: test_report
test_report: $(BTESTDIR)/gtester-log.html

$(BTESTDIR)/gtester-log.html: $(BTESTDIR)/gtester-log.xml
	gtester-report $(BTESTDIR)/gtester-log.xml > $(BTESTDIR)/gtester-log.html

.PHONY: $(BUILDDIR)
$(BUILDDIR): $(BUILDDIR)/.dir_sentinel

$(BUILDDIR)/.dir_sentinel:
	mkdir -p $(BUILDDIR)
	touch $(BUILDDIR)/.dir_sentinel


.PHONY: $(OBJDIR)
$(OBJDIR): $(OBJDIR)/.dir_sentinel | $(BUILDDIR)

$(OBJDIR)/.dir_sentinel:
	mkdir -p $(OBJDIR)
	touch $(OBJDIR)/.dir_sentinel


.PHONY: $(BINDIR)
$(BINDIR): $(BINDIR)/.dir_sentinel | $(BUILDDIR)

$(BINDIR)/.dir_sentinel:
	mkdir -p $(BINDIR)
	touch $(BINDIR)/.dir_sentinel


.PHONY: $(DOCDIR)
$(DOCDIR): $(DOCDIR)/.dir_sentinel | $(BUILDDIR)

$(DOCDIR)/.dir_sentinel:
	mkdir -p $(DOCDIR)
	touch $(DOCDIR)/.dir_sentinel


.PHONY: $(ASMDIR)
$(ASMDIR): $(ASMDIR)/.dir_sentinel | $(BUILDDIR)

$(ASMDIR)/.dir_sentinel:
	mkdir -p $(ASMDIR)
	touch $(ASMDIR)/.dir_sentinel


.PHONY: $(BTESTDIR)
$(BTESTDIR): $(BTESTDIR)/.dir_sentinel | $(BUILDDIR)

$(BTESTDIR)/.dir_sentinel:
	mkdir -p $(BTESTDIR)
	touch $(BTESTDIR)/.dir_sentinel


.PHONY: $(TEST_OBJDIR)
$(TEST_OBJDIR): $(TEST_OBJDIR)/.dir_sentinel | $(BTESTDIR)

$(TEST_OBJDIR)/.dir_sentinel:
	mkdir -p $(TEST_OBJDIR)
	touch $(TEST_OBJDIR)/.dir_sentinel


.PHONY: $(TEST_BINDIR)
$(TEST_BINDIR): $(TEST_BINDIR)/.dir_sentinel | $(BTESTDIR)

$(TEST_BINDIR)/.dir_sentinel:
	mkdir -p $(TEST_BINDIR)
	touch $(TEST_BINDIR)/.dir_sentinel


.PHONY: doxygen
doxygen: $(DOCDIR)
	doxygen >build/doxygen.log
	(cd build/doc/latex; make >make-stdout_and_stderr.log 2>&1)

ABC_DIR = build/abc

.PHONY: $(ABC_DIR)
$(ABC_DIR): $(BUILDDIR)
	@if test ! -d $(ABC_DIR); then \
	  echo "ABC_DIR = " $(ABC_DIR) " does not exist."; \
	  mkdir -p $(ABC_DIR); \
	else \
	  echo "ABC_DIR = " $(ABC_DIR) " does DO  exist."; \
	fi

ZZZ_DIR = build/zzz

.PHONY: $(ZZZ_DIR)
$(ZZZ_DIR): $(ZZZ_DIR)/.dir_sentinel

$(ZZZ_DIR)/.dir_sentinel:
	mkdir -p $(ZZZ_DIR)
	touch $(ZZZ_DIR)/.dir_sentinel


.PHONY: clean
clean:
	rm -Rf $(BUILDDIR)

.PHONY: info
info:
	@echo "SRCS = "$(SRCS)
	@echo "HDRS = "$(HDRS)
	@echo "OBJS = "$(OBJS)
	@echo "BINS = "$(BINS)
	@echo "OBJS_WITH_MAIN = "$(OBJS_WITH_MAIN)
	@echo "OBJS_NO_MAIN = "$(OBJS_NO_MAIN)
	@echo "TEST_SRCS = "$(TEST_SRCS)
	@echo "TEST_OBJS = "$(TEST_OBJS)
	@echo "TEST_BINS = "$(TEST_BINS)
	@echo "TEST_OBJDIR = "$(TEST_OBJDIR)
	@echo "TESTDIR = "$(TESTDIR)
	@echo "TEST_INCLUDE = "$(TEST_INCLUDE)
	@echo "CFLAGS = "$(CFLAGS)
	@echo "TEST_OBJS_NO_MAIN = "$(TEST_OBJS_NO_MAIN)
	@echo "TEST_OBJS_WITH_MAIN = "$(TEST_OBJS_WITH_MAIN)

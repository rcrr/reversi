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

MAINS = endgame_solver

CC = gcc 
CFLAGS = -ansi -std=c99 -pedantic -Wall -g -O0 `pkg-config --cflags glib-2.0` -D_POSIX_SOURCE
LDFLAGS = 
LIBS = `pkg-config --libs glib-2.0`
TEST_LIBS = -lcunit
SRCDIR = src
TESTDIR = test

BUILDDIR = build

OBJDIR   = $(BUILDDIR)/obj
BINDIR   = $(BUILDDIR)/bin

BTESTDIR = $(BUILDDIR)/test

SRCS = $(wildcard $(SRCDIR)/*.c)
HDRS = $(wildcard $(SRCDIR)/*.h)
OBJS = $(addsuffix .o,$(basename $(subst $(SRCDIR),$(OBJDIR),$(SRCS))))
BINS = $(addprefix $(BINDIR)/,$(MAINS))

OBJS_WITH_MAIN = $(addsuffix .o,$(addprefix $(OBJDIR)/,$(MAINS)))
OBJS_NO_MAIN = $(filter-out $(OBJS_WITH_MAIN),$(OBJS))

all: $(BINS)

$(BINS): $(OBJS) | $(BINDIR)
	$(eval OBJECT = $(addsuffix .o,$(basename $(subst $(BINDIR),$(OBJDIR),$@))))
	$(CC) $(LDFLAGS) $(OBJECT) $(OBJS_NO_MAIN) $(LIBS) -o $@

$(OBJS): $(OBJDIR)/%.o: $(SRCDIR)/%.c $(HDRS) | $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

# This must be reorganized, made general, and automated.
.PHONY: test
test: $(OBJS) | $(BTESTDIR)
	$(CC) -c -o build/test/board_cu.o test/board_cu.c -I./src
	$(CC) build/test/board_cu.o build/obj/board.o build/obj/bit_works.o -o build/test/board_cu $(TEST_LIBS)
	$(CC) $(CFLAGS) -c -o build/test/game_position_db_test.o test/game_position_db_test.c -I./src
	$(CC) build/test/game_position_db_test.o build/obj/game_position_db.o build/obj/rb.o build/obj/board.o build/obj/bit_works.o -o build/test/game_position_db_test $(LIBS)

.PHONY: $(BUILDDIR)
$(BUILDDIR):
	mkdir -p $(BUILDDIR)

.PHONY: $(OBJDIR)
$(OBJDIR): $(BUILDDIR)
	mkdir -p $(OBJDIR)

.PHONY: $(BINDIR)
$(BINDIR): $(BUILDDIR)
	mkdir -p $(BINDIR)

.PHONY: $(DOCDIR)
$(DOCDIR): $(BUILDDIR)
	mkdir -p $(DOCDIR)

.PHONY: $(BTESTDIR)
$(BTESTDIR): $(BUILDDIR)
	mkdir -p $(BTESTDIR)

.PHONY: doxygen
doxygen: $(DOCDIR)
	doxygen >build/doxygen.log
	(cd build/doc/latex; make >make-stdout_and_stderr.log 2>&1)

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

#
# __init__.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2022 Roberto Corradini. All rights reserved.
#
# License
# 
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
# or visit the site <http://www.gnu.org/licenses/>.
#

import ctypes as ct

import sys

#print('Running __init__.py in reversi package: start')

# pythonpath
#print('sys.path =', sys.path)

# The python shell has to be run from the $REVERSI_HOME/c directory.
libreversi = ct.cdll.LoadLibrary('./build/lib/reversi.so')
#print('Loaded C dynamic link library: ', libreversi)

# Initializing the Board module.
f = libreversi.board_module_init
f()
#print('Initialized the Board module.')

#print('Running __init__.py in reversi package: finish')

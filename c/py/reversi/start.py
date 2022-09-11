#!/usr/bin/env python

#
# Run this script from the REPL issuing this command:
#
# >>> exec(open("py/reversi/start.py").read())
#

import sys
sys.path.append('./py')

from importlib import reload

import reversi
import reversi.board
import reversi.pattern

reload(reversi)
reload(reversi.board)
reload(reversi.pattern)

from reversi.board import *
from reversi.pattern import *

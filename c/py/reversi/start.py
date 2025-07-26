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
import reversi.cfg
import reversi.regab
import reversi.rglm
import reversi.optimization
import reversi.opt_lbfgs

reload(reversi)
reload(reversi.board)
reload(reversi.pattern)
reload(reversi.cfg)
reload(reversi.regab)
reload(reversi.rglm)
reload(reversi.optimization)
reload(reversi.opt_lbfgs)

from reversi.board import *
from reversi.pattern import *
from reversi.cfg import *
from reversi.regab import *
from reversi.rglm import *
from reversi.optimization import *
from reversi.opt_lbfgs import *

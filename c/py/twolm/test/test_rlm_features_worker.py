#
# test_rlm_features_worker.py
#
# This file is part of the reversi program
# http://github.com/rcrr/reversi
# 
# Aauthor Roberto Corradini mailto:rob_corradini@yahoo.it
# Copyright 2026 Roberto Corradini. All rights reserved.
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

#
#
# How to use the unit tests rlmodel module.
#
# Change directory into $(REVERSI_HOME)/c
#
# -0- Activate the environmeant.
#
# $ source py/.reversi_venv/bin/activate
#
# -1- Run the tests.
#
# PYTHONPATH="./py" python3 -m unittest twolm.test.test_rlm_features_worker
#

import unittest
from unittest.mock import patch

from io import StringIO

import numpy as np
import numpy.typing as npt
import numpy.testing as nptest

import os
import tempfile
import shutil
import csv
import tempfile
import time

from pathlib import Path

from twolm.rlmwf import *
from twolm.feature import *

from twolm.board import legal_moves

class TestRLMFeaturesWorker(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_00.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=ReversiLogisticModel.Verbosity.LOW,
                                        base_dir_override=self.tmp_dir)
        self.assertEqual(self.rlm.current_level.value, 0)
        self.assertEqual(self.rlm.current_level.name, 'CREATED')
        self.rlm.move_to_level('PATTERNS')
        self.assertEqual(self.rlm.current_level.value, 3)
        self.assertEqual(self.rlm.current_level.name, 'PATTERNS')

    def tearDown(self):
        if False:
            os.system(f"ls -l {self.tmp_dir}")
        shutil.rmtree(self.tmp_dir)

    def test_build_pset(self):
        rlm = self.rlm
        
        self.assertIsNone(rlm.fset)

        rlm.verbosity = ReversiLogisticModel.Verbosity.LOW
        rlm.move_to_level('FEATURES')
        self.assertEqual(rlm.current_level.value, 4)
        self.assertEqual(rlm.current_level.name, 'FEATURES')            

        if False:
            print(f"self.tmp_dir = {self.tmp_dir}")
            os.system(f"ls -l {self.tmp_dir}")
        

class TestRLMFeaturesWorkerLMS(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_legal_moves_c1(self):

        mover = np.uint64(0x0000000000000001)
        opponent = np.uint64(0x0000000000000002)
        
        lms = legal_moves(mover, opponent)
        expected_lms = np.uint64(0x0000000000000004)
        self.assertEqual(lms, expected_lms)

    def test_legal_moves_h8(self):

        mover = np.uint64(0x0000000000000001)
        opponent = np.uint64(0x0040201008040200)
        
        lms = legal_moves(mover, opponent)
        expected_lms = np.uint64(0x8000000000000000)
        self.assertEqual(lms, expected_lms)

    def test_vectorized_legal_moves(self):

        mover    = np.array([0x0000000000000001,
                             0x0000000000000001], dtype=np.uint64)
        opponent = np.array([0x0000000000000002,
                             0x0040201008040200], dtype=np.uint64)
        
        lms = legal_moves(mover, opponent)
        
        expected_lms = np.array([0x0000000000000004,
                                 0x8000000000000000], dtype=np.uint64)
        
        nptest.assert_array_equal(lms, expected_lms)

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_performance_10m(self):
        size = 10_000_000
        movers = np.full(size, 0x0000000000000001, dtype=np.uint64)
        opponents = np.full(size, 0x0040201008040200, dtype=np.uint64)
        
        # Warmup
        legal_moves(movers[:10], opponents[:10])
        
        start = time.perf_counter()
        lms = legal_moves(movers, opponents)
        end = time.perf_counter()
        
        duration = end - start
        print(f"\n[PERF] Processed {size:,} boards in {duration:.4f}s ({(size/duration):,.0f} boards/sec)")
        
        expected_lms = np.full(size, 0x8000000000000000, dtype=np.uint64)
        nptest.assert_array_equal(lms, expected_lms)
        

class TestRLMFeaturesMobilities(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_mobilities(self):

        size = 1
        mask = np.uint64(0x0000000000000001)
        lms = np.full(size, mask, dtype=np.uint64)
        ms = mobilities(lms)

class TestRLMFeaturesMobilities1M(unittest.TestCase):

    def setUp(self):
        self.tmp_dir = tempfile.mkdtemp(dir='./build/tmp')
        self.json_config = 'py/twolm/test/data/rlm_02.json'
        self.rlm = ReversiLogisticModel(self.json_config,
                                        verbosity=ReversiLogisticModel.Verbosity.LOW,
                                        base_dir_override=self.tmp_dir)
        self.assertEqual(self.rlm.current_level.value, 0)
        self.assertEqual(self.rlm.current_level.name, 'CREATED')
        self.rlm.move_to_level('CONFIG')
        self.assertEqual(self.rlm.current_level.value, 1)
        self.assertEqual(self.rlm.current_level.name, 'CONFIG')
        self.rlm.move_to_level('POSITIONS')
        self.assertEqual(self.rlm.current_level.value, 2)
        self.assertEqual(self.rlm.current_level.name, 'POSITIONS')

    def tearDown(self):
        if True:
            os.system(f"ls -l {self.tmp_dir}")
        shutil.rmtree(self.tmp_dir)

    @unittest.skipUnless(os.environ.get('PERF') == '1', "Skipping performance test (set PERF=1 to run)")
    def test_mobilities(self):
        positions = self.rlm.rds.positions
        movers = positions['mover'].to_numpy().view(np.uint64)
        opponents = positions['opponent'].to_numpy().view(np.uint64)
        gv = positions['game_value'].to_numpy()

        lms = legal_moves(movers, opponents)
        print(f"len(lms) = {len(lms)}")
        
        ms = mobilities(lms)

        ms['game_value'] = positions['game_value'].values
        # Calcolo delle statistiche globali per game_value
        global_stats = ms['game_value'].agg(['min', 'max', 'mean', 'std', 'var'])

        print("--- STATISTICHE GLOBALI GAME_VALUE ---")
        print(global_stats)
        print(f"{ms}")

        import pandas as pd

        # Supponiamo che il tuo dataframe si chiami 'ms'
        # Definiamo le colonne della mobilità (tutte tranne game_value)
        mobility_columns = [col for col in ms.columns if col != 'game_value']

        print("--- ANALISI GAME_VALUE PER LIVELLI DI MOBILITÀ ---")

        for col in mobility_columns:
            # Raggruppiamo per il valore della maschera (0, 1, 2...) 
            # e calcoliamo le statistiche sul game_value
            stats = ms.groupby(col)['game_value'].agg(['min', 'max', 'mean', 'std', 'var', 'count'])
    
            # Rinominiamo per chiarezza
            stats.columns = ['MIN_GV', 'MAX_GV', 'AVG_GV', 'STD_GV', 'VAR_GV', 'SAMPLES']
    
            print(f"\nAnalisi per: {col}")
            # Formattiamo per una stampa leggibile
            print(stats.to_string(formatters={
                'AVG_GV': '{:,.2f}'.format,
                'STD_GV': '{:,.2f}'.format,
                'VAR_GV': '{:,.2f}'.format
            }))
            print("-" * 50)

        ############ Anti-Mobility ############
        print(f"############ Anti-Mobility ############")

        lms = legal_moves(opponents, movers)        
        ms = mobilities(lms)
        ms['game_value'] = positions['game_value'].values
        # Calcolo delle statistiche globali per game_value
        global_stats = ms['game_value'].agg(['min', 'max', 'mean', 'std', 'var'])

        print("--- STATISTICHE GLOBALI GAME_VALUE ---")
        print(global_stats)
        print(f"{ms}")

        # Supponiamo che il tuo dataframe si chiami 'ms'
        # Definiamo le colonne della mobilità (tutte tranne game_value)
        mobility_columns = [col for col in ms.columns if col != 'game_value']

        print("--- ANALISI GAME_VALUE PER LIVELLI DI MOBILITÀ ---")

        for col in mobility_columns:
            # Raggruppiamo per il valore della maschera (0, 1, 2...) 
            # e calcoliamo le statistiche sul game_value
            stats = ms.groupby(col)['game_value'].agg(['min', 'max', 'mean', 'std', 'var', 'count'])
    
            # Rinominiamo per chiarezza
            stats.columns = ['MIN_GV', 'MAX_GV', 'AVG_GV', 'STD_GV', 'VAR_GV', 'SAMPLES']
    
            print(f"\nAnalisi per: {col}")
            # Formattiamo per una stampa leggibile
            print(stats.to_string(formatters={
                'AVG_GV': '{:,.2f}'.format,
                'STD_GV': '{:,.2f}'.format,
                'VAR_GV': '{:,.2f}'.format
            }))
            print("-" * 50)

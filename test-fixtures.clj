;;;
;;; Copyright (c) 2009-2010 Roberto Corradini

;;; This file is part of the reversi program
;;; http://github.com/rcrr/reversi

;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3, or (at your option) any
;;; later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
;;; or visit the site <http://www.gnu.org/licenses/>.


(ns reversi
  (:load "reversi/constants"
	 "reversi/auxfns"
	 "reversi/GameOverException"
	 "reversi/reversi")
  (:require [clojure.contrib [fcase :as fcase]])
  (:use clojure.test)
  (:import (reversi GameOverException)))

;;; Cross dependencies
(declare opponent)

;;; Test functions
(declare test-weighted-squares)

;;; Unit test fixtures
(declare *fixt-ib* *fixt-eb* *fixt-board-a* *fixt-board-b*
	 *fixt-board-black-has-to-pass*
	 *fixt-game-black-has-to-pass*
	 *fixt-board-end-game-x*
	 *fixt-game-x*
	 *fixt-board-34* *fixt-board-43*
	 *fixt-board-56* *fixt-board-65*
	 *fixt-game-y*
	 *fixt-board-c*
	 *fixt-weights-1*)

(defn
  #^{:doc "Prepare a fiew board used by tests."}
  basic-test-fixture [f]
  (binding [
	    ;;; Test functions
	    test-weighted-squares
	    (fn [player board weights]
	      (let [opp (opponent player)]
		(reduce + (map * weights
			       (for [sq board]
				 (fcase/case sq
					     player 1
					     opp -1
					     outer 0
					     empty-square 0))))))
	    ;;; Fixtures
	    *fixt-ib* [3 3 3 3 3 3 3 3 3 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 2 1 0 0 0 3
		       3 0 0 0 1 2 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 3 3 3 3 3 3 3 3 3]
	    *fixt-eb* [3 3 3 3 3 3 3 3 3 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 0 0 0 0 0 0 0 0 3
		       3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-a* [3 3 3 3 3 3 3 3 3 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 1 0 0 0 0 3
			    3 0 0 0 1 1 0 0 0 3
			    3 0 0 0 1 2 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-b* [3 3 3 3 3 3 3 3 3 3
			    3 0 0 0 1 1 1 0 0 3
			    3 0 0 0 0 1 0 0 0 3
			    3 0 0 0 1 1 2 2 0 3
			    3 0 0 0 1 1 0 0 0 3
			    3 0 0 0 1 1 0 0 0 3
			    3 0 0 0 0 1 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-black-has-to-pass* [3 3 3 3 3 3 3 3 3 3
					    3 2 1 0 1 0 2 0 0 3
					    3 1 1 1 1 1 1 1 2 3
					    3 0 1 2 2 1 1 2 2 3
					    3 0 1 2 1 2 2 2 2 3
					    3 0 1 2 1 2 2 2 2 3
					    3 0 1 2 1 1 2 1 2 3
					    3 0 1 2 1 1 1 1 0 3
					    3 2 2 2 2 2 2 1 2 3
					    3 3 3 3 3 3 3 3 3 3]
	    *fixt-game-black-has-to-pass*
	    '(e6 f4 f3 f6 g4 h4 g6 e7 h5 h6 d3 c6 d6 c5 b6 b5 b7 
		 e2 c4 a8 g7 c3 d8 e3 b2 b3 g3 a1 b4 g5 f7 h3 f2 
		 f1 a2 h8 b1 f8 d2 e8 b8 c8 g8 c7 g2 f5 d7 c2 d1 
		 h2 h1 g1 a3 a4 e1 c1 a5 a6 a7 h7)
	    *fixt-board-end-game-x* [3 3 3 3 3 3 3 3 3 3
				     3 2 2 2 2 2 1 1 1 3
				     3 2 2 2 1 1 1 1 1 3
				     3 2 2 2 1 1 1 2 1 3
				     3 2 2 1 2 1 1 2 1 3
				     3 1 1 2 1 2 1 2 1 3
				     3 1 2 1 2 1 2 1 1 3
				     3 1 1 1 1 1 1 2 1 3
				     3 1 1 1 1 2 2 2 2 3
				     3 3 3 3 3 3 3 3 3 3]
	    *fixt-game-x*
	    '(c4 e3 f5 b4 e2 f4 c3 d2 c2 c1 b3 c6 b2 g5 f2 e1 f6 
		 d6 d3 f7 g6 f3 f8 g7 a5 g3 h6 a2 c5 e8 e7 b6 h3 
		 c7 b7 e6 a3 g4 g1 h2 d1 g8 h4 a7 b1 g2 d7 h5 h7 
		 b5 h1 h8 a8 a1 d8 a4 a6 b8 c8 f1)
	    *fixt-board-34* [3 3 3 3 3 3 3 3 3 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 1 0 0 0 0 3
			     3 0 0 0 1 1 0 0 0 3
			     3 0 0 0 1 2 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-43* [3 3 3 3 3 3 3 3 3 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 1 1 1 0 0 0 3
			     3 0 0 0 1 2 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-56* [3 3 3 3 3 3 3 3 3 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 2 1 0 0 0 3
			     3 0 0 0 1 1 1 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-65* [3 3 3 3 3 3 3 3 3 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 2 1 0 0 0 3
			     3 0 0 0 1 1 0 0 0 3
			     3 0 0 0 0 1 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 0 0 0 0 0 0 0 0 3
			     3 3 3 3 3 3 3 3 3 3]
	    *fixt-game-y*
	    '(d3 c3 b3 b2 c4 e3 f3 e2 d1 a3 a1 b4 b5 a4 a5 c2 a2 
		 c5 e6 f5 c1 e1 f1 b1 f4 g1 h1 f2 d2 d7 g2 g3 h3 
		 h2 g5 d6 g4 h5 g6 h4 h6 e7 f7 f8 f6 g7 e8 h7 c6)
	    *fixt-board-c* [3 3 3 3 3 3 3 3 3 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 2 1 1 1 0 0 2 0 3
			    3 0 2 0 2 1 2 0 0 3
			    3 0 2 2 1 2 0 0 0 3
			    3 0 0 1 1 0 2 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 0 0 0 0 0 0 0 0 3
			    3 3 3 3 3 3 3 3 3 3]
	    *fixt-weights-1* [0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 1 1 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0
			      0 0 0 0 0 0 0 0 0 0]
	    ]
    (f)))

(use-fixtures :each basic-test-fixture)

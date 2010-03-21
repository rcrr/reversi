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
	 "reversi/test-fixtures"
	 "reversi/strategies")
  (:use clojure.test)
  (:import (reversi GameOverException)))

(defstruct game-state :player :clock :board)
(defstruct game-node :game-state :previous-player :value)
(defstruct game-branch :game-node :level :branches)

;;; This function finds its hown reason to exist because
;;; testing minimax further is really complex without it.
;;;
;;; (def b *fixt-board-c*)
;;; (def s (struct game-state black nil b))
;;; (def n (struct game-node s white nil))
;;; (game-tree n 0)
;;; (game-tree n 1)
;;;
;;; value calculation is missing.
;;; pretty printing is missing.
;;; end of game is not handled.
;;; move passing is not handled.
;;; value roll-up is not considered.
;;; surely a unit test will be mandatory also for game-tree itself 
(defn game-tree
  [node level]
  (let [state (:game-state node)
	p (:player state)
	c (:clock state)
	b (:board state)
	pp (:previous-player node)
	lm (legal-moves p b)
	o (opponent p)
	value (:value node)]
    (when (>= level 0)
      (struct game-branch node level
	      (for [move lm]
		(game-tree (struct game-node (struct game-state o c b) p value) (- level 1)))))))

(deftest test-minimax
  ;;; The following tests are not really checked.
  (is (= (minimax white *fixt-board-b* 1 count-difference) [-4 33] ))
  (is (= (minimax white *fixt-board-b* 2 count-difference) [-7 33] ))
  (is (= (minimax white *fixt-board-b* 3 count-difference) [-2 33] ))
  (is (= (minimax white *fixt-board-b* 4 count-difference) [-9 33] ))
  (is (= (minimax white *fixt-board-b* 5 count-difference) [-4 33] ))
  (is (= (minimax white *fixt-board-b* 6 count-difference) [-11 33] ))
  ;;; The following tests have been fully checked.
  ;;; The minimax values have been verified using a real reversi board,
  ;;; a calculator and an OpenOffice calc sheet 
  (let [b *fixt-board-c*
	b-moves (legal-moves black b)]
    (is (= (count b-moves) 9))
    (is (= (minimax black b 0 count-difference) [-2 nil] ))
    (is (= (for [move (legal-moves black b)]
	     [(count-difference black (make-move move black b)) move])
	   '([5 28] [1 41] [3 43] [1 47] [5 51] [1 56] [3 62] [1 65] [5 77])))
    (is (= (minimax black b 1 count-difference) [5 28]))
    (is (= (for [move (legal-moves black b)]
	     [(weighted-squares black (make-move move black b)) move])
	   '([-13 28] [0 41] [20 43] [6 47] [-4 51] [14 56] [-20 62] [14 65] [7 77])))
    (is (= (minimax black b 1 weighted-squares) [20 43]))))
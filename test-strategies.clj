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

;;; game-state fully identify a gama state.
;;;  :player is the player that has to move
;;;  :clock is the game clock
;;;  :board is the game board 
(defstruct game-state :player :clock :board)

;;; game-node add the board value to the game-state.
;;;  :game-state is a game-state struct
;;;  :value is the board evaluation
(defstruct game-node :game-state :value)

;;; game-branch adds the level and the list of branches to the node definition
;;;  :move the move that originated the branch
;;;  :game-node is a game-node struct
;;;  :level is the branch level (0 is a leaf node)
;;;  :branches is the list of further game-branche structures
(defstruct game-branch :move :game-node :level :branches)


;;; A few auxiliaries functions ....
(defn game-over?
  [state]
  (if (and (nil? (any-legal-move? (:player state) (:board state)))
	   (nil? (any-legal-move? (opponent (:player state)) (:board state))))
    true
    false))

(defn game-tree-walker
  [move node level e-fn]
  (let [state (:game-state node)
	p (:player state)
	c (:clock state)
	b (:board state)
	lm (legal-moves p b)
	o (opponent p)
	value (:value node)]
    (when (>= level 0)
      (struct game-branch move node level
	      (let [make-move (fn [m p b] (if (nil? m) b (make-move m p b)))
		    lm (if (empty? lm) '(nil) lm)]
		(if (game-over? state)
		  'game-over
		  (for [move lm]
		    (let [b (make-move move p b)
			  state (struct game-state o c b)
			  value (e-fn state)
			  node (struct game-node state value)]
		      (game-tree-walker move node (- level 1) e-fn)))))))))

;;; This function finds its hown reason to exist because
;;; testing minimax further is really complex without it.
;;;
;;; (def b *fixt-board-c*)
;;; (def s (struct game-state black nil b))
;;; (game-tree n 0 nil)
;;; (game-tree n 0 count-difference)
;;; (game-tree n 1 count-difference)
;;;
;;; pretty printing is missing.
;;; end of game is not handled, but not the final value.
;;; value roll-up is not considered.
;;; surely a unit test will be mandatory also for game-tree itself 
(defn game-tree
  [state level eval-fn]
  (let [e-fn (if (nil? eval-fn)
	       (fn [_] nil)
	       (fn [state] (eval-fn (:player state) (:board state))))
	node (struct game-node state (e-fn state))]
    (game-tree-walker nil node level e-fn)))

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
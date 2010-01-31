;;;
;;; Copyright (c) 2009-2010 Roberto Corradini

;;; This file is part of the reversi program
;;; http://github.com/rcrr/reversi

;;; As stated in the original license, the modified version, this file, is
;;; distributed under the GNU GPL v3 license.

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


(ns reversi
  (:load "reversi/constants"
	 "reversi/auxfns"
	 "reversi/reversi")
  (:require [clojure.contrib [pprint :as pprint]])
  (:require [clojure.contrib [seq-utils :as seq-utils]])
  (:require [clojure.contrib [fcase :as fcase]]))

(defn
  #^{:doc "A strategy that maximize the differences in pieces."}
  maximize-difference [player board]
  ((maximizer count-difference) player board))

(defn
  #^{:doc "Given a sequence composed by numbers, return the index
   of the maximum value."}
  index-of-max [x]
  (if (empty? x)
      -1
      (loop [coll x
	     index 0
	     max (first x)
	     max-index 0]
	(if (empty? coll)
	  max-index
	  (let [val (first coll)
		is-max (if (> val max) true false)]
	    (recur (rest coll)
		   (inc index)
		   (if is-max val max)
		   (if is-max index max-index)))))))

(defn
  #^{:doc "Return a strategy that will consider every legal move,
   apply ENVAL-FN to each resulting board, and choose
   the move for which EVAL-FN returns the best score.
   FN takes two arguments: the player-to-move and board."}
  maximizer [eval-fn]
  (fn [player board]
    (let [moves (legal-moves player board)
	  scores (map (fn [move]
			(eval-fn
			 player
			 (make-move move player board)))
		      moves)
	  best (index-of-max scores)]
      (when *print* (println "moves, scores, best: " moves scores best))
      (nth moves best))))


(def *weights*
     [0   0   0   0  0  0   0   0   0 0
      0 120 -20  20  5  5  20 -20 120 0
      0 -20 -40  -5 -5 -5  -5 -40 -20 0
      0  20  -5  15  3  3  15  -5  20 0
      0   5  -5   3  3  3   3  -5   5 0
      0   5  -5   3  3  3   3  -5   5 0
      0  20  -5  15  3  3  15  -5  20 0
      0 -20 -40  -5 -5 -5  -5 -40 -20 0
      0 120 -20  20  5  5  20 -20 120 0
      0   0   0   0  0  0   0   0   0 0])

(defn
  #^{:doc "Sum of the weights of player's squares minus opponents's."}
  weighted-squares [player board]
  (let [opp (opponent player)]
    (reduce +
	    (map * *weights*
		 (for [sq board]
		   (fcase/case sq
			       player 1
			       opp -1
			       outer 0
			       empty-square 0))))))

;;; Running a few test:
;;; (reversi random-strategy random-strategy true)
;;; (reversi maximize-difference random-strategy true)
;;; (reversi random-strategy maximize-difference true)
;;; (reversi random-strategy (maximizer weighted-squares) true)
;;; (reversi (maximizer weighted-squares) random-strategy true)

;;; A few statistics:
;;;
;;; random-strategy (black) vs random-srategy (white)
;;; (def rand-rand (take 1000 (repeatedly (fn [] (reversi random-strategy random-strategy false)))))
;;; rand-rand
;;; (count (filter (fn [x] (= x 0)) rand-rand))
;;; (count (filter (fn [x] (> x 0)) rand-rand))
;;; (count (filter (fn [x] (< x 0)) rand-rand))
;;; around 5% are draws, 50% are black wins, 45% are white ones.
;;;
;;; random-strategy (black) vs maximize-difference (white)
;;; (def rand-maxdiff (take 1000 (repeatedly (fn [] (reversi random-strategy maximize-difference false)))))
;;; rand-maxdiff
;;; around 3% are draws, 60% are black wins, 37% are white ones.
;;;
;;; maximize-difference (black) vs random-strategy (white)
;;; (def maxdiff-rand (take 1000 (repeatedly (fn [] (reversi maximize-difference random-strategy false)))))
;;; maxdiff-rand
;;; around 3% are draws, 64% are black wins, 33% are white ones.
;;;
;;; maximize-difference (black) vs maximize-difference (white)
;;; being two "deterministic" strategies the outcam in always the same: -26, the white wins.
;;;
;;; (maximizer weighted-squares) (black) vs random-strategy (white)
;;; (def maxwei-rand (take 1000 (repeatedly (fn [] (reversi (maximizer weighted-squares) random-strategy false)))))
;;; maxwei-rand
;;; around 3% are draws, 79% are black wins, 18% are white ones.
;;;
;;; random-strategy (black) vs (maximizer weighted-squares) (white)
;;; (def rand-maxwei (take 1000 (repeatedly (fn [] (reversi random-strategy (maximizer weighted-squares) false)))))
;;; rand-maxwei
;;; around 3% are draws, 14% are black wins, 83% are white ones.


(defn
  #^{:doc "Is tihs a win, loss, or a draw for player?"}
  final-value [player board]
  (fcase/case (Integer/signum (count-difference player board))
	      -1 losing-value
	      0 0
	      +1 winning-value))

(defn
  #^{:doc "Find the best move, for PLAYER, according to EVAL-FN,
   searching PLY levels deep and backing up values.
   The function return a vector of two values:
   the best move value, the best move"}
  minimax [player board ply eval-fn]
  [100 11])

(defn
  #^{:doc "A strategy that searches PLY levels and then uses EVAL-FN."}
  minimax-searcher [ply eval-fn]
  (fn [player board]
    (let [[value move] (minimax player board ply eval-fn)]
      move)))

;;; minimax lisp function body to be translated into the clojure version
(if (= ply 0)
    (funcall eval-fn player board)
    (let ((moves (legal-moves player board)))
      (if (null moves)
	(if (any-legal-move? (opponent player) board)
	  (- (minimax (opponent player) board
		      (- ply 1) eval-fn))
	  (final-value player board))
	(let ((best-move nil)
	      (best-val nil))
	  (dolist (move moves)
		  (let* ((board2 (make-move move player
					    (copy-board board)))
			 (val (- (minimax
				  (opponent player) board2
				  (- ply 1) eval-fn))))
			(when (or (null best-val)
				  (> val best-val))
			  (setf best-val val)
			  (setf best-move move))))
	  (values best-val best-move)))))

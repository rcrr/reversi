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
  (:require [clojure.contrib [seq-utils :as seq-utils]]))

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
      (println "moves, scores, best: " moves scores best)
      (nth moves best))))


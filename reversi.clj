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
	 "reversi/auxfns")
  (:require [clojure.contrib [pprint :as pprint]]))

(defn
  #^{:doc "Return a specific character foreach valid piece value."}
  name-of [piece]
  (cond (= piece 0) \.
	(= piece 1) \@
	(= piece 2) \O
	(= piece 3) \?
	true \E))

(defn
  #^{:doc "Return the player opponent."}
  opponent [player]
  (if (= player black) white black))

;;; The game board is implemented as a Vector (clojure.lang.PersistentVector).
;;; The 100 squares are plain integer.
;;; A board structure does not appeare as required.

(defn
  #^{:doc "Query a board for a given square."}
  board-ref [board square]
  (get board square))

(defn
  #^{:doc "Set the value of a given board square pairs."}
  board-set [board square val]
  (assoc board square val))

(defn create-board []
  (vec (repeat 100 empty-square)))

(defn copy-board [board]
  (vec board))

(defn
  #^{:doc "Return a board, empty except for four pieced in the middle."}
  initial-board []
  ;; Boards are 100-element vectors (clojure.lang.PersistentVector),
  ;; with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER. Initially
  ;; the 4 center squares are taken, the others empty.
  ;; A square is represented by a plain integer (java.lang.Integer).
  (let [b (transient (vec (repeat 100 outer)))]
    (doseq [i all-squares]
      (assoc! b i empty-square))
    (assoc! b 44 white 45 black 54 black 55 white)
    (persistent! b)))

(defn
  #^{:doc "Count the player pieces."}
  count-pieces [board player]
  (count (filter (fn [piece] (= piece player)) board)))

(defn
  #^{:doc "Count player's pieces minus opponent's pieces."}
  count-difference [player board]
  (- (count-pieces board player)
     (count-pieces board (opponent player))))

;; to complete it I have to add the optional clock,
;; the optionality of board and the default board
(defn
  #^{:doc "Print a board, along with some statistics."}
  print-board [board]
  ;; First print the header and the current score
  (pprint/cl-format true "~2&    a b c d e f g h   [~c=~2a ~c=~2a (~@d)]"
		    (name-of black) (count-pieces board black)
		    (name-of white) (count-pieces board white)
		    (count-difference black board))
  ;; Print the board itself
  (doseq [row (range 1 9)]
    (pprint/cl-format true "~2&~& ~d " row)
    (doseq [col (range 1 9)]
      (let [piece (board-ref board (+ col (* 10 row)))]
	(pprint/cl-format true " ~c" (name-of piece)))))
  ;; Finally print the time remaining for each player
  (pprint/cl-format true "~2&~2&"))

(defn
  #^{:doc "Valid moves are a number in the range 11-88 that end in 1-8"}
  valid? [move]
  (and (integer? move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defn
 #^{:doc "A legal move must be into an empty square, and it must
   flip at least one opponent piece."}
 legal? [move player board]
 (and (= (board-ref board move) empty)
      (some (fn [dir] (would-flip? move player board dir))
	    all-directions)))

(defn
 #^{:doc "Would this move result in any flips in this direction?
   If so, return the square number of the bracketing piece."}
 would-flip? [move player board dir]
  ;; A flip occours if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, braketed by
  ;; one of player's pieces.
  (let [c (+ move dir)]
    (and (= (board-ref board c) (opponent player))
	 (find-bracketing-piece (+ c dir) player board dir))))

(defn
  #^{:doc "Return the square number of the bracketing piece."}
  find-bracketing-piece [square player board dir]
  (cond (= (board-ref board square) player) square
	(= (board-ref board square) (opponent player))
	(find-bracketing-piece (+ square dir) player board dir)
	true nil))

(defn
  #^{:doc "Return a new board to reflect move by player."}
  make-move [move player board]
  (let [b (transient (copy-board board))
	m move
	p player]
    ;; First make the move, then make any flips.
    (assoc! b m p)
    ;; Make any flips in the given direction.
    (letfn [(make-flips [dir]
			(let [bracketer (would-flip? m p b dir)]
			  (when bracketer
			    (loop [c (+ m dir)]
			      ;;(println "flipping:" c)
			      (assoc! b c p)
			      (if (not (= c (- bracketer dir)))
				(recur (+ c dir)))))))] 
      (doseq [dir all-directions]
	(make-flips dir)))
    (persistent! b)))

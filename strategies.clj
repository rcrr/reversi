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
  (:use clojure.test)
  (:require [clojure.contrib [pprint :as pprint]])
  (:require [clojure.contrib [seq-utils :as seq-utils]])
  (:require [clojure.contrib [fcase :as fcase]])
  (:require [clojure.contrib [math :as math]])
  (:require [clojure.contrib.generic [math-functions :as math-f]])
  (:import (reversi GameOverException)))

(defn
  #^{:doc "Given a sequence composed by numbers, return the index
   of the maximum value."
     :test (fn []
	     (is (= 0 (index-of-max [7])) "Has to return 0.")
	     (is (= 0 (index-of-max [7 1])) "Has to return 0.")
	     (is (= 1 (index-of-max [1 7])) "Has to return 1.")
	     (is (= 1 (index-of-max [-2 7 5])) "Has to return 1.")
	     (is (= 1 (index-of-max '(-2 7 5))) "Has to return 1.")
	     (is (= 3 (index-of-max [-2 1 7 8 -12])) "Has to return 3.")
	     (is (= -1 (index-of-max [])) "Has to return -1.")
	     (is (thrown? ClassCastException (index-of-max ['x])) "Has to throws an exception."))}
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
   FN takes two arguments: the player-to-move and board."
     :test (fn []
	     (let [weights *fixt-weights-1*
		   board *fixt-board-c*
		   tws (fn [p b] (test-weighted-squares p b weights))
		   max-tws (fn [p b] ((maximizer tws) p b))
		   value (tws black board)
		   move (max-tws black board)
		   next-value (tws black (make-move move black board))]
	       (is (= value -1))
	       (is (= move 47))
	       (is (= next-value 2))))}
  maximizer [eval-fn]
  (fn [player board]
    (let [moves (legal-moves player board)
	  scores (map (fn [move]
			(eval-fn
			 player
			 (make-move move player board)))
		      moves)
	  best (index-of-max scores)]
      (nth moves best))))

(defn
  #^{:doc "A strategy that maximize the differences in pieces."}
  maximize-difference [player board]
  ((maximizer count-difference) player board))

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
  #^{:doc "Sum of the weights of player's squares minus opponents's."
     :test (fn []
	     (is (= (weighted-squares black *fixt-board-c*) 5))
	     (letfn
		 [(test-f 
		   [move]
		   (weighted-squares black
				     (make-move move black
						*fixt-board-c*)))]
	       (are [x y] (= x y)
		    (test-f 28) -13
		    (test-f 41)   0
		    (test-f 43)  20
		    (test-f 47)   6
		    (test-f 51)  -4
		    (test-f 56)  14
		    (test-f 62) -20
		    (test-f 65)  14
		    (test-f 77)   7)))}
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
  #^{:doc "Is tihs a win, loss, or a draw for player?
   The function doesn't check that the board doesn't have further moves."
     :test (fn []
	     (is (= (final-value black *fixt-board-end-game-x*) winning-value))
	     (is (= (final-value white *fixt-board-end-game-x*) losing-value))
	     (is (= (final-value black *fixt-ib*) 0))
	     (is (= (final-value white *fixt-ib*) 0)))}
  final-value [player board]
  (let [cd (count-difference player board)]
    (fcase/case (Integer/signum cd)
	      -1 (+ losing-value 0)
	      0 0
	      +1 (+ winning-value 0))))

(defn
  #^{:doc "Being a given BOARD,
   find the best move, for PLAYER, according to EVAL-FN,
   searching PLY levels deep and backing up values.
   The function return a vector of two values:
   the best move value, the best move"}
  minimax [player board ply eval-fn]
  (if (= ply 0)
    [(eval-fn player board) nil]
    (let [moves (legal-moves player board)]
      (if (empty? moves)
	(if (any-legal-move? (opponent player) board)
	  (let [[v _] (minimax (opponent player) board
		       (- ply 1) eval-fn)]
	    [(- v) nil])
	  [(final-value player board) nil])
	(let [best-move (atom nil)
	      best-val (atom nil)]
	  (doseq [move moves]
	    (let [board2 (make-move move player
				    (copy-board board))
		  [v _] (minimax
			 (opponent player) board2
			 (- ply 1) eval-fn)
		  val (- v)]
	      (when (or (nil? @best-val)
			(> val @best-val))
		(reset! best-val val)
		(reset! best-move move))))
	  [@best-val @best-move])))))

(defn
  #^{:doc "A strategy that searches PLY levels and then uses EVAL-FN."}
  minimax-searcher [ply eval-fn]
  (fn [player board]
    (let [[value move] (minimax player board ply eval-fn)]
      move)))

;;;
;;; from wikipedia: http://en.wikipedia.org/wiki/Alpha-beta_pruning
;;; 
;;; function alphabeta(node, depth, α, β)         
;;;     (* β represents previous player best choice - doesn't want it if α would worsen it *)
;;;     if  depth = 0 "or" node is a terminal node
;;;         return the heuristic value of node
;;;     foreach child of node
;;;         α := max(α, -alphabeta(child, depth-1, -β, -α))     
;;;         (* use symmetry, -β becomes subsequently pruned α *)
;;;         if β≤α
;;;             break                             (* Beta cut-off *)
;;;     return α
;;; 
;;; (* Initial call *)
;;; alphabeta(origin, depth, -infinity, +infinity)
;;; 

(defn
  #^{:doc "Find the best move, for PLAYER, according to EVAL-FN,
   searching PLY levels deep and backing up values,
   using cutoff whenever possible."}
  alpha-beta [player board achievable cutoff ply eval-fn]
  ;; (println "alpha-beta: player=" player ", achievable=" achievable ", cutoff=" cutoff ", ply=" ply ", eval-fn=" eval-fn ", board=" board)
  (if (= ply 0)
    [(eval-fn player board) nil]
    (let [moves (legal-moves player board)]
      ;; (println "alpha-beta: moves=" moves)
      (if (empty? moves)
	(if (any-legal-move? (opponent player) board)
	  (let [[v _] (alpha-beta (opponent player) board
				  (- cutoff) (- achievable)
				  (- ply 1) eval-fn)]
	    [(- v) nil])
	  [(final-value player board) nil])
	(let [best-move (atom (first moves))
	      ac (atom achievable)]
	  (loop [xmoves moves]
	    (when (not (empty? xmoves))
	      (let [move (first xmoves)
		    board2 (make-move move player board)
		    [v _] (alpha-beta
			   (opponent player) board2
			   (- cutoff) (- @ac)
			   (- ply 1) eval-fn)
		    val (- v)]
		(when (> val @ac)
		    (reset! ac val)
		    (reset! best-move move))
		(when (< @ac cutoff)
		  (recur (rest xmoves))))))
	  ;;(println "alpha-beta: ac=" @ac ", best-move=" @best-move ", ply=" ply)
	  [@ac @best-move])))))

(defn
  #^{:doc "A strategy that searches to DEPTH and then uses EVAL-FN."}
  alpha-beta-searcher [depth eval-fn]
  (fn [player board]
    (let [[value move]
	  (alpha-beta player board losing-value winning-value
		      depth eval-fn)]
      move)))

(let [neighbor-table
      (let [nt (transient (vec (repeat 100 nil)))]
	;; Initialize the nieghbor table
	(doseq [square all-squares]
	  (doseq [dir all-directions]
	    (if (valid? (+ square dir))
	      (assoc! nt square
		      (conj (get nt square)
			    (+ square dir))))))
	(persistent! nt))]
  
  (defn
    #^{:doc "Return a list of all squares adjacent to a square."}
    neighbors [square]
    (get neighbor-table square)))


(defn
  #^{:doc "Like WEIGHTED-SQUARES, but don't take off for moving
   near an occupied corner."}
  modified-weighted-squares [player board]
  (let [w (atom (weighted-squares player board))]
    (doseq [corner '(11 18 81 88)]
      (when (not (= (board-ref board corner) empty-square))
	(doseq [c (neighbors corner)]
	  (when (not (= (board-ref board c) empty-square))
	    (reset! w (+ @w (* (- 5 (get *weights* c))
			      (if (= (board-ref board c) player)
				+1 -1))))))))
    @w))

;;;
;;; 18.9 - More Efficient Searching
;;;

(def *all-squares-static-ordered*
     (let [m (apply sorted-map (interleave (range 0 99) *weights*))]
       (sort (fn [x y] (if (> (m x) (m y)) true false)) all-squares)))

(defn
  #^{:doc "Returns an ordered list of legal moves for player."
     :test (fn []
	     (is (= (legal-moves-optimized black *fixt-ib*) '(34 43 56 65))
		 "Black's initial legal moves are d3, c4, f5, e6")
	     (is (= (legal-moves-optimized white *fixt-board-a*) '(33 35 53))
		 "White's valid moves are c3, e3, c5"))}
  legal-moves-optimized [player board]
  (filter (fn [move] (legal? move player board)) *all-squares-static-ordered*))

(defstruct node :square :board :value)

(defn
  #^{:doc "Set the value of a node to its negative."}
  negate-value [n]
  (struct-map node
    :square (n :square) :board (n :board) :value (- (n :value))))

(defn
  #^{:doc "Return a list of legal moves, each one packed into a node."}
  legal-nodes [player board eval-fn]
  (sort
   (fn [x y] (if (> (:value x) (:value y)) true false))
   (let [moves (legal-moves-optimized player board)]
     (for [move moves] (let [new-board (make-move move player board)]
			 (struct-map node
			   :square move
			   :board new-board
			   :value (eval-fn player new-board)))))))

(defn
  #^{:doc "A-B search, sorting moves by eval-fn."}
  alpha-beta2 [player node achievable cutoff ply eval-fn]
  ;; Returns two values: achievable-value and move-to-make
  (if (= ply 0)
    [(:value node) node]
    (let [board (:board node)
	  nodes (legal-nodes player board eval-fn)]
      (if (empty? nodes)
	(if (any-legal-move? (opponent player) board)
	  (let [[v _] (alpha-beta2 (opponent player)
				   (negate-value node)
				   (- cutoff) (- achievable)
				   (- ply 1) eval-fn)]
	    [(- v) nil])
	  [(final-value player board) nil])
	(let [best-node (atom (first nodes))
	      ac (atom achievable)]
	  (loop [xnodes nodes]
	    (when (not (empty? xnodes))
	      (let [move (first xnodes)
		    [v _] (alpha-beta2
			   (opponent player)
			   (negate-value move)
			   (- cutoff) (- @ac)
			   (- ply 1) eval-fn)
		    val (- v)]
		(when (> val @ac)
		  (reset! ac val)
		  (reset! best-node move))
		(when (< @ac cutoff)
		  (recur (rest xnodes))))))
	  [@ac @best-node])))))

(defn
  #^{:doc "Return a strategy that doas A-B search with sorted moves."}
  alpha-beta-searcher2 [depth eval-fn]
  (fn [player board]
    (let [[value node]
	  (alpha-beta2 player
		       (struct-map node
			 :square nil
			 :board board
			 :value (eval-fn player board))
		       losing-value winning-value depth eval-fn)]
      (:square node))))



;;;
;;; A few tests. The alpha-beta function is able (like the minimax one to reproduce exactly the sample match as reported into paragraph 18.6 of the paip book.
;;;
;;; reversi> (reversi (alpha-beta-searcher 4 count-difference) (alpha-beta-searcher 4 weighted-squares) true)
;;; The game is over. Final result:
;;; 
;;;     a b c d e f g h   [@=24 O=40 (-16)]
;;;  1  O O O O @ @ O O
;;;  2  @ @ O O @ @ @ O
;;;  3  @ @ O O O @ @ O
;;;  4  O @ O O O @ @ O
;;;  5  O @ O @ O @ @ O
;;;  6  O @ O @ O @ O O
;;;  7  O @ @ O @ O O O
;;;  8  O O O O O O O O
;;; 
;;; Game moves:  (d3 c5 b6 c3 f5 e3 f2 f3 b2 b3 g2 a1 a3 b4 a5 b5 b1 f6 c1 e2 f1 h1 g3 h3 e6 c6 f4 d1 h2 g4 c4 a6 d7 d6 h4 g1 e7 d2 e1 c2 a7 a4 a2 a8 b7 c8 g5 c7 b8 h6 d8 h5 g6 h7 g7 h8 f7 e8 f8 g8)
;;; -16
;;;
;;; The alpha-beta pruning function is a lot faster ....
;;;
;;; reversi> (time (loop [i 10 result ()](if (= i 0) result (recur (- i 1) (conj result (reversi random-strategy (minimax-searcher 2 weighted-squares) false))))))
;;; "Elapsed time: 1604.021254 msecs"
;;; (-32 -22 -18 -20 -24 -32 -4 -8 -22 -10)
;;; reversi> (time (loop [i 10 result ()](if (= i 0) result (recur (- i 1) (conj result (reversi random-strategy (minimax-searcher 4 weighted-squares) false))))))
;;; "Elapsed time: 148990.264899 msecs"
;;; (-6 -10 -26 -26 20 -26 -12 -12 6 -20)
;;; reversi> (time (loop [i 10 result ()](if (= i 0) result (recur (- i 1) (conj result (reversi random-strategy (alpha-beta-searcher 2 weighted-squares) false))))))
;;; "Elapsed time: 953.401627 msecs"
;;; (-6 -16 0 -6 -32 -10 -10 -8 6 -28)
;;; reversi> (time (loop [i 10 result ()](if (= i 0) result (recur (- i 1) (conj result (reversi random-strategy (alpha-beta-searcher 4 weighted-squares) false))))))
;;; "Elapsed time: 24385.574285 msecs"
;;; (-28 -6 -32 -20 -30 -32 -16 -38 -22 -12)
;;;
;;; Running the following test fully demostrate the power of searching deply ahead.
;;;
;;; reversi> (round-robin  (list
;;;			    (maximizer modified-weighted-squares)
;;;			    (alpha-beta-searcher  2 modified-weighted-squares)
;;;			    (alpha-beta-searcher2 2 modified-weighted-squares)
;;;			    (alpha-beta-searcher2 4 modified-weighted-squares)
;;;			    (alpha-beta-searcher2 6 modified-weighted-squares)
;;;			    (alpha-beta-searcher2 8 modified-weighted-squares)
;;;			    random-strategy)
;;;		           10 10
;;;		           '(MW AB-2-MW AB2-2-MW AB2-4-MW AB2-6-MW AB2-8-MW RANDOM))
;;; MW                     8.57:  -----    6.0    4.5    2.5    2.5    2.0   18.5 
;;; AB-2-MW               12.50:   14.0  -----   10.5    5.5    1.0    1.5   20.0 
;;; AB2-2-MW              13.21:   15.5    9.5  -----    6.0    2.5    3.0   19.0 
;;; AB2-4-MW              17.98:   17.5   14.5   14.0  -----    8.0    2.0   19.5 
;;; AB2-6-MW              21.07:   17.5   19.0   17.5   12.0  -----    2.5   20.0 
;;; AB2-8-MW              25.95:   18.0   18.5   17.0   18.0   17.5  -----   20.0 
;;; RANDOM                 0.71:    1.5   00.0    1.0    0.5   00.0   00.0  ----- 
;;; nil
;;; reversi>


;;;
;;; 18.10 - It Pays to Precycle
;;;

;;; Still not clear how to translate the board caching machinery.
(comment
  (defvar *ply-boards*
    (apply #'vector (loop repeat 40 collect (initial-board)))))


;;;
;;; 18.11 - Killer Moves
;;;

(defn
  #^{:doc "Move the killer move to the front of the moves,
   if the killer move is in fact a legal move."
     :test (fn []
	     (is (= (put-first 5 '(0 1 2 3 4 5 6 7 8)) '(5 0 1 2 3 4 6 7 8)))
	     (is (= (put-first 9 '(0 1 2 3 4 5 6 7 8)) '(0 1 2 3 4 5 6 7 8))))}
  put-first [killer moves]
  (if (seq-utils/includes? moves killer)
    (cons killer (remove (fn [item] (if (= item killer) true false)) moves))
    moves))

(defn
  #^{:doc "A-B search, putting killer moves first."}
  alpha-beta3 [player board achievable cutoff ply eval-fn killer]
  (if (= ply 0)
    [(eval-fn player board) nil]
    (let [moves (put-first killer (legal-moves player board))] ;;; legal-moves-optimized
      ;;(println "alpha-beta3 - moves: " moves)
      (if (empty? moves)
	(if (any-legal-move? (opponent player) board)
	  (let [[v _] (alpha-beta3 (opponent player) board
				   (- cutoff) (- achievable)
				   (- ply 1) eval-fn nil)]
	    [(- v) nil])
	  [(final-value player board) nil])
	(let [best-move (atom (first moves))
	      new-board (create-board) ;;; should use the prepared vector ...
	      killer2 (atom nil)
	      killer2-val (atom winning-value)
	      ac (atom achievable)]
	  ;;(println "alpha-beta3 - best-move: " @best-move)
	  (loop [xmoves moves]
	    (when (not (empty? xmoves))
	      (let [move (first xmoves)
		    board2 (make-move move player board)
		    [v reply] (alpha-beta3
			       (opponent player) board2
			       (- cutoff) (- @ac)
			       (- ply 1) eval-fn @killer2)
		    val (- v)]
		;;(println "alpha-beta3 - val: " val ", move: " move)
		(when (> val @ac)
		  (reset! ac val)
		  (reset! best-move move))
		(when (and reply (< val @killer2-val))
		  (reset! killer2 reply)
		  (reset! killer2-val val))
		(when (< @ac cutoff)
		  (recur (rest xmoves))))))
	  [@ac @best-move])))))

;;; still to be translated .....
(comment
  (defun alpha-beta3 (player board achievable cutoff ply eval-fn killer)
    "A-B search, putting killer moves first."
    (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (put-first killer (legal-moves-optimized player board))))
	(if (null moves)
	  (if (any-legal-move? (opponent player) board)
	    (- (alpha-beta3 (opponent player) board
			    (- cutoff) (- achievable)
			    (- ply 1) eval-fn nil))
	    (final-value player board))
	  (let ((best-move (first moves))
		(new-board (aref *ply-boards* ply))
		(killer2 nil)
		(killer2-val winning-value))
	    (loop for move in moves
		  do (multiple-value-bind (val reply)
					  (alpha-beta3
					   (opponent player)
					   (make-move move player
						      (replace new-board board))
					   (- cutoff) (- achievable)
					   (- ply 1) eval-fn killer2)
					  (setf val (- val))
					  (when (> val achievable)
					    (setf achievable val)
					    (setf best-move move))
					  (when (and reply (< val killer2-val))
					    (setf killer2 reply)
					    (setf killer2-val val)))
		  until (>= achievable cutoff))
	    (values achievable best-move))))))

  (defn
    #^{:doc "Find the best move, for PLAYER, according to EVAL-FN,
   searching PLY levels deep and backing up values,
   using cutoff whenever possible."}
    alpha-beta [player board achievable cutoff ply eval-fn]
    ;; (println "alpha-beta: player=" player ", achievable=" achievable ", cutoff=" cutoff ", ply=" ply ", eval-fn=" eval-fn ", board=" board)
    (if (= ply 0)
      [(eval-fn player board) nil]
      (let [moves (legal-moves player board)]
	;; (println "alpha-beta: moves=" moves)
	(if (empty? moves)
	  (if (any-legal-move? (opponent player) board)
	    (let [[v _] (alpha-beta (opponent player) board
				    (- cutoff) (- achievable)
				    (- ply 1) eval-fn)]
	      [(- v) nil])
	    [(final-value player board) nil])
	  (let [best-move (atom (first moves))
		ac (atom achievable)]
	    (loop [xmoves moves]
	      (when (not (empty? xmoves))
		(let [move (first xmoves)
		      board2 (make-move move player board)
		      [v _] (alpha-beta
			     (opponent player) board2
			     (- cutoff) (- @ac)
			     (- ply 1) eval-fn)
		      val (- v)]
		  (when (> val @ac)
		    (reset! ac val)
		    (reset! best-move move))
		  (when (< @ac cutoff)
		    (recur (rest xmoves))))))
	    ;;(println "alpha-beta: ac=" @ac ", best-move=" @best-move ", ply=" ply)
	    [@ac @best-move])))))

  (defn
    #^{:doc "A-B search, sorting moves by eval-fn."}
    alpha-beta2 [player node achievable cutoff ply eval-fn]
    ;; Returns two values: achievable-value and move-to-make
    (if (= ply 0)
      [(:value node) node]
      (let [board (:board node)
	    nodes (legal-nodes player board eval-fn)]
	(if (empty? nodes)
	  (if (any-legal-move? (opponent player) board)
	    (let [[v _] (alpha-beta2 (opponent player)
				     (negate-value node)
				     (- cutoff) (- achievable)
				     (- ply 1) eval-fn)]
	      [(- v) nil])
	    [(final-value player board) nil])
	  (let [best-node (atom (first nodes))
		ac (atom achievable)]
	    (loop [xnodes nodes]
	      (when (not (empty? xnodes))
		(let [move (first xnodes)
		      [v _] (alpha-beta2
			     (opponent player)
			     (negate-value move)
			     (- cutoff) (- @ac)
			     (- ply 1) eval-fn)
		      val (- v)]
		  (when (> val @ac)
		    (reset! ac val)
		    (reset! best-node move))
		  (when (< @ac cutoff)
		    (recur (rest xnodes))))))
	    [@ac @best-node])))))
  
  )


(defn
  #^{:doc "Return a strategy that does A-B search with killer moves."}
  alpha-beta-searcher3 [depth eval-fn]
  (fn [player board]
    (let [[value move]
	  (alpha-beta3 player board losing-value winning-value
		       depth eval-fn nil)]
      move)))

(defn
  #^{:doc "Current mobility is the number of legal moves.
   Potential mobility is the number of blank squares
   adiacent to an opponent that are not legal moves.
   Returns current potential mobility for player."
     :test (fn []
	     (is (= (mobility black (initial-board)) [4 10]))
	     (is (= (mobility white (initial-board)) [4 10]))
	     (is (= (mobility black *fixt-board-c*) [9 22]))
	     (is (= (mobility white *fixt-board-c*) [6 16]))
	     (is (= (mobility black *fixt-board-black-has-to-pass*) [0 5]))
	     (is (= (mobility black *fixt-board-end-game-x*) [0 0]))
	     (is (= (mobility white *fixt-board-end-game-x*) [0 0])))}
  mobility [player board]
  (let [opp (opponent player)
	current (atom 0)		; player's current mobility
	potential (atom 0)]		; player's potential mobility
    (doseq [square all-squares]
      (when (= (board-ref board square) empty-square)
	(cond (legal? square player board)
	      (reset! current (inc @current))
	      (some (fn [sq] (= (board-ref board sq) opp))
		    (neighbors square))
	      (reset! potential (inc @potential)))))
    [@current (+ @current @potential)]))

;;;
;;; Edge Stability - Section Begin
;;;

;;; OK
(def
 #^{:doc "Array of values to player-to-move for edge positions."}
 *edge-table* (make-array Integer (math/expt 3 10)))

;;; OK
(def
 #^{:doc "The four edges (with their X-squares)"}
 edge-and-x-lists
     '((22 11 12 13 14 15 16 17 18 27)
       (72 81 82 83 84 85 86 87 88 77)
       (22 11 21 31 41 51 61 71 81 72)
       (27 18 28 38 48 58 68 78 88 77)))

;;; OK
(def top-edge (first edge-and-x-lists))
(def bottom-edge (second edge-and-x-lists))
(def left-edge (nth edge-and-x-lists 2))
(def right-edge (nth edge-and-x-lists 3))

;;; OK
(defn
  #^{:doc "The index counts 1 for player; 2 for opponent,
   on each square--summed as a base 3 number."
     :test (fn []
	     (are [p b squares idx] (= (edge-index p b squares) idx)
		  white *fixt-board-black-has-to-pass* top-edge 50816
		  black *fixt-board-black-has-to-pass* top-edge 35290
		  white *fixt-board-end-game-x* top-edge 29564
		  black *fixt-board-end-game-x* top-edge 59008
		  white (initial-board) top-edge 0
		  white *fixt-board-edge-index* top-edge 59048
		  black *fixt-board-edge-index* top-edge 29524))}
  edge-index [player board squares]
  (loop [index 0
	 sqs squares]
    (if (empty? sqs)
      index
      (recur
       (+ (* index 3)
	  (cond (= (board-ref board (first sqs)) empty-square) 0
		(= (board-ref board (first sqs)) player) 1
		true 2))
       (rest sqs)))))


;;; KO!!!
(defn
  #^{:doc "Total edge evaluation for player to move on board"}
  edge-stability [player board]
  (dotimes [i (count *edge-table*)] (aset *edge-table* i 0)) ;;; it has to be removed!!!
  (reduce + (for [edge-list edge-and-x-lists]
	      (aget *edge-table* (edge-index player board edge-list)))))

;;; KO!!!
(defn
  #^{:doc "Initialize *edge-table*, starting from the empty board."}
  init-edge-table []
  ;; Initialize the static values
  (comment
    (loop for n-pieces from 0 to 10 do
	  (map-edge-n-pieces
	   #'(lambda (board index)
		     (setf (aref *edge-table* index)
			   (static-edge-stability black board)))
	   black (initial-board) n-pieces top-edge 0)))
  (comment
    ;; Now iterate five times trying to improve:
    (dotimes (i 5)
      ;; Do the indexes with most pieces first
      (loop for n-pieces from 9 downto 1 do
	    (map-edge-n-pieces
	     #'(lambda (board index)
		       (setf (aref *edge-table* index)
			     (possible-edge-moves-value
			      black board index)))
	     black (initial-board) n-pieces top-edge 0)))))

;;; KO!!!
(defn
  #^{:doc "Call fun on all edges with n pieces."}
  map-edge-n-pieces [fun player board n squares index]
  ;; Index counts 1 for player; 2 for opponent
  (cond
   (< (count squares) n) nil
   (empty? squares) (fun board index)
   true (let [index3 (* 3 index)
	      sq (first squares)]
	  (map-edge-n-pieces fun player board n (rest squares) index3)
	  (when (and (> n 0) (== (board-ref board sq) empty-square))
	    (map-edge-n-pieces fun player
			       (board-set board sq player)
			       (- n 1) (rest squares)
			       (+ 1 index3))
	    (map-edge-n-pieces fun player
			       (board-set board sq (opponent player))
			       (- n 1) (rest squares)
			       (+ 2 index3))
	    board))))


;;; KO!!!
(defn
  #^{:doc "Consider all possible edge moves.
   Combine their values into a single number."}
  possible-edge-moves-value [player board index]
  (combine-edge-moves
   (cons
    (list 1.0 (aget *edge-table* index)) ;; no move
    (for [sq top-edge :when (== (board-ref board sq) empty-square)]
      (possible-edge-move player board sq)))
   player))

;;; KO!!!
(defn
  #^{:doc "Return a (prob val) pair for a possible edge move."}
  possible-edge-move [player board sq]
  (let [new-board (make-move sq player board)]
    (list (edge-move-probability player board sq)
	  (- (aget *edge-table*
		   (edge-index (opponent player)
			       new-board top-edge))))))

;;; OK
(defn
  #^{:doc "Combines the best moves."
     :test (fn []
	     (are [poss player value] (= (combine-edge-moves poss player) value)
		  '((1.0 5800) (0.5 5800)) black 5800
		  '((1.0 5800) (0.5 5800)) white 5800
		  '((1.0 2075) (0.005 4000)) black 2085
		  '((1.0 2075) (0.005 4000)) white 2075))}
  combine-edge-moves [possibilities player]
  (let [comparator-fn (if (== player black) > <)]
    (loop [pairs (sort-by second comparator-fn possibilities)
	   prob 1.0
	   val 0.0]
      (if (and (not (empty? pairs)) (>= prob 0.0))
	(let [pair (first pairs)]
	  (recur
	   (rest pairs)
	   (- prob (* prob (first pair)))
	   (+ val (* prob (first pair) (second pair)))))
	(math/round val)))))

;;; OK
(let [corner_xsqs [{:c 11 :x 22} {:c 18 :x 27} {:c 81 :x 72} {:c 88 :x 77}]]
  (defn
    #^{:doc "Is the square a corner? If yes the corner map is returned, otherwise nil."
       :test (fn []
	       (is (= (corner? 11) {:c 11 :x 22}))
	       (are [sq] (if (corner? sq) true false)
		    11 18 81 88)
	       (are [sq] (not (if (corner? sq) true false))
		    0 10 12 21 22 68 100))}
    corner? [sq] (first (for [cx corner_xsqs :when (== (:c cx) sq)] cx)))
  (defn
    #^{:doc "Is the square an x square? If yes the corner map is returned, otherwise nil."
       :test (fn []
	       (is (= (x-square? 77) {:c 88 :x 77}))
	       (are [sq] (if (x-square? sq) true false)
		    22 27 72 77)
	       (are [sq] (not (if (x-square? sq) true false))
		    0 10 12 21 23 68 100))}
    x-square? [sq] (first (for [cx corner_xsqs :when (== (:x cx) sq)] cx)))
  (defn
    #^{:doc "Returns the x-square's corner, otherwise nil."
       :test (fn []
	       (are [corner x-sq] (== (x-square-for corner) x-sq)
		    11 22, 18 27, 81 72, 88 77)
	       (are [corner] (= (x-square-for corner) nil)
		    0 10 12 21 22 67))}
    x-square-for [corner] (first (for [cx corner_xsqs :when (== (:c cx) corner)] (:x cx))))
  (defn
    #^{:doc "Returns the corner's x-square, otherwise nil."
       :test (fn []
	       (are [corner x-sq] (== (corner-for x-sq) corner)
		    11 22, 18 27, 81 72, 88 77)
	       (are [x-sq] (= (corner-for x-sq) nil)
		    0 10 11 21 66 100))}
    corner-for [xsq] (first (for [cx corner_xsqs :when (== (:x cx) xsq)] (:c cx)))))

;;; OK
;;; It has the same result given by the CL version.
;;; A bit more of investigation should be done.
(defn
  #^{:doc "What's the probability that player can move to this square?"
     :test (fn []
	     (is (= (for [sq all-squares]
		      (edge-move-probability white *fixt-board-black-has-to-pass* sq))
		    '(0.9 0.05 1.0 0.1 1.0 0.1 1.0 1.0 0.4 0.5 0.7 0.7 0.7 0.7 0.5
			  0.4 1.0 0.05 0.3 0.3 0.3 0.3 0.3 0.05 1.0 0.05 0.7 0.01 0.3 
			  0.01 0.01 0.05 1.0 0.05 0.7 0.01 0.3 0.01 0.01 0.05 1.0 0.05 
			  0.7 0.3 0.3 0.7 0.01 0.4 1.0 0.5 0.7 0.3 0.7 0.7 0.5 1.0 0.9 
			  0.01 0.01 0.01 0.01 0.3 0.01 0.9))))}
  edge-move-probability [player board square]
  (cond
   (x-square? square) 0.5		; X-squares
   (legal? square player board) 1.0	; immediate capture
   (corner? square)		  ; move to corner depends on X-square
   (let [x-sq (x-square-for square)]
     (cond
      (== (board-ref board x-sq) empty-square) 0.1
      (== (board-ref board x-sq) player) 0.001
      true 0.9))
   true (/ (aget
	    (to-array-2d [[0.10 0.40 0.70]
			  [0.05 0.30   'x]
			  [0.01   'x   'x]])
	    (count-edge-neighbors player board square)
	    (count-edge-neighbors (opponent player) board square))
	   (if (legal? square (opponent player) board) 2 1))))

;;; OK
(defn
  #^{:doc "Count the neighbors of this square occupied by player."
     :test (fn []
	     (are [p sq n] (= (count-edge-neighbors
			    p *fixt-board-black-has-to-pass* sq) n)
		  black 13 2
		  white 13 0
		  black 15 1
		  white 15 1
		  black 17 0
		  white 17 1
		  black 18 0
		  white 18 0))}
  count-edge-neighbors [player board square]
  (count
   (filter
    (fn [inc] (= (board-ref board (+ square inc)) player)) [+1 -1])))

;;; OK
(def *static-edge-table*
     (to-array-2d [
		   [  'x   0 -2000]	; X
		   [ 700  'x    'x]	; corner
		   [1200 200   -25]	; C
		   [1000 200    75]	; A
		   [1000 200    50]	; B
		   [1000 200    50]	; B
		   [1000 200    75]	; A
		   [1200 200   -25]	; C
		   [ 700  'x    'x]	; corner
		   [  'x   0 -2000]	; X
		   ]))

;;; OK
(let [stable 0
      semi-stable 1
      unstable 2]
  (defn
    #^{:doc "Computes piece stability. It works only applyed to the top edge.
     Returns a ten values sequence containing the piece stability for the edge.
     Values are: 0 for stable, 1 for semi-stable, and 2 for unstable."
       :test (fn []
	       (are [board ps] (= (for [sq top-edge] (piece-stability board sq)) ps)
		    *fixt-board-black-has-to-pass* '(1 0 2 1 1 0 1 0 0 2)
		    (initial-board) '(2 0 0 0 0 0 0 0 0 2)
		    *fixt-board-edge-index* '(1 0 0 0 0 0 0 0 0 1)))}
    piece-stability [board sq]
    (cond
     (corner? sq) stable
     (x-square? sq) (if (= (board-ref board (corner-for sq)) empty-square)
		      unstable semi-stable)
     true (let [player (board-ref board sq)
		opp (opponent player)
		p1 (first (filter #(not (== player %)) (subvec board sq 19)))
		p2 (first (filter #(not (== player %)) (reverse (subvec board 11 sq))))]
	    (cond
	     ;; unstable pieces can be captured immediately
	     ;; by playing in the empty square
	     (or (and (= p1 empty-square) (= p2 opp))
		 (and (= p2 empty-square) (= p1 opp)))
	     unstable
	     ;; semi-stable pieces might be captured
	     (and (= p1 opp) (= p2 opp)
		  (first (filter #(== empty-square %) (subvec board 11 19))))
	     semi-stable
	     (and (= p1 empty-square) (= p2 empty-square))
	     semi-stable
	     ;; stable pieces can never be captured
	     true stable)))))

;;; OK
(defn
  #^{:doc "Compute this edge's static stability."
     :test (fn []
	     (are [player board ses] (= (static-edge-stability player board) ses)
		  black *fixt-board-edge-index* 7800
		  white *fixt-board-edge-index* -7800
		  black (initial-board) 0
		  white (initial-board) 0
		  black *fixt-board-black-has-to-pass* -2725))}
  static-edge-stability [player board]
  (loop [squares top-edge
	 i 0
	 sum 0]
    (if (empty? squares)
      sum
      (let [sq (first squares)
	    p (board-ref board sq)
	    x (aget *static-edge-table* i (piece-stability board sq))]
	(recur
	 (rest squares)
	 (inc i)
	 (+ sum (cond
		 (= p empty-square) 0
		 (= p player) x
		 true (- x))))))))



;;; (setf *edge-table* ...)

;;;   
;;;  --- To be completed and tested:
;;;
;;;  -2- init-edge-table
;;;  -3- map-edge-n-pieces
;;;  -4- possible-edge-moves-value
;;;  -5- possibe-edge-move
;;;  -6- combine-edge-moves
;;;  -7- corner-p ....
;;;  -8- edge-move-probability
;;;  -13- setf *edge-table*
;;;
;;; *edge-table* calculation, store, and retrieve functions has to be organized.

;;;
;;; Edge Stability - Section End
;;;

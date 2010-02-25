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
	 "reversi/reversi")
  (:require [clojure.contrib [pprint :as pprint]])
  (:require [clojure.contrib [seq-utils :as seq-utils]])
  (:require [clojure.contrib [fcase :as fcase]]))

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
	     (is (thrown? ClassCastException (index-of-max ['x])) "HAs to throws an exception."))}
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
      (when *print* (println "maximizer: player, moves, scores, best: " player moves scores best))
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
	  ;;(println "alpha-beta: ac=" @ac ", best-move=" @best-move)
	  [@ac @best-move])))))

(defn
  #^{:doc "A strategy that searches to DEPTH and then uses EVAL-FN."}
  alpha-beta-searcher [depth eval-fn]
  (fn [player board]
    (let [[value move]
	  (alpha-beta player board losing-value winning-value
		      depth eval-fn)]
      ;;(println "alpha-beta-searcher: value=" value ", move=" move)
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

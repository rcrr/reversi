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
	 "reversi/test-fixtures")
  (:use clojure.test)
  (:require [clojure.contrib [pprint :as pprint]])
  (:require [clojure.contrib [seq-utils :as seq-utils]])
  (:require [clojure.contrib [math :as math]])
  (:require [clojure.contrib [str-utils2 :as string]])
  (:require [clojure.contrib.generic [math-functions :as math-f]])
  (:import (reversi GameOverException))
  (:import (rcrr.reversi.ui ReversiBoard BoardSquareKey SquareColor)))

(defn
  #^{:doc "Return a specific character foreach valid piece value."
     :test (fn []
	     (is (= (name-of empty-square) \.)
		 "The empty square is represented as \".\".")
	     (is (= (name-of black) \@)
		 "The black square is represented as \"@\".")
	     (is (= (name-of white) \O)
		 "The white square is represented as \"O\".")
	     (is (= (name-of outer) \?)
		 "The out of board square is represented as \"?\".")
	     (is (= (name-of 100) \E)
		 "Errors (values other than above) are represented as \"E\"."))}
  name-of [piece]
  (cond (= piece 0) \.
	(= piece 1) \@
	(= piece 2) \O
	(= piece 3) \?
	true \E))

(defn
  #^{:doc "Return the player opponent."
     :test (fn []
	     (is (= (opponent black) white) "black's opponent is white.")
	     (is (= (opponent white) black) "white's opponent is black."))}
  opponent [player]
  (if (= player black) white black))

;;; The game board is implemented as a Vector (clojure.lang.PersistentVector).
;;; The 100 squares are plain integer.
;;; A board structure does not appear as required.

(def col-numbers [ 0  1  2  3  4  5  6  7  8  9])
(def row-numbers [ 0  1  2  3  4  5  6  7  8  9])
(def col-names   [:? :a :b :c :d :e :f :g :h :&])
(def row-names   [:0 :1 :2 :3 :4 :5 :6 :7 :8 :9])
(defstruct square :name :number :col-name :col-number :row-name :row-number)
(def reversi-board (vec (for [y row-numbers x col-numbers]
			  (struct square
				  (keyword (str (name (get col-names x)) (name (get row-names y))))
				  (+ (* 10 y) x)
				  (get col-names x)
				  x
				  (get row-names y)
				  y))))

(defn
  #^{:doc "Convert from alphanumeric to numeric square notation."
     :test (fn []
	     (is (= (conv-h8->88 'a1) 11) "Corner a1 is square 11.")
	     (is (= (conv-h8->88 'h8) 88) "Corner h8 is square 88.")
	     (is (= (conv-h8->88 'c2) 23) "Corner c2 is square 23."))}
  conv-h8->88 [val]
  (first (for [sq reversi-board :when (= (:name sq) (keyword val))] (:number sq))))

(defn
  #^{:doc "Convert from numeric to alphanumeric square notation."
     :test (fn []
	     (is (= (conv-88->h8 11) "a1") "Corner a1 is square 11.")
	     (is (= (conv-88->h8 88) "h8") "Corner h8 is square 88.")
	     (is (= (conv-88->h8 23) "c2") "Corner c2 is square 23."))}
  conv-88->h8 [val]
  (name ((get reversi-board val) :name)))

(defn
  #^{:doc "Query a board for a given square."
     :test (fn []
	     (is (= (board-ref *fixt-ib* 0) outer) "")
	     (is (= (board-ref *fixt-ib* 100) nil) "")
	     (is (= (board-ref *fixt-ib* 11) empty-square) "")
	     (is (= (board-ref *fixt-ib* 45) black) "")
	     (is (= (board-ref *fixt-ib* 44) white) ""))}
  board-ref [board square]
  (get board square))

(defn
  #^{:doc "Set the value of a given board square pairs.
   Return a new board. It is not destructive."
     :test (fn []
	     (let [sq 11
		   player black]
	       (is (= (board-ref (board-set *fixt-eb* sq player) sq) player)
		   "Set the a1 square to black")))}
  board-set [board square val]
  (assoc board square val))

(defn
  #^{:doc "Create a new completely emty board."}
  create-board []
  (vec (repeat 100 empty-square)))

(defn
  #^{:doc "Create a new copy of the given board."
     :test (fn []
	     (is (= *fixt-ib* (copy-board *fixt-ib*)) 
		 "copy-board has to return a new equal board"))}
  copy-board [board]
  (vec board))

(defn
  #^{:doc "Return a board, empty except for four pieced in the middle."
     :test (fn []
	     (let [b (initial-board)]
	       (is (= (board-ref b  0) outer) "")
	       (is (= (board-ref b  6) outer) "")
	       (is (= (board-ref b 11) empty-square) "")
	       (is (= (board-ref b 60) outer) "")
	       (is (= (board-ref b 69) outer) "")
	       (is (= (board-ref b 44) white) "")
	       (is (= (board-ref b 45) black) "")
	       (is (= (board-ref b 88) empty-square) "")
	       (is (= (board-ref b 93) outer) "")
	       (is (= (initial-board) *fixt-ib*))))}
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

;;: *move-number* *board*, and *clock* are three global variables
;;; defined by PAIP 18.7. According to the fully functional,
;;; side effect free paradigm proposed by Clojure, them should
;;; not be used at all .... 

(defn
  #^{:doc "Make a new game clock."}
  make-clock
  ([] (make-clock 30.))
  ([minutes]
     (vec (repeat 3 (* minutes 60 internal-time-units-per-second)))))

(def 
 #^{:doc "The number of the move to be played."}
 *move-number* (atom 0))

(def 
 #^{:doc "A copy of the game clock."}
 *clock* (atom (make-clock 0.)))

(def 
 #^{:doc "A copy of the game board."}
 *board* (initial-board))

(defn
  #^{:doc "Count the player pieces."
     :test (fn []
	     (is (= (count-pieces (initial-board) black) 2)
		 "An initial board should have two black pieces."))}
  count-pieces [board player]
  (count (filter (fn [piece] (= piece player)) board)))

(defn
  #^{:doc "Count player's pieces minus opponent's pieces."
     :test (fn []
	     (is (= (count-difference black (initial-board)) 0)
		 "An initial board has no piece's difference.")
	     (is (= (count-difference black *fixt-board-end-game-x*) 10) 
		 "on *fixt-board-end-game-x* board black has 10 pieces 
                  more than white.")
	     (is (= (count-difference white *fixt-board-end-game-x*) -10) 
		 "on *fixt-board-end-game-x* board white has 10 pieces 
                  less than black.")
	     (is (= (count-difference black *fixt-board-c*) -2) 
		 "on *fixt-board-end-game-c* board black has -2 pieces 
                  less than black."))}
  count-difference [player board]
  (- (count-pieces board player)
     (count-pieces board (opponent player))))

(defn
  #^{:doc "Return a string representing this internal time
   expressed in millisecond in min:secs."}
  time-string [time]
  (let [t (math/round (/ time internal-time-units-per-second))
	min (quot t 60)
	sec (rem t 60)]
    (pprint/cl-format nil "~2d:~2,'0d" min sec)))

(defn
  #^{:doc "Print a board, along with some statistics."}
  print-board
  ([clock] (print-board *board* clock))
  ([board clock]
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
  (when clock
    (pprint/cl-format true "~2&~2&Time remaining: [~c=~a ~c=~a]"
		      (name-of black) (time-string (get clock black))
		      (name-of white) (time-string (get clock white))))
  (pprint/cl-format true "~2&~2&")))

(defn
  #^{:doc "Valid moves are a number in the range 11-88 that end in 1-8"
     :test (fn []
	     (is (not (valid? 0)) "0 is not a valid move.")
	     (is (valid? 11) "The upper left corner is valid.")
	     (is (not (valid? 19)) "19 is in the board but is not valid.")
	     (is (not (valid? 100)) "100 is outside the board.")
	     (is (not (valid? "a-string")) "A string is not a valid move"))}
  valid? [move]
  (and (integer? move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defn
  #^{:doc "Return the square number of the bracketing piece."
     :test (fn []
	     (let [dir -1
		   move 'h7
		   b1 (+ (conv-h8->88 move) dir)
		   b2 (conv-h8->88 'c7)]
	       (is (= (find-bracketing-piece
		       b1 white 
		       *fixt-board-black-has-to-pass* dir) b2)
		   "Moving to g8, the white player, identify c7 
                    following the -1 (west) direction.")))}
  find-bracketing-piece [square player board dir]
  (cond (= (board-ref board square) player) square
	(= (board-ref board square) (opponent player))
	(find-bracketing-piece (+ square dir) player board dir)
	true nil))

(defn
 #^{:doc "Would this move result in any flips in this direction?
   If so, return the square number of the bracketing piece."
    :test (fn []
	    (is (= (would-flip? 78 white *fixt-board-black-has-to-pass* -1) 73)
		"The white move to 78 flips pieces up to 73 following the -1 dir.")
	    (is (= (would-flip? 78 white *fixt-board-black-has-to-pass* -10) false)
		"The white move to 78 does'nt flip anything following the -10 dir."))}
 would-flip? [move player board dir]
  ;; A flip occours if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, braketed by
  ;; one of player's pieces.
  (let [c (+ move dir)]
    (and (= (board-ref board c) (opponent player))
	 (find-bracketing-piece (+ c dir) player board dir))))

(defn
 #^{:doc "A legal move must be into an empty square, and it must
   flip at least one opponent piece."
    :test (fn []
	    (is (not (legal? 78 black *fixt-board-black-has-to-pass*))
		"78 is not a legal move for black.")
	    (is (not (not (legal? 78 white *fixt-board-black-has-to-pass*)))
		"78 is a legal move for white."))}
 legal? [move player board]
 (and (= (board-ref board move) empty-square)
      (some (fn [dir] (would-flip? move player board dir))
	    all-directions)))

(defn
  #^{:doc "Return a new board to reflect move by player."
     :test (fn []
	     (is (= (make-move 34 black (initial-board)) *fixt-board-a*)
		 "black does first move to 34, *fixt-board-a* is the resulting board."))}
  make-move [move player board]
  ;; make-move does't check if the move is legal. make-move is
  ;; used only by get-move, get move does check that the move
  ;; is legal before calling.
  ;; get-move is used only by reversi.
  ;;
  ;; There is space for improvenment.
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
			      (assoc! b c p)
			      (if (not (= c (- bracketer dir)))
				(recur (+ c dir)))))))] 
      (doseq [dir all-directions]
	(make-flips dir)))
    (persistent! b)))


(defn
  #^{:doc "Does player have any legal moves in this position?"
     :test (fn []
	     (is (not (not (any-legal-move? black (initial-board))))
		 "black has some legal moves as first move!")
	     (is (not (any-legal-move? black *fixt-board-black-has-to-pass*))
		 "black has no legal moves given the board *fixt-board-black-has-to-pass*.")
	     (is (not (not (any-legal-move? white *fixt-board-black-has-to-pass*)))
		 "white has some legal moves given the board *fixt-board-black-has-to-pass*.")
	     (is (not (any-legal-move? black *fixt-board-end-game-x*))
		 "given board *fixt-board-end-game-x* the game is over, black has no move.")
	     (is (not (any-legal-move? white *fixt-board-end-game-x*))
		 "given board *fixt-board-end-game-x* the game is over, white has no move."))}
  any-legal-move? [player board]
  (some (fn [move] (legal? move player board))
	all-squares))


(defn
  #^{:doc "Compute the player to move next, or nil if nobady can move."
     :test (fn []
	     (is (= (next-to-play *fixt-ib* white false) black)
		 "black has to move first.")
	     (is (= (next-to-play *fixt-board-end-game-x* white false) nil)
		 "The game is over nobady has to move.")
	     (is (= (next-to-play *fixt-board-end-game-x* black false) nil)
		 "The game is over nobady has to move.")
	     (is (= (next-to-play *fixt-board-black-has-to-pass* white false) white)
		 "black shold move, but has to pass.")
	     (is (= (next-to-play *fixt-board-black-has-to-pass* black false) white)
		 "white has to move."))}
  next-to-play [board previous-player print]
  (let [opp (opponent previous-player)]
    (cond (any-legal-move? opp board) opp
	  (any-legal-move? previous-player board)
	  (do
	    (when print
	      (pprint/cl-format true "~2&~&~c has no moves and must pass.~&"
		      (name-of opp)))
	    previous-player)
	  true nil)))

(defn
  #^{:doc "Returns a list of legal moves for player."
     :test (fn []
	     (is (= (legal-moves black *fixt-ib*) '(34 43 56 65))
		 "Black's initial legal moves are d3, c4, f5, e6")
	     (is (= (legal-moves white *fixt-board-a*) '(33 35 53))
		 "White's valid moves are c3, e3, c5")
	     (is (= (legal-moves black *fixt-board-c*) '(28 41 43 47 51 56 62 65 77))
		 "Black's valid moves are h2, a4, c4, g4, a5, f5, b6, e6, g7"))}
  legal-moves [player board]
  (filter (fn [move] (legal? move player board)) all-squares))

(defn
  #^{:doc "Call the player's strategy function to get move.
   Keep calling until a legal move is made."
     :test (fn []
	     (dotimes [i 10]
	       (let [[board move clock] (get-move (fn [_ _] (rand-elt (range 0 100)))
						  black (initial-board) false 
						  (make-clock 30.))]
		 (cond
		  (= move 34) (is (= board *fixt-board-34*))
		  (= move 43) (is (= board *fixt-board-43*))
		  (= move 56) (is (= board *fixt-board-56*))
		  (= move 65) (is (= board *fixt-board-65*))
		  true (is (contains? '(34 43 56 65) move)
			   "The black first move must be into (34 43 56 65).")))))}
  get-move [strategy player board print clock]
  (when print (print-board board clock))
  (let [t0 (get-internal-real-time)
	move (strategy player (copy-board board))
	t1 (get-internal-real-time)
	delta-t (- t1 t0)
	delta-clock (if (= player black) [0 delta-t 0] [0 0 delta-t])
	clock (vec (map - clock delta-clock))]
    (cond
     (< (get clock player) 0.)
     (do
       (when print (pprint/cl-format true "~&~c has no time left and forfeits.~&" (name-of player)))
       (throw (new GameOverException {:game-over-value (if (= player black) -64 64)} "Player has no time left.")))
     (= move 'resign)
     (throw (new GameOverException {:game-over-value (if (= player black) -64 64)} "Player resigns."))
     (and (valid? move) (legal? move player board))
     (do
       (when print
	 (pprint/cl-format true "~&~c moves to ~a." (name-of player) (conv-88->h8 move)))
       [(make-move move player board) move clock])
     true
     (do
       (when print
	 (pprint/cl-format true "warn: illegal move: ~a" (conv-88->h8 move)))
       (get-move strategy player board print clock)))))

(defn
  #^{:doc "A human player for the game of reversi."}
  human [player board]
  (pprint/cl-format true "~&~c to move ~a: " (name-of player)
		    (map conv-88->h8 (legal-moves player board)))
  (conv-h8->88 (read)))

(defn
  #^{:doc "Make any legal move."}
  random-strategy [player board]
  (seq-utils/rand-elt (legal-moves player board)))

;;; random-game is used by the random-reversi-series function.
;;; the function should be a special case of reversi itself.
(defn
  #^{:doc "Return a RANDOM game."}
  random-game []
  (let [bl-strategy random-strategy
	wh-strategy random-strategy]
    (loop [board (initial-board)
	   moves ()
	   player black]
      (try
       (if player
	 (let [[board move _] (get-move (if (= player black) bl-strategy wh-strategy) player board false (make-clock))]
	   (recur
	    board
	    (cons move moves)
	    (next-to-play board player false)))
	 (throw (new GameOverException {:game-over-value (count-difference black board)} "No more moves left.")))
       (catch GameOverException goe
	 [(:game-over-value @goe) board (reverse moves)])))))

;;; The two function wrapped by the let are a way to work around
;;; the absence of the make-random-state function
;;; The design is strongly based on "side effects" and does not allow any
;;; concurrence in its usage.
(let [index (atom 0)
      random-seq-of-moves (atom nil)]
  (defn new-random-seq-of-moves []
    (reset! index 0)
    (reset! random-seq-of-moves
	    (let [[_ _ moves] (random-game)]
	      moves)))
  (defn pre-computed-random-strategy
    [player board]
    (reset! index (inc @index))
    (nth @random-seq-of-moves (- @index 1)))
  (defn rewind-pre-computed-random-strategy []
    (reset! index 0)))

(defn
  #^{:doc "Play a game of Reversi. Return the score, where a positive
   difference means black (the first player) wins."
     :test (fn []
	     (is (= (reversi random-strategy random-strategy false 0.) -64)
		 "With no time the black starts and forfaits.")
	     (dotimes [i 100]
	       (let [result (reversi random-strategy random-strategy false 1.)]
		 (is (and (>= result -64) (<= result 64))
		     "A random game must have the result in between -64 and 64."))))}
  reversi
  ([bl-strategy wh-strategy]
     (reversi bl-strategy wh-strategy true))
  ([bl-strategy wh-strategy print]
     (reversi bl-strategy wh-strategy print 30.))
  ([bl-strategy wh-strategy print minutes]
     (reset! *move-number* 0)
     (loop [board (initial-board)
	    moves ()
	    player black
	    clock (make-clock minutes)]
       (try
	(if player
	  (let [[board move clock] (get-move (if (= player black) bl-strategy wh-strategy) player board print clock)]
	    (reset! *move-number* (inc @*move-number*))
	    (recur
	     board
	     (cons move moves)
	     (next-to-play board player print)
	     clock))
	  (throw (new GameOverException {:game-over-value (count-difference black board)} "No more moves left.")))
	(catch GameOverException goe 
	  (do
	    (when print
	      (pprint/cl-format true "~2&The game is over. Final result:~&")
	      (print-board board clock)
	      (println "Game moves: " (map conv-88->h8 (reverse moves))))
	    (:game-over-value @goe)))))))

(defn
  #^{:doc "Given a MOVES list, a BOARD, and the PLAYER that has to move,
   the function returns the resulting board. MOVES are in the h8 style.
   MOVES legality is not enforced. The function checks if a player has to pass
   one or more moves."
     :test (fn []
	     (is (= (play-moves *fixt-game-x* *fixt-ib* black) *fixt-board-end-game-x*)
		 "Following move by move the *fixt-game-x* sequence should result 
                  into the *fixt-board-end-game-x* final board."))}
  play-moves [moves board player]
  (if (or (empty? moves) (nil? player))
    board
    (let [next-board (make-move (conv-h8->88 (first moves)) player board)]
      (recur (rest moves)
	     next-board
	     (next-to-play next-board player false)))))

(defn
  #^{:doc "Given a MOVES list, a game is played returning the resulting board."
     :test (fn []
	     (is (= (play-game *fixt-game-x*) *fixt-board-end-game-x*)
		 "Following move by move the *fixt-game-x* sequence should result 
                  into the *fixt-board-end-game-x* final board."))}
  play-game [moves]
  (play-moves moves (initial-board) black))

(defn
  #^{:doc "Play a series of 2*n-pairs games, swapping sides."
     :test (fn []
	     (is ()
		 ""))}
  reversi-series [strategy1 strategy2 n-pairs]
  (let [scores (loop [i n-pairs
		      scores1 []
		      scores2 []]
		 (if (= i 0)
		   (vec (interleave scores1 scores2))
		   (do
		     (new-random-seq-of-moves)
		     (recur
		      (dec i)
		      (do
			(rewind-pre-computed-random-strategy)
			(conj scores1 (reversi strategy1 strategy2 nil)))
		      (do
			(rewind-pre-computed-random-strategy)
			(conj scores2 (- (reversi strategy2 strategy1 nil))))))))]
    ;; Return: 
    ;; the difference between wins and losses, 
    ;; the number of wins. (1/2 for a tie),
    ;; the total of the point differences, and the
    ;; scores themselves, all from strategy1's point of view.
    [(reduce + (for [i scores] (math-f/sgn i)))  ; this version is more intuitive.
     (+ (reduce + (for [i scores :when (> i 0)] 1)) ; this version is a literal transcslation of the PAIP version.
	(reduce + (for [i scores :when (= i 0)] 0.5)))
     (reduce + scores)
     scores]))

(defn
  #^{:doc "Make a new strategy that plays strategy1 for m moves,
   then plays according to strategy2."}
  switch-strategies [strategy1 m strategy2]
  (fn [player board]
    (apply (if (<= @*move-number* m) strategy1 strategy2) [player board])))

(defn
  #^{:doc "Play a series of 2*n games, starting from a random position."}
  random-reversi-series
  ([strategy1 strategy2 n-pairs] (random-reversi-series strategy1 strategy2 n-pairs 10))
  ([strategy1 strategy2 n-pairs n-random]
     (reversi-series
	(switch-strategies pre-computed-random-strategy n-random strategy1)
	(switch-strategies pre-computed-random-strategy n-random strategy2)
	n-pairs)))

(defn
  #^{:doc "Play a tournament among the strategies.
   N-PAIRS = games each strategy plays as each color against
   each opponent. So with N strategies, a total of
   N*(N-1)*N-PAIRS games are played."}
  round-robin
  ([strategies n-pairs] (round-robin strategies n-pairs 10 strategies))
  ([strategies n-pairs n-random] (round-robin strategies n-pairs n-random strategies))
  ([strategies n-pairs n-random names]
     (let [n (count strategies)
	   totals (make-array Double n)
	   scores (make-array Double n n)]
       (dotimes [i n] (aset totals i 0.) (dotimes [j n] (aset scores i j 0.)))
       ;; Play the games
       (dotimes [i n]
	 (loop [j (+ i 1)]
	   (when (< j n)
	     (let [[_ wins _ _] (random-reversi-series
				 (nth strategies i)
				 (nth strategies j)
				 n-pairs n-random)
		   losses (- (* 2 n-pairs) wins)]
	       (aset scores i j (+ (aget scores i j) wins))
	       (aset scores j i (+ (aget scores j i) losses))
	       (aset totals i (+ (aget totals i) wins))
	       (aset totals j (+ (aget totals j) losses))
	       (recur (inc j))))))
       ;; Print the results
       (dotimes [i n]
	 (pprint/cl-format true "~&~a~20T ~6,2,0f: " (nth names i) (/ (* (aget totals i) 100) (* 2 n-pairs (binomial n 2))))
	 (dotimes [j n]
	   (if (= i j)
	     (pprint/cl-format true " ----- ")
	     (pprint/cl-format true "~6,1,0f " (aget scores i j))))
	 (pprint/cl-format true "~2&")))))

;;; Here you see the comparison of some strategies that do not search more than one ply.
;;; Compare the results with "PAIP 18.8 - Playing a Series of Games (p. 629)".
;;; Totals are here calculated as a percentage of wins among the total number of games played.
;;;
;;; reversi> (round-robin
;;; 	      (list (maximizer count-difference)
;;;		    (maximizer basic-mobility)
;;;		    (maximizer weighted-squares)
;;;		    (maximizer modified-weighted-squares)
;;;		    random-strategy)
;;;	      700 10
;;;	      '(count-difference mobility weighted modified-weighted random))
;;; count-difference      13.95:  -----  687.5  229.0  173.0  863.5 
;;; mobility              16.39:  712.5  -----  367.5  238.5  976.0 
;;; weighted              28.60: 1171.0 1032.5  -----  598.0 1203.0 
;;; modified-weighted     31.78: 1227.0 1161.5  802.0  ----- 1258.5 
;;; random                 9.28:  536.5  424.0  197.0  141.5  ----- 
;;; nil
;;;
;;; reversi> (round-robin
;;;	  (list (minimax-searcher 4 count-difference)
;;;		(minimax-searcher 4 weighted-squares)
;;;		(minimax-searcher 4 modified-weighted-squares)
;;;		random-strategy)
;;;	  5 10
;;;	  '(count-difference weighted modified-weighted random))
;;; count-difference      20.00:  -----    2.0   00.0   10.0 
;;; weighted              38.33:    8.0  -----    5.0   10.0 
;;; modified-weighted     41.67:   10.0    5.0  -----   10.0 
;;; random                00.00:   00.0   00.0   00.0  ----- 
;;; nil


(defn basic-mobility [player board]
  "The number of moves a player has."
  (count (legal-moves player board)))


;;; Swing Graphical User Interface

(def square-color-map {empty-square "EMPTY", black "BLACK", white "WHITE"})

(defn up-case-symb2 [s]
  (symbol (string/upper-case (name s))))

(defn show-board
  ([board] (let [sgui (. ReversiBoard initDisplay)]
	     (show-board board sgui)))
  ([board sgui]
     (doseq [row (range 1 9)]
       (doseq [col (range 1 9)]
	 (let [square (+ col (* 10 row))
	       square-name (string/upper-case (conv-88->h8 square))
	       piece (board-ref board square)]
	   (. sgui setSquareColor
	      (BoardSquareKey/valueOf square-name)
	      (SquareColor/valueOf (square-color-map piece))))))))
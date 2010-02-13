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
	 "reversi/strategies")
  (:use clojure.test)
  (:require [clojure.contrib [pprint :as pprint]])
  (:require [clojure.contrib [seq-utils :as seq-utils]])
  (:require [clojure.contrib [math :as math]]))

(def *print* false)

;;; Test fixtures
(declare *fixt-ib* *fixt-eb* *fixt-board-a* *fixt-board-b*
	 *fixt-board-black-has-to-pass*
	 *fixt-game-black-has-to-pass*)

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
  #^{:doc "Create a new copy of the given board."}
  copy-board [board]
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
		 "An initial board has no piece's difference."))}
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


(defn
  #^{:doc "Does player have any legal moves in this position?"}
  any-legal-move? [player board]
  (some (fn [move] (legal? move player board))
	all-squares))


(defn
  #^{:doc "Compute the player to move next, or nil if nobady can move."}
  next-to-play [board previous-player print]
  (let [opp (opponent previous-player)]
    (cond (any-legal-move? opp board) opp
	  (any-legal-move? previous-player board)
	  (do
	    (when print
	      (pprint/cl-format true "~&~c has no moves and must pass.~&"
		      (name-of opp)))
	    previous-player)
	  true nil)))

(defn
  #^{:doc "Returns a list of legal moves for player."
     :test (fn []
	     (is (= (legal-moves black *fixt-ib*) '(34 43 56 65))
		 "Black's initial legal moves are d3, c4, f5, e6")
	     (is (= (legal-moves white *fixt-board-a*) '(33 35 53))
		 "White's valid moves are c3, e3, c5"))}
  legal-moves [player board]
  (filter (fn [move] (legal? move player board)) all-squares))

(defn
  #^{:doc "Call the player's strategy function to get move.
   Keep calling until a legal move is made."}
  get-move [strategy player board print]
  (when print (print-board board))
  (let [move (strategy player (copy-board board))]
    (cond
      (and (valid? move) (legal? move player board))
      (do
	(when print
	  (pprint/cl-format true "~&~c moves to ~a." (name-of player) (conv-88->h8 move)))
	[(make-move move player board) move])
      true
      (do
	(pprint/cl-format true "warn: illegal move: ~a" (conv-88->h8 move))
	(get-move strategy player board print)))))

(defn
  #^{:doc "Play a game of Reversi. Return the score, where a positive
   difference means black (the first player) wins."}
  reversi [bl-strategy wh-strategy print]
  (loop [board (initial-board)
	 moves ()
	 player black]
    (if player
      (let [[board move] (get-move (if (= player black) bl-strategy wh-strategy) player board print)]
	(recur
	 board
	 (cons move moves)
	 (next-to-play board player print)))
      (do
	(when print
	  (pprint/cl-format true "~2&The game is over. Final result:~&")
	  (print-board board)
	  (println "Game moves: " (map conv-88->h8 (reverse moves))))
	(count-difference black board)))))

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

(defn
  #^{:doc "Return a string representing this internal time
   expressed in millisecond in min:secs."}
  time-string [time]
  (let [t (math/round (/ time 1000000.0))
	min (quot t 60)
	sec (rem t 60)]
    (pprint/cl-format nil "~2d:~2,'0d" min sec)))


;;; Test env: fixtures

(defn
  #^{:doc "Prepare a fiew board used by tests."}
  basic-test-fixture [f]
  (binding [*fixt-ib* (initial-board)
	    *fixt-eb* 
	    [3 3 3 3 3 3 3 3 3 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-a*
	    [3 3 3 3 3 3 3 3 3 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 1 0 0 0 0 3
	     3 0 0 0 1 1 0 0 0 3
	     3 0 0 0 1 2 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-b*
	    [3 3 3 3 3 3 3 3 3 3
	     3 0 0 0 1 1 1 0 0 3
	     3 0 0 0 0 1 0 0 0 3
	     3 0 0 0 1 1 2 2 0 3
	     3 0 0 0 1 1 0 0 0 3
	     3 0 0 0 1 1 0 0 0 3
	     3 0 0 0 0 1 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 0 0 0 0 0 0 0 0 3
	     3 3 3 3 3 3 3 3 3 3]
	    *fixt-board-black-has-to-pass*
	    [3 3 3 3 3 3 3 3 3 3
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
	    ]
    (f)))

(use-fixtures :each basic-test-fixture)

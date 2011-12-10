;;;
;;; Copyright (c) 1998-2002 Peter Norvig
;;; Copyright (c) 2009 Roberto Corradini

;;; This file is part of the reversi program
;;; http://github.com/rcrr/reversi

;;; This pogram takes origin from the code
;;; described in the Peter Norvig's PAIP book (Paradigms of AI Programming).
;;; The internet location of the book is "http://norvig.com/paip.html"
;;; The page where to download the source code is "http://norvig.com/paip/README.html"
;;; The licence agreement page is located at "http://norvig.com/license.html"
;;; 
;;; Here the licence is reported:
;;; Copyright © 1998-2002 by Peter Norvig.

;;; Permission is granted to anyone to use this software, in source or object code form,
;;; on any computer system, and to modify, compile, decompile, run, and redistribute
;;; it to anyone else, subject to the following restrictions:
;;;   1. The author makes no warranty of any kind, either expressed or implied,
;;;      about the suitability of this software for any purpose.
;;;   2. The author accepts no liability of any kind for damages or other
;;;      consequences of the use of this software, even if they arise from
;;;      defects in the software.
;;;   3. The origin of this software must not be misrepresented,
;;;      either by explicit claim or by omission.
;;;   4. Altered versions must be plainly marked as such,
;;;      and must not be misrepresented as being the original software.
;;;      Altered versions may be distributed in packages
;;;      under other licenses (such as the GNU license). 
;;; If you find this software useful, it would be nice if you let me (peter@norvig.com)
;;; know about it, and nicer still if you send me modifications that you
;;; are willing to share. However, you are not required to do so.

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


(in-package :reversi)


;;; The reversi game board
;;;
;;;
;;;  0  1  2  3  4  5  6  7  8  9
;;; 10 11 12 13 14 15 16 17 18 19
;;; 20 21 22 23 24 25 26 27 28 29
;;; 30 31 32 33 34 35 36 37 38 39
;;; 40 41 42 43 44 45 46 47 48 49
;;; 50 51 52 53 54 55 56 57 58 59
;;; 60 61 62 63 64 65 66 67 68 69
;;; 70 71 72 73 74 75 76 77 78 79
;;; 80 81 82 83 84 85 86 87 88 89
;;; 90 91 92 93 94 95 96 97 98 99
;;;
;;; fig 1: The board representation as a 100 cells array
;;;
;;;
;;;      a    b    c    d    e    f    g    h
;;;   =========================================
;;; 1 = 11 = 12 = 13 = 14 = 15 = 16 = 17 = 18 =
;;;   =========================================
;;; 2 = 21 = 22 = 23 = 24 = 25 = 26 = 27 = 28 =
;;;   =========================================
;;; 3 = 31 = 32 = 33 = 34 = 35 = 36 = 37 = 38 =
;;;   =========================================
;;; 4 = 41 = 42 = 43 = 44 = 45 = 46 = 47 = 48 =
;;;   =========================================
;;; 5 = 51 = 52 = 53 = 54 = 55 = 56 = 57 = 58 =
;;;   =========================================
;;; 6 = 61 = 62 = 63 = 64 = 65 = 66 = 67 = 68 =
;;;   =========================================
;;; 7 = 71 = 72 = 73 = 74 = 75 = 76 = 77 = 78 =
;;;   =========================================
;;; 8 = 81 = 82 = 83 = 84 = 85 = 86 = 87 = 88 =
;;;   =========================================
;;;
;;; fig 2: Conversion between alphanumeric and sequential square denotaion
;;;        Eg. 63 <==> c6; 11 <==> a1 
;;;
;;;
;;; 3 3 3 3 3 3 3 3 3 3
;;; 3 0 0 0 0 0 0 0 0 3
;;; 3 0 0 0 0 0 0 0 0 3
;;; 3 0 0 0 0 0 0 0 0 3
;;; 3 0 0 0 1 2 0 0 0 3
;;; 3 0 0 0 2 1 0 0 0 3
;;; 3 0 0 0 0 0 0 0 0 3
;;; 3 0 0 0 0 0 0 0 0 3
;;; 3 0 0 0 0 0 0 0 0 3
;;; 3 3 3 3 3 3 3 3 3 3
;;;
;;; fig 3: Initial board population
;;;
;;;
;;;     a b c d e f g h
;;;
;;; 1   . C A B B A C .
;;; 2   C X . . . . X C
;;; 3   A . . . . . . A
;;; 4   B . . . . . . B
;;; 5   B . . . . . . B
;;; 6   A . . . . . . A
;;; 7   C X . . . . X C
;;; 9   . C A B B A C .
;;;
;;; fig 4: Board names for edge squares

;;; PAIP 18.2 - Representation Choices (p. 601)
(deftype piece () `(integer ,empty ,outer))

(defun name-of (piece) (char ".@O?" piece))

(defun opponent (player) (if (eql player black) white black))

(deftype board () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

;;; PAIP 18.2 - Representation Choices (p. 603)
(defun copy-board (board)
  (copy-seq board))

;;; PAIP 18.2 - Representation Choices (p. 603)
(defun initial-board ()
  "Return a board, empty except for four pieced in the middle."
  ;; Boards are 100-element vectors, with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER. Initially
  ;; the 4 center squares are taken, the others empty.
  (let ((board (make-array 100 :element-type 'piece
			   :initial-element outer)))
    (dolist (square all-squares)
      (setf (bref board square) empty))
    (setf (bref board 44) white (bref board 45) black
	  (bref board 54) black (bref board 55) white)
    board))

;;; PAIP 18.7 - The Tournament Version of Othello (p. 624)
(defvar *board* (initial-board) "A copy of the game board")
(defvar *move-number* 1 "The number of the move to be played")
(defvar *clock* (make-array 3) "A copy of the game clock")

;;; PAIP 18.2 - Representation Choices (p. 603)
;;; PAIP 18.7 - The Tournament Version of Othello (p. 625)
(defun print-board (&optional (board *board*) clock)
  "Print a board, along with some statistics."
  ;; First print the header and the current score
  (format t "~2&    a b c d e f g h   [~c=~2a ~c=~2a (~@d)]"
          (name-of black) (count black board)
          (name-of white) (count white board)
          (count-difference black board))
  ;; Print the board itself
  (loop for row from 1 to 8 do
       (format t "~&  ~d " row)
       (loop for col from 1 to 8
	  for piece = (bref board (+ col (* 10 row)))
	  do (format t "~c " (name-of piece))))
  ;; Finally print the time remaining for each player
  (when clock
    (format t "  [~c=~a ~c=~a]~2&"
            (name-of black) (time-string (elt clock black))
            (name-of white) (time-string (elt clock white)))))

;;; PAIP 18.2 - Representation Choices (p. 603)
(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

;;; PAIP 18.7 - The Tournament Version of Othello (p. 626)
(defun time-string (time)
  "Return a string representing this internal time in min:secs."
  (multiple-value-bind (min sec)
      (floor (round time internal-time-units-per-second) 60)
    (format nil "~2d:~2,'0d" min sec)))

;;; PAIP 18.2 - Representation Choices (p. 604)
(defun valid-p (move)
  "Valid moves are number in the range 11-88 that end in 1-8."
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-p (move player board)
  "A legal move must be into an empty square, and it must
   flip at least one opponent piece."
  (and (eql (bref board move) empty)
       (some #'(lambda (dir) (would-flip? move player board dir))
	     all-directions)))

;;; PAIP 18.2 - Representation Choices (p. 604)
(defun make-move (move player board)
  "Update board to reflect move by player."
  ;; First make the move, then make any flips.
  (setf (bref board move) player)
  (dolist (dir all-directions)
    (make-flips move player board dir))
  board)

;;; PAIP 18.2 - Representation Choices (p. 605)
(defun make-flips (move player board dir)
  "Make any flips in the given direction."
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (let ((c-from move) (c-to bracketer) (f-dir dir))
	(when (< dir 0)
	  (setf c-from bracketer)
	  (setf c-to move)
	  (setf f-dir (* -1 dir)))
	(loop for c
	   from (+ c-from f-dir) by f-dir until (eql c c-to)
	   do (setf (bref board c) player))))))

;;; PAIP 18.2 - Representation Choices (p. 605)
(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?
   If so, return the square number of the bracketing piece."
  ;; A flip occours if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, braketed by
  ;; one of player's pieces.
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
	 (find-bracketing-piece (+ c dir) player board dir))))

;;; PAIP 18.2 - Representation Choices (p. 605)
(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond ((eql (bref board square) player) square)
	((eql (bref board square) (opponent player))
	 (find-bracketing-piece (+ square dir) player board dir))
	(t nil)))

;;; PAIP 18.2 - Representation Choices (p. 605)
;;; PAIP 18.7 - The Tournament Version of Othello (p. 624)
(defun reversi (bl-strategy wh-strategy &optional (print t) (minutes 30))
  "Play a game of Reversi. Return the score, where a positive
   difference means black (the first player) wins."
  (let ((board (initial-board))
	(clock (make-array (+ 1 (max black white))
			   :initial-element
			   (* minutes 60
			      internal-time-units-per-second))))
    (catch 'game-over
      (loop for *move-number* from 1
	 for player = black
	 then (next-to-play board player print)
	 for strategy = (if (eql player black)
			    bl-strategy
			    wh-strategy)
	 until (null player)
	 do (get-move strategy player board print clock))
      (when print
	(format t "~&The game is over. Final result:")
	(print-board board))
      (count-difference black board))))

;;; PAIP 18.2 - Representation Choices (p. 606)
(defun next-to-play (board previous-player print)
  "Compute the player to move next, or nil if nobady can move."
  (let ((opp (opponent previous-player)))
    (cond ((any-legal-move? opp board) opp)
	  ((any-legal-move? previous-player board)
	   (when print
	     (format t "~&~c has no moves and must pass."
		     (name-of opp)))
	   previous-player)
	  (t nil))))

;;; PAIP 18.2 - Representation Choices (p. 606)
(defun any-legal-move? (player board)
  "Does player have any legal moves in this position?"
  (some #'(lambda (move) (legal-p move player board))
	all-squares))

;;; PAIP 18.2 - Representation Choices (p. 607)
;;; PAIP 18.7 - The Tournament Version of Othello (p. 625)
(defun get-move (strategy player board print clock)
  "Call the player's strategy function to get move.
   Keep calling until a legal move is made."
  (when print (print-board board clock))
  (replace *clock* clock)
  (let* ((t0 (get-internal-real-time))
	 (move (funcall strategy player (replace *board* board)))
	 (t1 (get-internal-real-time)))
    (decf (elt clock player) (- t1 t0))
    (cond
      ((< (elt clock player) 0)
       (format t "~&~c has no time left and forteits." (name-of player))
       (throw 'game-over (if (eql player black) -64 64)))
      ((eq move 'resign)
       (throw 'game-over (if (eql player black) -64 64)))
      ((and (valid-p move) (legal-p move player board))
       (when print
	 (format t "~&~c moves to ~a." (name-of player) (88->h8 move)))
       (make-move move player board))
      (t (warn "illegal move: ~a" (88->h8 move))
	 (get-move strategy player board print clock)))))

;;; PAIP 18.2 - Representation Choices (p. 607)
;;; PAIP 18.7 - The Tournament Version of Othello (p. 622)
(defun human (player board)
  "A human player for the game of Reversi."
  (format t "~&~c to move ~a: " (name-of player)
	  (mapcar #'88->h8 (legal-moves player board)))
  (finish-output)
  (h8->88 (read)))

;;; PAIP 18.2 - Representation Choices (p. 607)
(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

;;; PAIP 18.2 - Representation Choices (p. 607)
(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  (loop for move in all-squares
     when (legal-p move player board) collect move))

;;; PAIP 18.7 - The Tournament Version of Othello (p. 622)
(let ((square-names
       (cross-product #'i-symb
		      '(? a b c d e f g h ?)
		      '(? 1 2 3 4 5 6 7 8 ?))))

  (defun h8->88 (str)
    "Convert from alphanumeric to numeric square notation."
    (or (position (string str) square-names :test #'string-equal)
	str))

  (defun 88->h8 (num)
    "Convert from numeric to alphanumeric square notation."
    (if (valid-p num)
	(elt square-names num)
	num)))

;;; PAIP 18.8 - Playing a Series of Games (p. 626)
;;; PAIP 18.8 - Playing a Series of Games (p. 628)
(defun reversi-series (strategy1 strategy2 n-pairs)
  "Play a series of 2*n-pairs games, swapping sides."
  (let ((scores
	 (loop repeat n-pairs
	    for random-state = (make-random-state)
	    collect (reversi strategy1 strategy2 nil)
	    do (setf *random-state* random-state)
	    collect (- (reversi strategy2 strategy1 nil)))))
    ;; Return the number of wins. (1/2 for a tie),
    ;; the total of the point differences, and the
    ;; scores themselves, all from strategy1's point of view.
    (values (+ (count-if #'plusp scores) ;;; this seams to be a bug, the function counts just the strategy1 wins.
	       (/ (count-if #'zerop scores) 2))
	    (apply #'+ scores)
	    scores)))

;;; PAIP 18.8 - Playing a Series of Games (p. 627)
(defun random-reversi-series (strategy1 strategy2
					n-pairs &optional (n-random 10))
  "Play a series of 2*n games, starting from a random position."
  (reversi-series
   (switch-strategies #'random-strategy n-random strategy1)
   (switch-strategies #'random-strategy n-random strategy2)
   n-pairs))

;;; PAIP 18.8 - Playing a Series of Games (p. 627)
(defun switch-strategies (strategy1 m strategy2)
  "Make a new strategy that plays strategy1 for m moves,
  then plays according to strategy2."
  #'(lambda (player board)
      (funcall (if (<= *move-number* m) strategy1 strategy2)
	       player board)))

;;; PAIP 18.8 - Playing a Series of Games (p. 628)
(defun round-robin (strategies n-pairs &optional
		    (n-random 10) (names strategies))
  "Play a tournament among the strategies.
  N-PAIRS = games each strategy plays as each color against
  each opponent. So with N strategies, a total of
  N*(N-1)*N-PAIRS games are played."
  (let* ((N (length strategies))
	 (totals (make-array N :initial-element 0))
	 (scores (make-array (list N N)
			     :initial-element 0)))
    ;; Play the games
    (dotimes (i N)
      (loop for j from (+ i 1) to (- N 1) do
	   (let* ((wins (random-reversi-series
			 (elt strategies i)
			 (elt strategies j)
			 n-pairs n-random))
		  (losses (- (* 2 n-pairs) wins)))
	     (incf (aref scores i j) wins)
	     (incf (aref scores j i) losses)
	     (incf (aref totals i) wins)
	     (incf (aref totals j) losses))))
    ;; Print the results
    (dotimes (i N)
      (format t "~&~a~20T ~4f: " (elt names i) (elt totals i))
      (dotimes (j N)
	(format t "~4f " (if (= i j) '---
			 (aref scores i j))))
      (format t "~&"))))

;;; PAIP 18.8 - Playing a Series of Games (p. 629)
(defun basic-mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

;;; PAIP 18.8 - Playing a Series of Games (p. 629, 630)
;;;
;;; round-robin test run 500 n-pairs
;;;
;;; REVERSI> (round-robin
;;; 	      (list (maximizer #'count-difference)
;;;    	    	    (maximizer #'basic-mobility)
;;; 		    (maximizer #'weighted-squares)
;;; 		    (maximizer #'modified-weighted-squares)
;;; 		    #'random-strategy)
;;; 	      500 6
;;; 	      '(count-difference mobility weighted modified-weighted random))
;;; COUNT-DIFFERENCE     1547.: ---  463. 172. 146. 767. 
;;; MOBILITY             1835.: 537. ---  266. 179. 854. 
;;; WEIGHTED             2774.: 829. 735. ---  390. 821. 
;;; MODIFIED-WEIGHTED    3162.: 855. 821. 610. ---  877. 
;;; RANDOM                683.: 234. 147. 179. 124. ---  
;;; NIL
;;;
;;; REVERSI> (round-robin
;;; 	      (list (alpha-beta-searcher 4 #'count-difference)
;;; 	 	    (alpha-beta-searcher 4 #'weighted-squares)
;;; 		    (alpha-beta-searcher 4 #'modified-weighted-squares)
;;; 		    #'random-strategy)
;;; 	      50 6
;;; 	      '(count-difference weighted modified-weighted random))
;;; COUNT-DIFFERENCE     115.: ---  14.5  6.0 94.5 
;;; WEIGHTED             217.: 85.5 ---  41.5 90.0 
;;; MODIFIED-WEIGHTED    253.: 94.0 58.5 ---  100. 
;;; RANDOM               15.5:  5.5 10.0  0.0 ---  
;;;
;;;REVERSI> (round-robin
;;;	     (list (alpha-beta-searcher 4 #'count-difference)
;;;	   	   (alpha-beta-searcher 4 #'weighted-squares)
;;;		   (alpha-beta-searcher 4 #'modified-weighted-squares)
;;;		   #'random-strategy)
;;;	     5 10
;;;	     '(count-difference weighted modified-weighted random))
;;; COUNT-DIFFERENCE     10.5: ---   1.5  0.0  9.0 
;;; WEIGHTED             21.0:  8.5 ---   3.0  9.5 
;;; MODIFIED-WEIGHTED    27.0: 10.0  7.0 ---  10.0 
;;; RANDOM                1.5:  1.0  0.5  0.0 ---  
;;; NIL
;;; REVERSI> (round-robin
;;; 	      (list (alpha-beta-searcher 4 #'count-difference)
;;; 	 	    (alpha-beta-searcher 4 #'weighted-squares)
;;; 		    (alpha-beta-searcher 6 #'modified-weighted-squares)
;;; 		    #'random-strategy)
;;; 	      5 10
;;; 	      '(count-difference weighted modified-weighted random))
;;; COUNT-DIFFERENCE     12.0: ---   2.0  0.0 10.0 
;;; WEIGHTED             17.5:  8.0 ---   1.5  8.0 
;;; MODIFIED-WEIGHTED    28.5: 10.0  8.5 ---  10.0 
;;; RANDOM                2.0:  0.0  2.0  0.0 ---  
;;; NIL
;;; REVERSI> (round-robin
;;; 	      (list (alpha-beta-searcher 4 #'count-difference)
;;; 		    (alpha-beta-searcher 4 #'weighted-squares)
;;; 		    (alpha-beta-searcher 8 #'modified-weighted-squares)
;;; 		    #'random-strategy)
;;; 	      5 10
;;; 	      '(count-difference weighted modified-weighted random))
;;; COUNT-DIFFERENCE     10.5: ---   0.5  0.0 10.0 
;;; WEIGHTED             20.0:  9.5 ---   1.0  9.5 
;;; MODIFIED-WEIGHTED    29.0: 10.0  9.0 ---  10.0 
;;; RANDOM                0.5:  0.0  0.5  0.0 ---  
;;; NIL



;;;
;;; Copyright (c) 1991 Peter Norvig
;;; Copyright (c) 2009 Roberto Corradini

;;; This file is part of the reversi program

;;; This pogram takes origin from code
;;; found in the famous PAIP book.
;;; Code from Paradigms of AI Programming

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

(in-package #:reversi)

(deftype piece () `(integer ,empty ,outer))

(defun name-of (piece) (char ".@O?" piece))
(defun opponent (player) (if (eql player black) white black))

(deftype board () '(simple-array piece (100)))

(defun bref (board square) (aref board square))
(defsetf bref (board square) (val)
  `(setf (aref ,board ,square) ,val))

(defvar *clock* (make-array 3) "A copy of the game clock")

(defun copy-board (board)
  (copy-seq board))

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

(defvar *board* (initial-board) "A copy of the game board")

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

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

(defun time-string (time)
  "Return a string representing this internal time in min:secs."
  (multiple-value-bind (min sec)
      (floor (round time internal-time-units-per-second) 60)
    (format nil "~2d:~2,'0d" min sec)))

(defun valid-p (move)
  "Valid moves are number in the range 11-88 that end in 1-8."
  (and (integerp move) (<= 11 move 88) (<= 1 (mod move 10) 8)))

(defun legal-p (move player board)
  "A legal move must be into an empty square, and it must
   flip at least one opponent piece."
  (and (eql (bref board move) empty)
       (some #'(lambda (dir) (would-flip? move player board dir))
	     all-directions)))

(defun make-move (move player board)
  "Update board to reflect move by player."
  ;; First make the move, then make any flips.
  (setf (bref board move) player)
  (dolist (dir all-directions)
    (make-flips move player board dir))
  board)

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

(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?
   If so, return the square number of the bracketing piece."
  ;; A flip occours if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, braketed by
  ;; one of player's pieces.
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
	 (find-bracketing-piece (+ c dir) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond ((eql (bref board square) player) square)
	((eql (bref board square) (opponent player))
	 (find-bracketing-piece (+ square dir) player board dir))
	(t nil)))

(defun reversi (bl-strategy wh-strategy &optional (print t))
  "Play a game of Reversi. Return the score, where a positive
   difference means black (the first player) wins."
  (let ((board (initial-board)))
    (loop for player = black
	 then (next-to-play board player print)
	 for strategy = (if (eql player black)
			    bl-strategy
			    wh-strategy)
	 until (null player)
	 do (get-move strategy player board print))
    (when print
      (format t "~&The game is over. Final result:")
      (print-board board))
    (count-difference black board)))

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

(defun any-legal-move? (player board)
  "Does player have any legal moves in this position?"
  (some #'(lambda (move) (legal-p move player board))
	all-squares))

(defun get-move (strategy player board print)
  "Call the player's strategy function to get move.
   Keep calling until a legal move is made."
  (when print (print-board board))
  (let ((move (funcall strategy player (copy-board board))))
    (cond
      ((and (valid-p move) (legal-p move player board))
       (when print
	 (format t "~&~c moves to ~d." (name-of player) move))
       (make-move move player board))
      (t (warn "illegal move: ~d" move)
	 (get-move strategy player board print)))))

(defun human (player board)
  "A human player for the game of Reversi."
  (declare (ignore board))
  (format t "~&~c to move: " (name-of player))
  (read))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  (loop for move in all-squares
     when (legal-p move player board) collect move))

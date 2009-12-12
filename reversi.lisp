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

(defvar *move-number* 1 "The number of the move to be played")

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

(defun human (player board)
  "A human player for the game of Reversi."
  (format t "~&~c to move ~a: " (name-of player)
	  (mapcar #'88->h8 (legal-moves player board)))
  (h8->88 (read)))

(defun random-strategy (player board)
  "Make any legal move."
  (random-elt (legal-moves player board)))

(defun legal-moves (player board)
  "Returns a list of legal moves for player"
  (loop for move in all-squares
     when (legal-p move player board) collect move))

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

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values"
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x) (funcall fn x y))
		       xlist))
	   ylist))

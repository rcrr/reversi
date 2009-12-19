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

(defun maximize-difference (player board)
  "A strategy that maximize the differences in pieces."
  (funcall (maximizer #'count-difference) player board))

(defun maximizer (eval-fn)
  "Return a strategy that will consider every legal move,
   apply ENVAL-FN to each resulting board, and choose
   the move for which EVAL-FN returns the best score.
   FN takes two arguments: the player-to-move and board."
  #'(lambda (player board)
      (let* ((moves (legal-moves player board))
	     (scores (mapcar #'(lambda (move)
				 (funcall
				  eval-fn
				  player
				  (make-move move player (copy-board board))))
			     moves))
	     (best (apply #'max scores)))
	(elt moves (position best scores)))))

;;; The reversi game board
;;;
;;;     a   b   c   d   e   f   g   h
;;;   =================================
;;; 1 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 2 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 3 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 4 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 5 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 6 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 7 =   =   =   =   =   =   =   =   =
;;;   =================================
;;; 8 =   =   =   =   =   =   =   =   =
;;;   =================================
;;;

(defparameter *weights*
  '#(0   0   0   0  0  0   0   0   0 0
     0 120 -20  20  5  5  20 -20 120 0
     0 -20 -40  -5 -5 -5  -5 -40 -20 0
     0  20  -5  15  3  3  15  -5  20 0
     0   5  -5   3  3  3   3  -5   5 0
     0   5  -5   3  3  3   3  -5   5 0
     0  20  -5  15  3  3  15  -5  20 0
     0 -20 -40  -5 -5 -5  -5 -40 -20 0
     0 120 -20  20  5  5  20 -20 120 0
     0   0   0   0  0  0   0   0   0 0))

(defun weighted-squares (player board)
  "Sum of the weights of player's squares minus opponents's."
  (let ((opp (opponent player)))
    (loop for i in all-squares
	 when (eql (bref board i) player)
	 sum (aref *weights* i)
	 when (eql (bref board i) opp)
	 sum (- (aref *weights* i)))))

(defun final-value (player board)
  "Is ths a win, loss, or a draw for player?"
  (case (signum (count-difference player board))
    (-1 losing-value)
    (0 0)
    (+1 winning-value)))

(defun minimax (player board ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
   searching PLY levels deep and backing up values."
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
	      (values best-val best-move))))))

(defun minimax-searcher (ply eval-fn)
  "A strategy that searches PLY levels and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
	  (minimax player board ply eval-fn)
	(declare (ignore value))
	move)))

(defun alpha-beta (player board achievable cutoff ply eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
   searching PLY levels deep and backing up values,
   using cutoff whenever possible."
  (if (= ply 0)
      (funcall eval-fn player board)
      (let ((moves (legal-moves player board)))
	(if (null moves)
	    (if (any-legal-move? (opponent player) board)
		(- (alpha-beta (opponent player) board
			       (- cutoff) (- achievable)
			       (- ply 1) eval-fn))
		(final-value player board))
	    (let ((best-move (first moves)))
	      (loop for move in moves do
		   (let* ((board2 (make-move move player
					     (copy-board board)))
			  (val (- (alpha-beta
				   (opponent player) board2
				   (- cutoff) (- achievable)
				   (- ply 1) eval-fn))))
		     (when (> val achievable)
		       (setf achievable val)
		       (setf best-move move)))
		   until (>= achievable cutoff))
	      (values achievable best-move))))))

(defun alpha-beta-searcher (depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  #'(lambda (player board)
      (multiple-value-bind (value move)
	  (alpha-beta player board losing-value winning-value
		      depth eval-fn)
	(declare (ignore value))
	move)))

(defun modified-weighted-squares (player board)
  "Like WEIGHTED-SQUARES, but don't take off for moving
   near an occupied corner."
  (let ((w (weighted-squares player board)))
    (dolist (corner '(11 18 81 88))
      (when (not (eql (bref board corner) empty))
	(dolist (c (neighbors corner))
	  (when (not (eql (bref board c) empty))
	    (incf w (* (- 5 (aref *weights* c))
		       (if (eql (bref board c) player)
			   +1 -1)))))))
    w))

(let ((neighbor-table (make-array 100 :initial-element nil)))
  ;; Initialize the nieghbor table
  (dolist (square all-squares)
    (dolist (dir all-directions)
      (if (valid-p (+ square dir))
	  (push (+ square dir)
		(aref neighbor-table square)))))

  (defun neighbors (square)
    "Return a list of all squares adjacent to a square."
    (aref neighbor-table square)))

;;;
;;; 18.9 - More Efficient Searching
;;;

;;;
;;; 18.10 - It Pays to Precycle
;;;

;;;
;;; 18.11 - Killer Moves
;;;

;;;
;;; 18.12 - Championship Programs: Iago and Bill
;;;

;;;
;;; Mobility - Section Begin
;;;

(defun mobility (player board)
  "Current mobility is the number of legal moves.
   Potential mobility is the number of blank squares
   adiacent to an opponent that are not legal moves.
   Returns current potential mobility for player."
  (let ((opp (opponent player))
	(current 0)			; player's current mobility
	(potential 0))			; player's potential mobility
    (dolist (square all-squares)
      (when (eql (bref board square) empty)
	(cond ((legal-p square player board)
	       (incf current))
	      ((some #'(lambda (sq) (eql (bref board sq) opp))
		     (neighbors square))
	       (incf potential)))))
    (values current (+ current potential))))

;;;
;;; Mobility - Section Begin
;;;

;;;
;;; Edge Stability - Section Begin
;;;

(defvar *edge-table* (make-array (expt 3 10))
  "Array of values to player-to-move for edge positions.")

(defvar *ply-boards*
  (apply #'vector (loop repeat 40 collect (initial-board))))

(defun edge-index (player board squares)
  "The index counts 1 for player; 2 for opponent,
   on each square--summed as a base 3 number."
  (let ((index 0))
    (dolist (sq squares)
      (setq index (+ (* index 3)
		     (cond ((eql (bref board sq) empty) 0)
			   ((eql (bref board sq) player) 1)
			   (t 2)))))
    index))

(defun edge-stability (player board)
  "Total edge evaluation for player to move on board"
  (loop for edge-list in edge-and-x-lists
       sum (aref *edge-table*
		 (edge-index player board edge-list))))

(defun init-edge-table ()
  "Initialize *edge-table*, starting from the empty board."
  ;; Initialize the static values
  (loop for n-pieces from 0 to 10 do
       (map-edge-n-pieces
	#'(lambda (board index)
	    (setf (aref *edge-table* index)
		  (static-edge-stability black board)))
	black (initial-board) n-pieces top-edge 0))
  ;; Now iterate five times trying to improve:
  (dotimes (i 5)
    ;; Do the indexes with most pieces first
    (loop for n-pieces from 9 downto 1 do
	 (map-edge-n-pieces
	  #'(lambda (board index)
	      (setf (aref *edge-table* index)
		    (possible-edge-moves-value
		     black board index)))
	  black (initial-board) n-pieces top-edge 0))))

(defun map-edge-n-pieces (fn player board n squares index)
  "Call fn on all edges with n pieces."
  ;; Index counts 1 for player; 2 for opponent
  (cond
    ((< (length squares) n) nil)
    ((null squares) (funcall fn board index))
    (t (let ((index3 (* 3 index))
	     (sq (first squares)))
	 (map-edge-n-pieces fn player board n (rest squares) index3)
	 (when (and (> n 0) (eql (bref board sq) empty))
	   (setf (bref board sq) player)
	   (map-edge-n-pieces fn player board (- n 1) (rest squares)
			      (+ 1 index3))
	   (setf (bref board sq) (opponent player))
	   (map-edge-n-pieces fn player board (- n 1) (rest squares)
			      (+ 2 index3))
	   (setf (bref board sq) empty))))))

(defun possible-edge-moves-value (player board index)
  "Consider all possible edge moves.
   Combine their values into a single number."
  (combine-edge-moves
   (cons
    (list 1.0 (aref *edge-table* index)) ;; no move
    (loop for sq in top-edge             ;; possible moves
       when (eql (bref board sq) empty)
       collect (possible-edge-move player board sq)))
   player))

(defun possible-edge-move (player board sq)
  "Return a (prob val) pair for a possible edge move."
  (let ((new-board (replace (aref *ply-boards* player) board)))
    (make-move sq player new-board)
    (list (edge-move-probability player board sq)
	  (- (aref *edge-table*
		   (edge-index (opponent player)
			       new-board top-edge))))))

(defun combine-edge-moves (possibilities player)
  "Combines the best moves."
  (let ((prob 1.0)
	(val 0.0)
	(fn (if (eql player black) #'> #'<)))
    (loop for pair in (sort possibilities fn :key #'second)
       while (>= prob 0.0)
       do (incf val (* prob (first pair) (second pair)))
       (decf prob (* prob (first pair))))
    (round val)))

(let ((corner/xsqs '((11 . 22) (18 . 27) (81 . 72) (88 . 77))))
  (defun corner-p (sq) (assoc sq corner/xsqs))
  (defun x-square-p (sq) (rassoc sq corner/xsqs))
  (defun x-square-for (corner) (cdr (assoc corner corner/xsqs)))
  (defun corner-for (xsq) (car (rassoc xsq corner/xsqs))))

(defun edge-move-probability (player board square)
  "What's the probability that player can move to this square?"
  (cond
    ((x-square-p square) .5)		;; X-squares
    ((legal-p square player board) 1.0) ;; immediate capture
    ((corner-p square) ;; move to corner depends on X-square
     (let ((x-sq (x-square-for square)))
       (cond
	 ((eql (bref board x-sq) empty) .1)
	 ((eql (bref board x-sq) player) 0.001)
	 (t .9))))
    (t (/ (aref
	   '#2A((.10 .4 .7)
		(.05 .3  *)
		(.01  *  *))
	   (count-edge-neighbors player board square)
	   (count-edge-neighbors (opponent player) board square))
	  (if (legal-p square (opponent player) board) 2 1)))))

(defun count-edge-neighbors (player board square)
  "Count the neighbors of this square occupied by player."
  (count-if #'(lambda (inc)
		(eql (bref board (+ square inc)) player))
	    '(+1 -1)))

(defparameter *static-edge-table*
  '#2A(					;stable semi-stable unstable
       (   *   0 -2000)			; X
       ( 700   *     *)			; corner
       (1200 200   -25)			; C
       (1000 200    75)			; A
       (1000 200    50)			; B
       (1000 200    50)			; B
       (1000 200    75)			; A
       (1200 200   -25)			; C
       ( 700   *     *)			; corner
       (   *   0 -2000)			; X
       ))

(defun static-edge-stability (player board)
  "Compute this edge's static stability."
  (loop for sq in top-edge
     for i from 0
     sum (cond
	   ((eql (bref board sq) empty) 0)
	   ((eql (bref board sq) player)
	    (aref *static-edge-table* i
		  (piece-stability board sq)))
	   (t (- (aref *static-edge-table* i
		       (piece-stability board sq)))))))

(let ((stable 0) (semi-stable 1) (unstable 2))  
  (defun piece-stability (board sq)
    (cond
      ((corner-p sq) stable)
      ((x-square-p sq)
       (if (eql (bref board (corner-for sq)) empty)
	   unstable semi-stable))
      (t (let* ((player (bref board sq))
		(opp (opponent player))
		(p1 (find player board :test-not #'eql
			  :start sq :end 19))
		(p2 (find player board :test-not #'eql
			  :start 11 :end sq
			  :from-end t)))
	   (cond
	     ;; unstable pieces can be captured immediately
	     ;; by playing in the empty square
	     ((or (and (eql p1 empty) (eql p2 opp))
		  (and (eql p2 empty) (eql p2 opp)))
	      unstable)
	     ;; semi-stable pieces might be captured
	     ((and (eql p1 opp) (eql p2 opp)
		   (find empty board :start 11 :end 19))
	      semi-stable)
	     ((and (eql p1 empty) (eql p2 empty))
	      semi-stable)
	     ;; stable pieces can never be captured
	     (t stable)))))))

;;; #. is a read macro. Is to be verified how it works under sbcl.
;;; (setf *edge-table* '#.*edge-table*)

;;;
;;; Edge Stability - Section End
;;;

;;;
;;; Combining the Factors - Section Begin
;;;

(defun iago-eval (player board)
  "Combine edge-stability, current mobility and
   potential mobility to arrive at an evaluation."
  ;; The three factors are multiplied by coefficients
  ;; that vary by move number:
  (let ((c-edg (+ 312000 (* 6240 *move-number*)))
	(c-cur (if (< *move-number* 25)
		   (+ 50000 (* 2000 *move-number*))
		   (+ 75000 (* 1000 *move-number*))))
	(c-pot 20000))
    (multiple-value-bind (p-cur p-pot)
	(mobility player board)
      (multiple-value-bind (o-cur o-pot)
	  (mobility (opponent player) board)
	;; Combine the three factors into one sum:
	(+ (round (* c-edg (edge-stability player board)) 32000)
	   (round (* c-cur (- p-cur o-cur)) (+ p-cur o-cur 2))
	   (round (* c-pot (- p-pot o-pot)) (+ p-pot o-pot 2)))))))

(defun iago (depth)
  "Use an approximation of Iago's evaluation function."
  (alpha-beta-searcher3 depth #'iago-eval))

;;;
;;; Combining the Factors - Section Begin
;;;

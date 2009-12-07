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

(defparameter *winning-value* most-positive-fixnum)
(defparameter *losing-value* most-negative-fixnum)

(defun final-value (player board)
  "Is ths a win, loss, or a draw for player?"
  (case (signum (count-difference player board))
    (-1 *losing-value*)
    (0 0)
    (+1 *winning-value*)))

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
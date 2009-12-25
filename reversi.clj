;;;
;;; Copyright (c) 2009 Roberto Corradini

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


(ns rcrr.reversi)

(defn name-of [piece]
  (cond (= piece 0) \.
	(= piece 1) \@
	(= piece 2) \O
	(= piece 3) \?
	true \E))

(defn opponent [player]
  (if (= player black) white black))

(defstruct board :squares)

(defn board-ref [board square]
  (aget (:squares board) square))

(defn board-set [board square val]
  (aset (:squares board) square val)
  board)

(defn create-board []
  (struct board (make-array Integer 100)))

(defn copy-board [board]
  (struct board (aclone (:squares board))))

(defn
  #^{:doc "Return a board, empty except for four pieced in the middle."}
  initial-board []
  ;; Boards are 100-element vectors, with elements 11-88 used,
  ;; and the others marked with the sentinel OUTER. Initially
  ;; the 4 center squares are taken, the others empty. 
  (with-local-vars [board (create-board)]
    (doseq [i (range 100)]
      (var-set board (board-set (var-get board) i outer)))
    (doseq [i (all-squares)]
      (var-set board (board-set (var-get board) i empty-square)))
    (var-get board)))

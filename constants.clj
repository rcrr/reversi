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

(def all-directions '(-11 -10 -9 -1 1 9 10 11))

(def
 #^{:doc "An empty square."}
 empty-square 0)

(def
 #^{:doc "A black piece."}
 black 1)

(def
 #^{:doc "A white piece."}
white 2)

(def
 #^{:doc "Marks squares outside the board."}
outer 3)

(def all-squares 
     (count (filter (fn [i] (<= 1(mod i 10) 8)) (range 11 89))))

(def winning-value
     (last (for [elt (iterate (fn [i] (* 10 i)) 1) :while (instance? Integer elt)] elt)))

(def losing-value (* -1 winning-value))

(def
 #^{:doc "The four edges (with their X-squares)"}
edge-and-x-lists
    '((22 11 12 13 14 15 16 17 18 27)
      (72 81 82 83 84 85 86 87 88 77)
      (22 11 21 31 41 51 61 71 81 72)
      (27 18 28 38 48 58 68 78 88 77)))

(def top-edge (first edge-and-x-lists))
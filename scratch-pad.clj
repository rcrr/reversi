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


(ns reversi)

(import '(rcrr.reversi.ui ReversiBoard BoardSquareKey SquareColor))
(def board (atom nil))
(reset! board (. ReversiBoard initDisplay))
(. @board setSquareColor (. BoardSquareKey A1) (. SquareColor WHITE))

(defn show-board [board swing-board]
  (doseq [row (range 1 9)]
    (doseq [col (range 1 9)]
      (let [piece (board-ref board (+ col (* 10 row)))]
	(println "piece: " piece)))))

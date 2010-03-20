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
	 "reversi/test-fixtures"
	 "reversi/strategies")
  (:use clojure.test)
  (:import (reversi GameOverException)))

(deftest minimax-test
  (is (empty? ()))
  (is (= (minimax white *fixt-board-b* 1 count-difference) [-4 33] ))
  (is (= (minimax white *fixt-board-b* 2 count-difference) [-7 33] ))
  (is (= (minimax white *fixt-board-b* 3 count-difference) [-2 33] ))
  (is (= (minimax white *fixt-board-b* 4 count-difference) [-9 33] ))
  (is (= (minimax white *fixt-board-b* 5 count-difference) [-4 33] ))
  (is (= (minimax white *fixt-board-b* 6 count-difference) [-11 33] )))




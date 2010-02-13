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
  (:require [clojure.contrib [pprint :as pprint]]))

(defn rand-elt
  "Return a random element of this seq"
  [s]
  (nth s (rand-int (count s))))


;;; The utility and correctess of these following three functions is not sure.

(defn
  #^{:doc "Concatenate a map and an append (conj) functions."}
  mappend [f seq]
  (apply conj (map f seq)))

(defn
  #^{:doc "Concatenate symbols or strings to form an interned symbol."}
  i-symb [& args]
  (intern *ns* (symbol (pprint/cl-format nil "~{~a~}" args))))

(defn
  #^{:doc "Return a list of all (f x y) values."}
  cross-product [f xlist ylist]
  (mappend (fn [y] (map (fn [x] (f x y))
			xlist))
	   ylist))

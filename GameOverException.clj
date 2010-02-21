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


;;; To compile the class run:
;;; user> (compile 'reversi.GameOverException)

(ns reversi.GameOverException
  (:gen-class
   :implements [clojure.lang.IDeref]
   :extends java.lang.Exception
   :state state
   :init init
   :constructors {[clojure.lang.PersistentArrayMap String] [String]}
   :methods [[getMap [] clojure.lang.PersistentArrayMap]
	     [setMap [clojure.lang.PersistentArrayMap] clojure.lang.PersistentArrayMap]])
  (:import [java.lang Exception]
	   [clojure.lang IDeref PersistentArrayMap]))

(defn -deref
  [this]
  @(.state this))

(defn -init [e-map message]
  [[message] (atom e-map)])

(defn -getMap
  [this]
  @(.state this))

(defn -setMap
  [this m]
  (reset! (.state this) m))

;;; try example structure.
(comment
  (defn test-exception []
    (try
     (throw (new reversi.GameOverException "Exception message ..."))
     (catch Exception e (println "exception e:" e))
     (finally (println "finally statement ..."))))
  )
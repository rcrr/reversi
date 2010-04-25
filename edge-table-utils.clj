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
	 "reversi/reversi"
	 "reversi/strategies")
  (:use clojure.test)
  (:require [clojure.contrib [duck-streams :as duck-streams]])
  (:require [clojure.contrib [pprint :as pprint]])
  (:require [clojure.contrib [seq-utils :as seq-utils]])
  (:require [clojure.contrib [fcase :as fcase]])
  (:require [clojure.contrib [math :as math]])
  (:require [clojure.contrib.generic [math-functions :as math-f]])
  (:import (reversi GameOverException))
  (:import (java.io PrintWriter BufferedReader)))

(def
 #^{:doc "Array of values to player-to-move for edge positions."}
 *edge-table* (make-array Integer (math/expt 3 10)))

(let [tmp-dir (new java.io.File "tmp")
      tmp-dir-exists (if (. tmp-dir exists)
		       true
		       (. tmp-dir mkdir))
      tmp-reversi-dir (new java.io.File tmp-dir "reversi")
      tmp-reversi-dir-exists (if (. tmp-reversi-dir exists)
			       true
			       (. tmp-reversi-dir mkdir))
      edge-table-file (new java.io.File tmp-reversi-dir "edge-table-clj.dat")]

  (defn
    #^{:doc "Persist (save to disk) the *edge-table*. The destination file
     is given as the function's parameter etf, or when missing is the default
     value: tmp/reversi/edge-table-clj.dat.
     The etf parameter must be of type java.io.File.
     The file format has the first line being a comment,
     the second one has the length of the *edge-table* array,
     and then there is a value per line.
     The file is a plain text human readable document."}
    persist-edge-table
    ([] (persist-edge-table edge-table-file))
    ([etf]
       (with-open [#^PrintWriter w (duck-streams/writer etf)]
	 (println (str "Writing *dge-table* to file: " (. etf getAbsolutePath)))
	 (.println w "# Written by persist-edge-table function, in edge-table-utils.clj file.")
	 (let [len (count *edge-table*)]
	   (.println w len)
	   (dotimes [i len]
	     (.print w (aget *edge-table* i))
	     (.println w))))))

  (defn
    #^{:doc "Retrieve (load from file) the edge table array
     from a given (or default) file, and return it."}
    retrieve-edge-table
    ([] (retrieve-edge-table edge-table-file))
    ([etf]
       (println (str "Reading edge-table from file: " (. etf getAbsolutePath)))
       (with-open [#^BufferedReader r (duck-streams/reader etf)]
	 (println (.readLine r))
	 (let [len (. Integer valueOf (.readLine r))
	       edge-table (make-array Integer len)]
	   (println (str "edge-table array length: " len))
	   (dotimes [i len]
	     (aset edge-table i (. Integer valueOf (.readLine r))))
	   edge-table))))

  (defn
    #^{:doc "Delete the edge table file."}
    delete-edge-table
    ([] (delete-edge-table edge-table-file))
    ([etf]
       (when (and (. etf exists) (. etf isFile))
	 (. etf delete))))

  (defn
    #^{:doc "Regenerate the *dge-table* array and persist it to a file."}
    regenerate-edge-table
    ([] (regenerate-edge-table edge-table-file))
    ([etf]
       (init-edge-table)
       (persist-edge-table etf)))

  (defn
    #^{:doc "Either load the edge-table from file, or recalculate it
     if there is no file available.
     Finally assign the loaded array to the special variable *edge-table*."}
    load-edge-table
    ([] (load-edge-table edge-table-file))
    ([etf]
      (if (not (and (. etf exists) (. etf isFile)))
	(regenerate-edge-table etf)
	(def *edge-table* (retrieve-edge-table etf))))))
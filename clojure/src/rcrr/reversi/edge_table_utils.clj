;;;
;;; edge_table_utils.clj
;;;
;;; Copyright (c) 2009, 2010, 2012 Roberto Corradini

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

(ns rcrr.reversi.edge-table-utils
  (:use rcrr.reversi.core
        [clojure.test :only (is are run-tests)])
  (:require [clojure.math.numeric-tower :as math]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [rcrr.reversi.test-fixtures :as fixt]))

(def
  #^{:doc "Array of values to player-to-move for edge positions."}
  ^:dynamic
  *edge-table* (make-array Long (math/expt 3 10)))

;;;
;;; Edge Stability - Section Begin
;;;

;;; OK
(defn
  #^{:doc "The index counts 1 for player; 2 for opponent,
           on each square--summed as a base 3 number."
     :test (fn []
             (are [p b squares idx] (= (edge-index p b squares) idx)
                  white fixt/board-black-has-to-pass top-edge 50816
                  black fixt/board-black-has-to-pass top-edge 35290
                  white fixt/board-end-game-x top-edge 29564
                  black fixt/board-end-game-x top-edge 59008
                  white (initial-board) top-edge 0
                  white fixt/board-edge-index top-edge 59048
                  black fixt/board-edge-index top-edge 29524))}
  edge-index [player board squares]
  (loop [index 0
         sqs squares]
    (if (empty? sqs)
      index
      (recur
        (+ (* index 3)
           (cond (= (board-ref board (first sqs)) empty-square) 0
                 (= (board-ref board (first sqs)) player) 1
                 true 2))
        (rest sqs)))))

;;; OK
(defn
  #^{:doc "Total edge evaluation for player to move on board"
     :test (fn []
             (binding [*edge-table* (make-array Long (math/expt 3 10))]
               (dotimes [i (count *edge-table*)] (aset *edge-table* i i))
               (are [player es]
                    (= (edge-stability player fixt/board-black-has-to-pass) es)
                    white 192977
                    black 135868)))}
  edge-stability [player board]
  (reduce + (for [edge-list edge-and-x-lists]
              (aget *edge-table* (edge-index player board edge-list)))))

;;; OK
(defn
  #^{:doc "Call fun on all edges with n pieces."
     :test (fn []
             (binding [*edge-table* (make-array Long (math/expt 3 10))]
               (dotimes [i (count *edge-table*)] (aset *edge-table* i i))
               (are [player index]
                    (=
                      (map-edge-n-pieces
                        (fn [b i] nil)
                        player fixt/board-black-has-to-pass 1 '(11 12 13 14) index)
                      nil)
                    white 10
                    white 300)))}
  map-edge-n-pieces [fun player board n squares index]
  ;; Index counts 1 for player; 2 for opponent
  (cond
    (< (count squares) n) nil
    (empty? squares) (fun board index)
    true (let [index3 (* 3 index)
               sq (first squares)]
           (map-edge-n-pieces fun player board n (rest squares) index3)
           (when (and (> n 0) (== (board-ref board sq) empty-square))
             (map-edge-n-pieces fun player
                                (board-set board sq player)
                                (- n 1) (rest squares)
                                (+ 1 index3))
             (map-edge-n-pieces fun player
                                (board-set board sq (opponent player))
                                (- n 1) (rest squares)
                                (+ 2 index3))
             board))))

;;; OK
(defn
  #^{:doc "Combines the best moves."
     :test (fn []
             (are [poss player value] (= (combine-edge-moves poss player) value)
                  '((1.0 5800) (0.5 5800)) black 5800
                  '((1.0 5800) (0.5 5800)) white 5800
                  '((1.0 2075) (0.005 4000)) black 2085
                  '((1.0 2075) (0.005 4000)) white 2075))}
  combine-edge-moves [possibilities player]
  (let [comparator-fn (if (== player black) > <)]
    (loop [pairs (sort-by second comparator-fn possibilities)
           prob 1.0
           val 0.0]
      (if (and (not (empty? pairs)) (>= prob 0.0))
        (let [pair (first pairs)]
          (recur
            (rest pairs)
            (- prob (* prob (first pair)))
            (+ val (* prob (first pair) (second pair)))))
        (int (math/round val))))))

;;; OK
(let [corner_xsqs [{:c 11 :x 22} {:c 18 :x 27} {:c 81 :x 72} {:c 88 :x 77}]]
  (defn
    #^{:doc "Is the square a corner? If yes the corner map is returned, otherwise nil."
       :test (fn []
               (is (= (corner? 11) {:c 11 :x 22}))
               (are [sq] (if (corner? sq) true false)
                    11 18 81 88)
               (are [sq] (not (if (corner? sq) true false))
                    0 10 12 21 22 68 100))}
    corner? [sq] (first (for [cx corner_xsqs :when (== (:c cx) sq)] cx)))
  (defn
    #^{:doc "Is the square an x square? If yes the corner map is returned, otherwise nil."
       :test (fn []
               (is (= (x-square? 77) {:c 88 :x 77}))
               (are [sq] (if (x-square? sq) true false)
                    22 27 72 77)
               (are [sq] (not (if (x-square? sq) true false))
                    0 10 12 21 23 68 100))}
    x-square? [sq] (first (for [cx corner_xsqs :when (== (:x cx) sq)] cx)))
  (defn
    #^{:doc "Returns the x-square's corner, otherwise nil."
       :test (fn []
               (are [corner x-sq] (== (x-square-for corner) x-sq)
                    11 22, 18 27, 81 72, 88 77)
               (are [corner] (= (x-square-for corner) nil)
                    0 10 12 21 22 67))}
    x-square-for [corner] (first (for [cx corner_xsqs :when (== (:c cx) corner)] (:x cx))))
  (defn
    #^{:doc "Returns the corner's x-square, otherwise nil."
       :test (fn []
               (are [corner x-sq] (== (corner-for x-sq) corner)
                    11 22, 18 27, 81 72, 88 77)
               (are [x-sq] (= (corner-for x-sq) nil)
                    0 10 11 21 66 100))}
    corner-for [xsq] (first (for [cx corner_xsqs :when (== (:x cx) xsq)] (:c cx)))))

;;; OK
(defn
  #^{:doc "Count the neighbors of this square occupied by player."
     :test (fn []
             (are [p sq n] (= (count-edge-neighbors
                                p fixt/board-black-has-to-pass sq) n)
                  black 13 2
                  white 13 0
                  black 15 1
                  white 15 1
                  black 17 0
                  white 17 1
                  black 18 0
                  white 18 0))}
  count-edge-neighbors [player board square]
  (count
    (filter
      (fn [inc] (= (board-ref board (+ square inc)) player)) [+1 -1])))

;;; OK
(defn
  #^{:doc "What's the probability that player can move to this square?"
     :test (fn []
             (is (= (for [sq all-squares]
                      (edge-move-probability white fixt/board-black-has-to-pass sq))
                    '(0.9 0.05 1.0 0.1 1.0 0.1 1.0 1.0 0.4 0.5 0.7 0.7 0.7 0.7 0.5
                          0.4 1.0 0.05 0.3 0.3 0.3 0.3 0.3 0.05 1.0 0.05 0.7 0.01 0.3 
                          0.01 0.01 0.05 1.0 0.05 0.7 0.01 0.3 0.01 0.01 0.05 1.0 0.05 
                          0.7 0.3 0.3 0.7 0.01 0.4 1.0 0.5 0.7 0.3 0.7 0.7 0.5 1.0 0.9 
                          0.01 0.01 0.01 0.01 0.3 0.01 0.9))))}
  edge-move-probability [player board square]
  (cond
    (x-square? square) 0.5        ; X-squares
    (legal? square player board) 1.0    ; immediate capture
    (corner? square)          ; move to corner depends on X-square
    (let [x-sq (x-square-for square)]
      (cond
        (== (board-ref board x-sq) empty-square) 0.1
        (== (board-ref board x-sq) player) 0.001
        true 0.9))
    true (/ (aget
              (to-array-2d [[0.10 0.40 0.70]
                            [0.05 0.30   'x]
                            [0.01   'x   'x]])
              (count-edge-neighbors player board square)
              (count-edge-neighbors (opponent player) board square))
            (if (legal? square (opponent player) board) 2 1))))

;;; OK
(def ^:dynamic *static-edge-table*
  (to-array-2d [
                [  'x   0 -2000]    ; X
                [ 700  'x    'x]    ; corner
                [1200 200   -25]    ; C
                [1000 200    75]    ; A
                [1000 200    50]    ; B
                [1000 200    50]    ; B
                [1000 200    75]    ; A
                [1200 200   -25]    ; C
                [ 700  'x    'x]    ; corner
                [  'x   0 -2000]    ; X
                ]))

;;; OK
(let [stable 0
      semi-stable 1
      unstable 2]
  (defn
    #^{:doc "Computes piece stability. It works only applyed to the top edge.
             Returns a ten values sequence containing the piece stability for the edge.
             Values are: 0 for stable, 1 for semi-stable, and 2 for unstable."
       :test (fn []
               (are [board ps] (= (for [sq top-edge] (piece-stability board sq)) ps)
                    fixt/board-black-has-to-pass '(1 0 2 1 1 0 1 0 0 2)
                    (initial-board) '(2 0 0 0 0 0 0 0 0 2)
                    fixt/board-edge-index '(1 0 0 0 0 0 0 0 0 1)))}
    piece-stability [board sq]
    (cond
      (corner? sq) stable
      (x-square? sq) (if (= (board-ref board (corner-for sq)) empty-square)
                       unstable semi-stable)
      true (let [player (board-ref board sq)
                 opp (opponent player)
                 p1 (first (filter #(not (== player %)) (subvec board sq 19)))
                 p2 (first (filter #(not (== player %)) (reverse (subvec board 11 sq))))]
             (cond
               ;; unstable pieces can be captured immediately
               ;; by playing in the empty square
               (or (and (= p1 empty-square) (= p2 opp))
                   (and (= p2 empty-square) (= p1 opp)))
               unstable
               ;; semi-stable pieces might be captured
               (and (= p1 opp) (= p2 opp)
                    (first (filter #(== empty-square %) (subvec board 11 19))))
               semi-stable
               (and (= p1 empty-square) (= p2 empty-square))
               semi-stable
               ;; stable pieces can never be captured
               true stable)))))

;;; OK
(defn
  #^{:doc "Compute this edge's static stability."
     :test (fn []
             (are [player board ses] (= (static-edge-stability player board) ses)
                  black fixt/board-edge-index 7800
                  white fixt/board-edge-index -7800
                  black (initial-board) 0
                  white (initial-board) 0
                  black fixt/board-black-has-to-pass -2725))}
  static-edge-stability [player board]
  (loop [squares top-edge
         i 0
         sum 0]
    (if (empty? squares)
      sum
      (let [sq (first squares)
            p (board-ref board sq)
            x (aget *static-edge-table* i (piece-stability board sq))]
        (recur
          (rest squares)
          (inc i)
          (+ sum (cond
                   (= p empty-square) 0
                   (= p player) x
                   true (- x))))))))

;;; OK
(defn
  #^{:doc "Return a (prob val) pair for a possible edge move."
     :test (fn []
             (binding [*edge-table* (make-array Long (math/expt 3 10))]
               (dotimes [i (count *edge-table*)] (aset *edge-table* i i))
               (are [player sq pem]
                    (=
                      (possible-edge-move player fixt/board-black-has-to-pass sq)
                      pem)
                    black 71 '(0.025 -50816)
                    white 13 '(1.0 -38935))))}
  possible-edge-move [player board sq]
  (let [new-board (make-move sq player board)]
    (list (edge-move-probability player board sq)
          (- (aget *edge-table*
                   (edge-index (opponent player)
                               new-board top-edge))))))

;;; OK
(defn
  #^{:doc "Consider all possible edge moves.
           Combine their values into a single number."
     :test (fn []
             (binding [*edge-table* (make-array Long (math/expt 3 10))]
               (dotimes [i (count *edge-table*)] (aset *edge-table* i i))
               (are [player index pem]
                    (=
                      (possible-edge-moves-value player fixt/board-black-has-to-pass index)
                      pem)
                    white 13 -38935)))}
  possible-edge-moves-value [player board index]
  (combine-edge-moves
    (cons
      (list 1.0 (aget *edge-table* index)) ;; no move
      (for [sq top-edge :when (== (board-ref board sq) empty-square)]
        (possible-edge-move player board sq)))
    player))

;;; OK
(defn
  #^{:doc "Initialize *edge-table*, starting from the empty board."}
  init-edge-table []
  ;; Initialize to zero the array
  (dotimes [i (count *edge-table*)] (aset *edge-table* i 0))
  ;; Initialize the static values
  (doseq [n-pieces (range 0 11)]
    (map-edge-n-pieces
      (fn [board index]
        (aset *edge-table* index
              (static-edge-stability black board)))
      black (initial-board) n-pieces top-edge 0))
  (dotimes [i 5]
    (doseq [n-pieces (reverse (range 1 10))]
      (map-edge-n-pieces
        (fn [board index]
          (aset *edge-table* index
                (possible-edge-moves-value black board index)))
        black (initial-board) n-pieces top-edge 0))))

;;;
;;; Edge Stability - Section End
;;;

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
      (with-open [w (io/writer etf)]
                 (println (str "Writing *dge-table* to file: " (. etf getAbsolutePath)))
                 (pprint/cl-format w "# Written by persist-edge-table function, in edge_table_utils.clj file.~%")
                 (let [len (count *edge-table*)]
                   (pprint/cl-format w "~d~%" len)
                   (dotimes [i len]
                     (pprint/cl-format w "~d~%" (aget *edge-table* i)))))))
  
  (defn
    #^{:doc "Retrieve (load from file) the edge table array
             from a given (or default) file, and return it."}
    retrieve-edge-table
    ([] (retrieve-edge-table edge-table-file))
    ([etf]
      (println (str "Reading edge-table from file: " (. etf getAbsolutePath)))
      (with-open [r (io/reader (io/file etf))]
                 (println (.readLine r))
                 (let [len (. Long valueOf (.readLine r))
                       edge-table (make-array Long len)]
                   (println (str "edge-table array length: " len))
                   (dotimes [i len]
                     (aset edge-table i (. Long valueOf (.readLine r))))
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
        (def ^:dynamic *edge-table* (retrieve-edge-table etf)))))
  )


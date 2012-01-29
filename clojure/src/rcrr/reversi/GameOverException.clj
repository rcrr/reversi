(ns rcrr.reversi.GameOverException
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
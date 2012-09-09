(defproject rcrr.reversi "1.0.0-SNAPSHOT"
            :description "Reversi board program written in clojure"
            :dependencies [[org.clojure/clojure "1.4.0"]
                           [org.clojure/math.numeric-tower "0.0.1"]
                           [org.clojure/algo.generic "0.1.0"]]
            :aot [rcrr.reversi.GameOverException])

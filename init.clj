(in-ns 'reversi)

(clojure.core/load "reversi/auxfns")
(clojure.core/load "reversi/constants")
(clojure.core/load "reversi/strategies")
(clojure.core/load "reversi/reversi")
(clojure.core/load "reversi/test-fixtures")
(clojure.core/load "reversi/strategies-test")

(run-tests 'reversi)


(ns reversi)

;;; Not thread safe!
(let [counter (to-array [0])]
  (defn new-id [] (aset counter 0 (inc (aget counter 0))))
  (defn reset-id [] (aset counter 0 0))
  (defn set-id [x] (aset counter 0 x)))


(defn make-100-ids [id-fun]
  (doall (take 100 (repeatedly id-fun))))

(defn test-c [nthreads id-fun]
  (reset-id)
  (apply max (apply concat
    (apply pcalls (repeat nthreads #(make-100-ids id-fun))))))

(test-c 100 new-id)

;;; Thread safe but slow
(let [counter (ref 0)]
  (defn new-id [] (dosync (ref-set counter (inc @counter))))
  (defn reset-id [] (dosync (ref-set counter 0)))
  (defn set-id [x] (dosync (ref-set counter x))))

;;; Best solution
(let [counter (atom 0)]
  (defn new-id [] (swap! counter inc))
  (defn reset-id [] (reset! counter 0))
  (defn set-id [x] (reset! counter x))
  (defn get-id [] @counter))

;;; This is a board "template"
(def *bb*
     [3 3 3 3 3 3 3 3 3 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 0 0 0 0 0 0 0 0 3
      3 3 3 3 3 3 3 3 3 3])

;;; loop example
(defn
  my-loop [x]
  (if (empty? x)
      nil
      (loop [coll x
	     index 0
	     max (first x)
	     max-index 0]
	(if (empty? coll)
	  max-index
	  (let [val (first coll)
		is-max (if (> val max) true false)]
	    (recur (rest coll)
		   (inc index)
		   (if is-max val max)
		   (if is-max index max-index)))))))


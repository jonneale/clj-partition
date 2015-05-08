(ns clj-partition.core
  (:use [criterium.core]))

(def data
  [["first million numbers" (range 1000000)]
   ["infinite seq" (iterate inc 1)]
   ["empty vec" []]])

(def some-n-values
  [1
   100
   100000
   Integer/MAX_VALUE
   0
   -1
   Integer/MIN_VALUE
   ])


(def steps
  [0
   1
   100
   100000
   Integer/MAX_VALUE
   0
   -1
   Integer/MIN_VALUE])

  (defn partition-gt
    [n step coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (doall (take n s))]
         (when (> n (count p))
           (cons p (partition n step (nthrest s step))))))))

  (defn partition-max
    [n step coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (doall (take n s))]
         (when (= n (count p))
           (cons p (partition n (max step 1) (nthrest s step))))))))

  (defn partition-assert
    [n step coll]
    (assert pos? step)
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (doall (take n s))]
         (when (= n (count p))
           (cons p (partition n step (nthrest s step))))))))

  (defn partition-raw
    [n step coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (let [p (doall (take n s))]
         (when (= n (count p))
           (cons p (partition n step (nthrest s step))))))))

(defn test-fn
  "DON'T CALL ME WITH PARTITION-RAW! I WILL NEVER EXIT!"
  [partition-fn]
  (bench
   (do
     (doall
      (for [[data-description d] data
            n     some-n-values
            s     steps]
        (do
          (take 100000000 (partition-fn n s d)))))
     nil)))

(comment (test-fn parition-assert)
         (test-fn parition-gt)
         (test-fn parition-max))

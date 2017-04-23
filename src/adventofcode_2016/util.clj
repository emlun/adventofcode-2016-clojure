(ns adventofcode-2016.util
  (:require [clojure.test :refer [is]])
)

(declare count-filter)

(defn abs
  "absolute value"
  { :test #(assert (= 1 (abs 1) (abs -1))) }
  [n]
  { :pre [(number? n)] :post [(number? %)]}
  (max n (-' n))
)

(defn count=
  "([value] [value coll])
  Shortcut for (count (filter #(= value %) coll)). Returns a curried function if called without coll."
  {
   :test #(do
            (assert (= 2 (count= 5 [1 2 5 3 5 7])))
            (assert (= [1 2] (map (count= 5) [[1 5] [5 2 5]])))
            )
  }
  ([ value ]
    (fn [ coll ] (count-filter #(= value %) coll)))

  ([ value coll ]
    ((count= value) coll))
)

(defn count-filter
  "([pred] [pred coll])
  Shortcut for (count (filter pred coll)). Returns a curried function if called without coll."
  {
   :test (fn []
           (assert (= 2 (count-filter #(= 5 %) [1 2 5 3 5 7])))
           (assert (= [1 2] (map (count-filter #(= 5 %)) [[1 5] [5 2 5]])))
           )
  }
  ([ pred coll ]
    ((count-filter pred) coll))

  ([ pred ]
    (fn [ coll ] (count (filter pred coll))))
)

(defn first-recurrence
  { :test #(assert (= 5 (first-recurrence [1 5 2 5 2]))) }

  ( [future]
    { :pre (seq? future) }
      (first-recurrence #{} future)
  )

  ( [history [present & future]]
    { :pre [(or (seq? future) (nil? future)) (coll? history)] }
      (if (nil? present)
        nil
        (if (contains? history present)
          present
          (recur (conj history present) future)
        )
      )
  )
)

(defn flip
  "Apply f and its following arguments with the first two arguments flipped"
  {
   :test (fn []
            (assert (= (flip - 5 1) (- 1 5)))
            (assert (= (flip - 5 1 3) (- 1 5 3)))
            (assert (= (flip - 5 1 3 3) (- 1 5 3 3)))
            (assert (= (flip map [1 2 3] #(+ 3 %)) (map #(+ 3 %) [1 2 3])))
            )
   }
  [f a b & more]
  (apply f (cons b (cons a more))))

(defn splits
  { :test #(do
             (is (= (splits ()) [ [() ()] ]))
             (is (= (splits [0]) [ [() [0]] [[0] ()] ]))
             (is (= (splits [0 1]) [ [() [0 1]] [[0] [1]] [[0 1] ()]]))
             (is (= (splits [0 1 2]) [ [() [0 1 2]] [[0] [1 2]] [[0 1] [2]] [[0 1 2] ()]]))
             (is (= (first (first (splits (range 1e32)))) ()))
             ) }
  [coll]
    (lazy-cat
      (take-while #(not (= nil (second %)))
        (iterate (fn [ [prefix [middle & suffix]] ]
                   (if (nil? middle)
                     nil
                     [(lazy-cat prefix [middle]) suffix]
                   )
                 )
                 [() coll]
        )
      )
      (if (empty? coll)
        []
        [[coll ()]]
      )
    )
)

(defn transpose
  "Transform seq [[a1 a2 ...] [b1 b2 ...] ...] to seq [[a1 b1 ...] [a2 b2 ...] ...]"
  {
   :test #(do
            (assert (= (transpose []) (transpose [[]]) (transpose [[] []]) []), "Transpose of empty input is empty")
            (assert (= (transpose [[1]]) [[1]]), "Transpose of single element is identity function")
            (assert (= (transpose [[1 2]]) [[1] [2]]), "Transpose of two-column row is two one-column rows")
            (assert (= (transpose [[1 2] [3 4]]) [[1 3] [2 4]]), "Transpose of 2x2 matrix is matrix transpose")
            (assert (= (transpose [[1 2] []]) []), "Rows are truncated to shortest row")
            (assert (= (transpose [[1 2] [3]]) [[1 3]]), "Rows are truncated to shortest row")
            (assert (= (transpose (transpose [[1 2] [3 4]])) [[1 2] [3 4]]), "Transpose is its own inverse")
            )
   }
  [seqs]
  (if (seq seqs)
    (apply map (cons vector seqs))
    ()
  ))

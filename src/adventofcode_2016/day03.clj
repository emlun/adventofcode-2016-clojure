(ns adventofcode-2016.day03
  (:require clojure.string)
  (:require [adventofcode-2016.util :refer (flip grouped)])
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
            )
   }
  [seqs]
  (if (seq seqs)
    (apply map (cons vector seqs))
    ()
  ))

(defn possible-triangles [triangles]
  (->> triangles
    (map sort)
    (filter (fn [[a b c]] (> (+ a b) c)))
  ))

(defn to-numbers [line]
  (->> line
    (clojure.string/trim)
    (flip clojure.string/split #"\s+")
    (map read-string)
  ))


(defn solve-a [lines]
  (->> lines
    (map to-numbers)
    (possible-triangles)
  ))

(defn solve-b [lines]
  (->> lines
    (map to-numbers)
    (grouped 3)
    (mapcat transpose)
    (possible-triangles)
  ))

(defn solve
  "solve day 3 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solve-a] ["Subproblem B:" solve-b]] ]
    (println problem (str (count (solver input-lines))))
  )
)

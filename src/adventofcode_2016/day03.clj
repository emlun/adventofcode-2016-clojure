(ns adventofcode-2016.day03
  (:require clojure.string)
  (:require [adventofcode-2016.util :refer (flip transpose)])
)

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
    (partition 3)
    (mapcat transpose)
    (possible-triangles)
  ))

(defn solve
  "solve day 3 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solve-a] ["Subproblem B:" solve-b]] ]
    (println problem (count (solver input-lines)))
  )
)

(ns adventofcode-2016.day06
  (:require clojure.string)
  (:require [adventofcode-2016.util :refer (count= transpose)])
)

(defn solve-base [lines]
  (->> lines
    (transpose)
    (map (fn [column]
           (sort-by
             #(count= % column)
             column
           )))
  ))

(defn solve-a [lines]
  (map last (solve-base lines)))

(defn solve-b [lines]
  (map first (solve-base lines)))

(defn solve
  "solve day 6 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solve-a] ["Subproblem B:" solve-b]] ]
    (println problem (apply str (solver input-lines)))
  )
)

(ns adventofcode-2016.day03
  (:require clojure.string)
)

(defn possible-triangles [triangles]
  (->> triangles
    (map sort)
    (filter (fn [[a b c]] (> (+ a b) c)))
  ))

(defn to-numbers [line]
  (map read-string (clojure.string/split (clojure.string/trim line) #"\s+")))

(defn solve-a [lines]
  (possible-triangles (map to-numbers lines)))

(defn solve-b [lines]
  (->> lines
    (map to-numbers)
    (possible-triangles)
  ))

(defn solve
  "solve day 3 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solve-a] ["Subproblem B:" solve-b]] ]
    (println problem (apply str (map name (solver input-lines))))
  )
)

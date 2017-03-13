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

(defn solveA [lines]
  (possible-triangles (map to-numbers lines)))

(defn solveB [lines]
  (->> lines
    (map to-numbers)
    (possible-triangles)
  ))

(defn solve
  "solve day 3 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solveA] ["Subproblem B:" solveB]] ]
    (println problem (apply str (map name (solver input-lines))))
  )
)

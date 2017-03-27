(ns adventofcode-2016.day03
  (:require clojure.string)
)

(defn possible-triangles [triangles]
  (->> triangles
    (map sort)
    (filter (fn [[a b c]] (> (+ a b) c)))
  ))

(defn grouped
  "Return elements in x in groups of n"
  {
    :test #(do
             (assert (= [] (grouped 5 [])))
             (assert (= [] (grouped 5 [])))
             (assert (= [[0] [1] [2]] (grouped 1 [0 1 2])))
             (assert (= [[0]] (grouped 3 [0])))
             (assert (= [[0 1]] (grouped 3 [0 1])))
             (assert (= [[0 1 2]] (grouped 3 [0 1 2])))
             (assert (= [[0 1 2] [3]] (grouped 3 [0 1 2 3])))
             )
  }
  [n x]
  (loop [result [] remain x]
    (cond
      (= n (count result))
        (cons result (grouped n remain))
      (seq remain)
        (recur (conj result (first remain)) (rest remain))
      (seq result)
        (conj [] result)
      :else []
    )))

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

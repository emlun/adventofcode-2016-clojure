(ns adventofcode-2016.day04
  (:require clojure.string)
)

(defn subsolve-a [ line ]
  (let [
        dash-groups (clojure.string/split (clojure.string/trim line) #"-")
        last-groups (clojure.string/split (last dash-groups) #"\[")

        room-name (apply str (butlast dash-groups))
        sector-id (read-string (first last-groups))
        checksum (apply str (butlast (second last-groups)))

        expected-checksum (->> room-name
                            (set)
                            (flip disj \-)
                            (sort-by (fn [c] ; Sort by descending order of frequency, with natural order as tiebreaker
                                       [ (- (count (filter #(= % c) room-name))), c ]
                                       ))
                            (take 5)
                            (apply str)
                            )
       ]
    {
      :valid (= checksum expected-checksum)
      :id sector-id
    }
  ))

(defn solve-a [ input-lines ]
  (->> input-lines
    (map subsolve-a)
    (filter :valid)
    (map :id)
    (apply +)
    ))

(defn solve-b [])

(defn solve
  "solve day 4 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solve-a] ["Subproblem B:" solve-b]] ]
    (println problem (solver input-lines))
  )
)

(ns adventofcode-2016.day01
  (:gen-class)
  (:require [clojure.string])
  (:require [adventofcode-2016.util :refer (abs first-recurrence)])
)

(defn to-turn [word]
  (case (first word)
    \L :left
    \R :right
  )
)

(defn rotate
  [{ :keys [x y] :as dir } turn]
  { :pre [(number? x) (number? y) (keyword? turn)] }
  (case turn
    :left { :x y, :y (- x) }
    :forward dir
    :right  { :x (- y), :y x }
  )
)

(defn walk [pos dir dist]
  (merge-with (fn [p d] (+ p (* d dist))) pos dir )
)

(defn walk-step [state step]
  (let [ new-dir (rotate (:dir state) (:turn step)) ]
    {
      :dir new-dir
      :pos (walk (:pos state) new-dir (:dist step) )
    }
  )
)

(defn extract-steps [line]
  (map
    (fn [word]
      { :turn (to-turn word)
        :dist (read-string (apply str (rest word))) }
    )
    (clojure.string/split (clojure.string/trim line) #"\s+")
  )
)

(defn find-bunny-hq-a
  [steps start-state]
  (:pos (reduce walk-step start-state steps))
)

(defn expand-step [step]
  "Expand all steps on the form DN (D: direction; N: length) with one D1 step and N-1 forward steps"
  (cons (assoc step :dist 1)
        (repeat (dec (:dist step)) { :turn :forward, :dist 1 })
  )
)
(defn find-bunny-hq-b
  [steps start-state]
  (let [ steps (mapcat expand-step steps)
         states (reductions walk-step start-state steps)
       ]
    (first-recurrence (map :pos states))
  )
)

(defn solve
  "solve day 1 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solve] [["Subproblem A:" find-bunny-hq-a] ["Subproblem B:" find-bunny-hq-b]] ]
    (def bunny-hq  (solve (mapcat extract-steps input-lines) { :pos { :x 0, :y 0 } :dir { :x 1, :y 0 } }))
    (println problem)
    (println "Location of Bunny HQ:" bunny-hq)
    (println "Distance:" (apply + (map abs (map second (seq bunny-hq)))))
  )
)

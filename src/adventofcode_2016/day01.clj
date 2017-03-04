(ns adventofcode-2016.day01
  (:gen-class)
  (:require [clojure.string])
  (:require [adventofcode-2016.math :refer (abs)])
)

(defn to-turn [word]
  (case (first word)
    \L :left
    \R :right
  )
)

(defn rotate
  [dir turn]
  { :pre [(map? dir) (number? (:x dir)) (number? (:y dir)) (keyword? turn)] }
  (case turn
    :left { :x (:y dir), :y (- (:x dir)) }
    :right  { :x (- (:y dir)), :y (:x dir) }
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

(def start-state
  {
    :pos { :x 0, :y 0 }
    :dir { :x 1, :y 0 }
  }
)
(defn find-bunny-hq
  [lines]
  (let [ steps (mapcat extract-steps lines) ]
    (map second (seq (:pos (reduce walk-step start-state steps))))
  )
)

(defn solve
  "solve day 1 of Advent of Code 2016"
  [input-lines & args]
  (do
    (def bunny-hq (find-bunny-hq input-lines))
    (println "Location of Bunny HQ:" bunny-hq)
    (println "Distance:" (apply + (map abs bunny-hq)))
  )
)

(ns adventofcode-2016.day08
  (:require clojure.string)
  (:require [clojure.test :refer [is]])
  (:require [adventofcode-2016.util :refer [count-filter map-with-index split-around transpose]])
  (:require [adventofcode-2016.scalisp :as scalisp])
)

(def width 50)
(def height 6)

(def initial-state (take height (repeat (take width (repeat false)))))

(defn show-state
  { :test #(do (let [f show-state]
                 (assert (= (f []) ""))
                 (assert (= (f [[false]]) "."))
                 (assert (= (f [[true]]) "#"))
                 (assert (= (f [[false true] [true false]]) ".#\n#."))
                 ))
  }
  [state]
    (->> state
      (map (fn [row]
             (->> row
                  (map #(if % "#" "."))
                  (clojure.string/join "")
             )))
      (clojure.string/join "\n")
    )
)

(defn rotate
  { :test #(do (let [f rotate]
                 (assert (= (rotate [[1 2] [3 4]] 0 1) [[2 1] [3 4]]))
                 (assert (= (rotate [[1 2] [3 4]] 1 1) [[1 2] [4 3]]))
                 (assert (= (rotate [[1 2] [3 4]] 0 -1) [[1 2] [3 4]]))
                 ))
  }
  [rows index steps]
  (update (apply vector rows) index (fn [row]
                  (let [ [prefix suffix] (split-at (- (count row) steps) row) ]
                    (concat suffix prefix)
                ))
  ))


(defn parse-instructions
  [lines]
  (map (fn [line]
         (clojure.string/split (clojure.string/trim line) #"\s+|=")
         )
       lines
  ))

(defn reduce-instruction
  { :test #(do
             (assert (= [[1 2] [4 3]] (reduce-instruction [[1 2] [3 4]] ["rotate" "row" "2" "by" "1"])))
             )
  }
  [state instruction]
    (scalisp/match-vector instruction
      case ["rect" dimensions] =>
        (let [
              [w h] (map #(read-string (apply str %)) (split-around #{\x} dimensions))
              ]
          (map-with-index
            (fn [row, r]
              (map-with-index
                (fn [pixel, c]
                  (or pixel
                    (and (< r h) (< c w))
                  ))
                row
              ))
            state
          )
        )

      case ["rotate" "row" "y" y "by" steps] =>
        (rotate state (read-string y) (read-string steps))

      case ["rotate" "column" "x" x "by" steps] =>
        (-> state
            transpose
            (rotate (read-string x) (read-string steps))
            transpose
        )
    )
)


(defn execute
  [instructions initial-state]
  (reduce reduce-instruction initial-state instructions)
  )

(defn solve
  "solve day 8 of Advent of Code 2016"
  [input-lines & args]
  (let [ final-state (-> input-lines
                         parse-instructions
                         (execute initial-state)
                         )
       ]
    (println "Lit pixels: " (count-filter identity (flatten final-state)))
    (println (show-state final-state))
    )
)

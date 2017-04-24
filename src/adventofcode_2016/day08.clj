(ns adventofcode-2016.day08
  (:require clojure.string)
  (:require [adventofcode-2016.util :refer [count-filter map-with-index split-around transpose]])
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
             (assert (= [[1 2] [4 3]] (reduce-instruction [[1 2] [3 4]] ["rotate" "row" "2" "by" "1"])))
             )
  }
  [state instruction]
    (case (first instruction)
      "rect"
        (let [
              [w h] (map #(read-string (apply str %)) (split-around #{\x} (second instruction)))
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

      "rotate"
        (let [
              vinstruction (apply vector instruction)
              index (read-string (vinstruction 3))
              steps (read-string (vinstruction 5))
              ]
          (case (second instruction)
            "row"
              (rotate state index steps)

            "column"
              (-> state
                transpose
                (rotate index steps)
                transpose
              )
          )
        )
     ))

(defmacro match-vector [expr & cases]
  ;(println expr)
  ;(doseq [case (partition 4 cases)] (println case))
  (assert (= 0 (mod (count cases) 4)) "match-vector must be applied to 1+4n arguments.")
  (doseq [ [case-word pattern arrow-word result-expr] (partition 4 cases) ]
    (assert (= 'case case-word) (str "First form in a match case must be (symbol \"case\"), was: " case-word))
    (assert (or (vector? pattern) (= '_ pattern)) (str "Second form in a match case must be _ or a vector, was: " (type pattern)))
    (assert (= '=> arrow-word) (str "Third form in a match case must be (symbol \"=>\"), was: " arrow-word))
  )

  `(let [ vexpr (apply vector ~expr) ]
    (for [
          [case-word pattern arrow-word result-expr] (partition 4 cases)
          ]
      (do
        (let [
              matches-catchall (= '_ pattern)
              matches-length (= (count pattern) (count vexpr))
              matches-case (map (fn [pattern-element expr-element]
                                  (if (symbol? pattern-element)
                                    true
                                    (= pattern-element expr-element)
                                  )
                                )
                                pattern
                                vexpr
                           )
             ]

          (if (or matches-catchall (and matches-length matches-case))
            (do
              (println "This expression:" vexpr)
              (println "Matches this pattern:" pattern)
            )
          )
        )

        [pattern result-expr]
      )
    )
  )
)

(match-vector (range 4)
  case [0 a 2 c] => true
  case _ => false
)

(defn reduce-instruction-match
  [state instruction]
    (match-vector (first instruction)
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

      case ["rotate" "row" "y" y "by" steps "steps"] =>
        (rotate state (read-string index) (read-string steps))

      case ["rotate" "column" "x" x "by" steps "steps"] =>
        (-> state
            transpose
            (rotate (read-string x) (read-string steps))
            transpose
        )
    ))


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

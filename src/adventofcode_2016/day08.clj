(ns adventofcode-2016.day08
  (:require clojure.string)
  (:require [clojure.test :refer [is]])
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

(defn matches-pattern?
  { :test #(do
             (is (true? (matches-pattern? '_ [0])))
             (is (true? (matches-pattern? 'a [0 'foo])))

             (is (true? (matches-pattern? '_ 0)))
             (is (true? (matches-pattern? 'a 0)))

             (is (true? (matches-pattern? 0 0)))
             (is (false? (matches-pattern? 0 1)))

             (is (true? (matches-pattern? [0] [0])))
             (is (false? (matches-pattern? [0] [1])))

             (is (true? (matches-pattern? ['a] [0])))
             (is (true? (matches-pattern? ['a] [1])))
             (is (true? (matches-pattern? ['a 0] [0 0])))
             (is (false? (matches-pattern? ['a 1] [1 0])))
             )
  }
  [pattern expr]
    (cond
      (symbol? pattern)
        true

      (vector? pattern)
        (every? true? (map matches-pattern? pattern expr))

      :else
        (= pattern expr)
    )
)

(defn make-binding
  { :test #(do
             (is (=
                  (make-binding 'a 5)
                  '[a 5]
                  ))
             (is (=
                  (make-binding '[a 0] [5 0])
                  '[a 5]
                  ))
             (is (=
                  (make-binding '[a b] [5 0])
                  '[a 5 b 0]
                  ))
             (is (=
                  (make-binding '[a [b 1 c] [2]] [3 [4 5 6] [7]])
                  '[a 3 b 4 c 6]
                  ))
             )
  }
  [pattern match-expr]
    (cond
      (symbol? pattern)
        [ pattern match-expr ]

      (vector? pattern)
        (mapcat make-binding pattern match-expr)
    )
)

(defn bind-match-around-result
  { :test #(do
             (is (=
                  (bind-match-around-result 'a 5 'a)
                  '(clojure.core/let [a 5] a)
                  ))
             (is (=
                  (bind-match-around-result 'b 5 'b)
                  '(clojure.core/let [b 5] b)
                  ))
             (is (=
                  (bind-match-around-result [1 'a 2 ] [3 4 5] '(+ a 7))
                  '(clojure.core/let [a 4] (+ a 7))
                  ))
             (is (=
                  (bind-match-around-result '[1 a 2 b ] [3 4 5 6] '(+ a b 7))
                  '(clojure.core/let [a 4 b 6] (+ a b 7))
                  ))
             (is (=
                  (bind-match-around-result '[1 a 2 [b [c 3 d]] ] [4 5 6 [7 [8 9 10]] ] '(+ a b c d 11))
                  '(clojure.core/let [a 5 b 7 c 8 d 10] (+ a b c d 11))
                  ))
             )
  }
  [pattern match-expr result-expr]
    `(let [ ~@(make-binding pattern match-expr) ]
       ~result-expr
     )
)

(defmacro match-vector [expr & cases]
  ;(println expr)
  ;(doseq [case (partition 4 cases)] (println case))
  (assert (= 0 (mod (count cases) 4)) "match-vector must be applied to 1+4n arguments.")
  (doseq [ [case-word pattern arrow-word result-expr] (partition 4 cases) ]
    (assert (= 'case case-word) (str "First form in a match case must be (symbol \"case\"), was: " case-word))
    (assert (or (symbol? pattern) (vector? pattern)) (str "Second form in a match case must be a symbol or vector, was: " (type pattern)))
    (assert (= '=> arrow-word) (str "Third form in a match case must be (symbol \"=>\"), was: " arrow-word))
  )

  (if-let [
         [_ pattern _ result-expr & cases-rest] (seq cases)
       ]
     `(let [the-expr# ~expr]
        (if (matches-pattern? '~pattern the-expr#)
          (let [ ~@(make-binding '~pattern the-expr#) ]
            ~result-expr
          )
          (match-vector the-expr# ~@cases-rest)
        )
     )
     `(assert false (str "No case matched expression:" ~expr))
  )
)

(comment
  (match-vector (range 4)
    case [0 a 2 c] => (= 2 (+ a c))
    case [0 a] => (= 2 (+ a 2))
    case _ => false
  )
)

(comment (defn reduce-instruction-match
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

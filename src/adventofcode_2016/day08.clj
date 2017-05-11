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

             (is (false? (matches-pattern? ['a] [])))
             (is (false? (matches-pattern? [] [1])))
             (is (false? (matches-pattern? ['a 0] [0 0 0])))
             (is (false? (matches-pattern? '[a [1]] [1 [2 3]])))

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
        (and
          (= (count pattern) (count expr))
          (every? true? (map matches-pattern? pattern expr))
        )

      :else
        (= pattern expr)
    )
)

(defn discard-values-outside-pattern
  { :test #(do
             (is (=
                  (discard-values-outside-pattern 'a [1 2 3])
                  [1 2 3]
                  ))
             (is (=
                  (discard-values-outside-pattern '[a 2 b 4] [1 2 3 4])
                  [1 3]
                  ))
             (is (=
                  (discard-values-outside-pattern '[a 2 [3 [b 5 c]] d [8 9]] [1 2 [3 [4 5 6]] 7 [8 9]])
                  [1 [[4 6]] 7]
                  ))
             (is (=
                  (discard-values-outside-pattern '[a [b 3 c] [5]] [1 [2 3 4] [5]])
                  [1 [2 4]]
                  ))
             )
  }
  [ pattern value ]
  { :pre [(or (symbol? pattern) (vector? pattern))] }

    (cond
      (symbol? pattern)
        value

      (vector? pattern)
        (filter #(not (and (seq? %) (empty? %)))
          (mapcat (fn [pattern value]
                 (if (or (symbol? pattern) (vector? pattern))
                   [(discard-values-outside-pattern pattern value)]
                   []
               ))
               pattern value
          )
        )
    )
)

(defn discard-non-symbol-pattern-parts
  { :test #(do
             (is (=
                  (discard-non-symbol-pattern-parts 'a)
                  'a
                  ))
             (is (=
                  (discard-non-symbol-pattern-parts '[a 2 b 4])
                  '[a b]
                  ))
             (is (=
                  (discard-non-symbol-pattern-parts '[a 2 [3 [b 5 c]] d [8 9]])
                  '[a [[b c]] d]
                  ))
             )
  }
  [ pattern ]
  { :pre [(or (symbol? pattern) (vector? pattern))] }

    (cond
      (symbol? pattern)
        pattern

      (vector? pattern)
        (->> pattern
          (filter #(or (symbol? %) (vector? %)))
          (map discard-non-symbol-pattern-parts)
          (filter #(or (symbol? %) (seq %)))
          (apply vector)
        )
    )
)

(defn make-binding
{ :test #(do
           (is (=
                (make-binding 'a 'thing)
                '[a (adventofcode-2016.day08/discard-values-outside-pattern 'a thing)]
                ))
           (is (=
                (make-binding '[a 0] 'thing)
                '[[a] (adventofcode-2016.day08/discard-values-outside-pattern '[a 0] thing)]
                ))
           (is (=
                (make-binding '[a b] 'thing)
                '[[a b] (adventofcode-2016.day08/discard-values-outside-pattern '[a b] thing)]
                ))
           (is (=
                (make-binding '[a [b 1 c] [2]] 'thing)
                '[
                  [a [b c]] (adventofcode-2016.day08/discard-values-outside-pattern '[a [b 1 c] [2]] thing)
                  ]
                ))
           )
 }
  [pattern value]
    [
     (discard-non-symbol-pattern-parts pattern)
     `(discard-values-outside-pattern '~pattern ~value)
    ]
)


(defmacro bind-match-around-result
  { :test #(do
             (is (=
                  (macroexpand-1 '(bind-match-around-result a 5 a))
                  '(clojure.core/let [a 5] a)
                  ))
             (is (=
                  (macroexpand-1 '(bind-match-around-result b 5 b))
                  '(clojure.core/let [b 5] b)
                  ))
             (is (=
                  (macroexpand-1 '(bind-match-around-result [1 a 2 ] [3 4 5] (+ a 7)))
                  '(clojure.core/let [a 4] (+ a 7))
                  ))
             (is (=
                  (macroexpand-1 '(bind-match-around-result [1 a 2 b ] [3 4 5 6] (+ a b 7)))
                  '(clojure.core/let [a 4 b 6] (+ a b 7))
                  ))
             (is (=
                  (macroexpand-1 '(bind-match-around-result [1 a 2 [b [c 3 d]] ] [4 5 6 [7 [8 9 10]] ] (+ a b c d 11)))
                  '(clojure.core/let [a 5 b 7 c 8 d 10] (+ a b c d 11))
                  ))
             )
  }
  [pattern match-expr result-expr]
    `(let [ ~@(make-binding pattern match-expr) ]
       ~result-expr
     )
)

(defmacro match-vector-internal [value & cases]
  ;(println value)
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

     `(if (matches-pattern? '~pattern ~value)
        (let [ ~@(make-binding pattern value) ]
          ~result-expr
        )
        (match-vector-internal ~value ~@cases-rest)
      )

     `(assert false (str "No case matched expression:" ~value))
  )
)

(defmacro match-vector [expr & cases]
  `(let [expr-value# ~expr]
     (match-vector-internal expr-value# ~@cases)
     )
  )

(def test-code '(match-vector (range 4)
    case [0 a 2 c] => (= 2 (+ a c))
    case [0 a] => (= 2 (+ a 2))
    case _ => false
  )
)

(match-vector (range 2)
  case [0 a 2 c] => (do
                      (println "Matched first case")
                      (println "a: " a)
                      (println "c: " c)
                      (+ a c)
                      )
  case [0 a] => (do
                  (println "Matched second case")
                  (println "a: " a)
                  (+ a 2)
                  )

  case _ => (do
              (println "Matched third case")
              (println "_: " _)
              false
              )
)


(clojure.core/let [expr-value__26408__auto__ (range 2)]
  (adventofcode-2016.day08/match-vector-internal expr-value__26408__auto__
    case [0 a 2 c] => (do
                        (println "Matched first case")
                        (println "a: " a)
                        (println "c: " c)
                        (+ a c)
                        )

    case [0 a] => (do
                    (println "Matched second case")
                    (println "a: " a)
                    (+ a 2)
                    )

    case _ => (do
                (println "Matched third case")
                (println "_: " _)
                false
                )
))

(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.day08/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )

    (adventofcode-2016.day08/match-vector-internal expr-value__26408__auto__
      case [0 a] => (do
                      (println "Matched second case")
                      (println "a: " a)
                      (+ a 2)
                      )
      case _ => (do
                  (println "Matched third case")
                  (println "_: " _)
                  false
                  )
)))

(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.day08/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )


    (if (adventofcode-2016.day08/matches-pattern? (quote [0 a]) expr-value__26408__auto__)

      (clojure.core/let [
                         [a] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a]) expr-value__26408__auto__)
                         ]
        (do
          (println "Matched second case")
          (println "a: " a)
          (+ a 2)
          )
      )

      (adventofcode-2016.day08/match-vector-internal expr-value__26408__auto__
        case _ => (do
                    (println "Matched third case")
                    (println "_: " _)
                    false
                    )
))))

(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.day08/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )


    (if (adventofcode-2016.day08/matches-pattern? (quote [0 a]) expr-value__26408__auto__)

      (clojure.core/let [
                         [a] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a]) expr-value__26408__auto__)
                         ]
        (do
          (println "Matched second case")
          (println "a: " a)
          (+ a 2)
          )
      )

      (if (adventofcode-2016.day08/matches-pattern? (quote _) expr-value__26408__auto__)

        (clojure.core/let [
                           _ (adventofcode-2016.day08/discard-values-outside-pattern (quote _) expr-value__26408__auto__)
                           ]
          (do
            (println "Matched third case")
            (println "_: " _)
            false
            )
        )

        (adventofcode-2016.day08/match-vector-internal expr-value__26408__auto__)
))))

(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.day08/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )


    (if (adventofcode-2016.day08/matches-pattern? (quote [0 a]) expr-value__26408__auto__)

      (clojure.core/let [
                         [a] (adventofcode-2016.day08/discard-values-outside-pattern (quote [0 a]) expr-value__26408__auto__)
                         ]
        (do
          (println "Matched second case")
          (println "a: " a)
          (+ a 2)
          )
      )

      (if (adventofcode-2016.day08/matches-pattern? (quote _) expr-value__26408__auto__)

        (clojure.core/let [
                           _ (adventofcode-2016.day08/discard-values-outside-pattern (quote _) expr-value__26408__auto__)
                           ]
          (do
            (println "Matched third case")
            (println "_: " _)
            false
            )
        )

        (clojure.core/assert false (clojure.core/str "No case matched expression:" expr-value__26408__auto__))
))))


(def test-code-internal
  '(match-vector-internal expr-value
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

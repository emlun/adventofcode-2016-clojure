(ns adventofcode-2016.scalisp
  (:require [clojure.test :refer [is]])
)

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

(defn- discard-non-symbol-pattern-parts
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

(defn- make-binding
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


(defn- match-vector-internal [value cases]
  (if-let [
           [_ pattern _ result-expr & cases-rest] (seq cases)
           ]
    `(if (matches-pattern? '~pattern ~value)
       (let [ ~@(make-binding pattern value) ]
         ~result-expr
       )
       ~(match-vector-internal value cases-rest)
     )

     `(assert false (str "No case matched expression:" ~value))
  )
)

(defmacro match-vector [expr & cases]
  (assert (= 0 (mod (count cases) 4)) "match-vector must be applied to 1+4n arguments.")
  (doseq [ [case-word pattern arrow-word result-expr] (partition 4 cases) ]
    (assert (= 'case case-word) (str "First form in a match case must be (symbol \"case\"), was: " case-word))
    (assert (or (symbol? pattern) (vector? pattern)) (str "Second form in a match case must be a symbol or vector, was: " (type pattern)))
    (assert (= '=> arrow-word) (str "Third form in a match case must be (symbol \"=>\"), was: " arrow-word))
  )
  (let [ expr-value-symbol (gensym) ]
    `(let [~expr-value-symbol ~expr]
       ~(match-vector-internal expr-value-symbol cases)
     )
  )
)



; Examples to macroexpand during development

(def test-code '(match-vector (range 4)
    case [0 a 2 c] => (= 2 (+ a c))
    case [0 a] => (= 2 (+ a 2))
    case _ => false
  )
)

(def test-code-internal
  '(match-vector-internal expr-value
     case [0 a 2 c] => (= 2 (+ a c))
     case [0 a] => (= 2 (+ a 2))
     case _ => false
   )
)


; Usage examples

(match-vector (range 4)
  case [0 a 2 c] => (= 2 (+ a c))
  case [0 a] => (= 2 (+ a 2))
  case _ => false
)


(match-vector [1 2 [3 [4] [] 5 [[[6] 7] 8] 9]]
  case [a b [c [d] [] e [[[f] g] h] i]] => [a b c d e f g h i]
  case _ => false
)

(match-vector '(1 2 (3 (4) () 5 (((6) 7) 8) 9))
  case [a b [c [d] [] 42 [[[f] g] h] i]] => (println "Won't match")
  case [a b [c [d] [] 5 [[[f] g] h] i]] => (do (println "Will match") [a b c d f g h i])
  case _ => false
)

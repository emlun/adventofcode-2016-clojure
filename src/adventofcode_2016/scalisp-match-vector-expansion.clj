; No expansion
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


; 1 expansion
(clojure.core/let [expr-value__26408__auto__ (range 2)]
  (adventofcode-2016.scalisp/match-vector-internal expr-value__26408__auto__
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

; 2 expansions
(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )

    (adventofcode-2016.scalisp/match-vector-internal expr-value__26408__auto__
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

; 3 expansions
(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )


    (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a]) expr-value__26408__auto__)

      (clojure.core/let [
                         [a] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a]) expr-value__26408__auto__)
                         ]
        (do
          (println "Matched second case")
          (println "a: " a)
          (+ a 2)
          )
      )

      (adventofcode-2016.scalisp/match-vector-internal expr-value__26408__auto__
        case _ => (do
                    (println "Matched third case")
                    (println "_: " _)
                    false
                    )
))))

; 3 expansions
(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )


    (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a]) expr-value__26408__auto__)

      (clojure.core/let [
                         [a] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a]) expr-value__26408__auto__)
                         ]
        (do
          (println "Matched second case")
          (println "a: " a)
          (+ a 2)
          )
      )

      (if (adventofcode-2016.scalisp/matches-pattern? (quote _) expr-value__26408__auto__)

        (clojure.core/let [
                           _ (adventofcode-2016.scalisp/discard-values-outside-pattern (quote _) expr-value__26408__auto__)
                           ]
          (do
            (println "Matched third case")
            (println "_: " _)
            false
            )
        )

        (adventofcode-2016.scalisp/match-vector-internal expr-value__26408__auto__)
))))

; 4 expansions
(clojure.core/let [expr-value__26408__auto__ (range 2)]

  (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a 2 c]) expr-value__26408__auto__)

    (clojure.core/let [
                       [a c] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a 2 c]) expr-value__26408__auto__)
                       ]
      (do
        (println "Matched first case")
        (println "a: " a)
        (println "c: " c)
        (+ a c)
        )
    )


    (if (adventofcode-2016.scalisp/matches-pattern? (quote [0 a]) expr-value__26408__auto__)

      (clojure.core/let [
                         [a] (adventofcode-2016.scalisp/discard-values-outside-pattern (quote [0 a]) expr-value__26408__auto__)
                         ]
        (do
          (println "Matched second case")
          (println "a: " a)
          (+ a 2)
          )
      )

      (if (adventofcode-2016.scalisp/matches-pattern? (quote _) expr-value__26408__auto__)

        (clojure.core/let [
                           _ (adventofcode-2016.scalisp/discard-values-outside-pattern (quote _) expr-value__26408__auto__)
                           ]
          (do
            (println "Matched third case")
            (println "_: " _)
            false
            )
        )

        (clojure.core/assert false (clojure.core/str "No case matched expression:" expr-value__26408__auto__))
))))

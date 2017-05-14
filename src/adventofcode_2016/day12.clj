(ns adventofcode-2016.day12
  (:refer-clojure :exclude [resolve])
  (:require clojure.string)
  (:require [clojure.test :refer [is]])
  (:require [adventofcode-2016.scalisp :as scalisp])
)

(defn parse-instruction
  { :test #(do
            (is (=
                 (parse-instruction "cpy a b")
                 { :op :cpy, :from 'a, :to 'b }
                 ))
            (is (=
                 (parse-instruction "cpy 1 b")
                 { :op :cpy, :from 1, :to 'b }
                 ))
            (is (=
                 (parse-instruction "inc a")
                 { :op :inc, :register 'a }
                 ))
            (is (=
                 (parse-instruction "dec a")
                 { :op :dec, :register 'a }
                 ))
            (is (=
                 (parse-instruction "jnz a 1")
                 { :op :jnz, :test 'a, :diff 1 }
                 ))
            (is (=
                 (parse-instruction "jnz a -1")
                 { :op :jnz, :test 'a, :diff -1 }
                 ))
            (is (=
                 (parse-instruction "jnz 0 1")
                 { :op :jnz, :test 0, :diff 1 }
                 ))
            )
  }
  [line]
    (let [ words (clojure.string/split line #"\s+") ]
      (scalisp/match-vector words
                            case [ "cpy" x y ] => { :op :cpy, :from (read-string x), :to (symbol y) }
                            case [ "inc" x ]   => { :op :inc, :register (symbol x) }
                            case [ "dec" x ]   => { :op :dec, :register (symbol x) }
                            case [ "jnz" x y ] => { :op :jnz, :test (read-string x), :diff (read-string y) }
      )
    )
)

(defn parse-instructions [ lines ] (mapv parse-instruction lines))

(defn halted?
  { :test #(do
            (is (true? (halted? { :program [], :eip 0 })))
            (is (false? (halted? { :program [ 0 ], :eip 0 })))
            (is (true? (halted? { :program [ 0 ], :eip 1 })))
            (is (false? (halted? { :program [ 0 ], :eip 0 })))
            (is (true? (halted? { :program [ 0 ], :eip -1 })))
            )
  }
  [state]
  (or
    (>= (:eip state) (count (:program state)))
    (< (:eip state) 0)
  )
)

(defn resolve
  { :test #(do
            (is (= 1 (resolve { 'a 1 } 'a)))
            (is (= 1 (resolve { 'a 2 } 1)))
            ) }
  [registers expr]
    (cond
      (symbol? expr)
        (get registers expr)
      :else
        expr
    ))

(defn op-cpy
  { :test #(do
             (is (=
                  { :registers { 'a 5, 'b 5, 'c 0, 'd 0 } }
                  (op-cpy { :from 'b :to 'a } { :registers { 'a 0, 'b 5, 'c 0, 'd 0 } })
                  ))
             (is (=
                  { :registers { 'a 7, 'b 5, 'c 0, 'd 0 } }
                  (op-cpy { :from 7 :to 'a } { :registers { 'a 0, 'b 5, 'c 0, 'd 0 } })
                  ))
             ) }
  [ { :keys [from to] }, state ]
    (assoc-in state [:registers to] (resolve (:registers state) from))
)

(defn op-inc
  [ { :keys [register] }, state ]
    (update-in state [:registers register] inc)
)

(defn op-dec
  [ { :keys [register] }, state ]
    (update-in state [:registers register] dec)
)

(defn op-jnz
  { :test #(do
             (is (=
                  { :eip 1, :registers { 'a 0 } }
                  (op-jnz { :test 0, :diff 5 } { :eip 0, :registers { 'a 0 } })
                  )
                 "jnz on a zero constant increments eip by 1")
             (is (=
                  { :eip 5, :registers { 'a 0 } }
                  (op-jnz { :test 1, :diff 5 } { :eip 0, :registers { 'a 0 } })
                  )
                 "jnz on a nonzero constant modifies eip by the instruction diff")
             (is (=
                  { :eip 1, :registers { 'a 0 } }
                  (op-jnz { :test 'a, :diff 5 } { :eip 0, :registers { 'a 0 } })
                  )
                 "jnz on a zero register increments eip by 1")
             (is (=
                  { :eip 5, :registers { 'a 1 } }
                  (op-jnz { :test 'a, :diff 5 } { :eip 0, :registers { 'a 1 } })
                  )
                 "jnz on a nonzero register modifies eip by the instruction diff")
             ) }
  [ { :keys [test diff] }, state ]
    (if (= 0 (resolve (:registers state) test))
      (update state :eip inc)
      (update state :eip #(+ % diff))
    )
)

(defn advance-eip-after
  [ opfunc ]
    (fn [instruction state]
      (update (opfunc instruction state) :eip inc))
)

(def operations {
                 :cpy (advance-eip-after op-cpy)
                 :inc (advance-eip-after op-inc)
                 :dec (advance-eip-after op-dec)
                 :jnz op-jnz
})

(defn execute-step
  [ { :keys [eip program] :as state } ]
  (let [
        instruction (nth program eip)
        opfunc (get operations (:op instruction))
        ]
    (opfunc instruction state)
  )
)

(defn execute
  ([program registers]
     (execute { :program program, :eip 0, :registers registers })
  )

  ([state]
     (if (halted? state)
       state
       (recur (execute-step state))
     )
  )
)

(defn solve
  "solve day 12 of Advent of Code 2016"
  [input-lines & args]
  (let [ program (parse-instructions input-lines) ]
    (doseq [ [part initial-registers] [
                                       ["A" { 'a 0, 'b 0, 'c 0, 'd 0 }]
                                       ["B" { 'a 0, 'b 0, 'c 1, 'd 0 }]
                                       ]
            ]
      (println part ":")
      (println "Initial state:" initial-registers)
      (println "Final registers state:")
      (println (:registers (execute program initial-registers)))
      )
  )
)

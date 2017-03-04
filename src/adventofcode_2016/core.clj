(ns adventofcode-2016.core
  (:gen-class)
  (:require clojure.string)
  (:require adventofcode-2016.day01)
)

(defn -main
  "run the solver for the given day with lines from standard input as argument"
  [day & args]
  (do
    (def padded-day (if (< 1 (count day)) day (str "0" day)))
    (def lines (clojure.string/split-lines (slurp *in*)))
    (def solve (resolve (symbol (str "adventofcode-2016.day" padded-day) "solve")))
    (println (solve lines))
  )
)

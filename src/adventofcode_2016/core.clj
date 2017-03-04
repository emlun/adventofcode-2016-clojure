(ns adventofcode-2016.core
  (:gen-class)
  (:require clojure.string)
  (:require adventofcode-2016.day01)
)

(defn pad [day] (if (< 1 (count day)) day (str "0" day)))

(defn -main
  "Run the solver for the given day with lines from file as argument. If file is -, use standard input; if not given, use default."
  ([day] (-main day (str "resources/day" (pad day) ".in")))
  ([day file & args]
    (do
      (def input (if (= "-" file) *in* file))
      (def lines (clojure.string/split-lines (slurp input)))
      (def solve (resolve (symbol (str "adventofcode-2016.day" (pad day)) "solve")))
      (solve lines)
    )
  )
)

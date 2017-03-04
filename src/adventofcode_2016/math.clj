(ns adventofcode-2016.math
)

(defn abs
  "absolute value"
  [n]
  { :pre [(number? n)] :post [(number? %)]}
  (max n (-' n))
)

(ns adventofcode-2016.util
)

(defn abs
  "absolute value"
  [n]
  { :pre [(number? n)] :post [(number? %)]}
  (max n (-' n))
)

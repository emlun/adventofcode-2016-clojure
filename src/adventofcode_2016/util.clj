(ns adventofcode-2016.util
)

(defn abs
  "absolute value"
  [n]
  { :pre [(number? n)] :post [(number? %)]}
  (max n (-' n))
)

(defn first-recurrence
  ( [future]
    { :pre (seq? future) }
      (first-recurrence #{} future)
  )
  ( [history [present & future]]
    { :pre [(or (seq? future) (nil? future)) (coll? history)] }
      (if (nil? present)
        nil
        (if (contains? history present)
          present
          (recur (conj history present) future)
        )
      )
  )
)

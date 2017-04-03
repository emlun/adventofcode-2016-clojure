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

(defn grouped
  "Return elements in x in groups of n"
  {
    :test #(do
             (assert (= [] (grouped 5 [])))
             (assert (= [] (grouped 5 [])))
             (assert (= [[0] [1] [2]] (grouped 1 [0 1 2])))
             (assert (= [[0]] (grouped 3 [0])))
             (assert (= [[0 1]] (grouped 3 [0 1])))
             (assert (= [[0 1 2]] (grouped 3 [0 1 2])))
             (assert (= [[0 1 2] [3]] (grouped 3 [0 1 2 3])))
             )
  }
  [n x]
  (loop [result [] remain x]
    (cond
      (= n (count result))
        (cons result (grouped n remain))
      (seq remain)
        (recur (conj result (first remain)) (rest remain))
      (seq result)
        (conj [] result)
      :else []
    )))

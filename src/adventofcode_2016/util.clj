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

(defn flip
  "Apply f and its following arguments with the first two arguments flipped"
  {
   :test (fn []
            (assert (= (flip - 5 1) (- 1 5)))
            (assert (= (flip - 5 1 3) (- 1 5 3)))
            (assert (= (flip - 5 1 3 3) (- 1 5 3 3)))
            (assert (= (flip map [1 2 3] #(+ 3 %)) (map #(+ 3 %) [1 2 3])))
            )
   }
  [f a b & more]
  (apply f (cons b (cons a more))))

(defn grouped
  "Return elements in x in groups of n"
  {
    :test #(do
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

(defn transpose
  "Transform seq [[a1 a2 ...] [b1 b2 ...] ...] to seq [[a1 b1 ...] [a2 b2 ...] ...]"
  {
   :test #(do
            (assert (= (transpose []) (transpose [[]]) (transpose [[] []]) []), "Transpose of empty input is empty")
            (assert (= (transpose [[1]]) [[1]]), "Transpose of single element is identity function")
            (assert (= (transpose [[1 2]]) [[1] [2]]), "Transpose of two-column row is two one-column rows")
            (assert (= (transpose [[1 2] [3 4]]) [[1 3] [2 4]]), "Transpose of 2x2 matrix is matrix transpose")
            (assert (= (transpose [[1 2] []]) []), "Rows are truncated to shortest row")
            (assert (= (transpose [[1 2] [3]]) [[1 3]]), "Rows are truncated to shortest row")
            )
   }
  [seqs]
  (if (seq seqs)
    (apply map (cons vector seqs))
    ()
  ))

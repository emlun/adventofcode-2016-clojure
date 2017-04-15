(ns adventofcode-2016.day04
  (:require clojure.string)
  (:require [adventofcode-2016.util :refer (count= flip)])
)

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(defn rot
  {
   :test #(do
            (assert (= \c (rot 2 \a)))
            (assert (= \c (rot 4 \y)))
            (assert (= \y (rot -4 \c)))
            (assert (= \a (rot 26 \a)))
            )
   }
  [steps c]
  (let [
        index (clojure.string/index-of alphabet c)
        new-index (mod (+ index steps) (count alphabet))
        ]
    (get alphabet new-index)))

(defn decipher [ciphertext key]
  (->> ciphertext
    (map (fn [c]
           (case c
             \- \space

             (rot key c)
           )))
    (apply str)
    ))

(defn parse-room [ line ]
  (let [
        dash-groups (clojure.string/split (clojure.string/trim line) #"-")
        last-groups (clojure.string/split (last dash-groups) #"\[")

        room-name (clojure.string/join "-" (butlast dash-groups))
        sector-id (read-string (first last-groups))
        checksum (apply str (butlast (second last-groups)))

        expected-checksum (->> room-name
                            (set)
                            (flip disj \-)
                            (sort-by (juxt #(-(count= % room-name)) identity))
                            (take 5)
                            (apply str)
                            )
       ]
    {
      :name room-name
      :valid (= checksum expected-checksum)
      :id sector-id
    }
  ))

(defn solve-a [ input-lines ]
  (->> input-lines
    (map parse-room)
    (filter :valid)
    (map :id)
    (apply +)
    ))

(def search-keyword "pole")

(defn solve-b [ input-lines ]
  (->> input-lines
    (map parse-room)
    (filter :valid)

    (map #(assoc % :decrypted-name (decipher (:name %) (:id %))))
    (filter #(clojure.string/includes? (:decrypted-name %) search-keyword))

    (map #(str (:id %) " " (:decrypted-name %)))
    (clojure.string/join ", ")
  ))

(defn solve
  "solve day 4 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem solver] [["Subproblem A:" solve-a] ["Subproblem B:" solve-b]] ]
    (println problem (solver input-lines))
  )
)

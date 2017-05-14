(ns adventofcode-2016.day17
  (:refer-clojure :exclude [resolve])
  (:require clojure.string)
  (:require [clojure.test :refer [is]])
  (:require [adventofcode-2016.util :refer [bfs]])
)

(def ^{ :private true } MD5 (java.security.MessageDigest/getInstance "MD5"))

(defn md5
  { :test #(is (= "acbd" (md5 "foo"))) }
  [s]
    (->> s
         (map byte)
         (byte-array)
         (.digest MD5)
         (take 2)
         (map #(format "%02x" %))
         (apply str)
         ))

(defn represents-open? [c] (contains? #{\b \c \d \e \f} c))

(defn open-state
  { :test #(do
             (is (=
                  { :U false :D true :L true :R false }
                  (open-state "foo" "RDLU")
                  ))
             (is (=
                  { :U true :D true :L true :R false }
                  (open-state "hijkl" [])
                  ))
             )}
  [passcode path]
  (let [
        checksum (md5 (str passcode (apply str path)))
        [ U D L R ] (map represents-open? checksum)
        ]
    { :U U, :D D, :L L, :R R }
  )
)

(def deltas {
             :U { :x 0, :y -1 }
             :D { :x 0, :y 1 }
             :L { :x -1, :y 0 }
             :R { :x 1, :y 0 }
             })

(defn generate-next-states
  { :test #(do
             (is (=
                  [
                   {:pos {:x 0, :y -1}, :path [\U], :previous-door-open true}
                   {:pos {:x 0, :y 1}, :path [\D], :previous-door-open true}
                   {:pos {:x -1, :y 0}, :path [\L], :previous-door-open true}
                   {:pos {:x 1, :y 0}, :path [\R], :previous-door-open false}
                   ]
                  (generate-next-states "hijkl" { :pos { :x 0, :y 0 }, :path [] })
                  ))
             )}
  [passcode state]
    (let [
          options (open-state passcode (:path state))
          ]
      (map
        (fn [direction]
         (-> state
            (update :pos #(merge-with + % (get deltas direction)))
            (update :path #(conj % (name direction)))
            (assoc :previous-door-open (get options direction))
            )
        )
        [ :U :D :L :R ]
        )
    )
)

(defn valid-state?
  { :test #(do
             (is (false? (valid-state? {} { :previous-door-open false })))
             (is (false? (valid-state? { :x 0, :y 0 } { :previous-door-open true, :pos { :x 0, :y 0 } })))
             (is (true? (valid-state? { :x 1, :y 1 } { :previous-door-open true, :pos { :x 0, :y 0 } })))
             )}
  [
   map-dimensions
   {
    { :keys [x, y] } :pos
    :keys [previous-door-open]
   }
  ]
    (and
      previous-door-open
      (>= x 0)
      (>= y 0)
      (< x (:x map-dimensions))
      (< y (:y map-dimensions))
      ))

(defn find-path
  { :test #(do
             (is (=
               "DDRRRD"
               (find-path { :x 4, :y 4 } "ihgpwlah")
               ))
             )}
  [map-dimensions passcode]
  (let [
        target { :x (dec (:x map-dimensions))
                 :y (dec (:y map-dimensions))
                }
        ]
    (apply str (:path
      (bfs {
            :initial-state {
                            :path []
                            :pos { :x 0 :y 0 }
                            :previous-door-open true
                            }
            :terminate? (fn [state] (= target (:pos state)))
            :generate-next-states (partial generate-next-states passcode)
            :skip? (fn [state history] (not (valid-state? map-dimensions state)))
            })
    ))
  )
)

(defn solve
  "solve day 17 of Advent of Code 2016"
  [input-lines & args]
  (println "Shortest path: " (apply str (find-path { :x 4, :y 4 } (first input-lines))))
)

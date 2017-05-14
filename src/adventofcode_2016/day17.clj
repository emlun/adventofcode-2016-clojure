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
                   {:pos {:x 0, :y -1}, :path ["U"], :previous-door-open true}
                   {:pos {:x 0, :y 1}, :path ["D"], :previous-door-open true}
                   {:pos {:x -1, :y 0}, :path ["L"], :previous-door-open true}
                   {:pos {:x 1, :y 0}, :path ["R"], :previous-door-open false}
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
               nil
               (find-path { :x 4, :y 4 } "hijkl" (constantly false))
               ))
             (is (=
               "DDRRRD"
               (apply str (:path (find-path { :x 4, :y 4 } "ihgpwlah" (fn [state] (= (:pos state) { :x 3, :y 3 })))))
               ))
             (is (=
               "DDUDRLRRUDRD"
               (apply str (:path (find-path { :x 4, :y 4 } "kglvqrro" (fn [state] (= (:pos state) { :x 3, :y 3 })))))
               ))
             (is (=
               "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
               (apply str (:path (find-path { :x 4, :y 4 } "ulqzkmiv" (fn [state] (= (:pos state) { :x 3, :y 3 })))))
               ))
             )}
  [map-dimensions passcode terminate? finished?]
    (bfs {
          :initial-state {
                          :path []
                          :pos { :x 0 :y 0 }
                          :previous-door-open true
                          }
          :terminate? terminate?
          :generate-next-states (partial generate-next-states passcode)
          :skip? (fn [state history] (or
                                       (not (valid-state? map-dimensions state))
                                       (finished? (last history))
                                       ))
          })
)

(defn solve
  "solve day 17 of Advent of Code 2016"
  [input-lines & args]
  (let [
        map-dimensions { :x 4, :y 4 }
        target { :x (dec (:x map-dimensions))
                 :y (dec (:y map-dimensions))
                }
        passcode (first input-lines)
        finished? (fn [state] (= target (:pos state)))
        finish-states (atom #{})
        longest-path (atom [])
        check-part-b-terminate? (fn [state]
                                  (do
                                    (println
                                      "Current longest path:"
                                      (count @longest-path)
                                      "; finish states:"
                                      (count @finish-states)
                                      )

                                    (swap!
                                      longest-path
                                      (fn [current-longest-path candidate-path]
                                        (if (and
                                              (> (count candidate-path) (count current-longest-path))
                                              (= target (:pos state))
                                              )
                                          candidate-path
                                          current-longest-path
                                          )
                                        )
                                      (:path state)
                                      )

                                    (swap!
                                      finish-states
                                      (fn [current]
                                        (if (= target (:pos state))
                                          (conj current state)
                                          current
                                          )
                                        )
                                      )

                                    false
                                    ))
        ]
    (do
      (println "Shortest path: " (apply str (:path (find-path map-dimensions passcode finished? finished?))))
      (find-path map-dimensions passcode check-part-b-terminate? finished?)
      (println "Longest path: " (count @longest-path) (apply str @longest-path))
    )
  )
)

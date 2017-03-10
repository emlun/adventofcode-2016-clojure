(ns adventofcode-2016.day02)

(def moves1 {
              :1 { :U :1, :R :2, :D :4, :L :1 }
              :2 { :U :2, :R :3, :D :5, :L :1 }
              :3 { :U :3, :R :3, :D :6, :L :2 }
              :4 { :U :1, :R :5, :D :7, :L :4 }
              :5 { :U :2, :R :6, :D :8, :L :4 }
              :6 { :U :3, :R :6, :D :9, :L :5 }
              :7 { :U :4, :R :8, :D :7, :L :7 }
              :8 { :U :5, :R :9, :D :8, :L :7 }
              :9 { :U :6, :R :9, :D :9, :L :8 }
            })

(def moves2 {
              :1 { :U :1, :R :1, :D :3, :L :1 }
              :2 { :U :2, :R :3, :D :6, :L :2 }
              :3 { :U :1, :R :4, :D :7, :L :2 }
              :4 { :U :4, :R :4, :D :8, :L :3 }
              :5 { :U :5, :R :6, :D :5, :L :5 }
              :6 { :U :2, :R :7, :D :A, :L :5 }
              :7 { :U :3, :R :8, :D :B, :L :6 }
              :8 { :U :4, :R :9, :D :C, :L :7 }
              :9 { :U :9, :R :9, :D :9, :L :8 }
              :A { :U :6, :R :B, :D :A, :L :A }
              :B { :U :7, :R :C, :D :D, :L :A }
              :C { :U :8, :R :C, :D :C, :L :B }
              :D { :U :B, :R :D, :D :D, :L :D }
            })

(defn walk [moves startButton line]
  (reduce
    #(get-in moves [%1 (keyword (str %2))])
    startButton
    line))

(defn execute [initialButton moves lines]
  (rest (reductions (partial walk moves) initialButton lines)))

(defn solve
  "solve day 2 of Advent of Code 2016"
  [input-lines & args]
  (doseq [ [problem moves] [["Subproblem A:" moves1] ["Subproblem B:" moves2]] ]
    (println problem (apply str (map name (execute :5 moves input-lines))))
  )
)

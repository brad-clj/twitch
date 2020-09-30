(ns hr.core)

(set! *warn-on-reflection* true)

(def m 1000000007)

(defn solve
  [n]
  (let [n (mod n m)]
    #_(reduce (fn [acc n]
                (mod (+ acc
                        (- (* n n)
                           (let [n1 (dec n)]
                             (* n1 n1))))
                     m))
              0 (range 1 (inc n)))
    (mod (* n n) m)))

;;(map solve (range 1 20))
;;(1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361)

;;(map #(* % %) (range 1 20))
;;(1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361)

(defn main
  []
  (let [t (read)]
    (-> (repeatedly t (fn []
                        (let [n (read)]
                          (println (solve n)))))
        dorun)))

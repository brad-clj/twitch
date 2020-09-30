(ns hr.core)

(set! *warn-on-reflection* true)

(defn solve
  [n m]
  (+ (dec n)
     (* n (dec m))))

(defn main
  []
  (let [n (read)
        m (read)]
    (println (solve n m))))

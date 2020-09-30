(ns hr.core)

(set! *warn-on-reflection* true)

(defn solve
  [routes]
  (reduce (fn [x y]
            (mod (* x y) 1234567))
          1 routes))

(defn main
  []
  (let [t (read)]
    (-> (repeatedly t (fn []
                        (let [n (read)
                              routes (doall (repeatedly (dec n) read))]
                          (println (solve routes)))))
        dorun)))

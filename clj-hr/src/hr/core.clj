(ns hr.core)

(set! *warn-on-reflection* true)

(defn permutation-of-multisets
  [ms]
  (let [s (reduce +' ms)]
    (-> (/ (reduce *' (range s 0 -1))
           (reduce *' (map (fn [m]
                             (reduce *' (range m 0 -1)))
                           ms))))))

(def l
  (-> (fn [[sx sy sz]]
        [sy sz
         (reduce into
                 #{}
                 [(map (fn [m] (update m 3 #(inc (or % 0))))
                       sx)
                  (map (fn [m] (update m 2 #(inc (or % 0))))
                       sy)
                  (map (fn [m] (update m 1 #(inc (or % 0))))
                       sz)])])
      (iterate [#{{}}
                #{{1 1}}
                #{{2 1} {1 2}}])
      (->> (map first)
           (map (fn [m]
                  (->> m
                       (map (comp permutation-of-multisets vals))
                       (reduce +')))))))

(defn solve
  [n]
  (-> (nth l n)
      long))

(defn main
  []
  (let [s (read)]
    (-> (fn []
          (let [n (read)]
            (println (solve n))))
        (->> (repeatedly s))
        dorun)))

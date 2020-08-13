(ns hr.core)

(set! *warn-on-reflection* true)

;; https://www.hackerrank.com/challenges/matrix/problem

(defn ds-find
  [ds k]
  (loop [ds ds, k k, ks []]
    (let [v (ds k)]
      (if (or (nil? v) (= v k))
        (let [ds (reduce (fn [ds pk]
                           (assoc ds pk k))
                         ds ks)]
          [ds k])
        (recur ds v (conj ks k))))))

(defn ds-union
  [ds a b]
  (let [[ds av] (ds-find ds a)
        [ds bv] (ds-find ds b)]
    (assoc ds bv av)))

(defn solve
  [edges machines]
  (-> (reduce (fn [[acc ds] [a b wt]]
                (let [[ds av] (ds-find ds a)
                      [ds bv] (ds-find ds b)]
                  (if (and (machines av) (machines bv))
                    [(+ acc wt) ds]
                    (let [[av bv] (if (machines bv)
                                    [bv av]
                                    [av bv])]
                      [acc (ds-union ds av bv)]))))
              [0 {}] edges)
      first))

(defn main
  []
  (let [n (read)
        k (read)
        cities (-> (repeatedly (dec n)
                               (fn []
                                 (let [city1 (read)
                                       city2 (read)
                                       time (read)]
                                   [city1 city2 time])))
                   (->> (sort-by (fn [[_ _ time]]
                                   time)
                                 >)))
        machines (set (repeatedly k read))]
    (println (solve cities machines))))

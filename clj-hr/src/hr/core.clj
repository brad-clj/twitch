(ns hr.core)

(set! *warn-on-reflection* true)

;; https://www.hackerrank.com/challenges/friend-circle-queries/problem

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
  [qs]
  (-> (reduce (fn [[out m ds] [a b]]
                (let [max-v (or (peek out) 0)
                      [ds ak] (ds-find ds a)
                      [ds bk] (ds-find ds b)]
                  (if (= ak bk)
                    [(conj out max-v) m ds]
                    (let [a-count (get m ak 1)
                          b-count (get m bk 1)
                          u-count (+ a-count b-count)
                          ds (ds-union ds ak bk)]
                      [(conj out (max max-v u-count))
                       (assoc m ak u-count)
                       ds]))))
              [[] {} {}]
              qs)
      first))

(defn main
  []
  (let [q (read)
        qs (repeatedly q (fn []
                           (let [a (read)
                                 b (read)]
                             [a b])))]
    (run! println (solve qs))))

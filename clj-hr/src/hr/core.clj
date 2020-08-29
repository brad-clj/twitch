(ns hr.core)

(set! *warn-on-reflection* true)

;; https://www.hackerrank.com/challenges/recursive-digit-sum/problem

(defn solve
  [s k]
  (let [r-fn (fn [acc c]
               (let [n ({\0 0, \1 1,
                         \2 2, \3 3,
                         \4 4, \5 5,
                         \6 6, \7 7,
                         \8 8, \9 9} c)]
                 (+ acc n)))
         s (-> (reduce r-fn 0 s) (* k) str)]
    (loop [s s]
      (if (== (count s) 1)
        s
        (recur (-> (reduce r-fn 0 s) str))))))

(defn main
  []
  (let [n (read)
        k (read)]
    (println (solve (str n) k))))

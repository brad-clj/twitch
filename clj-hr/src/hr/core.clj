(ns hr.core)

(set! *warn-on-reflection* true)

#_
(defun prime-p (n)
  (declare (type (integer 0) n)
           (optimize speed))
  (or (= n 2)
      (= n 3)
      (and (/= 0 (rem n 2))
           (/= 0 (rem n 3))
           (loop :for i :from 5 :by 6 :while (<= (* i i) n)
                 :never (or (= 0 (rem n i))
                            (= 0 (rem n (+ i 2))))))))

#_
(defn prime?
  [n]
  (cond
    (<= n 1) false
    (<= n 3) true
    :else (and (pos? (rem n 2))
               (pos? (rem n 3))
               (-> (iterate #(+ % 6) 5)
                   (->> (take-while #(<= (* % %) n))
                        (every? #(not (or (zero? (rem n %))
                                          (zero? (rem n (+ % 2)))))))))))

(defn prime?
  [n]
  (cond
    (<= n 3)
    (> n 1)

    (or (zero? (rem n 2))
        (zero? (rem n 3)))
    false

    :else
    (loop [i 5]
      (if (<= (* i i) n)
        (if (or (zero? (rem n i))
                (zero? (rem n (+ i 2))))
          false
          (recur (+ i 6)))
        true))))

(defn solve
  [n]
  (prime? n))

(defn main
  []
  (let [p (read)]
    (-> (fn []
          (let [n (read)]
            (-> (if (solve n)
                  "Prime"
                  "Not prime")
                println)))
        (->> (repeatedly p))
        dorun)))

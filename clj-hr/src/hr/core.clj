(ns hr.core)

(set! *warn-on-reflection* true)

;; what xor is?
#_
(let [a 2r0011
      b 2r1001
      ret (bit-xor a b)]
  (map (fn [x] (Long/toBinaryString x)) [a b ret]))

;; the dump too slow solution
#_
(let [ms [92837498 234 123 56 3456 678]
      ns [234 123 345345 234234 123123 6456]]
  (-> ms
      (->> (map (fn [m]
                  (-> (map (fn [n] (bit-xor m n))
                           ns)
                      (->> (reduce max))))))))

;; thoughts on solution
#_
(do
  [2 1 0]

  2r010010100100101
  2r000000000110100
  )

(defn make-search
  [& {:keys [id cmp] :or {id identity, cmp <}}]
  (let [c (comparator cmp)]
    (fn search
      ([v x]
       (search v x 0 (count v)))
      ([v x start end]
       (let [f (fn f [start end edge]
                 (if (== start end)
                   (if (nil? edge)
                     [start start]
                     start)
                   (let [i (quot (+ start end) 2)
                         r (c x (id (v i)))]
                     (cond
                       (and (== r 0) (nil? edge))
                       [(f start i :left) (f (inc i) end :right)]

                       (or (> r 0)
                           (and (== r 0) (= edge :right)))
                       (recur (inc i) end edge)

                       (or (< r 0)
                           (and (== r 0) (= edge :left) ))
                       (recur start i edge)))))]
         (f start end nil))))))

(def search
  (make-search :cmp >))

(defn solve
  [])

(defn main
  [])

;; I'm struggling here
#_
(let [next-high-bit-in-q calc_it
      bit highest-bit-among-arr
      arr-subv _?
      ]
  (cond
    (< (count arr-subv) 1000)
    (do #_"brute force it")

    (> highest-bit-among-arr high-bit-in-q)
    (do #_"get some range over arr that matches that high bit")

    (== highest-bit-among-arr high-bit-in-q)
    (do #_"get some range over arr that is past that high bit")

    (< highest-bit-among-arr high-bit-in-q)
    (do #_"check next higest bit in q?")))

(ns hr.core)

(set! *warn-on-reflection* true)

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

(defn get-high-bit
  [num]
  (loop [bit 0, num num]
    (let [num (bit-clear num bit)]
      (if (zero? num)
        bit
        (recur (inc bit) num)))))

(defn fill-bits
  ^long [bit]
  (reduce (fn [num bit]
            (bit-set num bit))
          0 (range (inc bit))))

(defn solve
  [nums query]
  (loop [bit (-> (map get-high-bit
                      [(first nums) query])
                 (->> (reduce max)))
         upper (fill-bits bit)
         lower 0]
    (let [bit-set? (bit-test query bit)
          #_[upper lower] #_(let [bit-op (if bit-set? bit-clear bit-set)]
                              (map #(bit-op % bit)
                                   [upper lower]))
          upper (long ((if bit-set? bit-clear bit-set) upper bit))
          lower (long ((if bit-set? bit-clear bit-set) lower bit))
          upper-i (-> (search nums upper) first)
          lower-i (-> (search nums lower) second)]
      (cond
        (== upper-i lower-i)
        (let [#_[upper lower] #_(let [bit-op (if bit-set? bit-set bit-clear)]
                                  (map #(bit-op % bit)
                                       [upper lower]))
              upper (long ((if bit-set? bit-set bit-clear) upper bit))
              lower (long ((if bit-set? bit-set bit-clear) lower bit))]
          (recur (dec bit) upper lower))

        (<= (- lower-i upper-i) 100)
        (-> (map #(bit-xor query %)
                 (subvec nums upper-i lower-i))
            (->> (reduce max)))

        :else
        (recur (dec bit) upper lower)))))

(defn main
  []
  (let [n (read)
        arr (-> (repeatedly n read)
                (->> (sort >))
                vec)
        m (read)
        queries (repeatedly m read)]
    (run! (comp println (partial solve arr))
          queries)))

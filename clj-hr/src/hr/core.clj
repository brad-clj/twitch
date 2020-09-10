(ns hr.core)

(set! *warn-on-reflection* true)

;; https://www.hackerrank.com/challenges/maximum-xor

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
         lower 0
         upper-i 0
         lower-i (count nums)]
    (let [bit-set? (bit-test query bit)
          upper (long ((if bit-set? bit-clear bit-set) upper bit))
          lower (long ((if bit-set? bit-clear bit-set) lower bit))
          upper-i2 (-> (search nums upper
                               upper-i lower-i)
                       first
                       long)
          lower-i2 (-> (search nums lower
                               upper-i lower-i)
                       second
                       long)]
      (cond
        (== upper-i2 lower-i2)
        (let [upper (long ((if bit-set? bit-set bit-clear) upper bit))
              lower (long ((if bit-set? bit-set bit-clear) lower bit))]
          (recur (dec bit)
                 upper
                 lower
                 upper-i
                 lower-i))

        (<= (- lower-i2 upper-i2) 10)
        (-> (map #(bit-xor query %)
                 (subvec nums upper-i2 lower-i2))
            (->> (reduce max)))

        :else
        (recur (dec bit)
               upper
               lower
               upper-i2
               lower-i2)))))

(defn main
  []
  (let [n (read)
        arr (-> (repeatedly n read)
                (->> (sort >))
                vec)
        m (read)
        queries (-> (repeatedly m read) doall)
        lines (-> (map (partial solve arr) queries) doall)]
    (run! println lines)))

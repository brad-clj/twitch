(ns hr.core)

(set! *warn-on-reflection* true)

(require 'clojure.string)

(defn process-words
  [words]
  (reduce (fn [m word]
            (update m (count word)
                    (fn [fm]
                      (update (or fm {}) word
                              #(inc (or % 0))))))
          {} words))

(defn process-crossword
  [crossword]
  (let [[blanks index]
        (loop [blanks {}
               index {}
               column-pos 0
               line-pos 0
               right-scanned #{}
               down-scanned #{}]
          (cond
            (>= line-pos 10)
            [blanks index]

            (>= column-pos 10)
            (recur blanks
                   index
                   0
                   (inc line-pos)
                   right-scanned
                   down-scanned)

            :else
            (let [get-blank
                  (fn [blanks index scanned direction]
                    (let [inc-pos (fn [[line-pos column-pos]]
                                    (if (= direction :right)
                                      [line-pos (inc column-pos)]
                                      [(inc line-pos) column-pos]))]
                      (if-not (and (not (scanned [line-pos column-pos]))
                                   (= \- (get-in crossword [line-pos column-pos]))
                                   (= \- (get-in crossword (inc-pos [line-pos column-pos]))))
                        [blanks index scanned]
                        (let [ps (-> (fn [pos]
                                       (let [pos (inc-pos pos)]
                                         (if (= \- (get-in crossword pos))
                                           pos)))
                                     (iterate [line-pos column-pos])
                                     (->> (take-while some?)))
                              blanks-insert-k (count blanks)]
                          [(assoc blanks blanks-insert-k
                                  {:line-pos line-pos
                                   :column-pos column-pos
                                   :length (count ps)
                                   :direction direction
                                   :intersect {}})
                           (reduce (fn [index p]
                                     (update index p #(conj (or % #{}) blanks-insert-k)))
                                   index ps)
                           (into scanned ps)]))))

                  [blanks index right-scanned] (get-blank blanks index right-scanned :right)
                  [blanks index down-scanned] (get-blank blanks index down-scanned :down)]
              (recur blanks
                     index
                     (inc column-pos)
                     line-pos
                     right-scanned
                     down-scanned))))
        intersects (filter (fn [[k v]] (== (count v) 2))
                           index)]
    (reduce (fn [blanks [point ks]]
              (let [[k1 k2] (seq ks)]
                (-> blanks
                    (update-in [k1 :intersect] #(assoc % k2 point))
                    (update-in [k2 :intersect] #(assoc % k1 point)))))
            blanks intersects)))

(defn process-stack
  [stack]
  (let [{:keys [blanks words]} (peek stack)
        stack (pop stack)
        blanks-k (some (fn [k]
                         (if-not (-> (blanks k) :word)
                           k))
                       (keys blanks))
        blank (blanks blanks-k)]
    (into stack
          (-> (blank :length) words keys
              (->> (filter (fn [word]
                             (every? (fn [[int-k [int-line int-column]]]
                                       (let [intersect-blank (blanks int-k)]
                                         (if-not (intersect-blank :word)
                                           true
                                           (let [blank-word-pos (if (= (blank :direction) :right)
                                                                  (- int-column (blank :column-pos))
                                                                  (- int-line (blank :line-pos)))
                                                 intersect-word-pos (if (= (intersect-blank :direction) :right)
                                                                      (- int-column (intersect-blank :column-pos))
                                                                      (- int-line (intersect-blank :line-pos)))]
                                             (= (get word blank-word-pos)
                                                (get (intersect-blank :word) intersect-word-pos))))))
                                     (blank :intersect))))
                   (map (fn [word]
                          {:blanks (assoc-in blanks [blanks-k :word] word)
                           :words (if (<= (get-in words [(blank :length) word]) 1)
                                    (update words (blank :length)
                                            #(dissoc % word))
                                    (update-in words [(blank :length) word]
                                               dec))})))))))
(defn solve
  [crossword words]
  (let [initial-data {:blanks (process-crossword crossword)
                      :words (process-words words)}
        solved? (fn [stack]
                  (let [{:keys [blanks]} (peek stack)]
                    (every? :word (vals blanks))))]
    (loop [stack (process-stack [initial-data])]
      (cond
        (empty? stack)
        nil

        (solved? stack)
        (-> (peek stack) :blanks)

        :else
        (recur (process-stack stack))))))

(defn format-crossword
  [blanks]
  (let [m (reduce (fn [m blank]
                    (-> (map vector
                             (blank :word)
                             (iterate (fn [[line-pos column-pos]]
                                        (if (= (blank :direction) :right)
                                          [line-pos (inc column-pos)]
                                          [(inc line-pos) column-pos]))
                                      [(blank :line-pos) (blank :column-pos)]))
                        (->> (reduce (fn [m [c [line-pos column-pos]]]
                                       (assoc m [line-pos column-pos] c))
                                     m))))
                  {} (vals blanks))]
    (-> (mapv (fn [line-pos]
                (apply str (map (fn [column-pos]
                                  (get m [line-pos column-pos] \+))
                                (range 10))))
              (range 10))
        (->> (interpose \newline)
             (apply str)))))

(defn main
  []
  (let [crossword (mapv (fn [_]
                          (vec (read-line)))
                        (range 10))
        words (-> (read-line)
                  (clojure.string/split #";"))]
    (-> (solve crossword words)
        format-crossword
        println)))

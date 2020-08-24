(ns hr.core)

(set! *warn-on-reflection* true)

(require 'clojure.string)

(defn process-words
  [words]
  (reduce (fn [m word]
            (update m (count word)
                    #(conj (or % []) word)))
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

(let [crossword
      [[\+ \- \+ \+ \+ \+ \+ \+ \+ \+]
       [\+ \- \+ \+ \+ \+ \+ \+ \+ \+]
       [\+ \- \+ \+ \+ \+ \+ \+ \+ \+]
       [\+ \- \- \- \- \- \+ \+ \+ \+]
       [\+ \- \+ \+ \+ \- \+ \+ \+ \+]
       [\+ \- \+ \+ \+ \- \+ \+ \+ \+]
       [\+ \+ \+ \+ \+ \- \+ \+ \+ \+]
       [\+ \+ \- \- \- \- \- \- \+ \+]
       [\+ \+ \+ \+ \+ \- \+ \+ \+ \+]
       [\+ \+ \+ \+ \+ \- \+ \+ \+ \+]]

      words ["LONDON" "DELHI" "ICELAND" "ANKARA"]]
  (process-crossword crossword)
  (process-words words)
  (solve crossword words))

(defn solve
  [crossword words]
  (let [initial-data {:blanks (process-crossword crossword)
                      #_{0 {:line-pos 0, :column-pos 1, :length 6, :direction :down, :intersect {1 [3 1]}}
                         1 {:line-pos 3, :column-pos 1, :length 5, :direction :right, :intersect {0 [3 1], 2 [3 5]}}
                         2 {:line-pos 3, :column-pos 5, :length 7, :direction :down, :intersect {3 [7 5], 1 [3 5]}}
                         3 {:line-pos 7, :column-pos 2, :length 6, :direction :right, :intersect {2 [7 5]}}}
                      :words (process-words words)
                      ;; todo: process-words should emit a frequency map as the values???
                      #_{6 ["LONDON" "ANKARA"]
                         5 ["DELHI"]
                         7 ["ICELAND"]}}
        process (fn [stack]
                  (let [{:keys [blanks words]} (peek stack)
                        stack (pop stack)
                        blanks-k (some #(if-not (-> (blanks %) :word) %)
                                       (keys blanks))
                        word-length (-> (blanks blanks-k) :length)
                        ]
                    (into stack
                          (mapcat (fn [word]
                                    ;; todo: build out stack entry if the word can fit at the blanks-k. so this will entail
                                    ;; checking the :intersect(s) don't conflict with an existing word, if we are good we emit
                                    ;; a map with :blanks and :words updated where :blanks now has a :word key and :words has
                                    ;; the word disjoined, if we are not good, we just emit an empty seq, so mapcat will not
                                    ;; include it in the collection
                                    )
                                  (words word-length)))))
        solved? (fn [stack]
                  (let [{:keys [blanks]} (peek stack)]
                    (every? #(:word %) blanks)))]
    (loop [stack (process [initial-data])]
      (cond
        (empty? stack)
        nil

        (solved? stack)
        (peek stack)

        :else
        (recur (process stack))))))

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

{5 ["DELHI"]
 6 ["LONDON"
    "ANKARA"]
 7 ["ICELAND"]}

([[3 1] #{0 1}]
 [[7 5] #{3 2}]
 [[3 5] #{1 2}])

{0 {:line-pos 0, :column-pos 1, :length 6, :direction :down, :intersect {1 [3 1]}}
 1 {:line-pos 3, :column-pos 1, :length 5, :direction :right, :intersect {0 [3 1], 2 [3 5]}}
 2 {:line-pos 3, :column-pos 5, :length 7, :direction :down, :intersect {1 [3 5], 3 [7 5]}}
 3 {:line-pos 7, :column-pos 2, :length 6, :direction :right, :intersect {2 [7 5]}}}

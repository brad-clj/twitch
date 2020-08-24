(ns hr.core)

(set! *warn-on-reflection* true)

(require 'clojure.string)

(defn solve
  [])

(defn main
  []
  (let [crossword (mapv (fn [_]
                          (vec (read-line)))
                        (range 10))
        words (-> (read-line)
                  (clojure.string/split #";"))]
    (prn crossword)
    (prn words)))

(defn process-crossword
  [crossword]
  (let [[blanks index]
        (loop [blanks []
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
            (let [[blanks index right-scanned]
                  (if-not (and (not (right-scanned [line-pos column-pos]))
                               (= \- (get-in crossword [line-pos column-pos]))
                               (= \- (get-in crossword [line-pos (inc column-pos)])))
                    [blanks index right-scanned]
                    (let [ps (-> (fn [[line-pos column-pos]]
                                   (let [column-pos (inc column-pos)]
                                     (if (= \- (get-in crossword [line-pos column-pos]))
                                       [line-pos column-pos])))
                                 (iterate [line-pos column-pos])
                                 (->> (take-while some?)))]
                      [(conj blanks
                             {:line-pos line-pos
                              :column-pos column-pos
                              :length (count ps)
                              :direction :right})
                       (reduce (fn [index p]
                                 (update index p
                                         #(conj (or % #{})
                                                (count blanks))))
                               index ps)
                       (into right-scanned ps)]))

                  [blanks index down-scanned]
                  (if-not (and (not (down-scanned [line-pos column-pos]))
                               (= \- (get-in crossword [line-pos column-pos]))
                               (= \- (get-in crossword [(inc line-pos) column-pos])))
                    [blanks index down-scanned]
                    (let [ps (-> (fn [[line-pos column-pos]]
                                   (let [line-pos (inc line-pos)]
                                     (if (= \- (get-in crossword [line-pos column-pos]))
                                       [line-pos column-pos])))
                                 (iterate [line-pos column-pos])
                                 (->> (take-while some?)))]
                      [(conj blanks
                             {:line-pos line-pos
                              :column-pos column-pos
                              :length (count ps)
                              :direction :down})
                       (reduce (fn [index p]
                                 (update index p
                                         #(conj (or % #{})
                                                (count blanks))))
                               index ps)
                       (into down-scanned ps)]))]
              (recur blanks
                     index
                     (inc column-pos)
                     line-pos
                     right-scanned
                     down-scanned))))]
    [blanks index]
    ))

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
       [\+ \+ \+ \+ \+ \- \+ \+ \+ \+]]]
  (process-crossword crossword))

[[{:line-pos 0, :column-pos 1, :length 6, :direction :down}
  {:line-pos 3, :column-pos 1, :length 5, :direction :right}
  {:line-pos 3, :column-pos 5, :length 7, :direction :down}
  {:line-pos 7, :column-pos 2, :length 6, :direction :right}]
 {[7 6] #{3}
  [7 7] #{3}
  [7 2] #{3}
  [7 4] #{3}
  [3 3] #{1}
  [1 1] #{0}
  [3 4] #{1}
  [7 3] #{3}
  [6 5] #{2}
  [4 1] #{0}
  [5 1] #{0}
  [8 5] #{2}
  [5 5] #{2}
  [4 5] #{2}
  [3 1] #{0 1}
  [2 1] #{0}
  [9 5] #{2}
  [7 5] #{3 2}
  [3 5] #{1 2}
  [3 2] #{1}
  [0 1] #{0}}]

[{:down [0 3]
  :right [1 0]}
  ]

["LONDON" "DELHI" "ICELAND" "ANKARA"]

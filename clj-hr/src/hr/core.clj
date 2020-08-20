(ns hr.core)

(set! *warn-on-reflection* true)

(defn build-tree
  [tree-values tree-edges]
  (loop [tree (reduce (fn [tree [a b]]
                        (let [av (update (tree (dec a)) :children #(conj % (dec b)))
                              bv (update (tree (dec b)) :children #(conj % (dec a)))]
                          (-> tree
                              (assoc! (dec a) av)
                              (assoc! (dec b) bv))))
                      (-> (mapv (fn [value]
                                  {:value value
                                   :children #{}})
                                tree-values)
                          transient)
                      tree-edges)
         stack [0]
         visited #{}]
    (if (empty? stack)
      (persistent! tree)
      (let [selected-node (peek stack)]
        (if-not (visited selected-node)
          (recur (reduce (fn [tree child-node]
                           (let [child-node-v (-> (tree child-node)
                                                  (update :children #(disj % selected-node)))]
                             (assoc! tree child-node child-node-v)))
                         tree
                         (get-in tree [selected-node :children]))
                 (into stack (get-in tree [selected-node :children]))
                 (conj visited selected-node))
          (let [total-sum (reduce +
                                  (get-in tree [selected-node :value])
                                  (map #(get-in tree [% :value])
                                       (get-in tree [selected-node :children])))
                selected-node-v (-> (tree selected-node)
                                    (assoc :value total-sum))]
            (recur (assoc! tree selected-node selected-node-v)
                   (pop stack)
                   visited)))))))

(defn solve
  [tree-values tree-edges]
  (let [tree (build-tree tree-values tree-edges)
        root-value (get-in tree [0 :value])]
    (loop [min-result-value nil
           stack [0]
           visited #{}
           visited-sums #{}
           root-complement-sums #{}]
      (if (empty? stack)
        (or min-result-value -1)
        (let [selected-node (peek stack)
              selected-node-value (get-in tree [selected-node :value])]
          (if-not (visited selected-node)
            (let [result-value (if (and (or (visited-sums (* 2 selected-node-value))
                                            (visited-sums (- root-value
                                                             (* 2 selected-node-value))))
                                        (>= (* 3 selected-node-value) root-value))
                                 (- (* 3 selected-node-value)
                                    root-value))
                  min-result-value (if (and min-result-value result-value)
                                     (min min-result-value result-value)
                                     (or min-result-value result-value))
                  stack (into stack (get-in tree [selected-node :children]))
                  visited (conj visited selected-node)
                  root-complement-sums (conj root-complement-sums (- root-value selected-node-value))]
              (recur min-result-value
                     stack
                     visited
                     visited-sums
                     root-complement-sums))
            (let [selected-sum-comp (- root-value selected-node-value)
                  selected-sum-comp-half (quot selected-sum-comp 2)

                  result-value
                  (cond
                    (== (* 2 selected-node-value) root-value)
                    selected-node-value

                    (and (or (visited-sums selected-node-value)
                             (root-complement-sums selected-node-value))
                         (>= (* 3 selected-node-value) root-value))
                    (- (* 3 selected-node-value)
                       root-value)

                    (and (== (rem selected-sum-comp 2) 0)
                         (> selected-sum-comp-half selected-node-value)
                         (or (visited-sums selected-sum-comp-half)
                             (root-complement-sums selected-sum-comp-half)))
                    (- selected-sum-comp-half selected-node-value))

                  min-result-value (if (and min-result-value result-value)
                                     (min min-result-value result-value)
                                     (or min-result-value result-value))
                  stack (pop stack)
                  visited-sums (conj visited-sums selected-node-value)
                  root-complement-sums (disj root-complement-sums selected-sum-comp)]
              (recur min-result-value
                     stack
                     visited
                     visited-sums
                     root-complement-sums))))))))

(defn main
  []
  (let [q (read)]
    (-> (fn []
          (let [n (read)
                c (-> (repeatedly n read) doall)
                edges (-> (fn []
                            (let [x (read)
                                  y (read)]
                              [x y]))
                          (->> (repeatedly (dec n)))
                          doall)]
            (println (solve c edges))))
        (->> (repeatedly q))
        dorun)))

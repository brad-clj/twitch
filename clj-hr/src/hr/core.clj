(ns hr.core)

(set! *warn-on-reflection* true)

(require 'clojure.set)

(defn normalized-ed-key
  [n1 n2]
  (-> (sort [n1 n2]) vec))

(defn make-edge-data
  [orig-graph me-nodes]
  (loop [edge-data {}
         graph orig-graph
         me-nodes me-nodes]
    (if (<= (count graph) 1)
      edge-data
      (let [[me-node & me-nodes] me-nodes
            conn-node (-> me-node graph second first)
            ed-key (normalized-ed-key me-node conn-node)
            ed-val (let [ed-keys (-> me-node
                                     orig-graph
                                     second
                                     (->> (remove #{conn-node})
                                          (map (partial normalized-ed-key me-node))))]
                     (reduce (fn [ed-val ed-key]
                               (-> ed-val
                                   (update :edges #(into % (get-in edge-data [ed-key :edges])))
                                   (update :weight #(+ % (get-in edge-data [ed-key :weight])))))
                             {:side (if (= me-node (first ed-key)) :left :right)
                              :edges (into #{} ed-keys)
                              :weight (-> me-node graph first)}
                             ed-keys))
            graph (-> graph
                      (dissoc me-node)
                      (update-in [conn-node 1] #(disj % me-node)))
            me-nodes (cond-> me-nodes
                       (== 1 (-> conn-node graph second count))
                       (conj conn-node))]
        (recur (assoc edge-data ed-key ed-val)
               graph
               me-nodes)))))

(defn which-side
  [left right]
  (cond
    ;; hr problem description is lacking, sorry about this hack...
    (== left right)
    [:down-the-middle #{0 left}]

    (and (> left right)
         (== (rem left 2) 0)
         (>= (quot left 2) right))
    [:left #{(quot left 2)}]

    (and (> right left)
         (== (rem right 2) 0)
         (>= (quot right 2) left))
    [:right #{(quot right 2)}]

    (and (> left right)
         (< (/ left 2) right))
    [:left #{right (- left right)}]

    (and (> right left)
         (< (/ right 2) left))
    [:right #{left (- right left)}]))

(defn solve
  [graph]
  (let [total-wt (-> graph vals
                     (->> (map first)
                          (reduce +)))
        me-nodes (-> graph keys
                     (->> (filter #(-> (graph %) second count ((partial == 1))))))
        edge-data (make-edge-data graph me-nodes)
        edges (set (keys edge-data))]
    (-> (reduce (fn [ans ed-key]
                  (let [side (-> (edge-data ed-key) :side)
                        weight (-> (edge-data ed-key) :weight)
                        [direction split-wts] (if (= side :left)
                                                (which-side weight (- total-wt weight))
                                                (which-side (- total-wt weight) weight))
                        possible-ans (if split-wts
                                       (let [wts (conj split-wts weight)]
                                         (- (apply max wts)
                                            (apply min wts))))]
                    (cond
                      (and (= direction :down-the-middle)
                           (or (nil? ans)
                               (< possible-ans ans)))
                      possible-ans

                      (or (nil? possible-ans)
                          (and (some? ans) (< ans possible-ans))
                          (-> (if (= side direction)
                                (-> (edge-data ed-key) :edges)
                                (clojure.set/difference edges
                                                        (-> (edge-data ed-key) :edges)
                                                        #{ed-key}))
                              (->> (filter (fn [ed-key2]
                                             (let [ed-val2 (edge-data ed-key2)
                                                   split-v (if ((-> ed-val2 :edges) ed-key)
                                                             (- total-wt (-> ed-val2 :weight))
                                                             (-> ed-val2 :weight))]
                                               (split-wts split-v)))))
                              empty?))
                      ans

                      :else
                      possible-ans)))
                nil (keys edge-data))
        (or -1))))

(defn process-edges
  [edges c]
  (let [update-fn (fn [data x y]
                    (let [[wt nodes] (or data [(c (dec x)) #{}])]
                      [wt (conj nodes y)]))]
    (reduce (fn [graph [x y]]
              (-> graph
                  (update x update-fn x y)
                  (update y update-fn y x)))
            {} edges)))

(defn main
  []
  (let [q (read)]
    (-> (fn []
          (let [n (read)
                c (mapv (fn [_] (read)) (range n))
                graph (-> (fn []
                            (let [x (read)
                                  y (read)]
                              [x y]))
                          (->> (repeatedly (dec n)))
                          (process-edges c))]
            (println (solve graph))))
        (->> (repeatedly q))
        dorun)))

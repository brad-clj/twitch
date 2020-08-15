(ns hr.core)

(set! *warn-on-reflection* true)

1 2 2 1 1

7

1 2 [5 2]
1 3 [4 3]
3 5 [6 1]
1 4 [6 1]

:total-wt 7
:wt-and-edges-by-node
{1 [1 #{4 3 2}], 2 [2 #{1}], 3 [2 #{1 5}], 4 [1 #{1}], 5 [1 #{3}]}
:edge-node-by-node-mutating
{}

100

47 47 (- 47 6)
(let [twt 7
      gedge 4
      ledge (- twt gedge)]
  (- ledge (- twt (* 2 ledge))))

(def graph-data-by-edge
  {[1 2] {:side :right
          :nodes #{2}
          :edges #{}
          :weight 2}
   [1 4] {:side :right
          :nodes #{4}
          :edges #{}
          :weight 1}
   [1 3] {:side :left
          :nodes #{1 2 4}
          :edges #{[1 2] [1 4]}
          :weight 4}
   [3 5] {:side :left
          :nodes #{1 2 4 3}
          :edges #{[1 3] [1 2] [1 4]}
          :weight 6}})
:mono-edge-nodes
[]

(let [twt 7
      edge 6
      gedge (max edge (- twt edge))
      ledge (- twt gedge)]
  (- ledge (- twt (* 2 ledge))))

{[3 5] {:side :right
        :nodes #{5}
        :edges #{}
        :weight 1}
 [1 3] {:side :right
        :nodes #{3 5}
        :edges #{[3 5]}
        :weight 3}
 [1 2] {:side :right
        :nodes #{2}
        :edges #{}
        :weight 2}
 [1 4] {:side :left
        :nodes #{3 5 2 1}
        :edges #{[1 3] [1 2] [3 5]}
        :weight 6}}

(defn solve
  [graph]
  (let [total-wt (-> graph vals
                     (->> (map first)
                          (reduce +)))
        me-nodes (-> graph keys
                     (->> (filter #(-> (graph %)
                                       second
                                       count
                                       ((partial == 1))))))]
    [graph total-wt me-nodes]))

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
                (prn (solve graph))))
          (->> (repeatedly q))
          dorun)))

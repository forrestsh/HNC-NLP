(ns hnc-parse.graph)

(defn generate-graph [v]
  (let [n (inc (count v)) ; Number of nodes
        node-ids (range n) ; Node IDs from 0 to n-1
        ; Generate the nodes map with sets for :next and :prev
        nodes-map (into {}
                    (map (fn [i]
                           [i {:next (if (< i (dec n)) #{(inc i)} #{})
                               :prev (if (> i 0) #{(dec i)} #{})}])
                         node-ids))
        ; Generate the edge keys
        edge-keys (map vector (range (count v)) (map inc (range (count v))))
        ; Generate the edges map
        edges-map (zipmap edge-keys (map #(hash-set %) v))]
    {:nodes nodes-map
     :edges edges-map}))

;; Example usage
(println (generate-graph [\H \e]))

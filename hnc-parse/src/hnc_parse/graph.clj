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

 ; Map from [i j] to set of elements

(defn dg-add-edge
  "Add an edge from node i to node j with the given element.
   Updates the graph's nodes and edges accordingly."
  [graph i j element]
  (let [nodes (:nodes graph)
        edges (:edges graph)
        ;; Ensure nodes i and j exist
        node-i (get nodes i {:next #{}, :prev #{}})
        node-j (get nodes j {:next #{}, :prev #{}})
        ;; Update the edge
        edge-key [i j]
        edge-elements (get edges edge-key #{})
        new-edge-elements (conj edge-elements element)
        ;; Update nodes
        new-node-i (update node-i :next conj j)
        new-node-j (update node-j :prev conj i)]
    {:nodes (assoc nodes
              i new-node-i
              j new-node-j)
     :edges (assoc edges
              edge-key new-edge-elements)}))

(defn dg-remove-edge
  "Remove the element from the edge from node i to node j.
   If the edge set becomes empty, remove the edge and update nodes."
  [graph i j element]
  (let [nodes (:nodes graph)
        edges (:edges graph)
        edge-key [i j]
        edge-elements (get edges edge-key)]
    (if edge-elements
      (let [new-edge-elements (disj edge-elements element)]
        (if (empty? new-edge-elements)
          ;; Remove the edge completely
          (let [new-edges (dissoc edges edge-key)
                ;; Update node i's :next by removing j
                node-i (get nodes i {:next #{}, :prev #{}})
                updated-node-i (update node-i :next disj j)
                ;; Update node j's :prev by removing i
                node-j (get nodes j {:next #{}, :prev #{}})
                updated-node-j (update node-j :prev disj i)
                ;; Remove nodes if both :next and :prev are empty (optional)
                nodes-without-i (if (and (empty? (:next updated-node-i))
                                         (empty? (:prev updated-node-i)))
                                  (dissoc nodes i)
                                  (assoc nodes i updated-node-i))
                nodes-without-j (if (and (empty? (:next updated-node-j))
                                         (empty? (:prev updated-node-j)))
                                  (dissoc nodes-without-i j)
                                  (assoc nodes-without-i j updated-node-j))]
            {:nodes nodes-without-j
             :edges new-edges})
          ;; Edge still has elements, update the edge
          {:nodes nodes
           :edges (assoc edges edge-key new-edge-elements)}))
      ;; Edge does not exist; return graph unchanged
      graph)))

;; Initialize the graph
(def graph (dg-init))

;; Add edge from 0 to 1 with element \H
(def graph1 (dg-add-edge graph 0 1 \H))

;; Add edge from 1 to 2 with element \e
(def graph2 (dg-add-edge graph1 1 2 \e))

;; The graph now looks like:
;; {:nodes {0 {:next #{1}, :prev #{}},
;;          1 {:next #{2}, :prev #{0}},
;;          2 {:next #{}, :prev #{1}}},
;;  :edges {[0 1] #{\H}, [1 2] #{\e}}}

;; Remove element \H from edge [0 1]
(def graph3 (dg-remove-edge graph2 0 1 \H))

;; After removal, the graph is:
;; {:nodes {0 {:next #{}, :prev #{}},
;;          1 {:next #{2}, :prev #{}}},
;;  :edges {[1 2] #{\e}}}

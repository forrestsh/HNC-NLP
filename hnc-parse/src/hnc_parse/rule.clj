(ns hnc-parse.rule)

(def rules
  [
   {:lhs [:is-string :is-char-space] :rhs #(list %2 %1)}
   {:lhs [:is-list :is-string] :rhs #(str %1 %2)}
   ]
)

(def conditions
    {
        :is-string #(string? %1)
        :is-char-space #(= %1 \space)
        :is-list #(list? %1)
    }
)

(defn get-index [rules conditions]
  (let [all-conditions (keys conditions)]
    (into {}
          (for [cond all-conditions]
            [cond (set (for [[rule-idx rule] (map-indexed vector rules)
                             [pos-idx c] (map-indexed vector (:lhs rule))
                             :when (= c cond)]
                         [rule-idx pos-idx]))]))))

(def rule-index (get-index rules conditions))

(defn match-rules [exp]
  (let [satisfied-conds (filter (fn [cond]
                                  ((conditions cond) exp))
                                (keys conditions))]
    (set (mapcat rule-index satisfied-conds))))

((conditions :is-string) "Hello")
((conditions :is-char-space) \space)
((:rhs (rules 0)) '(God) '(CREATE))
;; (:rhs (rules 0))

(get-index rules conditions)


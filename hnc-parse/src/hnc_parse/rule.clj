(ns hnc-parse.rule)

(def rules
  [
;;  {:lhs [:is-string :is-char-space] :rhs #(list %2 %1)}
;;  {:lhs [:is-list :is-string] :rhs #(str %1 %2)}
   {:lhs [:is-non-space-char] :rhs #(str %1)}
   {:lhs [:is-string :is-non-space-char] :rhs #(str %1 %2)}
   {:lhs [:is-char-space] :rhs (fn [s] :Spaces)}
   {:lhs [:is-Spaces :is-char-space] :rhs (fn [a b] :Spaces)}
   {:lhs [:is-string :is-Spaces] :rhs (fn [a b] a)}
   {:lhs [:is-Spaces :is-string] :rhs (fn [a b] b)}
   {:lhs [:is-string-hello :is-string-world :is-question-mark] :rhs (fn [a b c] '(Greeting))}
   ]
)

(defn compare-case-insensitive [s1 s2]
  (compare (clojure.string/lower-case s1)
           (clojure.string/lower-case s2)))

(def conditions
    {
        :is-string #(string? %1)
        :is-char-space #(= %1 \space)
        :is-list #(list? %1)
        :is-non-space-char #(and (char? %1) (not= %1 \space))
        :is-Spaces #(= %1 :Spaces)
        :is-string-hello #(= 0 (compare-case-insensitive %1 "hello"))
        :is-string-world #(= 0 (compare-case-insensitive %1 "world"))
        :is-question-mark #(= 0 (compare-case-insensitive %1 "!"))
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


(defn completion? [item]
  (case (:type item)
    :lhs (let [rn ((:rule item) 0)
               b ((:rule item) 1)
               e ((:rule item) 2)]
;;           (println rn b e (count (:lhs (rules rn))))
           (and (= b 0) (= e (count (:lhs (rules rn)))))
           )
    :rhs false
    :term false
    )
)

(defn exec [item]
  (let [rn ((:rule item) 0)
        rule-func (:rhs (rules rn))
        exp (:exp item)]
    (array-map :type :rhs :rule (:rule item) :exp (apply rule-func exp))
    )
)

((conditions :is-string) "Hello")
((conditions :is-char-space) \space)
;; ((:rhs (rules 0)) '(God) '(CREATE))
;; (:rhs (rules 0))

(get-index rules conditions)


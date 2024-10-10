(ns hnc-parse.parse)

(defn print-queue [q]
  (if (instance? clojure.lang.PersistentQueue q)
    (str "<-" (vec q) "<-")
    (str q)))

(defn println-queue [q]
  (println (print-queue q)))

(vec "中文")

(defn create-chart [v]
  (let [N (count v)
        empty-matrix (vec (repeat (inc N) (vec (repeat (inc N) (sorted-set)))))]
    (reduce (fn [m i]
              (assoc-in m [i (inc i)] (sorted-set (v i))))
            empty-matrix
            (range N))))

(defn create-empty-chart [v]
  (let [N (count v)]
    (vec (repeat (inc N) (vec (repeat (inc N) (sorted-set)))))))

(create-chart (vec "Hello"))
(create-chart (vec "中文"))

(defn create-queue
  ([]
   (clojure.lang.PersistentQueue/EMPTY))
  ([v] 
   ;;(reduce conj clojure.lang.PersistentQueue/EMPTY v)
   (reduce conj clojure.lang.PersistentQueue/EMPTY
          (map-indexed (fn [i e] {:begin i :end (inc i) :value e}) v))))

(def word-table ["Hello" "World"])

(defn match-words [chars word-table]
  (let [char-str (apply str chars)
        matches  (vec (filter #(= char-str %) word-table))]
    (if (empty? matches)
      chars
      matches)))

;; Example usage:
(match-words [\H \e \l \l \o] ["Hello" "World"])
;; Returns: ["Hello"]


(defn combine3 [s item]
  (let [combined (flatten (vec [s item]))]
    (match-words combined word-table)
))

(defn combine2 [s item]
  (let [combined (str s item)]
    (case combined
      "Hello" :Hello
      "World" :World
      combined)))

(defn combine [s item]
  (str s item)
)

(defn parse0 [chart queue]
  (println "parse")
)


(defn parse1 [chart queue]
  (if (empty? queue)
    {:chart chart :queue queue}
    (let [element    (peek queue)
          queue-rest (pop queue)
          b          (:begin element)
          e          (:end element)
          v          (:value element)
          ;; Update the chart at position [b][e] by adding 'v' to the sorted-set
          updated-chart (update-in chart [b e] conj v)]
          
      ;; Recursively call Parse with the updated chart and the rest of the queue
      (parse1 updated-chart queue-rest))))

(defn parse [chart queue]
  (if (empty? queue)
    {:chart chart :queue queue}
    (let [item        (peek queue)
          queue-rest  (pop queue)
          b           (:begin item)
          e           (:end item)
          v           (:value item)
          ;; Update the chart at position [b][e] by adding 'item' to the sorted-set
          updated-chart (update-in chart [b e] conj v)
          N           (dec (count chart))]  ; Assuming chart is (N+1) x (N+1)
      
      ;; Define a helper function to process combinations and update the queue
      (letfn [(process-combinations-before [queue]
                (reduce
                  (fn [q1 pos]
                    (reduce conj q1
                      (reduce
                        (fn [q2 s]
                          (let [sv (combine s v)
                                r {:begin pos :end e :value sv}]
                            (if (contains? (get-in updated-chart [pos e]) sv)
                              q2
                              (conj q2 r))))
                        []
                        (get-in updated-chart [pos b]))))
                  queue
                  (range 0 b)))

              (process-combinations-after [queue]
                (reduce
                  (fn [q1 pos]
                    (reduce conj q1
                      (reduce
                        (fn [q2 s]
                          (let [vs (combine v s)
                                r {:begin b :end pos :value vs}]
                            (if (contains? (get-in updated-chart [b pos]) vs)
                              q2
                              (conj q2 r))))
                        []
                        (get-in updated-chart [e pos]))))
                  queue
                  (range (inc e) (inc N))))

              (process-combinations-after-1 [queue]
                (reduce
                  (fn [q pos]
                    (reduce
                      (fn [q s]
                        (let [r {:begin b :end pos :value (combine v s)}]
                          (if r
                            (conj q r)
                            q)))
                      q
                      ((fn [f] (get-in updated-chart [e f])) pos)))
                  queue
                  (range (inc e) (inc N))))
        ;;      (process-combinations-before [queue] queue)
        ;;      (process-combinations-after [queue] queue)
              ]
        
        ;; Process chart elements at locations [a][b] where a < b
        (let [queue-after-ab
              (process-combinations-before queue-rest)]
          
          ;; Process chart elements at locations [e][f] where e < f
          (let [queue-after-ef
                (process-combinations-after queue-after-ab)]
            
            ;; Recursively call Parse with the updated chart and queue
            ;;(print-queue queue)
            ;;{:chart updated-chart :queue queue-after-ef}

            [updated-chart queue-after-ef]
            ;(println "Length of Queue=" (count queue-after-ef) updated-chart)
           ;; (println updated-chart)
            (parse updated-chart queue-after-ef)
))))))

(def sentence "Hello World!")
(def test-chart (create-empty-chart (vec sentence)))
(def test-queue (create-queue (vec sentence)))

(parse test-chart test-queue)

;; Call the Parse function
(def result (parse test-chart test-queue))

;; Extract the updated chart and queue
(def updated-chart (:chart result))
(def updated-queue (:queue result))  ;; This will be empty

;; Display the updated chart
(prn updated-chart)

(def result (parse updated-chart updated-queue))

;;(parse (parse updated-chart updated-queue))
(def result (parse (result 0) (result 1)))
result

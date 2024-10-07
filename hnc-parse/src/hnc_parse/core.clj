(ns hnc-parse.core
  (:require [clojure.string :as str])
  (:require [clojure.pprint :refer [pprint]])
)

;; Define a CNF grammar (for demonstration purposes)
(def grammar
  {:S  [[:NP :VP]]               ;; S -> NP VP
   :VP [[:V :NP] [:V :PP]]       ;; VP -> V NP | V PP
   :PP [[:P :NP]]                ;; PP -> P NP
   :NP [[:Det :N]]               ;; NP -> Det N
   :Det [["a"] ["the"]]          ;; Det -> "a" | "the"
   :N  [["dog"] ["cat"] ["park"]] ;; N -> "dog" | "cat" | "park"
   '(SEE '(YOU))  [["sees"]]      ;; V -> "sees" | "likes"
   '(IN)  [["in"]]})         ;; P -> "in" | "on"

;; The input sentence to parse
(def input ["a" "dog" "sees" "in" "the" "park"])

;; Function to convert grammar to CNF form if needed (assuming CNF here)
(defn convert-to-cnf [grammar]
  grammar)  ;; For now, assume the grammar is in CNF

;; Function to initialize the hnc chart
(defn initialize-hnc-chart [n]
  (vec (repeat n (vec (repeat n #{})))))  ;; n x n chart initialized with empty sets

;; Function to get non-terminals that produce a given terminal
(defn get-nonterminals [grammar terminal]
  (for [[lhs rhs] grammar
        production rhs
        :when (and (vector? production) (= (first production) terminal))]
    lhs))

;; Function to get non-terminals that produce two non-terminals
(defn get-nonterminals-from-pair [grammar B C]
  (for [[lhs rhs] grammar
        production rhs
        :when (and (vector? production) (= production [B C]))]
    lhs))

;; hnc Parsing algorithm
(defn hnc-parse [grammar input]
  (let [n (count input)                     ;; Length of the input
        chart (atom (initialize-hnc-chart n)) ;; Initialize the chart as an atom for updates
        cnf-grammar (convert-to-cnf grammar)]

    ;; Step 1: Fill the diagonal with rules matching terminals
    (doseq [i (range n)
            :let [terminal (nth input i)
                  non-terminals (set (get-nonterminals cnf-grammar terminal))]]
      (swap! chart assoc-in [i i] non-terminals))  ;; Update atom-based chart

    ;; Step 2: Fill the upper triangle of the chart
    (doseq [span (range 2 (inc n))]          ;; span is the length of the substring
      (doseq [start (range (- n span -1))]   ;; start is the beginning of the substring
        (let [end (+ start span -1)]         ;; end is the end of the substring
          (doseq [split (range start end)]   ;; split is where the substring is divided
            (let [left (get-in @chart [start split])   ;; Left part of the split
                  right (get-in @chart [(inc split) end])]  ;; Right part of the split
              ;; Find all non-terminals that can produce the left and right parts
              (doseq [B left
                      C right
                      :let [non-terminals (get-nonterminals-from-pair cnf-grammar B C)]]
                ;; Add these non-terminals to the chart
                (swap! chart assoc-in [start end] (set (concat (get-in @chart [start end]) non-terminals))))))))

    (println "hnc Chart:")
    (pprint @chart)  ;; Pretty print the chart structure

    ;; Step 3: Check if the start symbol 'S' is in the top-right cell
    (if (contains? (get-in @chart [0 (dec n)]) :S)
      (println "The sentence is in the language.")
      (println "The sentence is not in the language."))
    @chart)))  ;; Return the filled chart

;; Uncomment the following line to run the hnc parser
(hnc-parse grammar input)

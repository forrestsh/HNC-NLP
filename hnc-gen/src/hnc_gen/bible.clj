(ns hnc-gen.bible)

(def language :English)

(defn is-single?
    [lst]
    (if (and (seq lst) (= (first lst) 'GOD))
        true
        false
    )
)

(defn make-sentence [words]
    (let
        [first-word (first words)
            capitalized-first (str (clojure.string/upper-case (subs first-word 0 1))
                               (subs first-word 1))
            rest-words (rest words)
        ]
        (str (clojure.string/join " " (cons capitalized-first rest-words)) ".")
    )
)

(defn CREATE
    ([] 
        (case language
            :English
            (vector "create")
            :Chinese
            (vector "创造")
        )
    )
    ([creator object & {:keys [tense time] 
                  :or {tense "Now" time nil}}]
         
         (case language
             :English
             (make-sentence
                 (flatten
                    (vector
                        (if (nil? time) [] (vector (eval time)))
                        (case tense
                          "Now"
                          (flatten (vector (eval creator) (if (is-single? creator) "creates" "create") (eval object)))
                          "Past"
                          (flatten (vector (eval creator) (if (is-single? creator) "created" "create") (eval object)))
                          )
                    )
                 ) 
             )
             :Chinese
             (make-sentence
                 (flatten
                    (vector
                        (if (nil? time) [] (vector (eval time)))
                          (flatten (vector (eval creator) (CREATE) (eval object)))                   
                    )
                  )  
             )
        )
    )
)

(defn GOD
    ([] 
        (case language
            :English
            (vector "God") 
            :Chinese
            (vector "神")
        )
    )
)

(defn BEGIN
    ([] 
        (case language
            :English
            (vector "in the beginning")
            :Chinese
            (vector "起初")
        )        
    )
)

(defn HEAVEN
    ([& {:keys [number spec]
         :or {number "Singular" spec "Definite"}}]
        (case language
            :English
            (case number
                "Singular"
                (case spec
                    "Definite"
                    (vector "the heaven")
                    "Indefinite"
                    (vector "a heaven")
                )
                "Plural"
                (case spec
                    "Definite"
                    (vector "the heavens")
                    "Indefinite"
                    (vector "heavens")
                )
            )
            :Chinese
            (vector "天")
        )
    )
)

(defn EARTH
    [& {:keys [number spec]
         :or {number "Singular" spec "Definite"}}]
    (case language
        :English 
        (case number
            "Singular"
            (case spec
                "Definite"
                (vector "the earth")
                "Indefinite"
                (vector "an earth")
            )
            "Plural"
            (case spec
                "Definite"
                (vector "the earths")
                "Indefinite"
                (vector "earths")
            )
        )
        :Chinese
        (vector "地")
    )
)

(defn AND
    ([x,y] 
        (case language
            :English
            (flatten (vector (eval x) "and" (eval y)))
            :Chinese
            (flatten (vector (eval x) "和" (eval y)))
        )
    )
)

(def language :English)
(defn BOOK-BIBLE
    []
    (vector
;        (CREATE '(GOD) '(AND '(HEAVEN) '(EARTH)))
        (CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN)})
    )
)

(CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN)})


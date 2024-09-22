(ns hnc-gen.bible)

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
    ([] (vector "create"))
;   ([{:keys [language]}] (if (= language "Chinese") (vector "创造") (vector "create")))
    ([creator object & {:keys [tense time] :or {tense "Now"}}]
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
    )
)

;   ([creator object & {:keys [time tense]}]
;   (if (= tense :single)
;      (flatten (vector time "," creator "creates" object))
;      (flatten (vector time "," creator "create" object)))))

(defn GOD
    ([] (vector "God")))

(defn BEGIN
    ([] (vector "begin"))
        ([x] (if (= x :time) (vector "in the beginning") (vector "begin"))))

(defn HEAVEN
    ([& {:keys [number spec]
         :or {number "Singular" spec "Definite"}}]
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
    )
)

(defn EARTH
    [& {:keys [number spec]
         :or {number "Singular" spec "Definite"}}]
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
)

(defn AND
    ([x,y] (flatten (vector (eval x) "and" (eval y)))))

(defn BOOK-BIBLE
    []
    (vector
;        (CREATE '(GOD) '(AND '(HEAVEN) '(EARTH)))
        (CREATE '(GOD) '(AND '(HEAVEN {:number "Plural" :spec "Definite"}) '(EARTH)) {:tense "Past" :time '(BEGIN :time)})
    )
)
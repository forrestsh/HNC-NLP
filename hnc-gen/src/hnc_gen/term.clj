(ns hnc-gen.term)

; Latin First Year, p6
; Latin Grammar, p5
(defn first-declension-latin
    "Generate Latin words in the first declension."
    ([stem number use-case]
        (case number
            "Singular"
            (case use-case
                "Nominative" (vector (str stem "a"))
                "Genitive" (vector (str stem "ae"))
                "Dative" (vector (str stem "ae"))
                "Accusative" (vector (str stem "am"))
                "Ablative" (vector (str stem "a"))
            )
            "Plural"
            (case use-case
                "Nominative" (vector (str stem "ae"))
                "Genitive" (vector (str stem "arum"))
                "Dative" (vector (str stem "is"))
                "Accusative" (vector (str stem "as"))
                "Ablative" (vector (str stem "is"))
            )
        )
    )
)

(defn plural
    ([word]
        (str word "s")
    )
)

(defn first-declension-english
    "Generate English words in the first declension."
    ([word number use-case]
        (case number
            "Singular"
            (case use-case
                "Nominative" (vector (str word ", the (a) " word))
                "Genitive" (vector (str "of the (a) " word))
                "Dative" (vector (str "to or for the (a) " word))
                "Accusative" (vector (str "the (a) " word))
                "Ablative" (vector (str "by, with, from the (a) " word))
            )
            "Plural"
            (case use-case
                (let [words (plural word)]
                    "Nominative" (vector (str words ", the " words))
                    "Genitive" (vector (str "of the " words))
                    "Dative" (vector (str "to or for the" words))
                    "Accusative" (vector (str words ", the " words))
                    "Ablative" (vector (str "by, with, from the " words))
                )
            )
        )
    )
)

; Latin First Year, p7
(defn LAND
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "land" number use-case)
            "Latin"
            (first-declension-latin "terr" number use-case)
        )
    )
)

(defn GATE
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "gate" number use-case)
            "Latin"
            (first-declension-latin "port" number use-case)
        )
    )
)

(defn MARY
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "Mary" number use-case)
            "Latin"
            (first-declension-latin "Mari" number use-case)
        )
    )
)

(defn SAILOR
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "Sailor" number use-case)
            "Latin"
            (first-declension-latin "naut" number use-case)
        )
    )
)

(defn VICTORY
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "victory" number use-case)
            "Latin"
            (first-declension-latin "victori" number use-case)
        )
    )
)

(defn FOREST
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "forest" number use-case)
            "Latin"
            (first-declension-latin "silv" number use-case)
        )
    )
)

(defn GLORY
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (first-declension-english "glory" number use-case)
            "Latin"
            (first-declension-latin "glori" number use-case)
        )
    )
)

(defn SINGULAR-3RD
    ([& {:keys [language]
         :or {language "English"}}]
        (case language
            "English"
            (vector "he, she, it")
            "Latin"
            (vector " ")
        )
    )
)

(defn PLURAL-3RD
    ([& {:keys [language]
         :or {language "English"}}]
        (case language
            "English"
            (vector "they")
            "Latin"
            (vector " ")
        )
    )
)

; Latin First Year, p11
(defn PRAY
    ([subject & {:keys [language]
                 :or {language "English"}}]
        (flatten
            (case language
                "English"
                (cond
                    (= subject '(SINGULAR-3RD)) (vector (eval subject) "prays")
                    (= subject '(PLURAL-3RD)) (vector (eval subject) "pray")
                    :else (vector "somebody" "pray")
                )
                "Latin"
                (cond
                    (= subject '(SINGULAR-3RD)) (vector "orat")
                    (= subject '(PLURAL-3RD)) (vector "orant")
                    :else (vector "orat")
                )
            )
        )
    )
)

(defn SEE
    ([subject & {:keys [language]
                 :or {language "English"}}]
        (flatten
            (case language
                "English"
                (cond
                    (= subject '(SINGULAR-3RD)) (vector (eval subject) "sees")
                    (= subject '(PLURAL-3RD)) (vector (eval subject) "see")
                    :else (vector "somebody" "pray")
                )
                "Latin"
                (cond
                    (= subject '(SINGULAR-3RD)) (vector "videt")
                    (= subject '(PLURAL-3RD)) (vector "vident")
                    :else (vector "videt")
                )
            )
        )
    )
)

; Latin First Year, p47
(defn BROTHER
    ([& {:keys [language]
         :or {language "English"}}]
         (case language
            "English" (vector "brother")
            "Latin" (vector "frater"))))

(defn FATHER
    ([& {:keys [language]
         :or {language "English"}}]
         (case language
            "English" (vector "father")
            "Latin" (vector "pater"))))

(defn MOTHER
    ([& {:keys [language]
         :or {language "English"}}]
         (case language
            "English" (vector "mother")
            "Latin" (vector "mater"))))

(defn MOUNTAIN
    ([& {:keys [language]
         :or {language "English"}}]
         (case language
            "English" (vector "mountain")
            "Latin" (vector "mons"))))

(defn SHOUT
    ([& {:keys [language]
         :or {language "English"}}]
         (case language
            "English" (vector "shout")
            "Latin" (vector "clamor"))))

(defn CHIEF
    ([& {:keys [language]
         :or {language "English"}}]
         (case language
            "English" (vector "chief")
            "Latin" (vector "princeps"))))


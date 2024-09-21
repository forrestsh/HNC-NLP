(ns hnc-gen.term)

; Latin First Year, p7
(defn LAND
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (case number
                "Singular"
                (case use-case
                    "Nominative" (vector "land, the (a) land")
                    "Genitive" (vector "of the (a) land")
                    "Dative" (vector "to or for the (a) land")
                    "Accusative" (vector "the (a) land")
                    "Ablative" (vector "by, with from the (a) land")
                )
                "Plural"
                (case use-case
                    "Nominative" (vector "lands, the lands")
                    "Genitive" (vector "of the lands")
                    "Dative" (vector "to or for the lands")
                    "Accusative" (vector "lands, the lands")
                    "Ablative" (vector "by, with, from the lands")
                )
            )
            "Latin"
            (case number
                "Singular"
                (case use-case
                    "Nominative" (vector "terra")
                    "Genitive" (vector "terrae")
                    "Dative" (vector "terrae")
                    "Accusative" (vector "terram")
                    "Ablative" (vector "terra")
                )
                "Plural"
                (case use-case
                    "Nominative" (vector "terrae")
                    "Genitive" (vector "terrarum")
                    "Dative" (vector "terris")
                    "Accusative" (vector "terras")
                    "Ablative" (vector "terria")
                )
            )
        )
    )
)

(defn GATE
    ([& {:keys [language number use-case]
         :or {language "English" number "Singular" use-case "Nominative"}}]
        (case language
            "English"
            (case number
                "Singular"
                (case use-case
                    "Nominative" (vector "gate, the (a) gate")
                    "Genitive" (vector "of the (a) gate")
                    "Dative" (vector "to or for the (a) gate")
                    "Accusative" (vector "the (a) gate")
                    "Ablative" (vector "by, with from the (a) gate")
                )
                "Plural"
                (case use-case
                    "Nominative" (vector "gates, the gates")
                    "Genitive" (vector "of the gates")
                    "Dative" (vector "to or for the gates")
                    "Accusative" (vector "gates, the gates")
                    "Ablative" (vector "by, with, from the gates")
                )
            )
            "Latin"
            (case number
                "Singular"
                (case use-case
                    "Nominative" (vector "porta")
                    "Genitive" (vector "portae")
                    "Dative" (vector "portae")
                    "Accusative" (vector "portam")
                    "Ablative" (vector "porta")
                )
                "Plural"
                (case use-case
                    "Nominative" (vector "portae")
                    "Genitive" (vector "portarum")
                    "Dative" (vector "portis")
                    "Accusative" (vector "portas")
                    "Ablative" (vector "portia")
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


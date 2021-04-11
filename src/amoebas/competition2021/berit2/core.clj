(ns amoebas.berit2.core
    (:use amoebas.defs amoebas.lib amoebas.run)
    )
(defn create-mindless-divider
    "create a mindless-divider with division probability division-prob"
    [division-prob]

    (fn [energy health species env data]        ;; <--- and this is the magic!

        (if (< energy (+ MinDivideEnergy (/ (- MaxAmoebaEnergy MinDivideEnergy) 2)))
            {:cmd :rest}
            (if (< (rand) division-prob)
                {:cmd :divide, :dir (rand-int 8)}
                {:cmd :move, :dir (rand-int 8)}
                )
            )
        )
    )
(def Evam (create-mindless-divider 0.3))
(def Ttest (tournament
            {
                :red (create-mindless-divider 0.3)
                })
    )
(ns amoebas.competition2021.berit2.core
    (:use amoebas.defs amoebas.lib amoebas.run)
    )
(defn create-berit2-test
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
(def Evam (create-berit2-test 0.3))

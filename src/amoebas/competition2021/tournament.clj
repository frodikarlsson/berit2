(ns amoebas.competition2021.tournament
  (:use
    amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples amoebas.competition2021.berit2.core clojure.set
    )
  )

;;
;;  Qualifying tournament
;;

(def SpeciesColors {
                    :md     (Color :blue)
                    :sb     (Color :orange)
                    :msb    (Color :cyan)

                    :this   (Color :red)    ;; in this tournament, red is the color of your bugs
                    }
  )

(defn make-qual
  [evam]

  (tournament
    {
     :md         (create-berit2-test 0.3)
     :sb         (create-slightlybrainy 10 70 most-energy-target-selector)
     :msb        (create-mutating-slightlybrainy 10 70 most-energy-target-selector 0.3 1)

     :this        evam
     }
    SpeciesColors
    )
  )

  (def Q01 (make-qual amoebas.competition2021.berit2.core/Evam))


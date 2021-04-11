(ns amoebas.competition2021.tournament
  (:use
    amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples clojure.set
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
     :md         (create-mindless-divider 0.3) 
     :sb         (create-slightlybrainy 10 70 most-energy-target-selector)
     :msb        (create-mutating-slightlybrainy 10 70 most-energy-target-selector 0.3 1)

     :this        evam
     }
    SpeciesColors
    )
  )


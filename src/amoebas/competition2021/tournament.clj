(ns amoebas.competition2021.tournament
  (:use
    amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples clojure.set
    )
    (:require
      amoebas.competition2020.theroots.core
      amoebas.competition2021.berit2.core
      amoebas.competition2021.philip.core
      amoebas.competition2019.Amoeboiz.core
      amoebas.competition2020.SuperProgMasters.core
      amoebas.competition2021.milla.core
      amoebas.competition2021.frodi.functional_core
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
     ;;:md         (create-mindless-divider 0.3)
     ;; :sb         amoebas.competition2019.Amoeboiz.core/Evam
     ;; :md        amoebas.competition2020.theroots.core/Evam
      :msb amoebas.competition2021.philip.core/Evam
     ;; :msb amoebas.competition2020.SuperProgMasters.core/Evam
     ;; :sb         (create-slightlybrainy 10 70 most-energy-target-selector)
     ;; :msb        (create-mutating-slightlybrainy 10 70 most-energy-target-selector 0.3 1)
     :this        evam
     }
    SpeciesColors
    )
  )
(def Q01 (make-qual amoebas.competition2021.berit2.core/Evam))


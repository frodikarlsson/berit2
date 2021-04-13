(ns amoebas.competition2021.andre.tournament
  (:use
    amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples clojure.set
  )
  (:require amoebas.competition2021.andre.core)
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
(def Colors
  {
    :blue       [0 0 255]
    :yellow     [255 255 0]
    :white      [255 255 255]
    :gray       [128 128 128]
    :red        [255 0 0]
    :orange     [255 128 0]
    :magenta    [255 0 255]
    :cyan       [0 255 255]
    :purple     [204 153 255]
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

(def Q01 (make-qual amoebas.competition2021.andre.core/Evam))

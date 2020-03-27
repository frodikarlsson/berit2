(ns amoebas.competition2020.tournament
    (:use 
        amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples clojure.set
    )    
;;      (:require
;;          make sure you add the package in which you define your Evam:
;;
;;          yourname.core
;;      )
)

;;
;;  Qualifying tournament
;;

(def SpeciesColors {
    :md     (Color :blue)
    :sb     (Color :orange)
    :msb    (Color :cyan)
    
    ;; add a color for your species
    ;;
    ;; :mybug   (Color :red)
    }
)

(def T (tournament
    {
        :md         (create-mindless-divider 0.3) 
        :sb         (create-slightlybrainy 10 70 most-energy-target-selector)
        :msb        (create-mutating-slightlybrainy 10 70 most-energy-target-selector 0.3 1)

    ;;  insert your species here:
    ;;  :mybug      yourname.core/Evam
    } 
    SpeciesColors)
)



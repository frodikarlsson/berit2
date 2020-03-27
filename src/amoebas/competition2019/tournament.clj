(ns amoebas.competition2019.tournament
    (:use 
        amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples clojure.set
    )    
    (:require
        amoebas.competition2019.Amoeboiz.core amoebas.competition2019.Necrosis.core
    )
)


(def SpeciesColors {
        :amoeboiz        [255 0 0] 
        :necrosis       [0 0 255] 
    }
)

(def T (tournament
    {
        :amoeboiz amoebas.competition2019.Amoeboiz.core/Evam
        :necrosis amoebas.competition2019.Necrosis.core/Evam
    } 
    SpeciesColors)
)


(ns amoebas.competition2020.tournament
    (:use 
        amoebas.defs amoebas.simulation amoebas.display amoebas.util amoebas.run amoebas.examples clojure.set
    )    
      (:require
          amoebas.competition2020.caesar.core
          amoebas.competition2020.covid.core
          amoebas.competition2020.gooftroop.core
          amoebas.competition2020.SuperProgMasters.core
          amoebas.competition2020.Untitled.core
          amoebas.competition2020.theroots.core
          amoebas.competition2020.young08.core       
          amoebas.competition2020.wuhanclan.core       
          
          amoebas.competition2019.Amoeboiz.core

      )
)

;;
;;  Qualifying tournament
;;

(def SpeciesColors {
    :md     (Color :blue)
    :sb     (Color :orange)
    :msb    (Color :cyan)
    
    :red   (Color :red)
    }
)

(defn qual2020
    [evam]

    (tournament
        {
            :md         (create-mindless-divider 0.3) 
            :sb         (create-slightlybrainy 10 70 most-energy-target-selector)
            :msb        (create-mutating-slightlybrainy 10 70 most-energy-target-selector 0.3 1)
            
            :red        evam
        } 
        SpeciesColors
    )
)

(def Q01 (qual2020 amoebas.competition2020.caesar.core/Evam))

(def Q02 (qual2020 amoebas.competition2020.covid.core/Evam))

(def Q03 (qual2020 amoebas.competition2020.gooftroop.core/Evam))

(def Q04 (qual2020 amoebas.competition2020.SuperProgMasters.core/Evam))

(def Q05 (qual2020 amoebas.competition2020.Untitled.core/Evam))

(def Q06 (qual2020 amoebas.competition2020.theroots.core/Evam))

(def Q07 (qual2020 amoebas.competition2020.young08.core/Evam))

(def Q08 (qual2020 amoebas.competition2020.wuhanclan.core/Evam))

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


(def Final
    (tournament
        {
            :blue       amoebas.competition2020.caesar.core/Evam
            
            :yellow     amoebas.competition2020.covid.core/Evam
            
            :white      amoebas.competition2020.gooftroop.core/Evam
            
            :gray       amoebas.competition2020.SuperProgMasters.core/Evam
            
            :red        amoebas.competition2020.Untitled.core/Evam
            
            :orange     amoebas.competition2020.theroots.core/Evam
            
            :magenta    amoebas.competition2020.young08.core/Evam
            
            :cyan       amoebas.competition2020.wuhanclan.core/Evam
        }
        Colors
    )
)

;;
;;  this is the all star of all champions so far
;;

(def HallOfFame
    (tournament
        {
            :red     amoebas.competition2020.theroots.core/Evam
            :blue    amoebas.competition2019.Amoeboiz.core/Evam
        }
        Colors
    )
)

            

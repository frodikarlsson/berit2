(ns amoebas.core
    (:use amoebas.defs amoebas.simulation amoebas.display amoebas.util clojure.set)    
)


(def Default-RGB-Map    ;; a few default species, named for the color that represents them
    {   
        :red [255 0 0] 
        :blue [0 0 255] 
        :yellow [255 255 0]
        :white [255 255 255]
        :gray [128 128 128]
        :orange [255 128 0]
        :magenta [255 0 255]
        :cyan [0 255 255]
    }
)



(defn step-all-and-set
    "make a step, return stats (and modify the references containing the world state)"
    [worldref popref]
    
    (let
        [
            [w p]       (step-all (deref worldref) (deref popref))
        ]
        
        (dosync
            (ref-set worldref w)
            (ref-set popref p)
        )
        (population-stats p)
    )
)



;;
;;  tournament flags: :graphics, :generational-stats, :report-extinctions, :last-gen-stats
;;                    :save-graphics :csv-stats :directory
;;

(defn tournament
    "create a tournament function, based on a genesis spec"

    (
        [genesis rgb-map]
        
        (fn [n flags]       ;; this is the tournament function
            (let
                [
                    world           (create-world MaxCellEnergy)
                    world-ref       (ref world)
                    population      (apply hash-map 
                                        (interleave
                                            (for [s (keys genesis)] (random-location))
                                            (for [s (keys genesis)] 
                                                (struct Amoeba (genesis s) s MaxAmoebaEnergy MaxAmoebaHealth)
                                            )
                                        )
                                    )
                    population-ref  (ref population)
                    
                    species-ref     (ref (keys genesis))    
                    frame           (if (:graphics flags) (create-frame "" world-ref population-ref rgb-map) nil)
                    dir             (:directory flags)
                    all-species     (vec (keys genesis))
                    stats-ref       (ref [(population-stats population)])
                ]

                (when dir
                    (create-directory dir)
                )
                (dotimes [k n]
                    (let
                        [
                            stats   (step-all-and-set world-ref population-ref)
                        ]
                    
                        (when frame 
                            (.repaint frame)
                            (when (and dir (:save-graphics flags))
                                (save-frame frame dir k)
                            )
                        )

                        (when (:generational-stats flags) (println k ": " stats))

                        (dosync (ref-set stats-ref (conj (deref stats-ref) stats)))

                        (when (:report-extinctions flags)
                            (let
                                [ 
                                    live-species  (:live-species stats) 
                                    extinctions   (remove (set live-species) (deref species-ref))
                                ]
                                
                                (when (< 0 (count extinctions))
                                    (dosync
                                        (ref-set species-ref live-species)
                                    )
                                    (println "extinct at " k ": " extinctions)
                                )
                            )
                        )
                    )
                )
                (when (flags :last-gen-stats)
                    (println "finished (" n " generations): " (population-stats (deref population-ref)))
                )
                (when (:return-last flags)
                    {:world (deref world-ref) :population (deref population-ref) :frame frame}
                )
                (when (:csv-stats flags)
                    (write-csv-stats dir all-species (deref stats-ref))
                )
            )
        )
    )
    (
        [genesis]
        
        (tournament genesis Default-RGB-Map)
    )
)

(ns amoebas.competition2021.milla.core
    (:use amoebas.defs amoebas.lib amoebas.run)
)

(defn target-selector  
    "picks a target with the lowest health score"
    [hs species env]   

    (let
        [shs (sort-by #(:health (:occupant (env %))) hs) ]
                
        (first shs)
    )
)

(defn explorer
    [direction select-target]
    
    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            (let                                        
                                [
                                    empty-nb     (empty-neighbors env)              
                                ]
                                
                                (if (empty? empty-nb)       ;; no empty neighbors?
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    (if (= data nil)
                                        {:cmd :move :dir direction :data 1}
                                        {:cmd :move, :dir direction :data (+ data 1)} 
                                    )
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                {:cmd :move, :dir (rand-int 8) :data data}
                            )
                        )
                do-div  (fn []
                            {:cmd :divide :dir (rand-int 8) :child-data data}
                            ;;(let [empty-nb   (empty-neighbors env)]
                            ;;    {:cmd :divide :dir (last (sections-by-fuel empty-nb env)) :child-data data}
                            ;;)    
                        )
            ]
            
            (cond
                (= data nil)      
                    (do-move)
                (< 10 data)
                    (do-fuel)
                (< energy MinDivideEnergy)
                    (do-fuel)
                :else
                    (if (< (rand) 0.1)
                        (do-div)
                        (do-move)
                    )    
            )            
        )
    )
)

(defn create-cell
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            (let                                        
                                [
                                    empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                                    by-fuel      (sections-by-fuel empty-nb env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                ]
                                
                                (if (empty? empty-nb)       ;; no empty neighbors?
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)                                   ;; otherwise, keep looking
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                
                                (if (empty? hs)                             ;; nobody to hit?
                                    (do-fuel)                               ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            ;; {:cmd :divide :dir (rand-nth empty-nb)}         ;; amoeba parenting: drop the child wherever...
                            (if ( < (rand) 0.1)
                                {:cmd :divide :dir (last (sections-by-fuel empty-nb env)) :function ( explorer (rand-int 8) select-target)}
                                {:cmd :divide :dir (last (sections-by-fuel empty-nb env))}
                            ) 
                            
                        )
            ]

            (cond
                (< energy low-energy)           ;; need some chow?
                    (do-fuel)
                (< divide-energy energy)               ;; parenthood!
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (hostiles species Neighbors env)            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em
                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)

(def Evam (create-cell 10 70 target-selector))
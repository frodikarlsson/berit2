(ns amoebas.competition2019.Necrosis.core
  (:use amoebas.defs amoebas.lib amoebas.run)
)

(defn most-energy-and-fuel-target-selector 
    "picks a target with the highest sum of stored energy and energy in the cell it is in"
    [hs species env]  
    
    (let
        [energy-and-fuel
            (fn [cell]
                (if (:occupant cell)
                    (+ (:fuel cell) (:energy (:occupant cell)))
                    (:fuel cell)
                )
            )
        ]

        (last (sort-by #(energy-and-fuel (env %)) hs))
    )
)

(def Dir-to-Neighbor1   ;; maps direction to the corresponding neighbor we need to move to/hit in/divide into
                        ;; it's the inverse map of Neighbors above
    { 0 [-1 -1] 1 [0 -1] 2 [1 -1] 3 [1 0] 4 [1 1] 5 [0 1] 6 [-1 1] 7 [-1 0] } 
)

(defn create-necrosis
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [
             
                do-hit2  (fn []
                             (let
                                 [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                                      ;; eat
                                  {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                               )
                         )
             
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                                    by-fuel      (sections-by-fuel empty-nb env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                ]
                                
                                (if (empty? empty-nb)       ;; no empty neighbors?
                                  (if (empty? (hostiles species Neighbors env))
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    (do-hit2)
                                    )
                                     (if (< (:fuel (env (Dir-to-Neighbor1 (last by-fuel)))) (:fuel (env Here))) 
                                          {:cmd :rest}
                                          {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel 
                                       )
                                     
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
                          (def diag [0 2 4 6])
                          (def intersect (apply clojure.set/intersection
                            (map set[empty-nb diag])))
                          (if (not (empty? intersect))
                            {:cmd :divide :dir (rand-nth (vec intersect))}
                            {:cmd :divide :dir (rand-nth empty-nb)})         ;; amoeba parenting: drop the child wherever...
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

(def Evam (create-necrosis 10 30 most-energy-and-fuel-target-selector))
  
  

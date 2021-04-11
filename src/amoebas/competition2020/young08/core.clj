(ns amoebas.competition2020.young08.core
    (:use amoebas.defs amoebas.lib amoebas.run )
)
   
;; v 2
   
(defn young 

    []

    (fn [energy health species env data]

       (let [
                weakest (fn [hs species env]   
                
                    (let
                        [shs (sort-by #(:health (:occupant (env %))) hs) ]
                                
                        (first shs)
                    )
                )
                do-move (fn []
                (
                    let [reg (sections-by-fuel (empty-neighbors env) env)
                    emE (sections-by-hostiles (empty-neighbors env) env species)
                    enemiS  (hostiles species Environment env)
                        ]
                    (if (< (+ MoveEnergy 7) energy)
                        (if (empty? (empty-neighbors env))
                            {:cmd :rest}
                            (if (not (empty? enemiS))
                                {:cmd :move :dir (last emE)}
                                {:cmd :move :dir (last reg)}
                            )
                        )
                        {:cmd :rest}   
                   )
                )
                )

                do-divide (fn []

                (let [ enemis  (hostiles species Neighbors env)
                    reg (sections-by-fuel (empty-neighbors env) env)
                    emE (sections-by-hostiles (empty-neighbors env) env species)
                    enemiS  (hostiles species Environment env)
                    ]
                    (if (and (> energy MinDivideEnergy) (< (int (count (friendlies species Neighbors env))) 5))
                        (if (empty? (empty-neighbors env))
                            {:cmd :rest}
                            (if (not (empty? enemiS))
                                {:cmd :divide :dir (last emE)}
                                {:cmd :divide :dir (last reg)}
                            )
                        )
                            (do-move)
                    )
                )
       )

            do-fuel (fn []
                (if (< MaxFuelingEnergy (:fuel (env Here)))     
                {:cmd :rest}                              
                (do-move)                            
                )
            ) 

            do-relocate (fn []
                (if (< energy 50)
                    (do-fuel)
                    (do-move)
                )
            )

            attack (fn []
                (let [enemis  (hostiles species Neighbors env)]
                (if (< AttackEnergy energy)
                    {:cmd :hit :dir (Neighbor-To-Dir (weakest enemis species env))}
                    (do-relocate)
                )
            )   
       )
                
            ]
        
            (let [enemis  (hostiles species Neighbors env)]
                (if (empty? enemis)
                    (if (< energy 50)
                        (do-fuel)
                        (do-divide)
                    )
                    (attack)
                )
            )
       )
 
    )
)

(def Evam (young))         

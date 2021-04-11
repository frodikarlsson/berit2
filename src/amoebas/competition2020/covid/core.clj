(ns amoebas.competition2020.covid.core
	(:use amoebas.defs amoebas.lib amoebas.run)
)

;;
;; target selector
;;
;;


(defn most-energy-target-selector
    "picks a target with the highest amount of energy stored"
    [hs species env]
    
    (last (sort-by #(:energy (:occupant (env %))) hs)) 
)

(defn weakest-and-sum-selector
    [hs species env]

    (let
        [ 
            sorted-by-health (sort-by #(:health (:occupant (env %))) hs)
            energy-and-fuel (fn [cell]
                                (if (:occupant cell)
                                    (+ (:fuel cell) (:energy (:occupant cell)))
                                    (:fuel cell)
                                )
                            )
            sorted-by-energy-and-fuel (reverse (sort-by #(energy-and-fuel (env %)) hs)) ;; highest = first
            scores-in-order-with-health (map #(+ (.indexOf sorted-by-health %) (.indexOf sorted-by-energy-and-fuel %)) sorted-by-health)
            largest-index (.indexOf scores-in-order-with-health (last (sort scores-in-order-with-health)))
        ]

         (nth sorted-by-health largest-index)
    )
)

;;
;;
;; Section-sorters
;;
;;

(defn average-hostile-energy-in-region
    "Average energy of hostiles in section"
    [sec env species]
    (let
        [
            hs-in-sec (filter #(contains-hostile? species % env) sec)
        ]
        (if (seq hs-in-sec) ;; not empty
            (/ (reduce + (map #(:energy (:occupant (env %))) hs-in-sec)) (count hs-in-sec))
            
            0
        )
    )
)

(defn remove-occupied-cells
    "removes occupied cells from section"
    [sec env]

    (filter #(cell-empty? env %) sec)
)

(defn remove-friendly-cells
    "removes friendly cells"
    [sec env species]

    (filter #(not (contains-friendly? species % env)) sec)
)

(defn sections-by-fuel-empty
    "sorts the sections according to fuel in empty cells"
    [dirs env species]
    
    (sort-by  #(total-fuel (remove-occupied-cells (Env-Sections %) env) env) dirs)
)

(defn sections-by-fuel-density-empty
    "sorts the sections according to fuel in empty cells"
    [dirs env species]

    (sort-by  #(average-fuel (remove-occupied-cells (Env-Sections %) env) env) dirs)
)

(defn sections-by-fuel-density-no-friendlies
    "sorts the sections according to fuel-density in non-friendly cells"
    [dirs env species]

    (sort-by  #(average-fuel (remove-friendly-cells (Env-Sections %) env species) env) dirs)
)

(defn sections-by-fuel-energy-density
    "sorts the sections according to fuel- and energy-density"
    [dirs env species]

    (sort-by #(+ (average-fuel (remove-friendly-cells (Env-Sections %) env species) env) (average-hostile-energy-in-region (Env-Sections %) env species)) dirs)
)


;;
;; factory function
;;
;;



(defn create-smarter 
    [early-move-world-divider max-nbr-nb after-div-energy after-move-energy target-selector crowded-dirs div-dirs]

    (fn [energy health species env data]

        (let
            [
                empty-nb        (empty-neighbors env)
                nbr-empty-nb    (count empty-nb)
                hs-nb           (hostiles species Neighbors env)
                fr-nb           (friendlies species Neighbors env)
                gen-nbr         (second (:data (:occupant (env Here)))) ;; what generation we are on
                next-gen-nbr    (if gen-nbr    ;; gen-nbr not= nil
                                    (inc gen-nbr)

                                    1
                                )                

            
                do-attack (fn []
                                (let 
                                    [
                                        target (target-selector hs-nb species env)
                                    ]
                                        (if (<= (:health (:occupant (env target))) HitLoss) ;; will kill
                                            {:cmd :hit :dir (Neighbor-To-Dir target) :data [target next-gen-nbr]}

                                            {:cmd :hit :dir (Neighbor-To-Dir target) :data [nil next-gen-nbr]}
                                        )
                                    
                                    )
                        )

                do-move-or-div (fn []
                                    (let
                                        [
                                            own-target              (first (:data (:occupant (env Here))))
                                            killed-last-round       (not= nil own-target) ;; true or false
                                            killed-by-neighbors     (set (filter #(not= % nil) (map #(first (:data (:occupant (env %)))) fr-nb))) ;; collects first from data-vector from friendly-neighbors
                                            empty-killed-nb         (filter #(contains? killed-by-neighbors %) empty-nb)
                                        ]
                                    
                                        (if (and killed-last-round (contains? (set empty-nb) own-target) (>= (- energy MoveEnergy) after-move-energy)) ;; own-kill is an empty neighbor and enough energy to move
                                            {:cmd :move :dir (Neighbor-To-Dir own-target) :data [nil next-gen-nbr]}

                                            (if (and (seq empty-killed-nb) (>= (- energy MoveEnergy) after-move-energy))    ;; else and has a killed as a neighbor
                                                {:cmd :move :dir (Neighbor-To-Dir (first empty-killed-nb)) :data [nil next-gen-nbr]}
    
                                                (if (and (<= max-nbr-nb (- 8 (count empty-nb))) (>= (- energy MoveEnergy) after-move-energy))       ;; else, and too crowded        
                                                    {:cmd :move :dir (last (crowded-dirs empty-nb env species)) :data [nil next-gen-nbr]}
                                                    
                                                    (if (>= (- energy DivideEnergyLoss) after-div-energy)      ;; not too crowded and enough energy to divide
                                                        {:cmd :divide :dir (last (div-dirs empty-nb env species)) :data [nil next-gen-nbr]}
                                                        
                                                        (if (< (:fuel (env Here)) MaxFuelingEnergy) ;; else and cell is almost empty, emergency move
                                                            {:cmd :move :dir (last (sections-by-fuel-energy-density empty-nb env species)) :data [nil next-gen-nbr]}
                                                            
                                                            {:cmd :rest :data [(first (:data (:occupant (env Here)))) next-gen-nbr]}  ;; else
                                                        )
                                                    )
                                                )
                                            )            
                                        )                          
                                    )
                            )


                do-early-move-or-div (fn []
                            (let
                                [
                                    own-target              (first (:data (:occupant (env Here))))
                                    killed-last-round       (not= nil own-target) ;; true or false
                                    killed-by-neighbors     (set (filter #(not= % nil) (map #(first (:data (:occupant (env %)))) fr-nb))) ;; collects first from data-vector from friendly-neighbors
                                    empty-killed-nb         (filter #(contains? killed-by-neighbors %) empty-nb)
                                ]
                            
                                (if (and killed-last-round (contains? (set empty-nb) own-target)) ;; own-kill is an empty neighbor and enough energy to move
                                    {:cmd :move :dir (Neighbor-To-Dir own-target) :data [nil next-gen-nbr]}

                                    (if (seq empty-killed-nb)    ;; else and has a killed as a neighbor
                                        {:cmd :move :dir (Neighbor-To-Dir (first empty-killed-nb)) :data [nil next-gen-nbr]}

                                        (if (<= max-nbr-nb (- 8 (count empty-nb)))       ;; else, and too crowded        
                                            {:cmd :move :dir (last (crowded-dirs empty-nb env species)) :data [nil next-gen-nbr]}
                                            
                                            (if (>= (- energy DivideEnergyLoss) 10)      ;; not too crowded and enough energy to divide
                                                {:cmd :divide :dir (last (div-dirs empty-nb env species)) :data [nil next-gen-nbr]}
                                                
                                                (if (< (:fuel (env Here)) MaxFuelingEnergy) ;; else and cell is almost empty, emergency move
                                                    {:cmd :move :dir (last (sections-by-fuel-energy-density empty-nb env species)) :data [nil next-gen-nbr]}
                                                    
                                                    {:cmd :rest :data [(first (:data (:occupant (env Here)))) next-gen-nbr]}  ;; else
                                                )
                                            )
                                        )
                                    )            
                                )                          
                            )
                    )
            ]
                
            (if (and (seq hs-nb) (>= energy AttackEnergy))     ;; has hostile nb and can attack
                (do-attack)
                
                (if (and (<= next-gen-nbr (/ WorldSize early-move-world-divider)) (seq empty-nb) (> energy MoveEnergy))
                    (do-early-move-or-div)

                    (if (and (seq empty-nb) (> energy MoveEnergy))    ;; can move
                        (do-move-or-div)
    
                        {:cmd :rest :data [(first (:data (:occupant (env Here)))) next-gen-nbr]}
                    )
                )
            )
        )
    )
)

(def Evam (create-smarter 20 2 (* (+ (/ MaxAmoebaHealth HitLoss) 1) AttackEnergy) (* (+ (/ MaxAmoebaHealth HitLoss) 1) AttackEnergy) most-energy-target-selector sections-by-fuel-energy-density sections-by-fuel-density-empty))

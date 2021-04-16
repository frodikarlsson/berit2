(ns amoebas.competition2021.berit2.core
    (:use amoebas.defs amoebas.lib amoebas.run)
    )

;----------Target-selector----------
(defn one-hit-kill-target-selector
    "Picks the target that has the most health that is less than HitLoss,
    if no enemy has less health than HitLoss, the enemy with the lowest energy is
    picked"
    [hs species env]

    (let
        [
            hostiles-by-health (sort-by #(:health (:occupant (env %))) hs)
            one-hit-kills (take-while
                           #(< (:health (:occupant (env %))) HitLoss) hostiles-by-health)
            ]
        (if (empty? one-hit-kills)
            (first (into [] hostiles-by-health))
            (last (into [] one-hit-kills))
            )
        )
    )
(defn meaf-target-selector
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
;----------Constants----------------
;(def ^:const x y)
;-------------Creator---------------
(defn create-berit2-test
    [low-energy divide-energy select-target]

    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            ;;todo move function
                            )
                            do-fuel (fn []
                            (let [by-fuel      (sections-by-fuel empty-nb env)]
                                (if (< (- (last by-fuel) MoveEnergy) (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                    {:cmd :rest}                                ;; chomp chomp
                                    (do-move)                                   ;; otherwise, keep looking
                                    )
                                )
                            )
                do-hit  (fn []
                    (if (< energy AttackEnergy)                                 ;; if we dont have energy to attack we fuel instead of default rest
                            (do-fuel)
                                (let
                                    [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                    
                                    (if (empty? hs)                             ;; nobody to hit?
                                        (do-fuel)                               ;; eat
                                        {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                    )
                                )
                            )
                    )
                    do-fuel (fn []
                    (let [by-fuel      (sections-by-fuel empty-nb env)]
                        (if (< (- (last by-fuel) MoveEnergy) (:fuel (env Here)))     ;; are we *at* a McDonald's?
                            {:cmd :rest}                                ;; chomp chomp
                            (do-move)                                   ;; otherwise, keep looking
                        )
                    )
                )
                
                do-div  (fn []
                            ;;todo div function
                            )
                ]
            ;;game logic här

            )
        )
    )
(def Evam (create-berit2-test))

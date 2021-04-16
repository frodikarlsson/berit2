(ns amoebas.competition2021.berit2.core
    (:use amoebas.defs amoebas.lib amoebas.run)
    )

;----------Target-selector----------
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
            (meaf-target-selector hs species env)
            (last (into [] one-hit-kills))
            )
        )
    )
;----------Constants----------------
;(def ^:const x y)
(def ^:const select-target one-hit-kill-target-selector)
(def ^:const max-fs 1)
(def ^:const low-energy AttackEnergy)
(def ^:const divide-energy (+ 20 MinDivideEnergy))
;------------Assistors--------------
(defn contains-fs-in-danger?
  "given a position, determines whether it contains a friend that sees an enemy"
  [species pos env]
  ;;(println "pos: " pos " data: " (:data (:occupant (env pos))) " equals 0: " )
  (let
    [
      cell (env pos)
      occupant    (:occupant cell)
      ]
    (and
     occupant
     (= species (:species occupant))
     (not (nil? (:data occupant)))
     (= (:data occupant) 1) )
    )
  )
(defn fs-in-danger
  "returns a subregion of the region argument that contains friends that see enemies"

  [species region env]

  (filter #(contains-fs-in-danger? species % env) region)
  )
(defn sections-by-fs-in-danger
  "sorts the sections by the number of friends that sees enemies in them, ascending"
  [dirs env species]

  (sort-by  #(count
              (fs-in-danger
               species
               (Env-Sections %)
               env))
            dirs)
  )
;-------------Creator---------------
(defn create-berit2-test
    []
    (fn [energy health species env data]
        (let
            [
                empty-nb     (empty-neighbors env)
         ;;       max-fs (if (= (total-fuel Neighbors env) 0) 0 (min 3 (Math/floor (double (/ 800 (total-fuel Neighbors env) ) ) ) ) )
                data-var (if (not-empty (hostiles species Environment env)) 1 0)
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    by-fuel      (sections-by-fuel empty-nb env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                    ]

                                (if (empty? empty-nb)       ;
                                    {:cmd :rest :data data-var}
                                  (if (not-empty (hostiles species Environment env))
                                    {:cmd :move :dir (last (sections-by-hostiles empty-nb env species)) :data data-var} ;; moves towards enemy if near
                                    (if (not-empty (fs-in-danger species Environment env))
                                      {:cmd :move :dir (last (sections-by-fs-in-danger empty-nb env species)) :data 0} ;; if friend sees enemy, move to friend
                                      {:cmd :move :dir (last by-fuel) :data data-var}    ;; move toward the most fuel
                                      )
                                    )
                                  )
                                )
                            )
                do-fuel (fn []
                                (let
                                    [
                                        empty-nb (empty-neighbors env)
                                        by-fuel      (sections-by-fuel empty-nb env)
                                        ]
                                    (if (and
                                         (not-empty empty-nb)
                                         (< (:fuel (env Here)) MaxFuelingEnergy))    ;; checks if worth moving
                                        (do-move)
                                        {:cmd :rest :data data-var}                                ;; chomp chomp                               ;; otherwise, keep looking
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
                                        {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env)) :data data-var}   ;; KAPOW!
                                        )
                                    )
                                )
                            )
                do-div  (fn [empty-nb]
                            (if (empty? empty-nb)
                                (do-fuel)
                                (if (<= (count (into [] (friendlies species Neighbors env))) max-fs)
                                  (if (not-empty (hostiles species Environment env))
                                                 {:cmd :divide :dir (last (sections-by-hostiles empty-nb env species)) :child-data data-var}
                                                 (if (not-empty (fs-in-danger species Environment env))
                                                   {:cmd :divide :dir (last (sections-by-fs-in-danger empty-nb env species)) :child-data data-var}
                                                   {:cmd :divide :dir (last (sections-by-fuel empty-nb env)) :child-data data-var}))
                                  (do-move)
                                    )
                                )
                            )


                ]
            (cond
             (or
              (< energy low-energy)
              (and (> (total-fuel Environment env) 4600) (> (count (friendlies species Neighbors env) ) 5)))
                (do-fuel)
             (not-empty (hostiles species Neighbors env))
             (do-hit)
             (> energy MinDivideEnergy)
                 (do-div (empty-neighbors env))


             :default
                (do-fuel)
             )

            )
        )
    )
(def Evam (create-berit2-test))

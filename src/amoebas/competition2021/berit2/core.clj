(ns amoebas.competition2021.berit2.core
  (:require [amoebas.defs :refer [MaxCellEnergy HitLoss AttackEnergy MinDivideEnergy MaxFuelingEnergy]])
  (:require [amoebas.lib :refer [Env-Sections Here Neighbor-To-Dir sections-by-fuel total-fuel Environment empty-neighbors Neighbors hostiles friendlies sections-by-hostiles]])
  )

;----------Target-selector----------
(defn- meaf-target-selector
    "picks a target with the highest sum of stored energy and energy in the cell it is in"
    [hs _ env]

    (let
        [energy-and-fuel
         (fn [cell]
             (if (:occupant cell) (+ (:fuel cell) (:energy (:occupant cell))) (:fuel cell))
             )
         ]

        (last (sort-by #(energy-and-fuel (env %)) hs))
        )
    )

(defn- one-hit-kill-target-selector
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
(def ^:private ^:const select-target one-hit-kill-target-selector)
(def ^:private ^:const low-energy AttackEnergy)
(def ^:private ^:const divide-energy (+ 35 MinDivideEnergy))
(def ^:private ^:const nr-of-cells-in-env (- (* 7 7) 1))
(def ^:private ^:const fuel-max-fs 4)

;------------Assistors--------------
(defn- contains-fs-in-danger?
  "given a position, determines whether it contains a friend that sees an enemy"
  [species pos env]
  (let
    [
      cell (env pos)
      occupant    (:occupant cell)
      ]
    (and occupant (= species (:species occupant)) (not (nil? (:data occupant))) (= (:data occupant) 1))
    )
  )
(defn- fs-in-danger
  "returns a subregion of the region argument that contains friends that see enemies"
  [species region env]
  (filter #(contains-fs-in-danger? species % env) region)
  )

(defn- sections-by-fs-in-danger
  "sorts the sections by the number of friends that sees enemies in them, ascending"
  [dirs env species]
  (sort-by  #(count (fs-in-danger species (Env-Sections %) env)) dirs)
  )

;-------------Creator---------------
(defn- create-berit2-test
    []
    (fn [energy _ species env _]
        (let
            [
                empty-nb     (empty-neighbors env)
                data-var (if (not-empty (hostiles species Environment env)) 1 0)
                do-move (fn []
                            (let [by-fuel (sections-by-fuel empty-nb env)]   ;; this sorts them by the amount of fuel in the corresponding sections
                                (cond
                                 (empty? empty-nb)       ;
                                  {:cmd :rest :data data-var}
                                 (not-empty (hostiles species Environment env))
                                  {:cmd :move :dir (last (sections-by-hostiles empty-nb env species)) :data data-var} ;; moves towards enemy if near
                                 (not-empty (fs-in-danger species Environment env))
                                  {:cmd :move :dir (last (sections-by-fs-in-danger empty-nb env species)) :data 0} ;; if friend sees enemy, move to friend
                                 :default
                                  {:cmd :move :dir (last by-fuel) :data data-var}    ;; move toward the most fuel
                                  )
                                )
                            )
                do-fuel (fn []
                                (let
                                    [
                                        empty-nb (empty-neighbors env)
                                        ]
                                    (if (and
                                         (not-empty empty-nb)
                                         (< (:fuel (env Here)) MaxFuelingEnergy)
                                         )    ;; checks if worth moving
                                        (do-move)
                                        {:cmd :rest :data data-var}
                                      )
                                  )
                          )
                do-hit  (fn []
                          (let
                            [
                              hs  (hostiles species Neighbors env)
                              ]
                            (cond
                             (< energy AttackEnergy)    ;; if we dont have energy to attack we fuel instead of default rest
                              (do-fuel)
                             (empty? hs)                             ;; nobody to hit?
                              (do-fuel)                               ;; eat
                             :default
                              {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env)) :data data-var}   ;; KAPOW!
                             )
                            )
                          )
                do-div  (fn [empty-nb]
                            (cond
                             (empty? empty-nb)
                              (do-fuel)
                             (> (count (into [] (friendlies species Environment env))) (/ nr-of-cells-in-env 4)) ;;changed to depend on environment and not neighbours, number is arbitrary but tested
                              (do-move)
                             (not-empty (hostiles species Environment env))
                              {:cmd :divide :dir (last (sections-by-hostiles empty-nb env species)) :child-data data-var}
                             (not-empty (fs-in-danger species Environment env))
                              {:cmd :divide :dir (last (sections-by-fs-in-danger empty-nb env species)) :child-data data-var}
                             :default
                              {:cmd :divide :dir (last (sections-by-fuel empty-nb env)) :child-data data-var}
                             )
                          )
                ]
            (cond
             (or
              (< energy low-energy)
              (and
               (> (total-fuel Environment env) (- (* MaxCellEnergy nr-of-cells-in-env) (* 2 MaxCellEnergy)))
               (> (count (friendlies species Neighbors env) ) fuel-max-fs)
               )
              )
              (do-fuel)
             (not-empty (hostiles species Neighbors env))
              (do-hit)
             (> energy divide-energy)
              (do-div (empty-neighbors env))
             :default
              (do-fuel)
             )
            )
        )
    )

(def Evam (create-berit2-test))

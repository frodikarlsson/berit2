(ns amoebas.competition2021.frodi.functional_core
    (:use amoebas.defs amoebas.lib amoebas.run)
)

;-----------Target-selectors----------
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
;-----------Basic-const----------------
(def ^:const divide-energy (+ MinDivideEnergy 0))
(def ^:const max-fs-nb 3)
(def ^:const select-target meaf-target-selector)
(def ^:const low-energy (+ AttackEnergy MoveEnergy MinDivideEnergy))
;-----------Assisting-functions--------
(defn- cmd
  "Executes a command, mostly implemented for the sake of abstraction"
  ([cmd] {:cmd cmd})
  ([cmd data] {:cmd cmd :data data})
  ([cmd dir data] (if (= cmd :divide)
                    {:cmd cmd :dir dir :child-data data}
                    {:cmd cmd :dir dir :data data})
   )
  )
(defn- avg-fuel
  "returns the average amount of fuel in the region; might not be an integer"
  [region env]
      (if (= (count region) 0)
        0
        (/ (total-fuel region env) (count region)))
  )

(defn- sections-by-friendlies-above-avg-fuel
  "Returns dirs of empty neighbors of the sections that contain fuel above average sorted by friendlies, asc"
  [env species]

  (let
    [empty-nb (shuffle (empty-neighbors env))
     empty-NB (map #(Neighbors %) empty-nb)
     env-sec (filter #(at-least-fuel-subregion(avg-fuel (Env-Sections %) env) (Env-Sections %) env) empty-nb)]
    (sections-by-friendlies
     env-sec
     env
     species
     )
    )
)
(defn- combat
  [energy species env data hs hs-env]
  (if (not-empty hs)
    (if (> energy low-energy)
      (cmd :hit (Neighbor-To-Dir (select-target hs species env)) data)
      (if (and  (>= MaxFuelingEnergy (:fuel (env Here))) (not-empty (empty-neighbors env)))
        (cmd :move (last (sections-by-fuel (empty-neighbors env) env)) data)
        (cmd :rest data)))
    (if(not-empty (empty-neighbors env))
      (if(> energy (+ MinDivideEnergy AttackEnergy))
        (cmd :divide (first (sections-by-friendlies (empty-neighbors env) env species)) data)
        (cmd :move (last (sections-by-hostiles (empty-neighbors env) env species)) data))
      (cmd :rest data))
    )
  )
(defn create-func
  []
  (fn
    [energy health species env data]
    (let[
          fs     (friendlies species Neighbors env)
          fs-above-avg (sections-by-friendlies-above-avg-fuel env species)
          hs     (hostiles species Neighbors env)
          hs-env (hostiles species Environment env)
          empty-nb (shuffle (empty-neighbors env))
          empty-NB (map #(Neighbors %) empty-nb)
          full-energy-nbs (filter #(=(:energy (:occupant %)) MaxCellEnergy) empty-NB)
          ]
      (cond
       (not-empty hs-env) ;;todo make advanced combat function, energy management, movement etc
       (combat energy species env data hs hs-env)
       (and
        (not-empty fs-above-avg)
        (> energy divide-energy)
        (<= (count (into [] fs)) max-fs-nb)
        )
       (cmd :divide (first fs-above-avg) data)
       (and (or
             (not-empty full-energy-nbs)
             (>= MaxFuelingEnergy (:fuel (env Here))))
            (not-empty fs-above-avg)
            (<= energy low-energy))
       (cmd :move (first fs-above-avg) data)
       :default
       (cmd :rest data)
       )
      )
  )
)


(def Evam (create-func))
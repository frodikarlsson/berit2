(ns amoebas.competition2021.frodi.grad_core
    (:use amoebas.defs amoebas.lib amoebas.run)
    )
(def job {
           :explorer 0
           :warrior 1
           :farmer 2
           })
(defstruct cell :job :steps :dir)
;------Targ-sel----
(defn one-hit-kill-target-selector
  "Picks the target that has the most health that is less than HitLoss,
  if no enemy has less health than HitLoss, the enemy with the lowest energy is
  picked"
  [hs species env]

  (let
    [
      hostiles-by-health (sort-by #(:health (:occupant (env %))) hs)
      one-hit-kills (take-while #(< % HitLoss) (:health hostiles-by-health))
      ]
    (if (empty? one-hit-kills)
      (first (into [] hostiles-by-health))
      (last (into [] one-hit-kills))
      )
    )
  )
(defn weak-target-selector
  "picks a target with the lowest health score"
  [hs species env]

  (let
    [shs (sort-by #(:health (:occupant (env %))) hs) ]

    (first shs)
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



;------Consts------
(def ^:const divide-energy (+ MinDivideEnergy 30))
(def ^:const low-energy 10)
(def ^:const max-fs-nb 4)
(def ^:const select-target one-hit-kill-target-selector)


(defn create-grad
  []
  (fn [energy health species env data]
      (let
        [ hs     (hostiles species Neighbors env)
          fs     (friendlies species Neighbors env)
          empty-nb     (empty-neighbors env)
          by-fuel (sections-by-fuel empty-nb env)
          data-var (if (some? data)
                      (struct cell (data :job) (+ (data :steps) 1) (data :dir))
                      (struct cell (job :explorer) 0 (rand-int 8))
                      )


          hit (
                (fn []
                  {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))})
                )
          move (
                 (fn[]
                   (if (empty? empty-nb)
                     (if (or (empty? hs) (< energy low-energy))
                       {:cmd :rest :data data-var}
                       hit
                       )
                     (if (not-empty (hostiles species Environment env))
                       {:cmd :move :dir (last (sections-by-hostiles empty-nb env species)) :data data-var}
                       {:cmd :move :dir (last by-fuel) :data data-var}
                       )
                   )
                 )
                )
          div (
                (fn[]
                  (cond
                   (not-empty hs)
                   {:cmd :divide :dir (last (into [] (sections-by-hostiles empty-nb env species))) :child-data data-var}
                   (<= (count (into [] fs)) max-fs-nb)
                    {:cmd :divide :dir (last by-fuel) :child-data data-var}
                   :default
                   move
                    )
                  )
                )

          ]
        (if (some? data)
          (def data-var (update data-var :steps inc))
          (def data-var (struct cell (job :explorer) 0 (rand-int 8)))
        )

          (cond
           (and (not-empty empty-nb) (> energy (+ MinDivideEnergy (data-var :steps))))
           div
           (not-empty hs)
           hit
           ;;(< energy low-energy)
           (and (>= MaxFuelingEnergy (:fuel (env Here))) (not-empty by-fuel))     ;; are we *at* a McDonald's?            ;; chomp chomp
           move                                  ;; otherwise, keep looking
           :default
           {:cmd :rest :data data-var}




          )
        )
    )
  )
(def Evam (create-grad))
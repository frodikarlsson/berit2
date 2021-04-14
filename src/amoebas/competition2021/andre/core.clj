(ns amoebas.competition2021.andre.core
  (:require [amoebas.defs :refer [HitLoss]])
  (:require [amoebas.lib :refer [empty-neighbors sections-by-fuel Here Neighbors hostiles Neighbor-To-Dir]])
)


;; @param hostiles
;;  sections that contains hostiles
;; @param env
;;  information about the environment
(defn one-hit-kill-target-selector
  "Picks the target that has the most health that is less than HitLoss,
  if no enemy has less health than HitLoss, the enemy with the lowest energy is
  picked"
  [hostiles _ env]

  (let
    [
      hostiles-by-health (sort-by #(:health (:occupant (env %))) hostiles)
      one-hit-kills (filter #(< (:health (:occupant (env %))) HitLoss) hostiles-by-health)
    ]
    (if (empty? one-hit-kills)
      (first hostiles-by-health)
      (last one-hit-kills)
    )
  )
)


(defn create-ape
  [min-fuel low-energy  divide-energy select-target]

  (fn [energy _ species env _]
    (let
      [
        do-move (fn []
          (let
            [
             empty-nb (empty-neighbors env)
             by-fuel (sections-by-fuel empty-nb env)
            ]

            (if (empty? empty-nb)
              {:cmd :rest}
              {:cmd :move :dir (last by-fuel)}
            )
          )
        )

        do-eat (fn []
          (if (< min-fuel (:fuel (env Here)))
            {:cmd :rest}
            (do-move)
          )
        )

        do-hit (fn []
          (let
            [hs (hostiles species Neighbors env)]

            (if (empty? hs)
              (do-eat)
              {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
            )
          )
        )

        do-divide (fn [empty-nb]
          {:cmd :divide :dir (rand-nth empty-nb)}
        )
      ]

      (cond
        (< energy low-energy)
          (do-eat)
        (< divide-energy energy)
          (let
            [empty-nb (empty-neighbors env)]

            (if (empty? empty-nb)
              (do-hit)
              (do-divide empty-nb)
            )
          )
        (hostiles species Neighbors env)
          (do-hit)
        :else
          (do-eat)
      )
    )
  )
)

(def Evam (create-ape 5 15 50 one-hit-kill-target-selector))

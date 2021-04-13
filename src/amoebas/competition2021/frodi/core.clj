(ns amoebas.competition2021.frodi.core
    (:use amoebas.defs amoebas.lib amoebas.run)
)
(def job {
           :warrior 0
           :farmer 1
           :explorer 2
           }
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

(defn create-frodis-vinnare
      [low-energy divide-energy explore-limit select-target]
    (fn [energy health species env data]
        (let
            [
                fs      (friendlies species Environment env)             ;; all friendlies
                hs      (hostiles species Environment env)               ;; all friendlies
                hostile-dist    (apply min (cons WorldSize (map #(distance Here %) hs)))            ;; minimal distance of visible hostiles
                data ( ;; list of: job, distance for explorer, explore-dir
                       (fn []
                       (if
                        (nil? data) (vector job :farmer (rand-int 8) (last (sections-by-hostiles (empty-neighbors env) env species))) data)
                        )
                       )

                determine-job (fn []
                                (cond
                                 (and (seq? data)
                                      (= (nth data 0) (job :explorer))
                                      (< (nth data 1) explore-limit)) (job :explorer) ;; if its a seq and has a second index and second index < explorelimit

                                 (and (< hostile-dist 3) (> energy low-energy)) (job :warrior)
                                 (< (rand) 0.7) (job :explorer)
                                  :default (job :farmer)
                                 )
                                )

                attempt-dir (fn
                              ;; "attempts to move in a direction, if that doesn't work attempts one counter-clockwise, then clockwise and if nothing works, rest"
                              [dir]
                              (cond
                               (some #(= dir %) (empty-neighbors env))        (do
                                                                      {:cmd :move :dir dir, :data data}
                                                                      (assoc data 1 (inc (nth data 1)))
                                                                      (print "data " (nth data 1))
                                                                    )
                               (some #(= (CCW dir) %) (empty-neighbors env))  (do
                                                                      {:cmd :move :dir (CCW dir), :data data}
                                                                      (assoc data 1 (inc (nth data 1)))
                                                                      (print "data " (nth data 1))
                                                                    )
                               (some #(= (CW dir) %) (empty-neighbors env))   (do
                                                                      {:cmd :move :dir (CW dir), :data data}
                                                                      (assoc data 1 (inc (nth data 1)))
                                                                      (print "data " (nth data 1))
                                                                    )
                               :default {:cmd :rest, :data data}
                               )
                            )
                do-move (fn []
                          (let                                        ;; otherwise we gotta move...
                            [
                              empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                              by-hostile      (sections-by-hostiles empty-nb env species)    ;; this sorts them by the amount of fuel in the corresponding sections
                              by-fuel      (sections-by-fuel-density empty-nb env)
                              by-friendlies (sections-by-friendlies empty-nb env species)
                              ]

                            (if (empty? empty-nb)       ;; no empty neighbors?
                              {:cmd :rest, :data data}            ;; hunker down, we can't move --- 
                              (if (= (determine-job) (job :warrior))
                                {:cmd :move :dir (last by-hostile), :data data}
                                {:cmd :move :dir (last by-fuel), :data data})    ;; move toward the most fuel
                              )
                            )
                          )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest, :data data}                                ;; chomp chomp
                                (do-move)                                   ;; otherwise, keep looking
                                )
                            )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]      ;; hostile neighbors

                                (if (empty? hs)                             ;; nobody to hit?
                                    (do-fuel)                               ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env)), :data data}   ;; KAPOW!
                                    )
                                )
                            )
                do-div  (fn [empty-nb]
                          (if (= (determine-job) (job :explorer))
                            {:cmd :divide :dir (first (sections-by-friendlies empty-nb env species)),
                             :data data, :child-data (vector (job :farmer) 0 (rand-int 8))}
                            {:cmd :divide :dir (first (sections-by-friendlies empty-nb env species)), :data data}))
                ]

            (cond
             (= (determine-job) (job :warrior)) (do-hit)
             (and (< (rand-int 10) 4) (>= energy divide-energy)) (do-div (sections-by-fuel-density (empty-neighbors env) env))
             (<= energy low-energy) (do-fuel)
             (= (determine-job) (job :explorer))(if (< (rand-int 10) 5) (attempt-dir (nth data 2 )) (do-div (empty-neighbors env)))
             (not= (determine-job) (job :farmer)) (do-move)
             (= (determine-job) (job :farmer)) (do-fuel)
             :default (do-move)
            )
          )
        )
)

(def Evam (create-frodis-vinnare 5 80 10 meaf-target-selector))
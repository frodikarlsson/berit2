(ns amoebas.competition2020.gooftroop.core
    (:use amoebas.examples amoebas.defs amoebas.lib amoebas.run)
)

(defn energy-neighbor-selector  
    "picks a neighbor with the highest engergy"
    [ns env]   

    (let
        [sns (sort-by #(:energy (env %)) ns) ]
                
        (last sns)
    )
)

(defn create-satellite
    [normalFunc dir totalDist]

    (fn [energy health species env data]
        (let
            [
                distSoFar (if (number? data) data 0)
            ]
            (if (< distSoFar totalDist)
                {:cmd :move :dir dir :data (inc distSoFar)}
                (assoc (normalFunc energy health species env data) :data distSoFar)
            )
        )
    )
)

(defn create-goofy
    "creates amoeba function of the gooftroop"

    [low-energy divide-energy select-target satellite-rate satellite-distance crowd-limit crowd-procedure-cycle]

    (fn [energy health species env data]
        (let 
            [
                round (get data :round 0)

                fs (friendlies species Environment env) ;; all friendlies

                crowd-procedure-step (get data :crowd-procedure-step 0)
                in-crowd (> (count fs) crowd-limit)
                opens   (filter #(region-empty? (drop 1(Env-Sections %)) env) Dirs)  ;; direction of all open sections, i.e. without hostiles or friendlies

                new-data {
                    :round (inc round)
                    :crowd-procedure-step (if in-crowd (mod (inc crowd-procedure-step) crowd-procedure-cycle) 0)
                    }

                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)  
                                    by-fuel      (sections-by-fuel empty-nb env)
                                ]
                                
                                (if (empty? empty-nb)
                                    {:cmd :rest :data new-data}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel) :data new-data}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (cond
                                in-crowd
                                    (cond
                                        (>= crowd-procedure-step (dec crowd-procedure-cycle))
                                            (do-move)
                                        :else
                                            {:cmd :rest :data new-data}
                                    )
                                :else
                                    (if (<= MaxFuelingEnergy (:fuel (env Here)))    ;; are we *at* a McDonald's?
                                        {:cmd :rest :data new-data}                                ;; chomp chomp
                                        ;;(if (< (+ 3 (:energy (env Here))) (:energy (energy-neighbor-selector Neighbors env)))
                                            (do-move)
                                         ;;   {:cmd :rest :data new-data}
                                        ;;)
                                    )
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]
                                
                                (if (empty? hs)      ;; nobody to hit?
                                    (do-fuel)       ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env)) :data new-data}
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                                (if (and (not-empty opens) (< (rand) satellite-rate))
                                    (let
                                        [dir (rand-nth opens)]
                                        {:cmd :divide :dir dir :function (create-satellite 
                                                                            (create-goofy low-energy divide-energy select-target satellite-rate satellite-distance crowd-limit crowd-procedure-cycle) dir satellite-distance
                                                                        ) :data new-data}
                                    )
                                    ;;{:cmd :rest :data new-data}
                                    {:cmd :divide :dir (rand-nth empty-nb) :data new-data}
                                )
                        )
            ]
        
            (cond
                (< energy low-energy)           ;; need some chow?
                    (do-fuel)
                (not-empty (hostiles species Neighbors env))            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em
                (< divide-energy energy)               ;; parenthood!
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)

(def Evam (create-goofy 20 30 weakest-target-selector 0.1 10 22 12))



(def TestTournament 
    (tournament 
        {
            :red (create-goofy 20 30 weakest-target-selector 0 40 22 10)
            :orange (create-goofy 20 30 weakest-target-selector 0.1 10 22 12)
            ;;:magenta (create-goofy 20 30 weakest-target-selector 0.2 22 8) ;;;denna vann
            :blue (create-nasty 0.2 random-target-selector)
            :cyan (create-nasty 0.2 random-target-selector)
            :gray (create-mindless-divider 0.25)
            :white (create-mindless-divider 0.32)
            ;;:magenta   (create-slightlybrainy 20 30 random-target-selector)

        }
    )
)

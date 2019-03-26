(ns amoebas.examples
    (:use amoebas.defs amoebas.lib amoebas.core)
)

;;
;;  
;;

(defn create-mindless 
    []

    (fn [energy health species env data]

        {:cmd :move, :dir (rand-int 8)}
    )
)


(defn create-mindless-divider
    [division-prob]

    (fn [energy health species env data]
    
        (if (< energy (+ MinDivideEnergy (/ (- MaxAmoebaEnergy MinDivideEnergy) 2)))
            {:cmd :rest}
            (if (< (rand) division-prob)
                {:cmd :divide, :dir (rand-int 8)}
                {:cmd :move, :dir (rand-int 8)}
            )
        )
    )
)

(defn random-target-selector   [hs species env]   (rand-nth hs) )

(defn weakest-target-selector  [hs species env]   
    (let
        [shs (sort-by #(:health (:occupant (env %))) hs) ]
                
        (first shs)
    )
)

(defn strongest-target-selector  [hs species env]   (last (sort-by #(:health (:occupant (env %))) hs)) )

(defn most-energy-target-selector [hs species env]  (last (sort-by #(:energy (:occupant (env %))) hs)) )

(defn most-energy-and-fuel-target-selector [hs species env]  
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


(defn create-nasty
    [division-prob select-target]
    
    (let
        [md (create-mindless-divider division-prob)]
    
        (fn [energy health species env data]
    
            (let
                [ hs     (hostiles species Neighbors env) ]
            
                (if (empty? hs)
                    (md energy health species env data)
                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
                )
            )
        )
    )
)



(defn create-slightlybrainy
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)  
                                    by-fuel      (sections-by-fuel empty-nb env)
                                ]
                                
                                (if (empty? empty-nb)
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]
                                
                                (if (empty? hs)      ;; nobody to hit?
                                    (do-fuel)       ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            {:cmd :divide :dir (rand-nth empty-nb)}
                        )
            ]

            (cond
                (< energy low-energy)           ;; need some chow?
                    (do-fuel)
                (< divide-energy energy)               ;; parenthood!
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (hostiles species Neighbors env)            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em
                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)
                    
                    
(defn create-adaptive-slightlybrainy
    [low-energy divide-energy]
    
    
    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)  
                                    by-fuel      (sections-by-fuel empty-nb env)
                                ]
                                
                                (if (empty? empty-nb)
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)
                            )
                        )
                do-hit  (fn []
                            (let
                                [h  (hostiles species Neighbors env)]
                                
                                (if (empty? h)      ;; nobody to hit?
                                    (do-fuel)       ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (rand-nth h))}
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            {:cmd :divide :dir (rand-nth empty-nb) 
                             :function (create-adaptive-slightlybrainy (+ low-energy (rand-int 3) -1) (+ divide-energy (rand-int 3) -1)) }
                        )
            ]

            (cond
                (< energy low-energy)           ;; need some chow?
                    (do-fuel)
                (< divide-energy energy)               ;; parenthood!
                    (let
                        [empty-nb   (empty-neighbors env)]
                        
                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (hostiles species Neighbors env)            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em
                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)
                    
                    
                    
                
                




;;
;;  sample tournaments
;;
;;

    

(def T0-1 (tournament 
    {
        :red (create-mindless-divider 0.01) 
    })
)

(def T0-2 (tournament 
    {
        :red (create-mindless-divider 0.1) 
    })
)

(def T0-3 (tournament 
    {
        :red (create-mindless-divider 0.3) 
    })
)

(def T0-4 (tournament 
    {
        :red (create-mindless-divider 0.5) 
    })
)

(def T0-5 (tournament 
    {
        :red (create-mindless-divider 0.8) 
    })
)

(def T0-6 (tournament 
    {
        :red (create-mindless-divider 0.99) 
    })
)

(def T0-7 (tournament 
    {
        :red (create-mindless-divider 0.1) 
        :blue (create-mindless-divider 0.9)
    })
)

(def T0-8(tournament 
    {
        :red (create-mindless-divider 0.28) 
        :blue (create-mindless-divider 0.32)
    })
)


(def T0 (tournament 
    {
        :red (create-mindless-divider 0.1) 
        :blue (create-mindless-divider 0.25)
        :yellow (create-mindless-divider 0.4)
        :white (create-mindless-divider 0.6)
        :gray (create-mindless-divider 0.75)
        :orange (create-mindless-divider 0.9)
    })
)

(def T1 (tournament {:red (create-nasty 0.8 random-target-selector) :gray (create-nasty 0.5 random-target-selector) :blue (create-nasty 0.2 random-target-selector)}))


(def T2 (tournament 
    {
        :red (create-mindless-divider 0.1) 
        :blue (create-mindless-divider 0.25)
        :yellow (create-mindless-divider 0.4)
        :white (create-mindless-divider 0.6)
        :gray (create-mindless-divider 0.75)
        :orange (create-mindless-divider 0.9)
        
    })
)

(def T3 (tournament 
    {
        :red        (create-nasty 0.2 random-target-selector)
        :blue       (create-nasty 0.3 random-target-selector)
        :yellow     (create-nasty 0.4 random-target-selector)        
        :white      (create-nasty 0.5 random-target-selector)
        :gray       (create-nasty 0.6 random-target-selector)
        :orange     (create-nasty 0.7 random-target-selector)
        :magenta    (create-nasty 0.8 random-target-selector)
        :cyan       (create-nasty 0.9 random-target-selector)
    })
)



(def T4 (tournament
    {
        :red       (create-slightlybrainy 15 50 random-target-selector)
        :blue       (create-nasty 0.3 random-target-selector)
        :yellow     (create-nasty 0.4 random-target-selector)        
        :white      (create-nasty 0.5 random-target-selector)
        :gray       (create-nasty 0.6 random-target-selector)
        :orange     (create-nasty 0.7 random-target-selector)
        :magenta    (create-nasty 0.8 random-target-selector)
        :cyan       (create-nasty 0.9 random-target-selector)
    })
)

(def T5 (tournament
    {
        :red       (create-slightlybrainy 10 30 random-target-selector)
        :blue      (create-slightlybrainy 20 30 random-target-selector)
        :yellow    (create-slightlybrainy 10 50 random-target-selector)        
        :white     (create-slightlybrainy 20 50 random-target-selector)
        :gray      (create-slightlybrainy 30 50 random-target-selector)
        :orange    (create-slightlybrainy 10 70 random-target-selector)
        :magenta   (create-slightlybrainy 20 70 random-target-selector)

        :cyan      (create-adaptive-slightlybrainy 20 30)
    })
)

(def T6 (tournament
    {
        :red       (create-slightlybrainy 10 30 weakest-target-selector)
        :blue      (create-adaptive-slightlybrainy 20 30)
    })
)


(def T7 (tournament
    {
        :blue      (create-slightlybrainy 10 50 weakest-target-selector)        
        :yellow    (create-slightlybrainy 10 50 random-target-selector)        
        :orange    (create-slightlybrainy 10 70 random-target-selector)
        :red       (create-slightlybrainy 10 70 weakest-target-selector)
    })
)

(def T8 (tournament
    {
        :blue      (create-slightlybrainy 10 70 random-target-selector)        
        :yellow    (create-slightlybrainy 10 70 weakest-target-selector)        
        :orange    (create-slightlybrainy 10 70 strongest-target-selector)
        :red       (create-slightlybrainy 10 70 most-energy-target-selector)
        :white     (create-slightlybrainy 10 70 most-energy-and-fuel-target-selector)
    })
)





;; (defn amoeba-factory
;;     [factories]
;;     
;;     (let
;;         [
;;             weight-sum  (apply + (map first factories))
;;             
;; 
;; 
;; (defn create-explorer
;;     [explore-prob division-prob select-target]
;;     
;;     (let
;;         [
;;             md (create-mindless-divider division-prob)
;;             
;;             walker    (fn [energy-health species env]
;;                             
;;         ]
;;     
;;         (fn [energy health species env data]
;;     
;;             (let
;;                 [ h     (neighboring-hostiles species env) ]
;;             
;;                 (if (empty? h)
;;                     (
;;                     (md energy health species env data)
;;                     {:cmd :hit, :dir (Neighbor-To-Dir (select-target h species env))}
;;                 )
;;             )
;;         )
;;     )
;;     
;; 

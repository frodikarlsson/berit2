(ns amoebas.examples
    (:use amoebas.defs amoebas.lib amoebas.run)
)

;;
;;  some very, very simple amoebas
;;

(defn mindless 
    "not a very clever amoeba function"

    [energy health species env data]

    {:cmd :move, :dir (rand-int 8)}
)

(defn mindless-divider

    [energy health species env data]
    
    (if (< energy (+ MinDivideEnergy (/ (- MaxAmoebaEnergy MinDivideEnergy) 2)))
        {:cmd :rest}
        (if (< (rand) 0.3)                      ;; divide with probability 0.3
            {:cmd :divide, :dir (rand-int 8)}
            {:cmd :move, :dir (rand-int 8)}
        )
    )
)

;;
;;  You don't want to write a different amoeba function for every value of the parameters it depends on.
;;  So write a function that *creates* amoeba functions instead.
;;

(defn create-mindless-divider
    "create a mindless-divider with division probability division-prob"
    [division-prob]

    (fn [energy health species env data]        ;; <--- and this is the magic!
    
        (if (< energy (+ MinDivideEnergy (/ (- MaxAmoebaEnergy MinDivideEnergy) 2)))
            {:cmd :rest}
            (if (< (rand) division-prob)
                {:cmd :divide, :dir (rand-int 8)}
                {:cmd :move, :dir (rand-int 8)}
            )
        )
    )
)

;;
;;  From now on, all amoebas will be created by a factory, because they usually depend on some parameter.
;;  Like this one. It's nasty, because when it sees someone it doesn't like, it unceremoniously whacks it over the head.
;;


(defn create-nasty
    "nasty behaves like mindless-divider when no enemies are around,
     but when there are, it picks one using select-target, and WHACK!
     hits it"
    [division-prob select-target]
    
    (let
        [md (create-mindless-divider division-prob)]    ;; we are reusing the mindless-divider here!!!
    
        (fn [energy health species env data]
    
            (let
                [ hs     (hostiles species Neighbors env) ]     ;; this is where we find enemies in the neighboring cells
            
                (if (empty? hs)                                 ;; no enemies?
                    (md energy health species env data)         ;; then mindlessly divide
                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}       ;; otherwise, pick someone and hit them
                )
            )
        )
    )
)

;;
;;  The following are a few target selector functions.
;;  They take three arguments: 
;;      hs: a non-empty sequence of potential target positions, 
;;      species: the species of firendlies
;;      env: the environment function
;;
;;  They return one of the elements in hs. 
;;  Some target selectors work by sorting the sequence of potential
;;  targets according to some criterion, and then picking the first or 
;;  the last.
;;

(defn random-target-selector   
    "randomely picks a target"
    [hs species env]   
    
    (rand-nth hs)
)

(defn weakest-target-selector  
    "picks a target with the lowest health score"
    [hs species env]   

    (let
        [shs (sort-by #(:health (:occupant (env %))) hs) ]
                
        (first shs)
    )
)

(defn strongest-target-selector
    "picks a target with the highest health score"
    [hs species env]   
    
    (last (sort-by #(:health (:occupant (env %))) hs)) 
)

(defn most-energy-target-selector
    "picks a target with the highest amount of energy stored"
    [hs species env]
    
    (last (sort-by #(:energy (:occupant (env %))) hs)) 
)


(defn most-energy-and-fuel-target-selector 
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

;;
;; This is the first slightly more complicated amoeba function.
;; Note how the different sub-behaviors are made into their own little functions
;;


(defn create-slightlybrainy
    [low-energy divide-energy select-target]
    
    
    (fn [energy health species env data]
        (let
            [
                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                                    by-fuel      (sections-by-fuel empty-nb env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                ]
                                
                                (if (empty? empty-nb)       ;; no empty neighbors?
                                    {:cmd :rest}            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)                                   ;; otherwise, keep looking
                            )
                        )
                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]      ;; hostile neighbors
                                
                                (if (empty? hs)                             ;; nobody to hit?
                                    (do-fuel)                               ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                            {:cmd :divide :dir (rand-nth empty-nb)}         ;; amoeba parenting: drop the child wherever...
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
;;  Sample tournaments
;;
;;  Here we instantiate populations with different amoeba functions and also different parameters.
;;  When you start building your own functions, it's a good idea to run experiments to check which parameter
;;  values work best.
;;


;; First a few single-species runs, to see how they expand.

(def T01 (tournament 
    {
        :red (create-mindless-divider 0.01) 
    })
)

(def T02 (tournament 
    {
        :red (create-mindless-divider 0.1) 
    })
)

(def T03 (tournament 
    {
        :red (create-mindless-divider 0.3) 
    })
)

(def T04 (tournament 
    {
        :red (create-mindless-divider 0.99) 
    })
)

;; Pitching mindless dividers against each other.

(def T05 (tournament 
    {
        :red (create-mindless-divider 0.1) 
        :blue (create-mindless-divider 0.9)
    })
)

(def T06(tournament 
    {
        :red (create-mindless-divider 0.28) 
        :blue (create-mindless-divider 0.32)
    })
)

(def T07 (tournament 
    {
        :red (create-mindless-divider 0.1) 
        :blue (create-mindless-divider 0.25)
        :yellow (create-mindless-divider 0.4)
        :white (create-mindless-divider 0.6)
        :gray (create-mindless-divider 0.75)
        :orange (create-mindless-divider 0.9)
    })
)


;; Now for some nasty business...

(def T08 (tournament 
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

;; Let's check the value of being "smart"...

(def T09 (tournament
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


;; A regular Mensa meet-up...

(def T10 (tournament
    {
        :red       (create-slightlybrainy 10 30 random-target-selector)
        :blue      (create-slightlybrainy 20 30 random-target-selector)
        :yellow    (create-slightlybrainy 10 50 random-target-selector)        
        :white     (create-slightlybrainy 20 50 random-target-selector)
        :gray      (create-slightlybrainy 30 50 random-target-selector)
        :orange    (create-slightlybrainy 10 70 random-target-selector)
        :magenta   (create-slightlybrainy 20 70 random-target-selector)
    })
)


;; What difference do target selectors make?

(def T11 (tournament
    {
        :blue      (create-slightlybrainy 10 70 random-target-selector)        
        :yellow    (create-slightlybrainy 10 70 weakest-target-selector)        
        :orange    (create-slightlybrainy 10 70 strongest-target-selector)
        :red       (create-slightlybrainy 10 70 most-energy-target-selector)
        :white     (create-slightlybrainy 10 70 most-energy-and-fuel-target-selector)
    })
)





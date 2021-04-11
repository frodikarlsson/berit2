(ns amoebas.competition2020.theroots.core) 

;; From defs.clj
(def ViewDistance   3)      ;; max distance of viewable cell (max norm)

;; From lib.clj Useful definitions
(def Neighbors          ;;  the positions of all neighbors, i.e. the cells we can directly move to, hit, or divide into
                        ;;  NB: this is a region
                        ;;  also note: the direction can be used as an index into this vector.
    [ [-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] ] 
)
(def Here [0 0])        ;; we're here!
(def Environment        ;;  the positions of all visible cells 
                        ;;  NB: this is a region
    (for [x (range (inc (* 2 ViewDistance))) y (range (inc (* 2 ViewDistance)))] [(- x ViewDistance) (- y ViewDistance)])
)
(def Neighbor-To-Dir    ;; maps neighboring position to the corresponding direction we need to move to/hit in/divide into
                        ;; it's the inverse map of Neighbors above
    { [-1 -1] 0 [0 -1] 1 [1 -1] 2 [1 0] 3 [1 1] 4 [0 1] 5 [-1 1] 6 [-1 0] 7 } 
)
(def Dirs (range 8))

;; From lib.clj useful functions
(defn sum
  [x]
  (reduce + x)
  )

(def Env-Sections
                        ;; this vector contains the sections corresponding to the eight directions
                        ;; moving in this direction means that the max norm distance to the positions
                        ;; in these sections does not increase
    [
        (for [a (range (inc ViewDistance)) b (range (inc ViewDistance))]  [(- a) (- b)])    ;; NW
        (for [a (range (inc ViewDistance)) b (range (inc (* 2 a)))]  [(- b a) (- a)])       ;; N
        (for [a (range (inc ViewDistance)) b (range (inc ViewDistance))]  [a (- b)])        ;; NE
        (for [a (range (inc ViewDistance)) b (range (inc (* 2 a)))]  [a (- b a)])           ;; E
        (for [a (range (inc ViewDistance)) b (range (inc ViewDistance))]  [a b])            ;; SE
        (for [a (range (inc ViewDistance)) b (range (inc (* 2 a)))]  [(- b a) a])           ;; S
        (for [a (range (inc ViewDistance)) b (range (inc ViewDistance))]  [(- a) b])        ;; SW
        (for [a (range (inc ViewDistance)) b (range (inc (* 2 a)))]  [(- a) (- b a)])       ;; W
    ]
)

(defn total-fuel
    "returns the total amount of fuel in the region"

    [region env]
    
    (sum (map #(:fuel (env %)) region))
)

(defn weakest-target-selector  
    "picks a target with the lowest health score"
    [hs species env]   

    (let
        [shs (sort-by #(:health (:occupant (env %))) hs) ]

        (first shs)
        )
    )

(defn sections-by-fuel
    "sorts the sections by the amount of fuel in them, ascending"
    [dirs env]
    
    (sort-by  #(total-fuel (Env-Sections %) env) dirs)
)


(defn cell-empty?
    [env pos]
    
    (not (:occupant (env pos)))
)

(defn empty-neighbors
    "computes the directions in which there are empty cells"
    [env]
    
    (filter #(cell-empty? env (Neighbors %)) (range 8))
)

(defn contains-hostile?
    "given a position, determines whether it contains a hostile"
    [species pos env]
    
    (let
        [
            cell        (env pos)
            occupant    (:occupant cell)
        ]
    
        (and occupant (not= species (:species occupant)))
    )
)

(defn hostiles
    "returns the sub-region of the region argument that contains hostiles"

    [species region env]
    
    (filter #(contains-hostile? species % env) region)
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



(defn create-slightlybrainy-ii
    [low-energy divide-energy select-target]

    (fn [energy health species env data]
        (let
            [
             do-move (fn []
                         (let                                        ;; otherwise we gotta move...
                             [
                              hs  (hostiles species Neighbors env)      ;; hostile neighbors
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
                         ;;(if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                         (if (< 5 (:fuel (env Here)))     ;; are we *at* a McDonald's?
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
                       (let [hs (hostiles species Neighbors env)]
                         ;; difficult (do (println (map #(average-fuel % env) (vec (sections-by-fuel empty-nb env))))
                         ;;(do (println (first (sections-by-fuel empty-nb env)))
                         ;;(do (println (:fuel env (last (sections-by-fuel empty-nb env))))
                         ;;(if (< 2 (:fuel env (last (sections-by-fuel empty-nb env))))
                         ;;(do (println (sum (:fuel env (sections-by-fuel empty-nb env))))
                         ;;(if (< 10 (sum (:fuel env (sections-by-fuel empty-nb env))))
                         (if (and (< 10 (sum (:fuel env (sections-by-fuel empty-nb env)))) (< 5 (count(sections-by-fuel empty-nb env))) )
                           (do ;;(println (count (sections-by-fuel empty-nb env)) )
                         ;;{:cmd :divide :dir (rand-nth empty-nb)}         ;; amoeba parenting: drop the child wherever...
                         {:cmd :divide :dir (last (sections-by-fuel empty-nb env))}    )     ;; amoeba parenting: drop the child wherever...
                         (do-move));;)
                         ;;)
                         )
                    )
             ]

            (cond
                ;; {:cmd :div, :dir 1, :function (create-nasty ...) :child_data [1 2 3] :data "parent data"}
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
                (not (= (list) (hostiles species Neighbors env)))            ;; someone looking at us funny?
                (do-hit)                    ;; whack 'em
                :else
                (do-fuel)                   ;; let's eat some more
                )
            )
        )
    )


(defn create-empty []
    (fn [energy health species env data]
      {:cmd :rest}
      )
)

(def Evam (create-slightlybrainy-ii 10 70 most-energy-and-fuel-target-selector))

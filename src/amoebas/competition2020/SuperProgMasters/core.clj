(ns amoebas.competition2020.SuperProgMasters.core    
    (:use amoebas.defs amoebas.lib amoebas.run)
)

;---Helper Functions---

(defn update-pos
    [current-pos dir] ;[x y] N
    (let 
        [
            pos-to-move (Neighbors dir) ;[x y]
            delta-x (get pos-to-move 0)
            delta-y (get pos-to-move 1)
            current-x (get current-pos 0)
            current-y (get current-pos 1)
        ]
        [(+ current-x delta-x) (+ current-y delta-y)] ;problem using mod when having negative numbers
    )
)

(defn abs [n] (max n (- n)))

(defn noorm-coords 
    [[x y]]
    [(if (= x 0) 0 (/ x (abs x))) (if (= y 0) 0 (/ y (abs y)))]
)
(defn add-coords
    [[x1 x2][y1 y2]]
    [(+ x1 y1) (+ x2 y2)]
)
(defn subtract-coords
    [[x1 x2][y1 y2]]
    [(- x1 y1) (- x2 y2)]
)

(defn dir-from-center
    "returns a direction between 0 and 7"
    [relative-pos]
    (let [dir-away-from-center (get Neighbor-To-Dir (noorm-coords relative-pos))]
        (if (nil? dir-away-from-center)
            (rand-int 8) ;it is in the center, every position around is away from center
            dir-away-from-center
        )
    )
)

(def Neighbors-Set
     (apply hash-set Neighbors)   
)

(def lower-resolution-Environment 
    (concat Neighbors [
        ; [0 -3] [0 -2] [0 -1] [0 1] [0 2] [0 3] 
        ; [-3 0] [-2 0] [-1 0] [1 0] [2 0] [3 0]
        ;[-3 -3] [-2 -2] [-1 -1] [1 1] [2 2] [3 3] 
        [-3 -3] [0 -3] [3 -3]
        [0 -3]         [0 3]
        [3 -3]  [3 0]  [3 3]
    ])
)

;------------------------Amoeba functions----------------------------------
(declare create-mariestad)
(defn create-staropramen
    "runns away from center and divides into better amoebas"
    ([]
        (create-staropramen
            [0 0]
            1
            0
        )
    )
    ([
        start-relative-pos
        start-round
        id
    ]
    (fn [energy health species env data]
        (let 
        [
            data-map (if (nil? data) {:relative-pos start-relative-pos :round start-round :id id} data)
            relative-pos (:relative-pos data-map) 
            round (:round data-map)
            empty-directions (empty-neighbors env)
            distance-from-center (distance [0 0] relative-pos)
            empty-neighbors-dirs (empty-neighbors env)
            empty-neighbors-count (count empty-neighbors-dirs)
            fuel-in-current-cell (:fuel (env [0 0]))
        ]   
            
            (defn divide-in-dir-func
                [dir]
                {:cmd :divide :dir dir :data (assoc data-map :round (inc round)) :function (create-staropramen (update-pos relative-pos dir) (inc round) (inc (:id data-map)))}
            )
            (defn divide-and-spawn-func
                [dir]
                {:cmd :divide :dir dir :data (assoc data-map :round (inc round)) :function (create-mariestad (update-pos relative-pos dir))}
            )
        
            (defn closest-towards-dir-func
                [direction]
                (let 
                    [
                        dir-set (apply hash-set empty-directions)
                        add   (+ direction 1)
                        subtr (- direction 1)
                        right (if (= add 8) 0 add)
                        left  (if (= subtr -1) 7 subtr)
                    ]
                    (cond 
                        (contains? dir-set direction)
                            direction
                        (contains? dir-set right)
                            right
                        (contains? dir-set left)
                            left
                        :else
                            nil
                    )
                )  
            )
            (defn rest-func []
                {:cmd :rest :data (assoc data-map :round (inc round))}
            )
            (defn move-in-dir-func
                [dir]
                {:cmd :move :dir dir :data (assoc data-map :round (inc round) :relative-pos (update-pos (:relative-pos data-map) dir))}
            )
            (defn flee-from-center []
                (let [dir (closest-towards-dir-func (dir-from-center relative-pos))]
                    (if (nil? dir)
                        (rest-func)
                        (move-in-dir-func dir)
                    )
                )
            )
            
            (cond
                (= id 0) ;predetermined steps, creates amoebas with id 1 in all directions, moves to get energy.
                    (cond 
                        (= round 1)
                            (divide-in-dir-func 0) 
                        (= round 2)
                            (divide-in-dir-func 4)
                        (= round 7)
                            (divide-in-dir-func 2)
                        (= round 11)
                            (divide-in-dir-func 6)
                        (= round 14)
                            (move-in-dir-func 3)
                        (= round 17)
                            (divide-in-dir-func 3)
                        (= round 18)
                            (move-in-dir-func 0)
                        (= round 24)
                            (divide-in-dir-func 1)
                        (= round 25)
                            (move-in-dir-func 6)
                        (= round 30)
                            (divide-in-dir-func 7)
                        (= round 31)
                            (move-in-dir-func 4)
                        (= round 37)
                            (divide-in-dir-func 5)
                        (= round 40)
                            (divide-and-spawn-func (get Neighbor-To-Dir (map #(* -1 %) (noorm-coords relative-pos)))) 
                        :else
                            (rest-func)       
                    )
                (= id 1) ;every amoeba that is a result of the first amoeba dividing
                    (if (and (= 8 empty-neighbors-count) (= 0 (mod distance-from-center 20)))
                        (if (> energy MinDivideEnergy) ;will not move until true
                            (divide-and-spawn-func (get Neighbor-To-Dir (map #(* -1 %) (noorm-coords relative-pos))))
                            (rest-func)    
                        )
                        (flee-from-center)
                )
                :else
                    (rest-func)
            )
        )
    ))   
)


(defn create-mariestad
    "Divides as fast as possible, selects target accoording to target of neighbour"
    ([start-relative-pos]
        (create-mariestad
            start-relative-pos
            nil     ;target 
        )
    )
    
    ([
        start-relative-pos
        target
     ]
    (fn [energy health species env data]
        
        (let 
            [
                data-map (if (nil? data) {:relative-pos start-relative-pos :target target :seen-enemies false} data)
                relative-pos (:relative-pos data-map) 
                target (:target data-map)

                empty-directions (empty-neighbors env)
                empty-neighbors-count (count empty-directions)
                rand-dir-full-cell (if (> empty-neighbors-count 0) (rand-nth empty-directions))
                fuel-in-current-cell (:fuel (env [0 0]))
                ShouldDivide (and (> energy MinDivideEnergy) (>= empty-neighbors-count 1))
                enemies-nearby-low-res (filter #(not= species (:species (:occupant (env %)))) (filter #(:occupant (env %)) lower-resolution-Environment)) ;maybe change to fewer checks to save time
                seen-enemies-before? (or (true? (:seen-enemies data-map)) (not (empty? enemies-nearby-low-res)))            
                enemies-nearby (if (true? seen-enemies-before?) (filter #(not= species (:species (:occupant (env %)))) (filter #(:occupant (env %)) Environment))) 
                enemies-nearby? (not (empty? enemies-nearby-low-res))
                healthy? (and (> energy 15) (> health 1))        
            ]

                (defn divide-in-dir-func
                    [dir]
                    {:cmd :divide :dir dir :data (assoc data-map :seen-enemies seen-enemies-before?) :function (create-mariestad (update-pos relative-pos dir))}
                )
                
                (defn closest-towards-dir-func
                    [direction]
                    (let 
                        [
                            dir-set (apply hash-set empty-directions)
                            add   (+ direction 1)
                            subtr (- direction 1)
                            right (if (= add 8) 0 add)
                            left  (if (= subtr -1) 7 subtr)
                        ]
                        (cond
                            (empty? dir-set)
                                nil 
                            (contains? dir-set direction)
                                direction
                            (contains? dir-set right)
                                right
                            (contains? dir-set left)
                                left
                            :else
                                nil
                        )
                    )  
                )
            
                (defn move-in-dir-func
                    [dir]
                    {:cmd :move :dir dir :data (assoc data-map :relative-pos (update-pos (:relative-pos data-map) dir) :seen-enemies seen-enemies-before?)} 
                )
 
                (defn rest-func []
                    {:cmd :rest :data (assoc data-map :seen-enemies seen-enemies-before?)}
                )
                
                (defn fuel-func []
                    (cond 
                        (or (> fuel-in-current-cell 0) (= empty-neighbors-count 0) (= energy 100))
                            (rest-func)
                        :else
                            (let 
                                [
                                    dir-with-most-fuel (last (sort-by  #(total-fuel (Env-Sections %) env) [1 3 5 7]))
                                    selected-dir (closest-towards-dir-func dir-with-most-fuel)  
                                ]
                                (if (nil? selected-dir)
                                    (rest-func)
                                    (move-in-dir-func selected-dir)
                                )
                            )
                    )     
                )
                
                (defn select-enemy-and-hit [enemies-in-hitting-dist] 
                    "If a neighbour has a target that is reachable for this amoeba, use that target. Else, choose the target with the least amount of health"
                    (let 
                        [
                            neighbours-targets (filter #(not (nil? %)) (map #(:target (:data (:occupant (env %)))) Neighbors))
                            neighbours-targets-close (filter #(= 1 (distance relative-pos %)) neighbours-targets)
                            neighbours-targets-close-actual-pos (map #(subtract-coords % relative-pos) neighbours-targets-close)
                            relevant-neighbours-targets-close-actual-pos (filter (apply hash-set enemies-in-hitting-dist) neighbours-targets-close-actual-pos)
                            own-pos (first (sort-by #(:health (env %)) enemies-in-hitting-dist))
                            position (if (empty? relevant-neighbours-targets-close-actual-pos) own-pos (rand-nth relevant-neighbours-targets-close-actual-pos))
                            target (add-coords relative-pos position) 
                            dir (get Neighbor-To-Dir position)

                        ]
                        {:cmd :hit :dir dir :data (assoc data-map :target target)}     
                    )                   
                )

                (defn move-towards-enemy [closest-enemy-pos]
                    (let [dir (closest-towards-dir-func (get Neighbor-To-Dir (noorm-coords closest-enemy-pos)))]
                        (if (nil? dir)
                            (fuel-func)
                            (move-in-dir-func dir)
                        )
                    )
                )

                (cond
                    (and ShouldDivide (> energy MinDivideEnergy))
                        (divide-in-dir-func rand-dir-full-cell)             
                    (true? enemies-nearby?)
                        (let 
                            [
                                enemies-in-hitting-dist (filter Neighbors-Set enemies-nearby)
                                enemies-in-hitting-dist? (not (empty? enemies-in-hitting-dist))
                                closest-enemy-pos (first (sort-by #(distance [0 0] %) enemies-nearby))
                            ]
                            (cond
                                (and (true? healthy?) (true? enemies-in-hitting-dist?))
                                    (select-enemy-and-hit enemies-in-hitting-dist)
                                (true? healthy?)
                                    (move-towards-enemy closest-enemy-pos)
                                :else
                                    (fuel-func)
                                    
                            )
                        )
                    :else
                        (fuel-func)
                )
            )
        ) 
    )        
)

(def Evam
    (create-staropramen)    
)


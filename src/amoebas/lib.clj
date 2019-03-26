(ns amoebas.lib
    (:use amoebas.defs amoebas.util)
)

;;
;;  directions
;;      directions correspond to the eight neighboring cells
;;      a cell can reach directly, either to move, divide, or attack
;;      they are represented by the numbers from 0 to 7, starting with the
;;      upper left (NW, North-West) neighbor 0, and counting clockwise.
;;


(def dNW            0)

(def dN             1)

(def dNE            2)

(def dE             3)

(def dSE            4)

(def dS             5)

(def dSW            6)

(def dW             7)


(def Neighbors          ;; NB: this is a region
    [ [-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] ] 
)

(def Here [0 0])

(def Environment        ;; NB: this is a region
    (for [x (range (inc (* 2 ViewDistance))) y (range (inc (* 2 ViewDistance)))] [(- x ViewDistance) (- y ViewDistance)])
)

(def Neighbor-To-Dir 
    { [-1 -1] 0 [0 -1] 1 [1 -1] 2 [1 0] 3 [1 1] 4 [0 1] 5 [-1 1] 6 [-1 0] 6 } 
)


(defn- add-dir [dir d] (mod (+ dir d) 8) )

(defn CW [dir]      (add-dir dir 1))

(defn CCW [dir]      (add-dir dir -1))

(defn CW90 [dir]      (add-dir dir 2))

(defn CCW90 [dir]      (add-dir dir -2))

(defn AWAY [dir]      (add-dir dir 4))


(defn add-coordinates
    [p1 p2]
    
    (mapv + p1 p2)
)

;;
;;  sections
;;      sections are regions that are contiguous parts of the environment
;;

(def Env-Sections 
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

(defn cells-in-section
    [dir env]
    
    (map env (Env-Sections dir))
)

;;
;;  energy utility functions
;;

(defn energy-level [r] (int (* r MaxAmoebaEnergy)))

(defn above-division-energy-level [r] (int (+ MinDivideEnergy (* r (- MaxAmoebaEnergy MinDivideEnergy)))) )

;;
;;  functions operating individual cells
;;      (a cell is represented by a vector pair of [x y] coordinates
;;

(defn cell-empty?
    [env pos]
    
    (not (:occupant (env pos)))
)

(defn empty-neighbors
    [env]
    
    (filter #(cell-empty? env (Neighbors %)) (range 8))
)

(defn contains-hostile
    [species pos env]
    
    (let
        [
            cell        (env pos)
            occupant    (:occupant cell)
        ]
    
        (and occupant (not= species (:species occupant)))
    )
)

(defn contains-friendly
    [species pos env]
    
    (let
        [
            cell (env pos)
            occupant    (:occupant cell)
        ]
    
        (and occupant (= species (:species occupant)))
    )
)

;;
;;  functions operating on regions
;;      (a region is represented by a seq of [x y] coordinates of cells)
;;      note that sections are regions
;;


(defn hostiles
    "returns the sub-region of the region argument that contains hostiles"

    [species region env]
    
    (filter #(contains-hostile species % env) region)
)

(defn friendlies
    "returns a subregion of the region argument that contains friendlies"

    [species region env]
    
    (filter #(contains-friendly species % env) region)
)

(defn at-least-fuel-subregion
    "returns the subregion of cells containing at least min-fuel amount of fuel"

    [min-fuel region env]
    
    (filter #(<= min-fuel (:fuel (env %))) region)
)

(defn total-fuel
    "returns the total amount of fuel in the region"

    [region env]
    
    (sum (map #(:fuel (env %)) region))
)

(defn average-fuel
    "returns the average amount of fuel in the region; might not be an integer"
    
    [region env]
    
    (/ (total-fuel region env) (count region))
)

(defn empty-subregion
    [region env]
    
    (filter #(cell-empty? env %) region)
)

;;
;;  section sorters
;;      take a list of directions (numbers from 0 to 7) as input
;;      evaluates each direction based on a metric of its corresponding section
;;

(defn sections-by-hostiles
    [dirs env species]
    
    (sort-by  #(count (hostiles (Env-Sections %))) dirs)
)

(defn sections-by-friendlies
    [dirs env species]
    
    (sort-by  #(count (friendlies (Env-Sections %))) dirs)
)

(defn sections-by-fuel
    [dirs env]
    
    (sort-by  #(total-fuel (Env-Sections %) env) dirs)
)

(defn sections-by-fuel-density
    [dirs env]
    
    (sort-by  #(average-fuel (Env-Sections %) env) dirs)
)



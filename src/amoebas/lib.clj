(ns amoebas.lib
    (:use amoebas.defs amoebas.util)
)

;;
;;  this namespace contains a (hopefully growing) collection of stuff that
;;  can be sueful when writing amoeba functions
;;

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

(defn- add-dir [dir d] (mod (+ dir d) 8) )

(defn CW 
    "given a direction, this returns the next direction in clockwise rotation"
    [dir]      
    
    (add-dir dir 1)
)

(defn CCW 
    "given a direction, this returns the next direction in counter-clockwise rotation"
    [dir]      
    
    (add-dir dir -1)
)

(defn CW90 
    "given a direction, this returns the direction in clockwise rotation that is perpendicular to it"
    [dir]      
    
    (add-dir dir 2)
)

(defn CCW90
    "given a direction, this returns the direction in counter-clockwise rotation that is perpendicular to it"
    [dir]      
    
    (add-dir dir -2)
)

(defn AWAY 
    "given a direction, this returns the opposite direction"
    [dir]      
    
    (add-dir dir 4)
)


(defn add-coordinates
    "adds two coordinate vectors"
    [p1 p2]
    
    (mapv + p1 p2)
)


(defn distance
    "distance between two points coordinate vectors,
     as defined by the L^infty norm of their difference;
     corresponds to the number of steps to get from one to the other"
    [[ax ay] [bx by]]
    
    (max (abs (- ax bx)) (abs (- ay by)))
)

;;
;;  sections
;;      sections are regions that are contiguous parts of the environment
;;

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

(defn cells-in-section
    "returns all cells of a specified section; the section is specified by its direction"
    [dir env]
    
    (map env (Env-Sections dir))
)

;;
;;  energy utility functions
;;

(defn energy-level 
    [r] 
    
    (int (* r MaxAmoebaEnergy))
)

(defn above-division-energy-level 
    [r] 
    
    (int (+ MinDivideEnergy (* r (- MaxAmoebaEnergy MinDivideEnergy)))) 
)

;;
;; 
;;
;;

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

(defn contains-friendly?
    "given a position, determines whether it contains a friendly"
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
    
    (filter #(contains-hostile? species % env) region)
)

(defn friendlies
    "returns a subregion of the region argument that contains friendlies"

    [species region env]
    
    (filter #(contains-friendly? species % env) region)
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
    "determines the subregion of empty cells in a region"
    [region env]
    
    (filter #(cell-empty? env %) region)
)

(defn region-empty?
    [region env]
    
    (every? #(cell-empty? env %) region)
)

;;
;;  section sorters
;;      take a list of directions (numbers from 0 to 7) as input
;;      evaluates each direction based on a metric of its corresponding section
;;

(defn sections-by-hostiles
    "sorts the sections by the number of hostiles in them, ascending"
    [dirs env species]
    
    (sort-by  #(count (hostiles species (Env-Sections %) env)) dirs)
)

(defn sections-by-friendlies
    "sorts the sections by the number of friendlies in them, ascending"
    [dirs env species]
    
    (sort-by  #(count (friendlies species (Env-Sections %) env)) dirs)
)

(defn sections-by-fuel
    "sorts the sections by the amount of fuel in them, ascending"
    [dirs env]
    
    (sort-by  #(total-fuel (Env-Sections %) env) dirs)
)

(defn sections-by-fuel-density
    "sorts the sections by the average amount of fuel in them, ascending"
    [dirs env]
    
    (sort-by  #(average-fuel (Env-Sections %) env) dirs)
)



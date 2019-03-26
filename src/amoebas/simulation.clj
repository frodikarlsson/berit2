(ns amoebas.simulation
    (:use amoebas.defs amoebas.util)
)


;;
;;  definitions
;;

(defstruct Amoeba       :function :species :energy :health :data)

(def Neighbor-Cells 
    [ [-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0] ] 
)


;;
;;  world stuff
;;

(defn create-world 
    [fuel]
        
    (vec (for [i (range WorldSize) j (range WorldSize)] fuel))
)

(defn wrap
    [a]
    
    (mod a WorldSize)
)

(defn add-pos
    [[x y] [dx dy]]
    
    [(wrap (+ x dx)) (wrap (+ y dy))]
)

(defn world-index [[x y]] (+ (* WorldSize y) x))


(defn cell-fuel
    [world pos]

    (world (world-index pos))
)

(defn cell-fuel!
    [world pos fuel]
    
    (assoc! world (world-index pos) fuel)
)

(defn add-fuel!
    [world pos delta]
    
    (let [idx (world-index pos)]
        (assoc! world idx (bound 0 (+ (world idx) delta) MaxCellEnergy))
    )
)

(defn world-add-fuel
    [world delta]
    
        (doseq
            [x (range WorldSize) y (range WorldSize)]
            
            (add-fuel! world [x y] delta)
        )
)
        
(defn sunshine
    [world pop]
    
    (vec 
        (for [y (range WorldSize) x (range WorldSize) :let [pos [x y]]]
            (if (contains? pop pos)
                (cell-fuel world pos)
                (bound 0 (+ (cell-fuel world pos) SunEnergy) MaxCellEnergy)
            )
        )
    )
)

;;
;;  making a step
;;

(defn free?
    [pop pos]
    
    (not (pop pos))   ;; contains? not working on transient array maps
)

(defn occupied?
    [pop pos]
    
    (pop pos)   ;; contains? not working on transient array maps
)



(defn environment-cell
    [world pop pos]
    
    (let
        [
            fuel        (cell-fuel world pos)
            occupant    (pop pos)
        ]
    
        {:fuel fuel :occupant occupant}     ;; FIXME (?): leaking data
    )
)


(defn environment 
    [pos world pop]
    
    (memoize (fn 
        (
            [dx dy]
            
            (if
                (and (<= (- ViewDistance) dx ViewDistance) (<= (- ViewDistance) dy ViewDistance))
                (environment-cell world pop (add-pos pos [dx dy]))
                nil
            )
        )
        (
            [dpos]
            
            (if
                (and (<= (- ViewDistance) (dpos 0) ViewDistance) (<= (- ViewDistance) (dpos 1) ViewDistance))
                (environment-cell world pop (add-pos pos dpos))
                nil
            )
        )
        ( [] pos )      ;; FIXME (this is a hack to be able to obtain the absolute position, for debugging)
    ))
)


(defn update-amoeba 
    (
        [a energy health data]
        
        (struct Amoeba (:function a) (:species a) energy health data)
    )
    (
        [a energy data]
        
        (update-amoeba a energy (:health a) data)
    )
    (
        [a health]

        (update-amoeba a (:energy a) health (:data a))
    )
)

;;
;;  actions
;;
;;      {:cmd :rest}
;;      {:cmd :move, :dir n}                    n in 1..8
;;      {:cmd :hit, :dir n}                     n in 1..8
;;      {:cmd :divide, :dir n}                  n in 1..8
;;      {:cmd :divide, :dir n, :function f}     n in 1..8, f an amoeba function
;;
;;      all actions can have an optional entry {..., :data v}   where v is any object
;;      divide actions can have an additional entry {..., :child-data v}    with v any object
;;      both entries default to nil
;;


(defn step-rest
    [world pop pos a data]
    
    (let
        [
            energy-xfer     (min (cell-fuel world pos) (- MaxAmoebaEnergy (:energy a)) MaxFuelingEnergy) 
            new-fuel        (+ energy-xfer (:energy a) (- RestEnergy))
            new-health      (min (+ (:health a) RecoveryGain) MaxAmoebaHealth)
        ]
        
        (if (<= 0 new-fuel)
            (do
                (add-fuel! world pos (- energy-xfer))
                (assoc! pop pos (update-amoeba a new-fuel new-health data))     ;; fuel and recover
            )
            (dissoc! pop pos)                   ;; amoeba starved
        )
    )
)

(defn step-move
    [world pop pos a dir data]
    
    (let
        [ new-pos (add-pos pos (Neighbor-Cells dir)) ]
    
        (if (and (<= MoveEnergy (:energy a)) (free? pop new-pos))
            (do
                (dissoc! pop pos)
                (assoc! pop new-pos (update-amoeba a (- (:energy a) MoveEnergy) data))
            )
            (step-rest world pop pos a data)
        )
    )
)

(defn step-hit
    [world pop pos a dir data]
    
    (let
        [
            target-pos  (add-pos pos (Neighbor-Cells dir))
            target      (pop target-pos)
            new-energy  (- (:energy a) AttackEnergy)
        ]
        
        (if (and (<= AttackEnergy (:energy a)) target)
            (do
                (if (<= (:health target) HitLoss)
                    (do
                        (dissoc! pop target-pos)                                                          ;; target dead
                        (add-fuel! world target-pos (:energy target))
                    )
                    (assoc! pop target-pos (update-amoeba target (- (:health target) HitLoss)))       ;; target injured
                )
                (assoc! pop pos (update-amoeba a (- (:energy a) AttackEnergy) data))      ;; expended attack energy
            )
            (step-rest world pop pos a data)
        )
    )
)

(defn step-divide
    [world pop pos a dir f data child-data]
    
    (let
        [ 
            new-pos         (add-pos pos (Neighbor-Cells dir)) 
            new-energy      (int (/ (- (:energy a) DivideEnergyLoss) 2))
        ]
    
        (if (and (<= MinDivideEnergy (:energy a)) (free? pop new-pos))
            (do
                (assoc! pop new-pos (struct Amoeba f (:species a) new-energy (:health a) child-data))  ;; the spawn

                (assoc! pop pos (update-amoeba a new-energy data))
            )
            (step-rest world pop pos a data)
        )
    )
)
    

(defn step
    [world pop pos a]

    (try
        (let
            [
                action      ((:function a) (:energy a) (:health a) (:species a) (environment pos world pop) (:data a) )        ;; call to amoeba function
                cmd         (:cmd action)
                data        (:data action)
                child-data  (:child-data action)
                dir         (:dir action)
                f           (if (:function action) (:function action) (:function a))
            ]

            (case cmd
                :rest   (step-rest world pop pos a data)
                :move   (step-move world pop pos a dir data)
                :hit    (step-hit world pop pos a dir data)
                :divide (step-divide world pop pos a dir f data child-data)
            )
        )
        (catch Exception e
            (println "ERROR: " e)     ;; DEBUG
            (step-rest world pop pos a (:data a))
        )
    )
)
                    
    
    
(defn step-all
    [world population]
    
    (let
        [
            w       (transient world)
            pop     (transient population)
            start   (System/nanoTime)
        ]

        
        (doseq
            [pos (keys population)]
            
            (when (pop pos)     ;; if still alive
                (step w pop pos (pop pos))                
            )
        )
        
        (let
            [
                new-pop     (persistent! pop)
                new-w       (persistent! w)
            ]
            
            [(sunshine new-w new-pop) new-pop]       ;; return new world and population, with all changes made persistent
        )
    )
)

;; 
;; 

    
    

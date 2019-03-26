(ns amoebas.defs)

;;
;; these definitions represent the basic constants of the
;; world the amoebas live in, including the amoebas themselves
;;


;;;; the world

(def WorldSize      200)


(def SunEnergy      1)      ;; amount of energy to add to empty cells after each round

(def MaxCellEnergy  100)    ;; max amount of fuel energy available in a cell

(def ViewDistance   3)      ;; max distance of viewable cell (max norm)


;;;;    amoeba energy

(def MaxAmoebaEnergy  100)  ;; maximum energy an individual amoeba can store

(def RestEnergy     1)      ;; energy cost of rest action

(def MoveEnergy     3)      ;; energy cost of move action

(def AttackEnergy   10)     ;; energy cost of hit action

(def MaxFuelingEnergy  10)  ;; max fuel uptake when resting

(def MinDivideEnergy    30) ;; min energy required before division

(def DivideEnergyLoss   20) ;; energy cost of division


;;;;    amoeba health

(def MaxAmoebaHealth  10)   ;; max health of an amoeba

(def HitLoss        4)      ;; health loss when injured by  a hit

(def RecoveryGain   1)      ;; health gain when resting



    



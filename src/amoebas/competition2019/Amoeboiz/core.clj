(ns amoebas.competition2019.Amoeboiz.core
  (:use amoebas.defs amoebas.lib amoebas.run amoebas.examples) 
)

(defn core
  [soldier-function soldier-behaviour border-function border-behaviour middle-behaviour]
  (fn [energy health species env data]
    (if (soldier-function energy health species env data)
      (soldier-behaviour energy health species env data)
      (if (border-function energy health species env data)
        (border-behaviour energy health species env data)
        (middle-behaviour energy health species env data)
      )
    )
  )
)

(defn soldier-if-enemies-nearby
  [energy health species env data]
  (< 0 (count (hostiles species Environment env)))
)

(defn soldier-divide-attack
  [divideUntil divide-behaviour attack-behaviour]
  (fn [energy health species env data]
    (if (> divideUntil (count (friendlies species Environment env)))
      (divide-behaviour energy health species env data)
      (if (> 10 energy)
        (divide-behaviour energy health species env data) ;Fuel
        (attack-behaviour energy health species env data)
      )
    )
  )
)

(defn divide-and-fuel [divide-energy fuel-targeter]
  (fn [energy health species env data]
    (if (< energy divide-energy)
      (if (< 9 (:fuel (env 0 0)))
        {:cmd :rest}
        (fuel-targeter energy health species env data :move)
      )
      (fuel-targeter energy health species env data :divide)
    )
  )
)

(defn goto-highest-fuel
  [energy health species env data command]
  (def goto (last (sort-by  #(:fuel (env (Neighbors %))) (empty-neighbors env))))
  (if goto
    {:cmd command :dir goto}
    {:cmd :rest}
  )
)

(defn goto-highest-fuel-if-greater-than
  [limit]
  (fn
    [energy health species env data command]
    (def goto (last (sort-by  #(:fuel (env (Neighbors %))) (empty-neighbors env))))
    (if goto
      (if (< limit (:fuel (env (Neighbors goto))))
        {:cmd command :dir goto}
        {:cmd :rest}
      )
      {:cmd :rest}
    )
  )
)

(defn attack-if-reach-else-advance
  [target-selector]
  (fn [energy health species env data]
    (def hs (hostiles species Neighbors env)) ;; hostile neighbors
    (if (empty? hs)
      (if (empty? (empty-neighbors env))
        {:cmd :rest}
        {:cmd :move :dir (last (sections-by-hostiles (empty-neighbors env) env species))}
      )
      {:cmd :hit :dir (Neighbor-To-Dir (target-selector hs species env))}
    )
  )
)

(defn most-energy-target-selector-local
    "picks a target with the highest amount of energy stored"
    [hs species env]
    (last (sort-by #(:energy (:occupant (env %))) hs))
)

(defn border-by-diff-atleast-sum-atmost
  [leastdiff mostsum]
  (fn [energy health species env data]
    (def sects (sections-by-friendlies [0 1 2 3 4 5 6 7] env species))
    (def counts (vec (map #(count (friendlies species (Env-Sections %) env)) sects)))
    (def sum1 (+ (counts 0) (counts 1) (counts 2) (counts 3)))
    (def sum2 (+ (counts 4) (counts 5) (counts 6) (counts 7)))
    (def diff (- sum2 sum1))
    (and (<= leastdiff diff) (>= mostsum sum1)) ; top 4 populated regions at least 5 more frendlies than bot 4. At most 10 in least populated
  )
)

(defn divide-if-few-else-move ;For border
  [limit divide-behaviour move-behaviour]
  (fn [energy health species env data]
    (if (> limit (count (friendlies species Environment env)))
      (divide-behaviour energy health species env data)
      (move-behaviour energy health species env data)
    )
  )
)

(defn move-to-new-ground
  [energy health species env data]
  (if (empty? (empty-neighbors env))
    {:cmd :rest}
    {:cmd :move :dir (first (sections-by-friendlies (empty-neighbors env) env species))}
  )
)

(defn move-to-new-ground-if-diff-greater-than
  [difflim]
  (fn
    [energy health species env data]
    (def section-order (sections-by-friendlies (empty-neighbors env) env species))
    (def sum1 (count (friendlies species (Env-Sections (first section-order)) env)))
    (def sum2 (count (friendlies species (Env-Sections (last section-order)) env)))
    (def diff (- sum1 sum2))
    (if (empty? (empty-neighbors env))
      {:cmd :rest}
      (if (> diff difflim)
        {:cmd :move :dir (first (sections-by-friendlies (empty-neighbors env) env species))}
        {:cmd :rest}
      )
    )
  )
)

(defn divide-if-few-else-fuel
  [limit divide-behaviour fuel-behaviour]
  (fn [energy health species env data]
    (if (> limit (count (friendlies species Environment env)))
      (divide-behaviour energy health species env data)
      (fuel-behaviour energy health species env data)
    )
  )
)

(defn fuel-if-need-energy-else-move
  [fuel-targeter move-behaviour energy-target]
  (fn [energy health species env data]
    (if (< energy-target energy)
      (move-behaviour energy health species env data) ;have enough energy
      (if (< 9 (:fuel (env 0 0)))
        {:cmd :rest}
        (fuel-targeter energy health species env data :move)
      )
    )
  )
)

(def mysoldier (soldier-divide-attack 15 (divide-and-fuel 30 goto-highest-fuel) (attack-if-reach-else-advance most-energy-target-selector-local)))
(def myborder (divide-if-few-else-move 5 (divide-and-fuel 30 goto-highest-fuel) move-to-new-ground))
(def mymiddle (divide-if-few-else-fuel 20 (divide-and-fuel 30 goto-highest-fuel) (fuel-if-need-energy-else-move goto-highest-fuel move-to-new-ground 80)))
(def mymiddle2 (divide-if-few-else-fuel 20 (divide-and-fuel 50 (goto-highest-fuel-if-greater-than 25)) (fuel-if-need-energy-else-move (goto-highest-fuel-if-greater-than 50) (move-to-new-ground-if-diff-greater-than 3) 80)))


;(def Evam-bad (core soldier-if-enemies-nearby mysoldier (border-by-diff-atleast-sum-atmost 5 30) myborder mymiddle))
(def Evam (core soldier-if-enemies-nearby mysoldier (border-by-diff-atleast-sum-atmost 5 30) myborder mymiddle2))

; (def T (tournament
;     {
;         :red      Evam
;         :blue     Evam2
;         ;:yellow   (erik_bot 15 0.2 10)
;         ;:gray     (bot1 weakest-target-selector3)
;         :white     (create-slightlybrainy 10 70 most-energy-and-fuel-target-selector)
;         :orange    (create-mutating-slightlybrainy 10 70 most-energy-target-selector-local 0.3 2)
;     })
; )

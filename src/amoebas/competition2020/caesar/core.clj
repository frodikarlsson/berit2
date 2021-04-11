(ns amoebas.competition2020.caesar.core
  (:require [amoebas.defs :refer [MoveEnergy AttackEnergy MaxFuelingEnergy MinDivideEnergy]]
            [amoebas.lib :refer [Neighbors Here Environment Dirs CW CCW AWAY Env-Sections empty-neighbors hostiles
                                 friendlies]]))

(def ^:const explorer-if-friends-factor 3)                  ;; FIXME optimize
(def ^:const stay-fuel (- MaxFuelingEnergy 2))              ;; FIXME optimize
(def ^:const divide-energy (- MinDivideEnergy 1))           ;; FIXME optimize
(def ^:const move-energy-explorer 10)                       ;; FIXME optimize
(def ^:const move-energy-farmer (* 7 MoveEnergy))           ;; FIXME optimize
(def ^:const divide-until-friendlies-warrior 23)            ;; FIXME optimize
(def ^:const divide-until-friendlies-explorer 14)           ;; FIXME optimize
(def ^:const divide-until-friendlies-farmer 9)              ;; FIXME optimize
(def ^:const max-rounds 2000)

(defn- sees-enemy?
  "Determines whether the position contains a friendly amoeba that saw an enemy"
  [species pos env]

  (let
    [cell (env pos)
     occupant (:occupant cell)]

    (and occupant
         (= species (:species occupant))
         (not-empty (:data occupant))                       ;; :data will be nil for us (0 0) during first round
         (> 0 (:hostiles-count (:data occupant))))))

(defn- amoeba-task
  "Decides the current amoeba task based on the environment.
  It will return one task: :warrior :explorer :farmer or :last-round."
  [my-hostiles species env round]



  (if (= round max-rounds)
    :last-round
    (if (or (not-empty my-hostiles)                         ;; we see an enemy
            (not-empty (filter #(sees-enemy? species % env)
                               Environment)))               ;; we or any friendly amoeba nearby saw an enemy
      :warrior
      (let [friendlies-per-dir (zipmap Dirs (map #(count (friendlies species (Env-Sections %) env)) Dirs))
            scores-per-dir (zipmap Dirs (map #(reduce + 0 (map friendlies-per-dir [(CCW %) % (CW %)])) Dirs))
            best (apply max-key val scores-per-dir)
            best-score (val best)
            opposite-dir (AWAY (key best))
            opposite-score (get scores-per-dir opposite-dir)
            ]
        (if (> best-score (* explorer-if-friends-factor opposite-score))
          :explorer
          :farmer)))))

(defn- highest-fuel-dir-among-possible
  "Calculates which direction among possible that has the highest fuel."
  [env possible]

  (apply max-key #(:fuel (env (Neighbors %))) possible))

(defn- empty-neighbor-dir-with-highest-fuel
  "Calculates which direction with empty neighbor that has the highest fuel, or nil if empty neighbors."
  [env]

  (let [current-empty-neighbor-dirs (empty-neighbors env)]

    (if (empty? current-empty-neighbor-dirs)
      nil
      (highest-fuel-dir-among-possible env current-empty-neighbor-dirs))))

(defn- command-for-empty-neighbor-with-highest-fuel
  "Return command for the empty neighbor with the highest fuel if more than specified, or :rest if no empty neighbors."
  [command env min-fuel-new-cell]

  (let [best-neighbor-dir (empty-neighbor-dir-with-highest-fuel env)]
    (if (and best-neighbor-dir
             (or (< min-fuel-new-cell 0)
                 (> (:fuel (env (Neighbors best-neighbor-dir))) min-fuel-new-cell)))
      {:cmd command :dir best-neighbor-dir}
      {:cmd :rest})))

(defn- action-or-fuel
  "Do action if enough energy, otherwise move if not enough fuel here and more elsewhere, else :rest.
   Choosing the empty neighbor with the highest fuel as destination."
  [energy env action-energy stay-fuel action]

  (if (> energy action-energy)
    (command-for-empty-neighbor-with-highest-fuel action env -1)
    (if (> (:fuel (env Here)) stay-fuel)
      {:cmd :rest}
      (command-for-empty-neighbor-with-highest-fuel :move env stay-fuel))))

(defn- divide-or-fuel
  "Divide if enough energy, otherwise move if not enough fuel here and more elsewhere, else :rest.
   Choosing the empty neighbor with the highest fuel as destination."
  [energy env divide-energy stay-fuel]

  (action-or-fuel energy env divide-energy stay-fuel :divide))

(defn- move-or-fuel
  "Move if enough energy, otherwise move if not enough fuel here and more elsewhere, else :rest.
   Choosing the empty neighbor with the highest fuel as destination."
  [energy env move-energy stay-fuel]

  (action-or-fuel energy env move-energy stay-fuel :move))

(defn- highest-energy-and-fuel-among-possible-dirs
  "Calculates which amoeba among possible directions that has the highest fuel and energy combined."
  [env possible]

  (apply max-key
         #(let [neighbor (env (Neighbors %))]
            (+ (:fuel neighbor)
               (:energy (:occupant neighbor))))
         possible))

(defn- hostile-dirs
  "Returns directions among possible with hostile immediate neighbors."
  [env species possible]

  (filter #(let [occupant (:occupant (env (Neighbors %)))]
             (and occupant
                  (not= species (:species occupant))))
          possible))


(defn- divide-move-or-fuel
  "Returns an action appropriate for explorers."
  [energy env my-friendlies divide-until move-energy-limit]

  (if (< (count my-friendlies) divide-until)
    (divide-or-fuel energy env divide-energy stay-fuel)
    (move-or-fuel energy env move-energy-limit stay-fuel)))


(defn- warrior-behavior
  "Returns an action appropriate for warriors."
  [energy species env my-friendlies]

  (let [current-hostile-dirs (hostile-dirs env species Dirs)]
    (if (or (< energy AttackEnergy)
            (empty? current-hostile-dirs))
      (divide-move-or-fuel energy env my-friendlies divide-until-friendlies-warrior divide-energy)
      {:cmd :hit :dir (highest-energy-and-fuel-among-possible-dirs env current-hostile-dirs)})))


(defn- explorer-behavior
  "Returns an action appropriate for explorers."
  [energy env my-friendlies]
  (divide-move-or-fuel energy env my-friendlies divide-until-friendlies-explorer move-energy-explorer))

(defn- farmer-behavior
  "Returns an action appropriate for farmers."
  [energy env my-friendlies]
  (divide-move-or-fuel energy env my-friendlies divide-until-friendlies-farmer move-energy-farmer))


(defn create-caesar-amoeba
  "Returns the caesar amoeba function."
  []

  (fn [energy health species env data]
    (let
      [my-friendlies (friendlies species Environment env)
       my-hostiles (hostiles species Environment env)
       round (+ 1
                (if (empty? data)
                  0
                  (:round data)))
       new-data {:round round :hostiles-count (count my-hostiles)} ; :friendlies-count (count my-friendlies)
       current-task (amoeba-task my-hostiles species env round)
       new-command (case current-task
                     :warrior (warrior-behavior energy species env my-friendlies)
                     :explorer (explorer-behavior energy env my-friendlies)
                     :farmer (farmer-behavior energy env my-friendlies)
                     :last-round {:cmd :rest}
                     )
       ]
      (assoc new-command :data new-data :child-data new-data))))

(def Evam (create-caesar-amoeba))


(ns amoebas.competition2020.Untitled.core
  (:use amoebas.defs amoebas.lib amoebas.run amoebas.examples)
)
  
(defn create-untitled
  [is-soldier soldier-divide-attack feeling-lonely divide-and-spread hit-if-reach-else-move to-cell-with-highest-fuel]

  (fn [energy health species env data]
        
    (if (is-soldier energy health species env data)
      (soldier-divide-attack energy health species env data 20 hit-if-reach-else-move);23
      (if (feeling-lonely energy health species env data 15);30 -
        (divide-and-spread energy health species env data 9);10
        (if (< 19 (:fuel (env 0 0))) ;19
	        {:cmd :rest}
          (if (empty? (empty-neighbors env))
            {:cmd :rest}
            (to-cell-with-highest-fuel energy health species env data :move)
          )
	      )
      )
    )
  )  
)

(defn is-soldier
  [energy health species env data]
  (< 0 (count (hostiles species Environment env)))
)

(defn feeling-lonely
  [energy health species env data minFriends]
  (> minFriends (count (friendlies species Environment env)))
)

(defn to-cell-with-highest-fuel
  [energy health species env data command]
  (def go-to (last (sort-by  #(:fuel (env (Neighbors %))) (empty-neighbors env))))
  (if go-to
    {:cmd command :dir go-to}
    {:cmd :rest}
  )
)

(defn soldier-divide-attack
  [energy health species env data minFriendly hit-if-reach-else-move]
  (if (and (< energy (+ MinDivideEnergy 4)) (> energy (- MinDivideEnergy 1)) (not (empty? (empty-neighbors env))) true) ;energy between MinDivideEnergy and MinDivideEnergy+5
    (to-cell-with-highest-fuel energy health species env data :divide)
	  (if (< minFriendly (count (friendlies species Environment env)))
	    (hit-if-reach-else-move energy health species env data)
	    (if (and (> energy (- MinDivideEnergy 1)) (not (empty? (empty-neighbors env))))
	      (to-cell-with-highest-fuel energy health species env data :divide)
	      (if (< 5 (:fuel (env 0 0)))
	        {:cmd :rest}
          (if (empty? (empty-neighbors env))
            {:cmd :rest}
;          (to-cell-with-highest-fuel energy health species env data :move)
	          {:cmd :move :dir (last (sections-by-hostiles (empty-neighbors env) env species))}
          )
	      )
	    )
	  )
  )
)

(defn hit-if-reach-else-move
  [energy health species env data]
  (if (and (> energy 10) (> 5 (:fuel (env 0 0))) (not (empty? (empty-neighbors env))))
    {:cmd :move :dir (last (sections-by-hostiles (empty-neighbors env) env species))}
    (if (and (< energy 10) (< 5 (:fuel (env 0 0))))
      {:cmd :rest}
		  (if (not (empty? (hostiles species Neighbors env)))
;  	    {:cmd :hit :dir (Neighbor-To-Dir (most-energy-and-fuel-target-selector (hostiles species Neighbors env) species env))}
;	      {:cmd :hit :dir (Neighbor-To-Dir (weakest-target-selector (hostiles species Neighbors env) species env))}
        {:cmd :hit :dir (Neighbor-To-Dir (weakest-target-selector (hostiles species Neighbors env) species env))}
		    (if (empty? (empty-neighbors env))
		      {:cmd :rest}
		      {:cmd :move :dir (last (sections-by-hostiles (empty-neighbors env) env species))}
;		      (to-cell-with-highest-fuel energy health species env data :move)
        )
	    )
	  )
  )
)


(defn divide-and-spread
  [energy health species env data minFriendly]
  (if (empty? (empty-neighbors env))
    {:cmd :rest}
    (if (and (< energy 30) (< 19 (:fuel (env 0 0))))
      {:cmd :rest}
	    (if (> minFriendly (count (friendlies species Environment env)))
	     (to-cell-with-highest-fuel energy health species env data :divide)
	     {:cmd :move :dir (first (sections-by-friendlies (empty-neighbors env) env species))} ;goes in direction of least friendlies
	;     (to-cell-with-highest-fuel energy health species env data :move)
	    )
    )
  )
)
    

(def Evam (create-untitled is-soldier soldier-divide-attack feeling-lonely divide-and-spread hit-if-reach-else-move to-cell-with-highest-fuel))
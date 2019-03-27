(ns amoebas.util
    (:use amoebas.defs)
    
    (:require 
        [clojure.java.io :as io]
        [clojure.data.csv :as csv]
    )
)


(import '(java.awt.image BufferedImage))
(import '(javax.imageio ImageIO))

;;
;;  this namespace contains a collection of stuff that doesn't 
;;  fit anywhere else
;;


(defn bound
    "bounds the middle value by the other two; if it sits
     between them, returns the value itself, otherwise it
     returns the lower bound (the first parameter) if less than it,
     or the upper bound (the last parameter) if greater than it."
    [a x b]
    
    (cond
        (< x a)     a
        (< b x)     b
        :else       x
    )
)

(defn sum
    "sums all values in s"
    [s]
    
    (apply + s)
)

(defn population-stats
    "given a population p, which is a map from locations to amoebas,
     compute the subpopulation sizes for each species, 
     the total energy for each subpopulation,
     and the set of live species in the population"
     
    [p]
    
    (let
        [
            amoebas     (vals p)
            subpops     (group-by :species amoebas)
            species     (keys subpops)
            pop-sizes   (zipmap species (map #(count (subpops %)) species))
            energy-sums (zipmap species (map #(sum (map :energy (subpops %))) species))
        ]
        
        {:size pop-sizes :energy energy-sums :live-species species}
    )
)

(defn random-location 
    "produce a random location"
    []  
    
    [(rand-int WorldSize) (rand-int WorldSize)] 
)
    
    
    
;;
;;  writing images
;;
    
(defn get-png-imagewriter
    "get an image write for the PNG format"
    []
    
    (let 
        [ iterator (ImageIO/getImageWritersBySuffix "png") ]
        
        (if (.hasNext iterator) 
            (.next iterator)
            (throw (Exception. "No image writer for PNG"))
        )
    )
)
    
    
(defn create-directory 
    "create a directory at the specified path,
     unless one exists already"
    [dir]

    (let
        [ f (io/file dir) ]
        
        (when (not (.exists f))
            (.mkdir f)
        )
    )
)

(defn save-frame
    "save the bitmap representing the specified frame as an image 
     in the specfied directory, with a name generated from the round
     number gen; in PNG format"
    [frame dir gen]
    
    (let
        [
            content (.getContentPane frame)
            img     (BufferedImage. (.getWidth content) (.getHeight content) BufferedImage/TYPE_INT_ARGB)
        ]
        
        (.paint content (.getGraphics img))
        
        (ImageIO/write img "png" (io/file (format "%s/g%05d.png" dir gen)))
    )
)

;;
;;  writing stats
;;

(defn write-csv-stats
    "write two files containing the population sizes for each species 
     and the total energy for each round, as a CSV file"

    [dir species statsv]
    
    (with-open
        [w (io/writer (str dir "/popsize.csv"))]
        
        (csv/write-csv w (cons species (for [stat statsv] (for [s species] ((:size stat) s)))))
    )
        
    (with-open
        [w (io/writer (str dir "/energy.csv"))]
        
        (csv/write-csv w (cons species (for [stat statsv] (for [s species] ((:energy stat) s)))))
    )
)
            
            
            
            
            
            

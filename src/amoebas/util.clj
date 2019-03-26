(ns amoebas.util
    (:use amoebas.defs)
    
    (:require 
        [clojure.java.io :as io]
        [clojure.data.csv :as csv]
    )
)

(import '(java.awt.image BufferedImage))
(import '(javax.imageio ImageIO))

(defn bound
    [a x b]
    
    (cond
        (< x a)     a
        (< b x)     b
        :else       x
    )
)

(defn sum
    [s]
    
    (apply + s)
)

(defn population-stats
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

(defn random-location []  [(rand-int WorldSize) (rand-int WorldSize)] )
    
    
    
;;
;;  writing images
;;
    
(defn get-png-imagewriter
    []
    
    (let 
        [ iterator (ImageIO/getImageWritersBySuffix "png") ]
        
        (if (.hasNext iterator) 
            (.next iterator)
            (throw (Exception. "No image writer for PNG"))
        )
    )
)
    
    
(defn create-directory [dir]
    (let
        [ f (io/file dir) ]
        
        (when (not (.exists f))
            (.mkdir f)
        )
    )
)

(defn save-frame
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
            
            
            
            
            
            

(ns amoebas.display
    (:use amoebas.defs amoebas.simulation amoebas.util)
)

(import 
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))
 

;;
;;  this namespace contains stuff to create a simple graphic display of the state of an amoebas simulation
;;
 
(def CellSize           4)  ;; visual size of a cell, in pixels


(def FuelRGB          [0 255 0] )   ;; color value of a fully fueled cell; it's green.


(defn scale-rgb
    "scale RGB triple by a number from 0.0 to 1.0"
    [r rgb]
    
    (vec (for [c rgb] (bound 0 (int (* r c)) 255)))
)


(defn fill-cell 
    "fill cell with specified RGB value"
    [#^Graphics g x y rgb]
  
    (doto g
        (.setColor (new Color (rgb 0) (rgb 1) (rgb 2) 255) )
        (.fillRect (* x CellSize) (* y CellSize) CellSize CellSize)
    )
)

(defn draw-world
    "draw the world: scaled FuelRGB for empty cells, the color corresponding to the species
     for those that contain an amoeba"
    [#^Graphics g world population species-rgb]

    (doseq [x (range WorldSize) y (range WorldSize)]  
        (fill-cell g x y (scale-rgb (/ (cell-fuel world [x y]) MaxCellEnergy) FuelRGB))
    )

    (doseq [[[x y] a] population]
        (fill-cell g x y (species-rgb (:species a)))
    )
    
)
  
(defn render-world
    [g world population species-rgb]
    
    (let 
        [
            img     (new BufferedImage (* CellSize WorldSize) (* CellSize WorldSize) (. BufferedImage TYPE_INT_ARGB))
            bg      (. img (getGraphics))
        ]
    
    
        (doto bg
            (.setColor (. Color white))
            (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))

        (draw-world bg world population species-rgb)
    
        (.drawImage g img 0 0 nil)
        (. bg (dispose))
    )
)


(defn create-frame
    [name world popref species-rgb]
    
    (let
        [
            panel   (doto (proxy [JPanel] []  (paint [g] (render-world g world (deref popref) species-rgb)  ))    
                          (.setPreferredSize (new Dimension (* CellSize WorldSize) (* CellSize WorldSize))) )
        ]
        
        (doto (new JFrame) (.add panel) .pack .show)
    )
)



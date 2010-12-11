(ns highlife.goant (:require [clojure.contrib.math :as math]
			     [highlife.parameters :as params]
			     [highlife.coords :as coords]
			     [highlife.moves :as moves]
			     [highlife.moveactions :as mas]
			     [highlife.lib :as lib] :reload-all))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get local information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;just take the first occupier
(defn neighbor-whos [coord whowhere]
  (map #(first (get whowhere % [params/no-one])) (params/GET-NEIGHBOR-COORDS coord)))

(defn neighbor-whats [coord whatwhere]
  (map #(get-in whatwhere % params/oob-tile) (params/GET-NEIGHBOR-COORDS coord)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn blank-whats [coord] 0)

(defn pheremone-target [dim target]
  (let [max-distance (coords/distance-between [0 0] [dim dim])]
    (fn [coord] (- max-distance (coords/distance-between target coord)))))

(defn make-whatwhere [dim what-func]
  (vec (for [x (range dim)]
	 (vec (for [y (range dim)]
		(ref (what-func [x y])))))))

(defn print-whatwhere [whatwhere]
  (let [dim (count whatwhere)]
    (for [x (range dim)]
      (for [y (range dim)]
	(do (if (and (= x 0) (= y 0)) (print "          "))
	    (if (= y (dec dim))
	      (print (deref (get-in whatwhere [x y])) "\n")
	      (print (deref (get-in whatwhere [x y])) " | "))
	    (inc x))))))   


(defn reset []
  ;(def whatwhere (make-whatwhere params/DIM (pheremone-target params/DIM [7 5])))
  (def whatwhere (make-whatwhere params/DIM blank-whats))
  (def whowhere {[1 1] [(ref "loner")]})); [5 6] [(ref "a friend!")]})

(reset)  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make things happen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;has whatwhere side-effects
(defn get-moves-do-actions [whowhere]
  (map (fn [keyvalue]
         (let [[coord who] keyvalue
	       neighbors (neighbor-whos coord whowhere)
	       asdf (println "neighbors" neighbors)
	       neighbor-tiles (neighbor-whats coord whatwhere)
	       lkjd (println "heighbor-tiles" coord neighbor-tiles)
               [move action] (mas/explore who neighbors neighbor-tiles)
	       depr (println "lksadflajhlkga")]
           (action coord whatwhere)
	   (println "action done, now do move" move coord)
           (partial move coord)))
       whowhere))

(defn make-moves [whowhere]
  (println "make-moves")
  (reduce (fn [new-ww move] (move new-ww))
          whowhere
          (get-moves-do-actions whowhere)))

(defn step [num]
  (loop [ww whowhere
         gen num]
    (println gen ww)
    (println (print-whatwhere whatwhere))
    (if (= 0 gen)
      (def whowhere ww)
      (recur (make-moves ww) (dec gen)))))





	

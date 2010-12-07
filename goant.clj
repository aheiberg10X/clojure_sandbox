(ns highlife.goant (:require [clojure.contrib.math :as math]
			     [highlife.parameters :as params]
			     [highlife.coords :as coords]
			     [highlife.moves :as moves]
			     [highlife.moveactions :as mas]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get local information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;just take the first occupier
(defn neighbor-whos [coord whowhere]
  (map #(first (get whowhere %)) (params/GET-NEIGHBOR-COORDS coord)))

(defn neighbor-whats [coord whatwhere]
  (map #(get whatwhere %) (params/GET-NEIGHBOR-COORDS coord)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initial conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn blank-whats [coord] 0)

(def something-new 3)

(defn pheremone-target [dim target]
  (let [max-distance (coords/distance-between [0 0] [dim dim])]
    (fn [coord] (- max-distance (coords/distance-between target coord)))))

(defn make-whatwhere [dim what-func]
  (vec (for [x (range dim)]
	 (vec (for [y (range dim)]
		(ref (what-func [x y])))))))

;(defn make-whatwhere-with-target [dim target]
;  (let [p (partial (partial pheremore-target dim) target)]
;    (make-whatwhere dim p)))

(defn print-whatwhere [dim whatwhere]
  (let [coords (for [x (range dim) y (range dim)] [x y])]
    (map #(println % (get-in whatwhere % -1)) coords)))
					      

(def whatwhere (make-whatwhere DIM (pheremone-target DIM [7 5])))
;(def whatwhere (make-whatwhere DIM blank-whats))
;(def whowhere {[1 1] [(ref "loner")] [5 6] [(ref "a friend!")]})
(def whowhere {[5 6] [(ref "the dude")]})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make things happen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;has whatwhere side-effects
(defn get-moves-do-actions [whowhere]
  (map (fn [keyvalue]
         (let [[coord who] keyvalue
	       what (get whatwhere coord)
	       neighbors (neighbor-whos coord whowhere)
	       neighbor-tiles (neighbor-whats coord whatwhere)
               [move action] (mas/explore who what neighbors neighbor-tiles)]
           (action coord whatwhere)
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
    (println gen)
    (if (= 0 gen)
      (def whowhere ww)
      (recur (make-moves ww) (dec gen)))))





	

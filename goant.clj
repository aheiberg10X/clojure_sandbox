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

(defn print-whatwhere [whatwhere whowhere]
  (let [dim (count whatwhere)]
    (for [x (range dim)]
      (for [y (range dim)]
	(do (if (and (= x 0) (= y 0)) (print "          "))
	    (if (= y (dec dim))
	      (if (get whowhere [x y])
		(print "x\n")
		(print (deref (get-in whatwhere [x y])) "\n"))
	      (if (get whowhere [x y])
		(print "x | ")
		(print (deref (get-in whatwhere [x y])) "| ")))
	    "")))))   


(defn reset []
  ;(def whatwhere (make-whatwhere params/DIM (pheremone-target params/DIM [7 5])))
  (def whatwhere (make-whatwhere params/DIM blank-whats))
  (def whowhere {[6 5] [(ref "loner")]})); [9 9] [(ref "a friend!")]}))

(reset)  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make things happen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;perform the actions, sideeffects on whatwhere
;;execute the moves
(defn make-moves [whowhere whatwhere]
  (reduce (fn [new-ww move] (move new-ww)) whowhere
	  (map (fn [keyvalue]
		 (let [[coord who] keyvalue
		       neighbors (neighbor-whos coord whowhere)
		       their-tiles (neighbor-whats coord whatwhere)
		       [move action] (mas/walk-away-boundary who neighbors their-tiles)]
		   (action coord whatwhere)
		   (partial move coord)))
	       whowhere)))

(defn step [whowhere whatwhere num]
  (loop [ww whowhere
         gen 0]
    (println gen ww)
    (println (print-whatwhere whatwhere ww))
    (if (< gen num)
      (recur (make-moves ww whatwhere) (inc gen))
      (def whowhere ww))))

(defn move-until-target-found [whowhere whatwhere target cutoff]
  (loop [ww whowhere gen 0]
    (let [matches (filter (fn [x] (let [[coord ref] x] (= coord target))) ww)
	  found? (>= (count matches) 1)]
      ;;(println (print-whatwhere whatwhere ww))
      (if found?
	gen
	(if (< gen cutoff)
	  (recur (make-moves ww whatwhere) (inc gen))
	  cutoff)))))

(defn find-steps-seq [trials]
  (loop [i 1 acc []]
    (let [starting-coord (coords/make-random-coord params/DIM)
	  ;;der (println "starting coord" starting-coord)
	  whatwhere (make-whatwhere params/DIM blank-whats)
	  whowhere {starting-coord (ref "youknowtheguy")}
	  target-coord (coords/make-random-coord params/DIM)
	  ;;der (println "target-coord" target-coord)
	  cutoff (* params/DIM params/DIM 2)
	  steps (move-until-target-found whowhere whatwhere target-coord cutoff)]
	  ;;der (println "steps" steps)]
      (if (< i trials)
	(recur (inc i) (cons steps acc))
	(cons steps acc)))))

;;5000 trials
;; walk-around-boundary (weighted-choose): 60.1, 61.1 (52549 msec)
;;                      (first-positive ): 82.2 (32191 msec)
;; walk-away-boundary : 
;; explore : 62.7, 61.9 (60068 msec)
(defn avg [seq] (/ (apply + seq) (count seq)))

(defn count-visited [whatwhere]
  (reduce (fn [c n] (+ c (apply + (map mas/pheremone-level n)))) 0 whatwhere))





	

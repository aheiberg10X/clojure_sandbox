(ns highlife.goant (:require [clojure.contrib.math :as math]
			     [highlife.parameters :as params]
			     [highlife.coords :as coords]
			     [highlife.moves :as moves]
			     [highlife.moveactions :as mas]
			     [highlife.lib :as lib]
			     [highlife.prob :as prob]
                             [highlife.whostate :as ws]
			     :reload-all))

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


(defn make-whos-from-dist [ids move-actions dist]
  (if (= (count move-actions) (count dist))
    (take (count ids)
          (map (fn [id]
                 (ws/new-who (str id) (dist (rand))))
               ids))))

(defn expand-quota [quota]
  (loop [i 0
	 qt quota
	 expqt []]
    (if (= 0 (count qt))
      expqt
      (recur (inc i) (rest qt) (concat expqt (take (first qt) (repeat i)))))))

(defn make-whos-from-quota [ids move-actions quota]
  (if (= (count ids) (apply + quota)) 
    (if (= (count move-actions) (count quota))
      (take (count ids) (map (fn [tuple]
                          (let [[id ma-ix] tuple 
                                move-action (get move-actions ma-ix)]
                            (ws/new-who (str id) move-action)))
                        (lib/tupleize ids (expand-quota quota))))
      (println "number of move-actions != number of quotas"))
    (println "mismatch between moveaction assignments and population ")))

(defn make-whowhere [dim who-ids]
  (reduce (fn [ww who-id]
            (let [coord (coords/make-random-coord dim)]
              (loop [c coord]
                (if (get ww c) (recur (coords/make-random-coord dim)) (assoc ww c who-id)))))
          {}
          who-ids))

(defn make-whatwhere [dim what-func]
  (vec (for [x (range dim)]
	 (vec (for [y (range dim)]
		(ref (what-func [x y])))))))

(defn make-worldstate-ref [dim pop-size]
  (let [poss-mas [mas/explore]
        who-ids (take pop-size (iterate inc 0))
        whos (make-whos-from-quota who-ids poss-mas [pop-size])
        whowhere (make-whowhere dim who-ids)
        whatwhere (make-whatwhere dim blank-whats)]
    (ref {:whos whos :whowhere whowhere :whatwhere whatwhere})))

;; (defn make-whowhere [dim pop]
;;   (let [who-tuples (take pop (map #(ws/blank-who dim (str "dude" %)) (iterate inc 0)))]
;;     (reduce (fn [ww tuple]
;; 	      (let [[coord name] tuple]
;; 		(loop [c coord]
;; 		  (if (get ww c) (recur (coords/make-random-coord dim)) (assoc ww c name)))))
;; 	    {}
;;             who-tuples)))

;; (defn prob-make-who-moves-how [whowhere moveactions scores]
;;   (if (= (count moveactions) (count scores))
;;     (reduce (fn [move-map wwkv]
;; 	      (let [[coord whoref] wwkv
;; 		    chosen-move (get moveactions ((prob/make-dist scores) (rand)))]
;; 		(assoc move-map whoref chosen-move))) {} whowhere)

;;     (println "mismatch between moveactions and scores")))

;; ;;if we want to count up the actual distribution
;; (defn summarize-who-moves-how [who-moves-how]
;;   (reduce (fn [count-map wmhkv]
;; 	    (let [[who how] wmhkv]
;; 	      (assoc count-map how (inc (get count-map how 0))))) {} who-moves-how))


  
;; (defn make-who-moves-how [whowhere moveactions scores]
;;   (if (= (count whowhere) (apply + scores))
;;     (if (= (count moveactions) (count scores))
;;       (reduce (fn [move-map whoix]
;; 		(let [[who maix] whoix]
;; 		  (assoc move-map who (get moveactions maix))))
;; 	      {} (lib/tupleize (map second whowhere) (expand-scores scores)))
;;       (println "mismatch between moveactions and scores"))
;;     (println "move assignments don't line up with the number of whos")))

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

;; (defn reset []
;;   ;(def whatwhere (make-whatwhere params/DIM (pheremone-target params/DIM [7 5])))
;;   (def whatwhere (make-whatwhere params/DIM blank-whats))
;;   (def whowhere (make-whowhere params/DIM 1))
;;   (def who-moves-how (make-who-moves-how whowhere
;;   					 [mas/explore mas/walk-around-boundary mas/walk-away-boundary]
;;   					 [1 0 0])))


;; (reset)   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make things happen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;perform the actions, sideeffects on whatwhere
;;execute the moves
(defn make-moves [whowhere whatwhere who-moves-how]
  (reduce (fn [new-ww pending-move] (pending-move new-ww)) whowhere
	  (map (fn [keyvalue]
		 (let [[coord who] keyvalue
		       neighbors (neighbor-whos coord whowhere)
		       their-tiles (neighbor-whats coord whatwhere)
		       [move action] ((get who-moves-how who) who neighbors their-tiles)]
		   (action coord whatwhere)
		   (partial move coord)))
	       whowhere)))

(defn step [num]
  (loop [ww whowhere
         gen 0]
    (println gen ww)
    (println (print-whatwhere whatwhere ww))
    (if (< gen num)
      (recur (make-moves ww whatwhere who-moves-how) (inc gen))
      (def whowhere ww))))

(defn move-until-target-found [whowhere whatwhere who-moves-how target cutoff]
  (loop [ww whowhere gen 0]
    (let [matches (filter (fn [x] (let [[coord ref] x] (= coord target))) ww)
	  found? (>= (count matches) 1)]
      (if found?
	gen
	(if (< gen cutoff)
	  (recur (make-moves ww whatwhere who-moves-how) (inc gen))
	  cutoff)))))

(defn find-steps-seq [trials how-move]
  (loop [i 1 acc []]
    (let [starting-coord (coords/make-random-coord params/DIM)
	  ;;der (println "starting coord" starting-coord)
	  whatwhere (make-whatwhere params/DIM blank-whats)
	  whowhere (make-whowhere params/DIM (apply + how-move))
	  ;;adsf (println whowhere)
	  poss-moves [mas/explore mas/walk-around-boundary mas/walk-away-boundary]
	  who-moves-how (make-who-moves-how whowhere poss-moves how-move)
	  target-coord (coords/make-random-coord params/DIM)
	  ;;der (println "target-coord" target-coord)
	  cutoff (* params/DIM params/DIM 2)
	  steps (move-until-target-found whowhere whatwhere who-moves-how target-coord cutoff)]
	  ;;der (println "steps" steps)]
      (if (< i trials)
	(recur (inc i) (cons steps acc))
	(cons steps acc)))))

;;5000 trials
;; walk-around-boundary (weighted-choose): 60.1, 61.1 (52549 msec)
;;                      (first-positive ): 82.2 (32191 msec)
;; walk-away-boundary : 62.1 (55113 msec)
;; explore : 62.7, 61.9 (60068 msec)
(defn avg [seq] (time (/ (apply + seq) (count seq))))

(defn count-visited [whatwhere]
  (reduce (fn [c n] (+ c (apply + (map mas/pheremone-level n)))) 0 whatwhere))





	

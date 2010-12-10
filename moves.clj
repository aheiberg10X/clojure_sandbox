(ns highlife.moves (:require [highlife.parameters :as params]
		             [highlife.coords :as coords]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   move primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn move [oldcoord newcoord translator whowhere dim]
  (println "main move")
  (let [self (get whowhere oldcoord)
	translated (translator newcoord dim)]
    (if (and translated (not (= oldcoord translated)))
      (assoc (dissoc whowhere oldcoord)
             translated
             (concat self (get whowhere translated)))
      whowhere)))

;filter neighbors to only valid points?  
(defn get-moves [translator dim]
  (vec (map (fn [func]
	      (fn [coord whowhere]
		(println "can you see meeee?")
		(move coord (func coord) translator whowhere dim)))
	    (coords/neighbor-func-seq))))

(def moves (get-moves params/TRANSLATOR params/DIM))

(defn no-move [coord whowhere] whowhere)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   complex moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn which-side [point partition]
  (if (= point partition) 0 (if (< point partition) -1 1)))

(defn which-quadrant [coord target]
  (let [[x y] coord
	[a b] target
	xside (which-side x a)
	yside (which-side y b)]
    [xside yside]))

(def quadrant-move-map
     (apply hash-map (interleave [[1 1]  [1 0]  [1 -1]
                                  [0 1]  [0 0]  [0 -1]
                                  [-1 1] [-1 0] [-1 -1]] moves)))

(defn move-to-target [target coord whowhere]
  (let [quadrant (which-quadrant coord target)]
    ((get quadrant-move-map quadrant) coord whowhere)))
    
(defn random-move [coord whowhere]
  ((rand-nth moves) coord whowhere))

(defn make-distribution [percents]
  (if (not (= 100 (apply + percents)))
    (println "Error: percents don't add up to 100.")
    (if (not (= 9 (count percents)))
      (println "Error. Distribution needs 9 buckets.")
      (loop [dist []
             left 0
             percs percents]
        (let [perc (first percs)
              right (+ left perc)
              tail (rest percs)]
          (if (empty? tail)
            (conj dist [left right])
            (recur (conj dist [left right]) right tail)))))))

(def move-down-dist (make-distribution [0 0 0 0 0 50 0 50 0]))

(defn random-dist []
  (let [eight-rands (take 8 (repeatedly #(int (rand 100))))
        eight-sum ()]
    ))

(defn move-from-distribution [distribution coord whowhere]
  (loop [ix 0
         dist distribution
         random (rand 100)]
    (let [[l r] (first dist)
          tail (rest dist)]
      (if (and (>= random l) (< random r))
        ((nth moves ix) coord whowhere)
        (if (empty? tail)
          (println "distribution is messed, random not found in any interval")
          (recur (inc ix) tail random))))))



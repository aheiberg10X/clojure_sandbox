(ns highlife.moveactions (:require [clojure.contrib.math :as math]
			           [highlife.moves :as m]
				   [highlife.actions :as a]
				   [highlife.lib :as lib]
				   [highlife.parameters :as params]))

;move directly to a target
(defn move-to-44 [self neighbors]
  [(partial m/move-to-target [4 4]) a/no-action])

;;;;randomly pick a direction to move until you have found at least one neighbor
;;;;took out mytile from input, included in 'neighbors'
;; (defn find-someone [myself mytile neighbors tiles]
;;   (let [num-neighbors (reduce (fn [sum n] (+ sum (if n 1 0))) 0 neighbors)]
;;     (println "findsomeone" num-neighbors)
;;     (if (<= num-neighbors 1)
;;       ;(partial move-from-distribution move-down-dist)
;;       [m/random-move (a/lay-pheremone 1)] 
;;       [m/no-move a/no-action])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pheremone-level [tile-ref]
  (if (not (= tile-ref params/oob-tile)) @tile-ref -1))

(defn most-attractive-tile [tile-scoring-func tiles]
  (reduce (fn [acc new]
            (let [[maxix maxv curix] acc
                  newv (tile-scoring-func new)]                    
              (if (> newv maxv)
                [curix newv (inc curix)]
                [maxix maxv (inc curix)])))
          [0 0 0]
          tiles))

;;;;took out mytile, it's included in 'neighbors'
;; (defn follow-your-nose [myself mytile neighbors tiles]
;;   (let [mytile-sexiness @mytile
;;         [ix sexiness whocare] (most-attractive-tile pheremone-level tiles)]
;;     (if (> mytile-sexiness sexiness)
;;       [m/no-move a/no-action]
;;       [(nth m/moves ix) a/lay-pheremone])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-moves [neighbor-tiles]
  (let [tuples (map #(vector %1 %2) neighbor-tiles m/moves)]    
    (reduce (fn [valid-moves [tile tile-move]]
              (if (= 0 (pheremone-level tile))
	        (cons tile-move valid-moves)
	        valid-moves))
	    []
	    tuples)))

(defn random-valid-move [neighbor-tiles]
  (let [[valid-ixs ix]
	(reduce (fn [[good-ixs curix] tile]
		  (if (= 0 (pheremone-level tile))
		    [(cons curix good-ixs) (inc curix)]
		    [good-ixs (inc curix)]))
		[[] 0] neighbor-tiles)]
    (if (not (empty? valid-ixs))
      (get m/moves (rand-nth valid-ixs))
      m/no-move)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   scoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;tupleize unnecessary, we don't care about the whos
(defn explorer-scorer [neighbors their-tiles]
  (map (fn [tile]
	 (if (= tile params/oob-tile)
	   -1
	   (if (= 0 (pheremone-level tile)) 100 0)))
       their-tiles))


(defn boundary-map-cycle [tiles]
  (cycle (map (fn [x] ((partial m/which-side 0) (pheremone-level x))) tiles)))

;;each shingle represents a tile and its neighbors on either side
;;return -1 if tile OOB, 0 if tile is occupied, or a computed score if empty
(defn shingle-score [follow-boundary? shingle]
  (let [middle (second shingle)
	abssum (apply + (map math/abs shingle))
	switch-sum (if follow-boundary? abssum (- (count shingle) abssum))]
		                               ;;(if (> abssum 0) 0 1))]
    (if (= middle 0) switch-sum
	(if (= middle -1) -1 0))))

;;add 'dont move if someone there'
(defn boundary-scorer [neighbors their-tiles walk-around?]
  (loop [neig (take (+ 2 (count their-tiles)) (boundary-map-cycle their-tiles))
	 acc []]
    (let [shingle-len 3
	  score (shingle-score walk-around? (take shingle-len neig))]
      (if (= (count neig) shingle-len)
	(cons score acc)
	(recur (rest neig) (conj acc score))))))

(defn walk-around-boundary-scorer [neighbors their-tiles]
  (boundary-scorer neighbors their-tiles true))

(defn walk-away-boundary-scorer [neighbors their-tiles]
  (boundary-scorer neighbors their-tiles false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  choosing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn not-all-zero [sorted-scores]
  (let [no-pos-choice (every? #(<= % 0) sorted-scores)]
    (if no-pos-choice
      (map (fn [x] (if (< x 0) 0 1)) sorted-scores)
      (map (fn [x] (if (< x 0) 0 x)) sorted-scores))))

;;return function that takes 0 <= random <= 1 and returns 0 <= ix <= (count sorted-scores)
;;  this returned function will return an index i
;;  with P( sorted-scores[i] / SUM(sorted-scores) )
(defn make-dist [sorted-scores]
  (let [sorted-scores (not-all-zero sorted-scores)
	total (apply + sorted-scores)]
    (loop [prev-num 0
	   acc []
	   scores sorted-scores]
      (if (empty? scores)
	(fn [random]
	  (loop [s acc i 0]
	    (if (<= random (first s)) i (recur (rest s) (inc i)))))
	(let [new-num (+ prev-num (/ (first scores) total))]
	  (recur new-num (conj acc new-num) (rest scores)))))))
	
;; returns 0-8, to be 'i' in: (get m/moves i)
(defn top-x-weighted-choose [x scores]
  (let [scores-and-ixs (lib/two-tupleize scores (range (count scores)))
        sorted-tuples (reverse (sort-by #(first %) scores-and-ixs))
	sorted-scores (take x (map #(first %) sorted-tuples))
	sorted-ixs (vec (take x (map #(second %) sorted-tuples)))
	chosen ((make-dist sorted-scores) (rand))]
    (get sorted-ixs chosen)))

(defn test [num]
  (let [results {}]
    (loop [results {}
	   n num]
      (let [chix (top-x-weighted-choose 9 [1 1 0 1 1 0 1 0 0])
	    newcount (inc (get results chix 0))]
	(if (= 0 n)
	  results
	  (recur (assoc (dissoc results chix) chix newcount) (dec n)))))))

(defn first-positive-score [scores]
  (let [scores (not-all-zero scores)]
    (loop [s scores ix 0]
      (if (> (first s) 0) ix (recur (rest s) (inc ix))))))
      
  
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn explore [myself neighbors their-tiles]
  (let [scores (explorer-scorer neighbors their-tiles)
        chosen-ix (top-x-weighted-choose (count scores) scores)]            
    [(get m/moves chosen-ix) (a/lay-pheremone 1)]))

;;if possible move to an open space that borders an explored or off-map piece
(defn walk-around-boundary [myself neighbors their-tiles]
  (let [scores (walk-around-boundary-scorer neighbors their-tiles)
	;;chosen-ix (first-positive-score scores)]
	chosen-ix (top-x-weighted-choose (count scores) scores)]
    [(get m/moves chosen-ix) (a/lay-pheremone 1)]))

(defn walk-away-boundary [myself neighbors their-tiles]
  (let [scores (walk-away-boundary-scorer neighbors their-tiles)
	chosen-ix (top-x-weighted-choose (count scores) scores)]
    [(get m/moves chosen-ix) (a/lay-pheremone 1)]))




    
  

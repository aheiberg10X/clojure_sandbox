(ns highlife.moveactions (:require [highlife.moves :as m]
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
  (if tile-ref @tile-ref -1))

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

;(defn explore [myself neighbors their-tiles]
;  (let [scores (score-possible-moves explorer-scorer neighbors their-tiles)]
;    5))
    ;;pick from scores
    ;;get that index from m/moves, return along with (a/lay-pheremone 1)
    
					;[(random-valid-move neighbor-tiles) (a/lay-pheremone 1)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   scoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(defn score-possible-moves [scoring-func whos-around whats-around]
  (println "whos around" whos-around "what's around" whats-around)
  (map (fn [x] (scoring-func x)) (lib/tupleize whos-around whats-around)))

(defn explorer-scorer [whos-around whats-around]
  (map (fn [infotuple]
	 (let [[dude tile] infotuple]
	   (if (= tile params/oob-tile)
	     0
	     (if (= 0 (pheremone-level tile)) 100 1))))
       (lib/tupleize whos-around whats-around)))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  choosing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;return function that takes 0 <= random <= 1 and returns 0 <= ix <= (count sorted-scores)
;;  this returned function will return an index i
;;  with P( sorted-scores[i] / SUM(sorted-scores) )
(defn make-dist [sorted-scores]
  (let [all-positive (every? #(>= % 0) sorted-scores)
	total (apply + sorted-scores)
	sanity-check (and all-positive (> total 0))]
    (if sanity-check
      (loop [prev-num 0
	     acc []
	     scores sorted-scores]
	(if (empty? scores)
	  (fn [random]
	    (loop [s acc i 0]
	      (if (<= random (first s)) i (recur (rest s) (inc i)))))
	  (let [new-num (+ prev-num (/ (first scores) total))]
	    (recur new-num (conj acc new-num) (rest scores)))))
      (println "make-dist sanity check failed, sorted-scores:" sorted-scores))))
	
;; returns 0-8, to be 'i' in: (get m/moves i)
(defn top-x-weighted-choose [x scores]
  (let [scores-and-ixs (lib/tupleize scores (range (count scores)))
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
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn explore [myself neighbors their-tiles]
  (let [scores (explorer-scorer neighbors their-tiles)
	asdf (println "scores" scores)
        chosen-ix (top-x-weighted-choose (count scores) scores)]            
    [(get m/moves chosen-ix) (a/lay-pheremone 1)]))

;;if possible move to an open space that borders an explored or off-map piece
(defn walk-boundary [myself neighbors their-tiles] 0)




    
  

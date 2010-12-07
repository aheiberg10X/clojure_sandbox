(ns highlife.moveactions (:require [highlife.moves :as m]
				   [highlife.actions :as a]))

;move directly to a target
(defn move-to-44 [self neighbors]
  [(partial m/move-to-target [4 4]) actions/no-action])

;randomly pick a direction to move until you have found at least one neighbor
(defn find-someone [myself mytile neighbors tiles]
  (let [num-neighbors (reduce (fn [sum n] (+ sum (if n 1 0))) 0 neighbors)]
    (println "findsomeone" num-neighbors)
    (if (<= num-neighbors 1)
      ;(partial move-from-distribution move-down-dist)
      [m/random-move a/mark-explored] 
      [m/no-move a/no-action])))

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

(defn follow-your-nose [myself mytile neighbors tiles]
  (let [mytile-sexiness @mytile
        [ix sexiness whocare] (most-attractive-tile pheremone-level tiles)]
    (if (> mytile-sexiness sexiness)
      [m/no-move a/no-action]
      [(nth m/moves ix) a/lay-pheremone])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-moves [neighbor-tiles]
  (let [tuples (map #(vector %1 %2) neighbor-tiles m/moves)]
    (println tuples)
    (reduce (fn [valid-moves [tile tile-move]]
              (if (= 0 (pheremone-level tile))
	        (cons tile-move valid-moves)
	        valid-moves))
	    []
	    tuples)))  

(defn explore [myself mytile neighbors neighbor-tiles]
  (println "explore")
  [(rand-nth (valid-moves neighbor-tiles)) a/lay-pheremone])

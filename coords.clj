(ns highlife.coords)

(defn same [coord] coord)

(defn up [coord]
  (let [[x y] coord]
    [(dec x) y]))

(defn down [coord]
  (let [[x y] coord]
    [(inc x) y]))

(defn left [coord]
  (let [[x y] coord]
    [x (dec y)]))

(defn right [coord]
  (let [[x y] coord]
    [x (inc y)]))

(defn upleft [coord] (up (left coord)))

(defn upright [coord] (up (right coord)))

(defn downright [coord] (down (right coord)))

(defn downleft [coord] (down (left coord)))

(defn neighbor-func-seq []
  [upleft up upright right downright down downleft left])
  ;[upleft   up   upright
  ; left     same right
  ; downleft down downright])

(def no-move-ix 4)

(defn neighbor-coords [coord]
  (map #(% coord) (neighbor-func-seq)))

(defn coord-in-bounds? [coord dim]
  (let [[x y] coord]
    (if (or (= x dim) (= y dim) (< x 0) (< y 0))
      false
      true)))

(defn distance-between [coorda coordb]
  (let [[x y] coorda
        [p q] coordb]
    (+ (* (- x p) (- x p)) (* (- y q) (- y q)))))

;; (defn bounded-neighbor-coords [coord dim]
;;   (map #(if (coord-in-bounds? % dim)
;; 	  %
;; 	  nil) (neighbor-coords coord)))
  
(defn toroid-translate [coord dim]
  (map #(if (= % dim)
          0
          (if (< % 0)
            (dec dim)
	    %)) coord))

(defn bounded-translate [coord dim]
  (if (coord-in-bounds? coord dim)
    coord
    (vec (take (count coord) (repeat nil)))))

;(defn toroid-neighbor-coords [coord dim]
;  (map #(toroidify % dim) (neighbor-coords coord)))

(defn get-translated-neighbors [dim translator]
  (fn [coord] 
    (map #(translator % dim) (neighbor-coords coord))))
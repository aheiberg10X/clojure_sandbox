(defn tim []
  '((fn [x] (+ x 2))
    (fn [x] (+ x 4))
    (fn [x] (- x 1))))

(defn eric[]
  '((fn [x] (+ x 7))
    (fn [x] (- x 2))))

(defn eval-fitness [input output organisms]
   (map (fn [org]
	 (let [computed (reduce (fn [acc nf] ((eval nf) acc)) input (org))
	       diff (- output computed)]
	   (* diff diff)))
       organisms))

(def organisms [tim eric])

(defn run[]
  (eval-fitness 2 10 organisms))
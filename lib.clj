(ns highlife.lib)

;;group the items in multiple collections together
;;by their position
;;[1 2 3] [4 5 6] -> [[1 4] [2 5] [3 6]]
;;[nil 4] [1 2] -> [[nil 1] [4 2]]
;;[3 nil] [4 nil] -> [[3 4]]

(defn old-tupleize [& colls]
  (loop [acc []
	 c colls]
    (let [newtuple (vec (map first c))]
      (if (every? #(if % false true) newtuple)
	acc
	(recur (conj acc newtuple) (map rest c))))))

(defn generate-X-symbols [x]
  (let [letters "abcdefghijklmnopqrstuvwxyz"]
    (if (> x 26)
      (println "can't generate more than 26 symbols")
      (vec (map #(symbol (str %)) (take x letters))))))

;;heck yes so much faster than old-tupleize. and cleverer, my first macro
(defmacro tupleize [& colls]
  (let [syms (generate-X-symbols (count colls))] `(map (fn ~syms ~syms) ~@colls)))


(ns highlife.lib)

;;group the items in multiple collections together
;;by their position
;;[1 2 3] [4 5 6] -> [[1 4] [2 5] [3 6]]
;;[nil 4] [1 2] -> [[nil 1] [4 2]]
;;[3 nil] [4 nil] -> [[3 4]]
(defn tupleize [& colls]
  (loop [acc []
	 c colls]
    (let [newtuple (vec (map first c))]
      (if (every? #(if % false true) newtuple)
	acc
	(recur (conj acc newtuple) (map rest c))))))



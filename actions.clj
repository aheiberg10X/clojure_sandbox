(ns highlife.actions)

(defn no-action [coord whatwhere])

(defn lay-pheremone [strength]
  (fn [coord whatwhere]
    (dosync (alter (get-in whatwhere coord) + strength))))
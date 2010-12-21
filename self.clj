(ns highlife.self)

(defn get-married [self]
  (dosync (ref-set self "married!")))
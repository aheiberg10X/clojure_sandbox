(ns highlife.whostate (:require [highlife.coords :as coords]
                                [highlife.moves :as m]
                                [highlife.actions :as a]))

(defn blank-who [id] (ref {:id id :last-move nil :dna nil}))

(defn new-who [id dna] (ref {:id id :last-move nil :dna dna}))




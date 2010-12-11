(ns highlife.parameters (:require [highlife.coords :as coords]
				  [highlife.moves :as moves]))

(def DIM 5)
(def TRANSLATOR coords/bounded-translate)
(def GET-NEIGHBOR-COORDS (coords/get-translated-neighbors DIM TRANSLATOR))
(def oob-tile (ref "verboten"))
(def no-one (ref "knockknock"))
(ns highlife.parameters (:require [highlife.coords :as coords]
				  [highlife.moves :as moves]))

(def DIM 10)
(def TRANSLATOR coords/no-translate)
(def GET-NEIGHBOR-COORDS (coords/get-translated-neighbors DIM TRANSLATOR))

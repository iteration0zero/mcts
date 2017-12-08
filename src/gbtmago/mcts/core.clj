(ns gbtmago.mcts.core
  (:gen-class)
  (:require [clojure.core.async :as async :refer [chan go go-loop >! <! close! alt! alts! <!!]]))

(defn -main
  "I don't do a wholes lot ... yet."
  [& args]
  (println "Hello, World!"))

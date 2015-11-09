(ns inf4375.model-util
  (:gen-class))

(def inc-seq
  (atom 0))

(defn gen-id []
  (swap! inc-seq inc))

(defn stamp []
  (quot (System/currentTimeMillis) 1000))

(ns inf4375.model.tweet
  (:gen-class)
  (:require [inf4375.model-util :as util]))

(def tweets
  (atom {}))

(defn create [message]
  (let [id (util/gen-id)]
    (swap! tweets conj {id {:id id :message message}})
    id))

(defn all []
  @tweets)

(defn fetch [id]
  (get @tweets id))

(defn del [id]
  (swap! tweets dissoc id))